module View3d.EffectsRenderer exposing
    ( Mesh
    , backgroundEntity
    , convertMesh
    , entities
    )

import Array exposing (Array)
import Color exposing (Color)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe
import Point3d
import TriangularMesh exposing (TriangularMesh)
import Vector3d
import View3d.Camera as Camera
import View3d.Types as Types
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias VertexExtended =
    { position : Vec3
    , normal : Vec3
    , barycentric : Vec3
    }


type alias Mesh =
    WebGL.Mesh VertexExtended


type alias Uniforms =
    { sceneCenter : Vec3
    , sceneRadius : Float
    , color : Vec3
    , alpha : Float
    , fadeStrength : Float
    , blueShift : Float
    , wireStrength : Float
    , pushOut : Float
    , transform : Mat4
    , viewing : Mat4
    , perspective : Mat4
    }


type alias Varyings =
    { vpos : Vec3
    , vbary : Vec3
    }


extend : Types.Vertex units coords -> Float -> Float -> Float -> VertexExtended
extend v x y z =
    { position =
        v.position
            |> Point3d.unwrap
            |> Math.Vector3.fromRecord
    , normal =
        v.normal
            |> Vector3d.unwrap
            |> Math.Vector3.fromRecord
            |> Math.Vector3.normalize
    , barycentric = vec3 x y z
    }


convertMesh : TriangularMesh (Types.Vertex units coords) -> Mesh
convertMesh mesh =
    TriangularMesh.faceVertices mesh
        |> List.map (\( u, v, w ) -> ( w, u, v ))
        |> List.map
            (\( u, v, w ) ->
                ( extend u 1 0 0, extend v 1 1 0, extend w 1 0 1 )
            )
        |> WebGL.triangles


colorAsVec3 : Color -> Vec3
colorAsVec3 color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    vec3 red green blue


entities :
    Array Mesh
    -> Types.Model coords a
    -> Types.Options
    -> List WebGL.Entity
entities meshes model options =
    let
        radius =
            3 * model.radius

        perspective =
            if options.orthogonalView then
                Camera.orthogonalMatrix model.cameraState model.center radius

            else
                Camera.perspectiveMatrix model.cameraState model.center radius

        viewing =
            Camera.viewingMatrix model.cameraState

        drawFog =
            (options.fadeToBackground > 0)
                || (options.fadeToBlue > 0)
                || options.drawWires

        wireStrength =
            if options.drawWires then
                0.8

            else
                0.0

        uniforms =
            { sceneCenter = Mat4.transform viewing model.center
            , sceneRadius = model.radius
            , color = vec3 0 0 0
            , alpha = 1.0
            , fadeStrength = 0.5 * options.fadeToBackground
            , blueShift = options.fadeToBlue
            , wireStrength = wireStrength
            , pushOut = 0.0
            , transform = Mat4.identity
            , viewing = viewing
            , perspective = perspective
            }

        fogBlender =
            Blend.custom
                { r = 0
                , g = 0
                , b = 0
                , a = 0
                , color = Blend.customAdd Blend.srcAlpha Blend.oneMinusSrcAlpha
                , alpha = Blend.customAdd Blend.zero Blend.one
                }

        makeFog { transform } mesh =
            [ WebGL.entityWith
                [ fogBlender
                , DepthTest.default
                , WebGL.Settings.polygonOffset -1.0 -4.0

                -- TODO eliminate mirror transformations first
                --, WebGL.Settings.cullFace WebGL.Settings.back
                ]
                vertexShader
                fragmentShaderFog
                mesh
                { uniforms
                    | transform = transform
                    , color = colorAsVec3 options.backgroundColor
                }
            ]

        makeOutline { transform } mesh =
            [ WebGL.entityWith
                [ DepthTest.default
                , WebGL.Settings.cullFace WebGL.Settings.front
                ]
                vertexShader
                fragmentShaderConstant
                mesh
                { uniforms
                    | transform = transform
                    , color = colorAsVec3 options.outlineColor
                    , pushOut = 0.1 * options.outlineWidth
                }
            ]

        fogEntities =
            if not drawFog then
                []

            else
                model.scene
                    |> List.concatMap
                        (\(Types.Instance item) ->
                            Array.get item.idxMesh meshes
                                |> Maybe.map (makeFog item)
                                |> Maybe.withDefault []
                        )

        outlineEntities =
            if not options.addOutlines then
                []

            else
                model.scene
                    |> List.concatMap
                        (\(Types.Instance item) ->
                            Array.get item.idxMesh meshes
                                |> Maybe.map (makeOutline item)
                                |> Maybe.withDefault []
                        )
    in
    fogEntities ++ outlineEntities


backgroundEntity : Color -> WebGL.Entity
backgroundEntity color =
    let
        fullScreenQuadMesh =
            WebGL.triangleStrip
                [ { position = vec3 -1 -1 0 }
                , { position = vec3 1 -1 0 }
                , { position = vec3 -1 1 0 }
                , { position = vec3 1 1 0 }
                ]
    in
    WebGL.entityWith
        []
        vertexShaderTrivial
        fragmentShaderConstant
        fullScreenQuadMesh
        { color = colorAsVec3 color }


vertexShaderTrivial :
    WebGL.Shader
        { a | position : Vec3 }
        { a | color : Vec3 }
        Varyings
vertexShaderTrivial =
    [glsl|
        precision lowp float;
        attribute vec3 position;
        varying vec3 vpos;
        varying vec3 vbary;

        void main() {
            gl_Position = vec4(position, 1.0);
        }
    |]


vertexShader : WebGL.Shader VertexExtended Uniforms Varyings
vertexShader =
    [glsl|

    attribute vec3 position;
    attribute vec3 normal;
    attribute vec3 barycentric;
    uniform mat4 transform;
    uniform mat4 viewing;
    uniform mat4 perspective;
    uniform float pushOut;
    varying vec3 vpos;
    varying vec3 vbary;

    void main () {
        vec3 normal = normalize((viewing * transform * vec4(normal, 0.0)).xyz);
        vpos = (viewing * transform * vec4(position, 1.0)).xyz
            + pushOut * normal;
        vbary = barycentric;
        gl_Position = perspective * vec4(vpos, 1.0);
    }

    |]


fragmentShaderConstant : WebGL.Shader {} { a | color : Vec3 } Varyings
fragmentShaderConstant =
    [glsl|

    precision mediump float;
    uniform vec3 color;
    varying vec3 vpos;
    varying vec3 vbary;

    void main () {
        gl_FragColor = vec4(color, 1);
    }

    |]


fragmentShaderFog : WebGL.Shader {} Uniforms Varyings
fragmentShaderFog =
    [glsl|

    precision mediump float;
    uniform vec3 sceneCenter;
    uniform float sceneRadius;
    uniform vec3 color;
    uniform float fadeStrength;
    uniform float blueShift;
    uniform float wireStrength;
    varying vec3 vpos;
    varying vec3 vbary;

    void main () {
        float depth = (sceneCenter - vpos).z;
        float coeff = smoothstep(-0.9 * sceneRadius, 1.1 * sceneRadius, depth);

        // fade to blue

        float t = 0.0;
        float alpha = blueShift > 0.0 ? pow(coeff, 1.0 / blueShift) : 0.0;
        float beta = 0.0;
        vec3 colorOut = vec3(0.0, 0.0, 1.0);

        // fade to background

        t = fadeStrength > 0.0 ? pow(coeff, 1.0 / fadeStrength) : 0.0;
        alpha = t + alpha - t * alpha;
        beta = alpha > 0.0 ? t / alpha : 0.0;
        colorOut = beta * color + (1.0 - beta) * colorOut;

        // add wireframes

        vec3 delta = abs(dFdx(vbary)) + abs(dFdy(vbary));
        vec3 bary = smoothstep(0.5 * delta, 1.5 * delta, vbary);

        t = wireStrength * (1.0 - min(bary.x, min(bary.y, bary.z)));
        alpha = t + alpha - t * alpha;
        beta = alpha > 0.0 ? t / alpha : 0.0;
        colorOut = beta * vec3(0.0, 0.0, 0.0) + (1.0 - beta) * colorOut;

        // output

        gl_FragColor = vec4(colorOut, alpha);
    }

    |]
