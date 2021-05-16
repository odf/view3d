module View3d.SceneRenderer exposing
    ( Mesh
    , convertMesh
    , entities
    )

import Angle
import Array exposing (Array)
import Axis3d
import Camera3d
import Color
import Direction3d
import Illuminance
import Length exposing (Meters)
import LineSegment3d
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d
import View3d.Camera as Camera
import View3d.Types as Types
import Viewpoint3d
import WebGL


type alias Mesh coords =
    { surface : Scene3d.Mesh.Uniform coords
    , shadow : Scene3d.Mesh.Shadow coords
    }


inMeters : Point3d units coords -> Point3d Meters coords
inMeters =
    Point3d.unwrap >> Point3d.unsafe


asPointInMeters : Vec3 -> Point3d Meters coords
asPointInMeters p =
    Point3d.meters (Vec3.getX p) (Vec3.getY p) (Vec3.getZ p)


convertSurface :
    TriangularMesh (Types.Vertex units coords)
    -> Scene3d.Mesh.Uniform coords
convertSurface mesh =
    let
        verts =
            TriangularMesh.vertices mesh
                |> Array.map
                    (\v ->
                        { position = inMeters v.position
                        , normal = v.normal
                        }
                    )
    in
    TriangularMesh.indexed verts (TriangularMesh.faceIndices mesh)
        |> Scene3d.Mesh.indexedFaces


convertMesh : TriangularMesh (Types.Vertex units coords) -> Mesh coords
convertMesh mesh =
    let
        surface =
            convertSurface mesh

        shadow =
            Scene3d.Mesh.shadow surface
    in
    { surface = surface
    , shadow = shadow
    }


convertCamera :
    Camera.State
    -> Types.Options
    -> Camera3d.Camera3d Length.Meters coords
convertCamera camState options =
    let
        focalPoint =
            Point3d.meters 0 0 -(Camera.cameraDistance camState)

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = focalPoint
                , eyePoint = Point3d.origin
                , upDirection = Direction3d.positiveY
                }

        fovy =
            Camera.verticalFieldOfView camState |> Angle.degrees

        height =
            Camera.viewPortHeight camState |> Length.meters
    in
    if options.orthogonalView then
        Camera3d.orthographic
            { viewpoint = viewpoint, viewportHeight = height }

    else
        Camera3d.perspective
            { viewpoint = viewpoint, verticalFieldOfView = fovy }


orthonormalized : Mat4 -> Mat4
orthonormalized mat =
    let
        project a b =
            Vec3.scale (Vec3.dot a b) b

        u =
            vec3 1 0 0 |> Mat4.transform mat |> Vec3.normalize

        v0 =
            vec3 0 1 0 |> Mat4.transform mat

        v =
            project v0 u
                |> Vec3.sub v0
                |> Vec3.normalize

        w0 =
            vec3 0 0 1 |> Mat4.transform mat

        w =
            Vec3.add (project w0 u) (project w0 v)
                |> Vec3.sub w0
                |> Vec3.normalize
    in
    Mat4.makeBasis u v w


determinant3d : Mat4 -> Float
determinant3d mat =
    Vec3.dot
        (Mat4.transform mat <| vec3 1 0 0)
        (Vec3.cross
            (Mat4.transform mat <| vec3 0 1 0)
            (Mat4.transform mat <| vec3 0 0 1)
        )


longestTwo : Vec3 -> Vec3 -> Vec3 -> ( Vec3, Vec3 )
longestTwo u v w =
    let
        ( a, b ) =
            if Vec3.length u >= Vec3.length v then
                if Vec3.length v >= Vec3.length w then
                    ( u, v )

                else
                    ( u, w )

            else if Vec3.length u >= Vec3.length w then
                ( u, v )

            else
                ( v, w )
    in
    if Vec3.length a >= Vec3.length b then
        ( a, b )

    else
        ( b, a )


sign : number -> number
sign n =
    if n < 0 then
        -1

    else
        1


analyzeRotation :
    Mat4
    -> { axis : Axis3d.Axis3d Meters coords, angle : Angle.Angle }
analyzeRotation mat =
    let
        moved vec =
            Mat4.transform mat vec |> Vec3.sub vec

        ( v, w ) =
            longestTwo (moved Vec3.i) (moved Vec3.j) (moved Vec3.k)
    in
    if Vec3.length v < 1.0e-3 then
        { axis = Axis3d.x, angle = Angle.degrees 0 }

    else
        let
            n =
                Vec3.cross v w |> Vec3.normalize

            axis =
                n
                    |> asPointInMeters
                    |> Axis3d.throughPoints Point3d.origin
                    |> Maybe.withDefault Axis3d.x

            a =
                Vec3.normalize v

            b =
                Mat4.transform mat a |> Vec3.normalize

            c =
                Vec3.cross a b

            angle =
                if Vec3.length c < 1.0e-3 then
                    pi

                else
                    sign (Vec3.dot c n) * acos (Vec3.dot a b)
        in
        { axis = axis, angle = Angle.radians angle }


applySimilarityMatrix : Mat4 -> Scene3d.Entity coords -> Scene3d.Entity coords
applySimilarityMatrix matrix entity =
    let
        shift =
            vec3 0 0 0
                |> Mat4.transform matrix

        shiftVector =
            shift
                |> asPointInMeters
                |> Vector3d.from Point3d.origin

        mat1 =
            Mat4.mul (Mat4.makeTranslate (Vec3.negate shift)) matrix

        det =
            determinant3d mat1

        scale =
            sign det * (abs det ^ (1 / 3))

        { axis, angle } =
            Mat4.scale3 (1 / scale) (1 / scale) (1 / scale) mat1
                |> orthonormalized
                |> analyzeRotation
    in
    entity
        |> Scene3d.scaleAbout Point3d.origin scale
        |> Scene3d.rotateAround axis angle
        |> Scene3d.translateBy shiftVector


wireframeBox : Vec3 -> Float -> Float -> Float -> Scene3d.Mesh.Plain coords
wireframeBox center dimX dimY dimZ =
    let
        segment xa ya za xb yb zb =
            LineSegment3d.from
                (Point3d.meters xa ya za)
                (Point3d.meters xb yb zb)

        { x, y, z } =
            Vec3.toRecord center

        rx =
            dimX / 2

        ry =
            dimY / 2

        rz =
            dimZ / 2
    in
    Scene3d.Mesh.lineSegments
        [ segment (x - rx) (y - ry) (z - rz) (x + rx) (y - ry) (z - rz)
        , segment (x - rx) (y - ry) (z + rz) (x + rx) (y - ry) (z + rz)
        , segment (x - rx) (y + ry) (z - rz) (x + rx) (y + ry) (z - rz)
        , segment (x - rx) (y + ry) (z + rz) (x + rx) (y + ry) (z + rz)
        , segment (x - rx) (y - ry) (z - rz) (x - rx) (y + ry) (z - rz)
        , segment (x - rx) (y - ry) (z + rz) (x - rx) (y + ry) (z + rz)
        , segment (x + rx) (y - ry) (z - rz) (x + rx) (y + ry) (z - rz)
        , segment (x + rx) (y - ry) (z + rz) (x + rx) (y + ry) (z + rz)
        , segment (x - rx) (y - ry) (z - rz) (x - rx) (y - ry) (z + rz)
        , segment (x - rx) (y + ry) (z - rz) (x - rx) (y + ry) (z + rz)
        , segment (x + rx) (y - ry) (z - rz) (x + rx) (y - ry) (z + rz)
        , segment (x + rx) (y + ry) (z - rz) (x + rx) (y + ry) (z + rz)
        ]


entities :
    Array (Mesh coords)
    -> Types.Model a
    -> Types.Options
    -> List WebGL.Entity
entities meshes model options =
    let
        convert index { material, transform } mesh =
            let
                highlight =
                    Set.member index model.selected

                mOut =
                    if highlight then
                        Material.matte Color.red

                    else
                        Material.pbr
                            { baseColor = material.color
                            , roughness = material.roughness
                            , metallic = material.metallic
                            }

                surface =
                    if options.drawShadows then
                        Scene3d.meshWithShadow mOut mesh.surface mesh.shadow

                    else
                        Scene3d.mesh mOut mesh.surface
            in
            surface
                |> applySimilarityMatrix transform

        viewing =
            Camera.viewingMatrix model.cameraState

        sceneGroup =
            model.scene
                |> List.indexedMap
                    (\index (Types.Instance item) ->
                        Array.get item.idxMesh meshes
                            |> Maybe.map (convert index item)
                            |> Maybe.withDefault Scene3d.nothing
                    )
                |> Scene3d.group
                |> applySimilarityMatrix viewing

        sceneCenter =
            Mat4.transform viewing model.center

        boxWidth =
            6 * model.radius

        dummyBox =
            wireframeBox sceneCenter boxWidth boxWidth boxWidth
                |> Scene3d.mesh (Material.color options.backgroundColor)

        sun =
            Light.directional (Light.castsShadows options.drawShadows)
                { direction = Direction3d.yz (Angle.degrees -120)
                , intensity = Illuminance.lux 80000
                , chromaticity = Light.sunlight
                }

        sky =
            Light.overhead
                { upDirection = Direction3d.z
                , chromaticity = Light.skylight
                , intensity = Illuminance.lux 30000
                }

        environment =
            Light.overhead
                { upDirection = Direction3d.reverse Direction3d.z
                , chromaticity = Light.daylight
                , intensity = Illuminance.lux 5000
                }

        lights =
            Scene3d.threeLights sun sky environment
    in
    Scene3d.toWebGLEntities
        { lights = lights
        , camera = convertCamera model.cameraState options
        , clipDepth = Length.meters 0.5
        , exposure = Scene3d.exposureValue 15
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , aspectRatio = model.size.width / model.size.height
        , supersampling = 1
        , entities = [ sceneGroup, dummyBox ]
        }
