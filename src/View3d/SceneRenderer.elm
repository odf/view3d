module View3d.SceneRenderer exposing
    ( Mesh
    , convertMesh
    , entities
    )

import Angle
import Array exposing (Array)
import Camera3d
import Color
import Direction3d
import Illuminance
import Length
import LineSegment3d
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Maybe
import Point3d
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh
import Set
import TriangularMesh exposing (TriangularMesh)
import View3d.Camera as Camera
import View3d.SimilarityTransform as Similarity exposing (SimilarityTransform)
import View3d.Types as Types
import Viewpoint3d
import WebGL


type alias Mesh coords =
    Types.SceneMesh coords


convertMesh : TriangularMesh (Types.Vertex coords) -> Mesh coords
convertMesh mesh =
    let
        surface =
            Scene3d.Mesh.indexedFaces mesh
    in
    { surface = surface
    , shadow = Scene3d.Mesh.shadow surface
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


makeMaterial :
    Types.Material
    -> Bool
    -> Material coords { a | normals : () }
makeMaterial material highlight =
    if highlight then
        Material.matte Color.red

    else
        Material.pbr
            { baseColor = material.color
            , roughness = material.roughness
            , metallic = material.metallic
            }


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


apply :
    SimilarityTransform coords
    -> Scene3d.Entity coords
    -> Scene3d.Entity coords
apply transform entity =
    entity
        |> Scene3d.scaleAbout Point3d.origin (Similarity.scale transform)
        |> Scene3d.placeIn (Similarity.frame transform)


sceneLights : Types.Options -> Scene3d.Lights coords
sceneLights options =
    let
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
    in
    Scene3d.threeLights sun sky environment


entities : Types.Model coords b -> Types.Options -> List WebGL.Entity
entities model options =
    let
        makeEntity index (Types.Instance { material, transform, mesh }) =
            let
                mOut =
                    makeMaterial material (Set.member index model.selected)

                surface =
                    if options.drawShadows then
                        Scene3d.meshWithShadow mOut
                            mesh.scene.surface
                            mesh.scene.shadow

                    else
                        Scene3d.mesh mOut mesh.scene.surface
            in
            apply transform surface

        viewing =
            Camera.viewingMatrix model.cameraState

        sceneGroup =
            model.scene
                |> List.indexedMap makeEntity
                |> Scene3d.group
                |> apply (Similarity.fromMatrix viewing)

        sceneCenter =
            Mat4.transform viewing model.center

        boxWidth =
            6 * model.radius

        dummyBox =
            wireframeBox sceneCenter boxWidth boxWidth boxWidth
                |> Scene3d.mesh (Material.color options.backgroundColor)
    in
    Scene3d.toWebGLEntities
        { lights = sceneLights options
        , camera = convertCamera model.cameraState options
        , clipDepth = Length.meters 0.5
        , exposure = Scene3d.exposureValue 15
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , aspectRatio = model.size.width / model.size.height
        , supersampling = 1
        , entities = [ sceneGroup, dummyBox ]
        }
