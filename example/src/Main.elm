module Main exposing (main)

import Array
import Browser
import Color
import Dict
import Html
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3)
import Mesh
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import View3d


type alias Flags =
    {}


type alias Model =
    View3d.Model


type alias Msg =
    View3d.Msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( meshes, instances ) =
            geometry flags

        model =
            View3d.init
                |> View3d.setSize { width = 768, height = 768 }
                |> View3d.setScene (Just meshes) instances
                |> View3d.encompass
    in
    ( model, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    let
        defaults =
            View3d.defaultOptions

        options =
            { defaults | drawWires = False }
    in
    Html.div [] [ View3d.view identity model options ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, _ ) =
            View3d.update msg model
    in
    ( newModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    View3d.subscriptions identity model



-- Geometry


geometry : Flags -> ( List View3d.Mesh, List View3d.Instance )
geometry _ =
    cylinder 0.2 1.0 48


cylinder : Float -> Float -> Int -> ( List View3d.Mesh, List View3d.Instance )
cylinder radius length nrSegments =
    let
        a =
            2 * pi / toFloat nrSegments

        d =
            radius * 0.1

        pos u r z =
            Point3d.meters
                (r * cos (a * toFloat u))
                (r * sin (a * toFloat u))
                z

        position u v =
            case v of
                0 ->
                    Point3d.meters 0 0 0

                1 ->
                    pos u (radius - d) 0

                2 ->
                    pos u (radius - d / 2) 0

                3 ->
                    pos u radius (d / 2)

                4 ->
                    pos u radius d

                5 ->
                    pos u radius (length - d)

                6 ->
                    pos u radius (length - d / 2)

                7 ->
                    pos u (radius - d / 2) length

                8 ->
                    pos u (radius - d) length

                _ ->
                    Point3d.meters 0 0 length

        mesh =
            Mesh.indexedBall nrSegments 9 position
                |> convertMesh

        inst =
            { material =
                { color = Color.hsl 0.13 0.9 0.7
                , roughness = 0.5
                , metallic = 0.1
                }
            , transform = Mat4.identity
            , idxMesh = 0
            , idxInstance = 0
            }
    in
    ( [ mesh ], [ inst ] )


ball : ( List View3d.Mesh, List View3d.Instance )
ball =
    let
        positions =
            [ ( ( 0, 0 ), Point3d.meters 0 0 -1 )
            , ( ( 0, 1 ), Point3d.meters 1 0 0 )
            , ( ( 1, 1 ), Point3d.meters 0 1 0 )
            , ( ( 2, 1 ), Point3d.meters -1 0 0 )
            , ( ( 3, 1 ), Point3d.meters 0 -1 0 )
            , ( ( 0, 2 ), Point3d.meters 0 0 1 )
            ]
                |> Dict.fromList

        getPosition =
            flip Dict.get positions
                >> Maybe.withDefault (Point3d.meters 0 0 0)

        subD =
            Mesh.subdivideSmoothly (always False) identity (always identity)

        mesh =
            Mesh.indexedBall 4 2 Tuple.pair
                |> Mesh.mapVertices getPosition
                |> subD
                |> subD
                |> subD
                |> convertMesh

        inst =
            { material =
                { color = Color.hsl 0.0 0.6 0.5
                , roughness = 0.5
                , metallic = 0.1
                }
            , transform = Mat4.identity
            , idxMesh = 0
            , idxInstance = 0
            }
    in
    ( [ mesh ], [ inst ] )


convertMesh : Mesh.Mesh (Point3d units coords) -> View3d.Mesh
convertMesh meshIn =
    let
        makeVertex point normal =
            { position = pointToVec3 point
            , normal = normalToVec3 normal
            }

        mesh =
            Mesh.withNormals identity makeVertex meshIn

        verts =
            Mesh.vertices mesh |> Array.toList

        faces =
            Mesh.faceIndices mesh
    in
    View3d.surface verts faces


pointToVec3 : Point3d units coordinates -> Vec3
pointToVec3 point =
    Math.Vector3.fromRecord (Point3d.unwrap point)


normalToVec3 : Vector3d units coordinates -> Vec3
normalToVec3 normal =
    Math.Vector3.fromRecord (Vector3d.unwrap normal)


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
