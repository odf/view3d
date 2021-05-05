module Main exposing (main)

import Array
import Browser
import Color
import Dict
import Html
import Length
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (Vec3)
import Mesh
import Plane3d
import Point3d exposing (Point3d)
import TriangularMesh exposing (TriangularMesh)
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


geometry :
    Flags ->
    ( List (TriangularMesh View3d.Vertex), List View3d.Instance )
geometry _ =
    let
        mesh =
            cylinder 0.2 1.0 48

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


cylinder : Float -> Float -> Int -> TriangularMesh View3d.Vertex
cylinder radius length nrSegments =
    let
        d =
            radius * 0.1

        angle u =
            toFloat u * 2 * pi / toFloat nrSegments

        radial a r z =
            Point3d.meters (r * cos a) (r * sin a) z

        midplane =
            Plane3d.xy |> Plane3d.offsetBy (Length.meters (length / 2))

        position u v =
            if v > 4 then
                position u (9 - v) |> Point3d.mirrorAcross midplane

            else
                case v of
                    0 ->
                        radial (angle u) 0 0

                    1 ->
                        radial (angle u) (radius - d) 0

                    2 ->
                        radial (angle u) (radius - d / 2) 0

                    3 ->
                        radial (angle u) radius (d / 2)

                    _ ->
                        radial (angle u) radius d
    in
    Mesh.indexedBall nrSegments 9 position |> convertMesh


ball : TriangularMesh View3d.Vertex
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
    in
    Mesh.indexedBall 4 2 Tuple.pair
        |> Mesh.mapVertices getPosition
        |> subD
        |> subD
        |> subD
        |> convertMesh


convertMesh : Mesh.Mesh (Point3d units coords) -> TriangularMesh View3d.Vertex
convertMesh meshIn =
    let
        makeVertex point normal =
            { position = pointToVec3 point
            , normal = normalToVec3 normal
            }
    in
    Mesh.withNormals identity makeVertex meshIn |> Mesh.toTriangularMesh


pointToVec3 : Point3d units coordinates -> Vec3
pointToVec3 point =
    Math.Vector3.fromRecord (Point3d.unwrap point)


normalToVec3 : Vector3d units coordinates -> Vec3
normalToVec3 normal =
    Math.Vector3.fromRecord (Vector3d.unwrap normal)


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
