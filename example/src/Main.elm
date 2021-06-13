module Main exposing (main)

import Angle
import Axis3d
import Browser
import Color
import Html
import Length
import Mesh
import Point3d exposing (Point3d)
import Set
import Vector3d
import View3d
import View3d.Instance as Instance exposing (Instance)


type alias Flags =
    {}


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    View3d.Model WorldCoordinates


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
        model =
            View3d.init
                |> View3d.setSize { width = 768, height = 768 }
                |> View3d.setScene (geometry flags)
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


geometry : Flags -> List (Instance coords)
geometry _ =
    let
        material i =
            case i of
                0 ->
                    { color = Color.hsl 0.13 0.9 0.7
                    , roughness = 0.5
                    , metallic = 0.1
                    }

                _ ->
                    { color = Color.hsl 0.0 0.6 0.5
                    , roughness = 0.5
                    , metallic = 0.1
                    }

        inst i mesh =
            Instance.make (material i) mesh
                |> Instance.scaleAbout
                    (Point3d.meters 0.9 0.9 0.1)
                    (1 + toFloat i)
                |> Instance.rotateAround Axis3d.y (Angle.degrees -45.0)
    in
    List.indexedMap inst sheet


sheet : List (View3d.Mesh coords)
sheet =
    let
        makeVertex u v =
            let
                a =
                    v * 2 * pi

                r =
                    u + 1
            in
            Point3d.meters (r * cos a) (r * sin a) 0

        subD =
            Mesh.subdivideSmoothly (always False) identity (always identity)

        pushVertex ( point, normal, tag ) =
            ( Point3d.translateBy
                (Vector3d.scaleTo (Length.meters 0.2) normal)
                point
            , normal
            , tag + 1
            )

        checkTags predFn face =
            List.map (\( _, _, t ) -> t) face
                |> Set.fromList
                |> Set.size
                |> predFn

        baseMesh =
            Mesh.tube 1 4 makeVertex
                |> subD
                |> subD
                |> subD
                |> Mesh.withNormals identity (\p n -> ( p, n, 0 ))
                |> Mesh.extrude pushVertex
    in
    List.map
        (\predFn ->
            baseMesh
                |> Mesh.filterFaces (checkTags predFn)
                |> Mesh.mapVertices (\( p, _, _ ) -> p)
                |> subD
                |> convertMesh
        )
        [ (==) 1, (==) 2 ]


convertMesh : Mesh.Mesh (Point3d Length.Meters coords) -> View3d.Mesh coords
convertMesh meshIn =
    let
        makeVertex position normal =
            { position = position, normal = normal }
    in
    Mesh.withNormals identity makeVertex meshIn
        |> Mesh.toTriangularMesh
        |> View3d.mesh
