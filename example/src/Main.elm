module Main exposing (main)

import Array
import Browser
import Color
import Dict
import Html
import Length
import Math.Matrix4 as Mat4
import Math.Vector3
import Mesh
import Point3d exposing (Point3d)
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d
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
    Flags
    -> ( List (TriangularMesh View3d.Vertex), List View3d.Instance )
geometry _ =
    let
        meshes =
            sheet

        color i =
            case i of
                0 -> Color.hsl 0.13 0.9 0.7
                _ -> Color.hsl 0.0 0.6 0.5

        inst i =
            { material =
                { color = color i
                , roughness = 0.5
                , metallic = 0.1
                }
            , transform = Mat4.identity
            , idxMesh = i
            , idxInstance = 0
            }
    in
    ( meshes, [ inst 0, inst 1] )


sheet : List (TriangularMesh View3d.Vertex)
sheet =
    let
        makeVertex u v =
            let
                a =
                    v * 2 * pi

                r =
                    u + 1
            in
            Point3d.meters (r * cos a) (r * sin a) (u * cos a)

        subD =
            Mesh.subdivideSmoothly (always False) identity (always identity)

        pushVertex ( point, normal, tag ) =
            ( Point3d.translateBy
                (Vector3d.scaleTo (Length.meters 0.2) normal)
                point
            , normal
            , tag + 1
            )

        checkTags predFn face verts =
            List.filterMap (flip Array.get verts) face
                |> List.map (\( _, _, t ) -> t)
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
                |> extractFaces (checkTags predFn)
                |> Mesh.mapVertices (\( p, _, _ ) -> p)
                |> subD
                |> convertMesh
        )
        [ (==) 1, (==) 2 ]


extractFaces :
    (List Int -> Array.Array vertex -> Bool)
    -> Mesh.Mesh vertex
    -> Mesh.Mesh vertex
extractFaces testFace mesh =
    let
        verts =
            Mesh.vertices mesh

        goodFaces =
            Mesh.faceIndices mesh |> List.filter (flip testFace verts)

        keptIndices =
            List.concat goodFaces |> Set.fromList

        indexMap =
            Set.toList keptIndices
                |> List.indexedMap (\i v -> ( v, i ))
                |> Dict.fromList

        vertsOut =
            List.range 0 (Array.length verts - 1)
                |> List.filter (flip Set.member keptIndices)
                |> List.filterMap (flip Array.get verts)
                |> Array.fromList
    in
    Mesh.fromOrientedFaces
        vertsOut
        (List.map (List.filterMap (flip Dict.get indexMap)) goodFaces)
        |> Result.withDefault Mesh.empty


convertMesh : Mesh.Mesh (Point3d units coords) -> TriangularMesh View3d.Vertex
convertMesh meshIn =
    let
        makeVertex point normal =
            { position = Math.Vector3.fromRecord (Point3d.unwrap point)
            , normal = Math.Vector3.fromRecord (Vector3d.unwrap normal)
            }
    in
    Mesh.withNormals identity makeVertex meshIn |> Mesh.toTriangularMesh


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x
