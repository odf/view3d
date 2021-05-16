module View3d.Picker exposing
    ( Mesh
    , convertMesh
    , pick
    )

import Array exposing (Array)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Point3d
import TriangularMesh exposing (TriangularMesh)
import View3d.Camera as Camera
import View3d.Types as Types exposing (Instance)


type alias Mesh =
    { centroid : Vec3
    , radius : Float
    , triangles : List ( Vec3, Vec3, Vec3 )
    }


convertMesh : TriangularMesh (Types.Vertex units coords) -> Mesh
convertMesh mesh =
    let
        vertexToPoint =
            .position >> Point3d.unwrap >> Vec3.fromRecord

        triangles =
            mesh
                |> TriangularMesh.mapVertices vertexToPoint
                |> TriangularMesh.faceVertices

        vertices =
            TriangularMesh.vertices mesh
                |> Array.toList
                |> List.map vertexToPoint

        n =
            List.length vertices

        centroid =
            vertices
                |> List.foldl Vec3.add (vec3 0 0 0)
                |> Vec3.scale (1 / toFloat n)

        radius =
            vertices
                |> List.map (\v -> Vec3.distance v centroid)
                |> List.maximum
                |> Maybe.withDefault 0.0
    in
    { triangles = triangles
    , centroid = centroid
    , radius = radius
    }



-- Möller-Trumbore method for ray-triangle intersection:


rayTriangleIntersection :
    Vec3
    -> Vec3
    -> ( Vec3, Vec3, Vec3 )
    -> Maybe ( Float, Float, Float )
rayTriangleIntersection origin dir ( v0, v1, v2 ) =
    Mat4.makeBasis (Vec3.negate dir) (Vec3.sub v1 v0) (Vec3.sub v2 v0)
        |> Mat4.inverse
        |> Maybe.andThen
            (\invA ->
                let
                    { x, y, z } =
                        Mat4.transform invA (Vec3.sub origin v0)
                            |> Vec3.toRecord

                    ( t, u, v ) =
                        ( x, y, z )
                in
                if u < 0 || v < 0 || u + v > 1 then
                    Nothing

                else
                    Just ( t, u, v )
            )


rayIntersectsSphere : Vec3 -> Vec3 -> Vec3 -> Float -> Bool
rayIntersectsSphere orig dir center radius =
    let
        t =
            Vec3.sub center orig

        lambda =
            Vec3.dot t dir
    in
    Vec3.lengthSquared t - lambda ^ 2 <= radius ^ 2


rayMeshIntersection : Vec3 -> Vec3 -> Mesh -> Maybe Float
rayMeshIntersection orig dir mesh =
    if rayIntersectsSphere orig dir mesh.centroid mesh.radius then
        let
            step triangle bestSoFar =
                case rayTriangleIntersection orig dir triangle of
                    Nothing ->
                        bestSoFar

                    Just ( tNew, _, _ ) ->
                        case bestSoFar of
                            Nothing ->
                                Just tNew

                            Just tOld ->
                                if tNew < tOld && tNew > 0 then
                                    Just tNew

                                else
                                    bestSoFar

            intersect =
                List.foldl step Nothing
        in
        intersect mesh.triangles

    else
        Nothing


intersection : Camera.Ray -> Mat4 -> Mesh -> Maybe Float
intersection ray mat mesh =
    let
        target =
            Vec3.add ray.origin ray.direction

        mappedOrig =
            Mat4.transform mat ray.origin

        mappedTarget =
            Mat4.transform mat target

        factor =
            1 / Vec3.length (Vec3.sub mappedTarget mappedOrig)

        mappedDir =
            Vec3.scale factor (Vec3.sub mappedTarget mappedOrig)
    in
    rayMeshIntersection mappedOrig mappedDir mesh
        |> Maybe.map ((*) factor)


pick : Camera.Ray -> Array Mesh -> List Instance -> Maybe Int
pick ray meshes scene =
    let
        step ( index, Types.Instance item ) bestSoFar =
            let
                intersectionDistance =
                    Maybe.map2
                        (intersection ray)
                        (Mat4.inverse item.transform)
                        (Array.get item.idxMesh meshes)
                        |> Maybe.andThen identity
            in
            case intersectionDistance of
                Nothing ->
                    bestSoFar

                Just tNew ->
                    case bestSoFar of
                        Nothing ->
                            Just ( tNew, index )

                        Just ( tOld, _ ) ->
                            if tNew < tOld then
                                Just ( tNew, index )

                            else
                                bestSoFar
    in
    scene
        |> List.indexedMap Tuple.pair
        |> List.foldl step Nothing
        |> Maybe.map Tuple.second
