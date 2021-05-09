module View3d.Picker exposing
    ( Mesh
    , convertMesh
    , mappedRayMeshIntersection
    )

import Array
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import TriangularMesh exposing (TriangularMesh)
import View3d.Types as Types


type alias Mesh =
    { centroid : Vec3
    , radius : Float
    , triangles : List ( Vec3, Vec3, Vec3 )
    }


convertMesh : TriangularMesh Types.Vertex -> Mesh
convertMesh mesh =
    let
        triangles =
            mesh
                |> TriangularMesh.mapVertices .position
                |> TriangularMesh.faceVertices

        vertices =
            TriangularMesh.vertices mesh
                |> Array.toList
                |> List.map .position

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


rayMeshIntersection :
    Vec3
    -> Vec3
    -> List ( Vec3, Vec3, Vec3 )
    -> Vec3
    -> Float
    -> Maybe Float
rayMeshIntersection orig dir tris center radius =
    if rayIntersectsSphere orig dir center radius then
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
        intersect tris

    else
        Nothing


mappedRayMeshIntersection :
    Vec3
    -> Vec3
    -> Mat4
    -> List ( Vec3, Vec3, Vec3 )
    -> Vec3
    -> Float
    -> Maybe Float
mappedRayMeshIntersection orig dir mat tris center radius =
    let
        target =
            Vec3.add orig dir

        mappedOrig =
            Mat4.transform mat orig

        mappedTarget =
            Mat4.transform mat target

        factor =
            1 / Vec3.length (Vec3.sub mappedTarget mappedOrig)

        mappedDir =
            Vec3.scale factor (Vec3.sub mappedTarget mappedOrig)
    in
    rayMeshIntersection mappedOrig mappedDir tris center radius
        |> Maybe.map ((*) factor)
