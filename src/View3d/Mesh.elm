module View3d.Mesh exposing
    ( Mesh(..)
    , getVertices
    , mapVertices
    , mappedRayMeshIntersection
    , resolved
    , surface
    )

import Array
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)


type Mesh vertex
    = Triangles (List ( vertex, vertex, vertex ))
    | IndexedTriangles (List vertex) (List ( Int, Int, Int ))


type alias FaceSpec =
    List Int


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: w :: rest ->
            ( w, u, v ) :: List.map2 (\r s -> ( u, r, s )) (w :: rest) rest

        _ ->
            []


surface : List vertex -> List FaceSpec -> Mesh vertex
surface vertices faces =
    List.concatMap triangulate faces
        |> IndexedTriangles vertices


resolved : Mesh vertex -> Mesh vertex
resolved mesh =
    case mesh of
        Triangles _ ->
            mesh

        IndexedTriangles vertices triangles ->
            let
                verts =
                    Array.fromList vertices
            in
            triangles
                |> List.map (\( i, j, k ) -> [ i, j, k ])
                |> List.map (List.filterMap (\i -> Array.get i verts))
                |> List.concatMap triangulate
                |> Triangles


getVertices : Mesh vertex -> List vertex
getVertices mesh =
    case mesh of
        Triangles triangles ->
            triangles
                |> List.concatMap (\( u, v, w ) -> [ u, v, w ])

        IndexedTriangles vertices _ ->
            vertices


mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices fn mesh =
    case mesh of
        Triangles triangles ->
            triangles
                |> List.map (\( p, q, r ) -> ( fn p, fn q, fn r ))
                |> Triangles

        IndexedTriangles vertices triangles ->
            IndexedTriangles (List.map fn vertices) triangles



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


rayMeshIntersection : Vec3 -> Vec3 -> Mesh Vec3 -> Vec3 -> Float -> Maybe Float
rayMeshIntersection orig dir mesh center radius =
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
        case mesh of
            Triangles triangles ->
                intersect triangles

            IndexedTriangles _ _ ->
                rayMeshIntersection orig dir (resolved mesh) center radius

    else
        Nothing


mappedRayMeshIntersection :
    Vec3
    -> Vec3
    -> Mat4
    -> Mesh Vec3
    -> Vec3
    -> Float
    -> Maybe Float
mappedRayMeshIntersection orig dir mat mesh center radius =
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
    rayMeshIntersection mappedOrig mappedDir mesh center radius
        |> Maybe.map ((*) factor)
