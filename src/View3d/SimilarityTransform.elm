module View3d.SimilarityTransform exposing
    ( Frame
    , SimilarityTransform
    , frame
    , fromMatrix
    , identity
    , matrix
    , mirrorAcross
    , placeIn
    , rotateAround
    , scale
    , scaleAbout
    , translateBy
    , translateIn
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Interop.LinearAlgebra.Frame3d exposing (toMat4)
import Length exposing (Length)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


type alias Frame coords =
    Frame3d Length.Meters coords { defines : coords }


type SimilarityTransform coords
    = Similarity
        { matrix : Mat4
        , frame : Frame coords
        , scale : Float
        }


matrix : SimilarityTransform coords -> Mat4
matrix (Similarity sim) =
    sim.matrix


frame : SimilarityTransform coords -> Frame coords
frame (Similarity sim) =
    sim.frame


scale : SimilarityTransform coords -> Float
scale (Similarity sim) =
    sim.scale


identity : SimilarityTransform coords
identity =
    Similarity
        { matrix = Mat4.identity
        , frame = Frame3d.atOrigin
        , scale = 1.0
        }


rotateAround :
    Axis3d Length.Meters coords
    -> Angle
    -> SimilarityTransform coords
    -> SimilarityTransform coords
rotateAround axis angle inst =
    updateFrame (Frame3d.rotateAround axis angle) inst


translateBy :
    Vector3d Length.Meters coords
    -> SimilarityTransform coords
    -> SimilarityTransform coords
translateBy shift inst =
    updateFrame (Frame3d.translateBy shift) inst


translateIn :
    Direction3d coords
    -> Length
    -> SimilarityTransform coords
    -> SimilarityTransform coords
translateIn dir dist inst =
    updateFrame (Frame3d.translateIn dir dist) inst


mirrorAcross :
    Plane3d Length.Meters coords
    -> SimilarityTransform coords
    -> SimilarityTransform coords
mirrorAcross plane inst =
    updateFrame (Frame3d.mirrorAcross plane) inst


scaleAbout :
    Point3d Length.Meters coords
    -> Float
    -> SimilarityTransform coords
    -> SimilarityTransform coords
scaleAbout center scale_ (Similarity inst) =
    let
        s =
            scale_ * inst.scale

        p =
            Point3d.scaleAbout Point3d.origin scale_ center

        frame_ =
            Frame3d.translateBy (Vector3d.from p center) inst.frame

        mat =
            toMat4 frame_ |> Mat4.scale3 s s s
    in
    Similarity { inst | matrix = mat, frame = frame_, scale = s }


placeIn :
    Frame coords
    -> SimilarityTransform coords
    -> SimilarityTransform coords
placeIn frame_ =
    updateFrame (Frame3d.placeIn frame_)


updateFrame :
    (Frame coords -> Frame coords)
    -> SimilarityTransform coords
    -> SimilarityTransform coords
updateFrame fn (Similarity inst) =
    let
        s =
            inst.scale

        frame_ =
            fn inst.frame

        mat =
            toMat4 frame_ |> Mat4.scale3 s s s
    in
    Similarity { inst | matrix = mat, frame = frame_ }


determinant3d : Mat4 -> Float
determinant3d mat =
    Vec3.dot
        (Mat4.transform mat <| vec3 1 0 0)
        (Vec3.cross
            (Mat4.transform mat <| vec3 0 1 0)
            (Mat4.transform mat <| vec3 0 0 1)
        )


sign : number -> number
sign n =
    if n < 0 then
        -1

    else
        1


asPointInMeters : Vec3 -> Point3d Length.Meters coords
asPointInMeters p =
    Point3d.meters (Vec3.getX p) (Vec3.getY p) (Vec3.getZ p)


fromMatrix : Mat4 -> SimilarityTransform coords
fromMatrix matIn =
    let
        shift =
            vec3 0 0 0 |> Mat4.transform matIn

        mat =
            Mat4.mul (Mat4.makeTranslate (Vec3.negate shift)) matIn

        det =
            determinant3d mat

        scale_ =
            sign det * (abs det ^ (1 / 3))

        xIn =
            Mat4.transform mat Vec3.i |> Vec3.toRecord |> Direction3d.unsafe

        yIn =
            Mat4.transform mat Vec3.j |> Vec3.toRecord |> Direction3d.unsafe

        zIn =
            Mat4.transform mat Vec3.k |> Vec3.toRecord |> Direction3d.unsafe

        ( xOut, yOut, zOut ) =
            Direction3d.orthogonalize xIn yIn zIn
                |> Maybe.withDefault ( xIn, yIn, zIn )

        frame_ =
            Frame3d.unsafe
                { originPoint = asPointInMeters shift
                , xDirection = xOut
                , yDirection = yOut
                , zDirection = zOut
                }
    in
    Similarity { scale = scale_, matrix = matIn, frame = frame_ }
