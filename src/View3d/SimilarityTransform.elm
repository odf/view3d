module View3d.SimilarityTransform exposing
    ( Frame
    , SimilarityTransform
    , frame
    , identity
    , matrix
    , mirrorAcross
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
import Math.Matrix4 exposing (Mat4)
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
        { matrix = Math.Matrix4.identity
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
            toMat4 frame_ |> Math.Matrix4.scale3 s s s
    in
    Similarity { inst | matrix = mat, frame = frame_, scale = s }


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
            toMat4 frame_ |> Math.Matrix4.scale3 s s s
    in
    Similarity { inst | matrix = mat, frame = frame_ }
