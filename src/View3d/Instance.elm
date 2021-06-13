module View3d.Instance exposing
    ( Instance
    , make
    , mirrorAcross
    , placeIn
    , rotateAround
    , scaleAbout
    , transform
    , translateBy
    , translateIn
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Length exposing (Length)
import Math.Matrix4 exposing (Mat4)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import View3d.SimilarityTransform as Similarity exposing (SimilarityTransform)
import View3d.Types as Types


type alias Instance coords =
    Types.Instance coords


make : Types.Material -> Types.Mesh coords -> Instance coords
make mat (Types.Mesh mesh) =
    Types.Instance
        { material = mat
        , transform = Similarity.identity
        , mesh = mesh
        }


rotateAround :
    Axis3d Length.Meters coords
    -> Angle
    -> Instance coords
    -> Instance coords
rotateAround axis angle =
    update (Similarity.rotateAround axis angle)


translateBy :
    Vector3d Length.Meters coords
    -> Instance coords
    -> Instance coords
translateBy =
    Similarity.translateBy >> update


translateIn :
    Direction3d coords
    -> Length
    -> Instance coords
    -> Instance coords
translateIn dir dist =
    update (Similarity.translateIn dir dist)


mirrorAcross :
    Plane3d Length.Meters coords
    -> Instance coords
    -> Instance coords
mirrorAcross =
    Similarity.mirrorAcross >> update


scaleAbout :
    Point3d Length.Meters coords
    -> Float
    -> Instance coords
    -> Instance coords
scaleAbout center scale =
    update (Similarity.scaleAbout center scale)


placeIn : Similarity.Frame coords -> Instance coords -> Instance coords
placeIn =
    Similarity.placeIn >> update


transform : Mat4 -> Instance coords -> Instance coords
transform =
    Similarity.fromMatrix >> Similarity.compose >> update


update :
    (SimilarityTransform coords -> SimilarityTransform coords)
    -> Instance coords
    -> Instance coords
update fn (Types.Instance inst) =
    Types.Instance { inst | transform = fn inst.transform }
