module View3d.Types exposing
    ( FrameSize
    , Instance(..)
    , Material
    , Model
    , Options
    , Vertex
    )

import Color exposing (Color)
import Length
import Math.Vector3 exposing (Vec3)
import Point3d exposing (Point3d)
import Quantity
import Set exposing (Set)
import Vector3d exposing (Vector3d)
import View3d.Camera as Camera
import View3d.SimilarityTransform exposing (SimilarityTransform)


type alias FrameSize =
    { width : Float, height : Float }


type alias Vertex coords =
    { position : Point3d Length.Meters coords
    , normal : Vector3d Quantity.Unitless coords
    }


type alias Material =
    { color : Color
    , roughness : Float
    , metallic : Float
    }


type Instance coords
    = Instance
        { material : Material
        , idxMesh : Int
        , transform : SimilarityTransform coords
        }


type alias Model coords a =
    { a
        | size : FrameSize
        , scene : List (Instance coords)
        , selected : Set Int
        , center : Vec3
        , radius : Float
        , cameraState : Camera.State
    }


type alias Options =
    { orthogonalView : Bool
    , drawWires : Bool
    , fadeToBackground : Float
    , fadeToBlue : Float
    , backgroundColor : Color
    , addOutlines : Bool
    , outlineWidth : Float
    , outlineColor : Color
    , drawShadows : Bool
    }
