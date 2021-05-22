module View3d.Types exposing
    ( FrameSize
    , Instance(..)
    , Material
    , Model
    , Options
    , Vertex
    )

import Color exposing (Color)
import Frame3d exposing (Frame3d)
import Length
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Set exposing (Set)
import Vector3d exposing (Vector3d)
import View3d.Camera as Camera


type alias FrameSize =
    { width : Float, height : Float }


type alias Vertex units coords =
    { position : Point3d units coords
    , normal : Vector3d Unitless coords
    }


type alias Material =
    { color : Color
    , roughness : Float
    , metallic : Float
    }


type Instance coords
    = Instance
        { material : Material
        , transform : Mat4
        , frame : Frame3d Length.Meters coords {}
        , scale : Float
        , idxMesh : Int
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
