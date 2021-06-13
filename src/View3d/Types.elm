module View3d.Types exposing
    ( EffectsMesh
    , EffectsVertex
    , FrameSize
    , Instance(..)
    , Material
    , Mesh(..)
    , MeshImpl
    , Model
    , Options
    , PickerMesh
    , SceneMesh
    , Vertex
    )

import Color exposing (Color)
import Length
import Math.Vector3 exposing (Vec3)
import Point3d exposing (Point3d)
import Quantity
import Scene3d.Mesh
import Set exposing (Set)
import Vector3d exposing (Vector3d)
import View3d.Camera as Camera
import View3d.SimilarityTransform exposing (SimilarityTransform)
import WebGL


type alias FrameSize =
    { width : Float, height : Float }


type alias Vertex coords =
    { position : Point3d Length.Meters coords
    , normal : Vector3d Quantity.Unitless coords
    }


type alias EffectsVertex =
    { position : Vec3
    , normal : Vec3
    , barycentric : Vec3
    }


type alias Material =
    { color : Color
    , roughness : Float
    , metallic : Float
    }


type alias EffectsMesh =
    WebGL.Mesh EffectsVertex


type alias SceneMesh coords =
    { surface : Scene3d.Mesh.Uniform coords
    , shadow : Scene3d.Mesh.Shadow coords
    }


type alias PickerMesh =
    { centroid : Vec3
    , radius : Float
    , triangles : List ( Vec3, Vec3, Vec3 )
    }


type alias MeshImpl coords =
    { scene : SceneMesh coords
    , effects : EffectsMesh
    , picking : PickerMesh
    }


type Mesh coords
    = Mesh (MeshImpl coords)


type Instance coords
    = Instance
        { material : Material
        , mesh : MeshImpl coords
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
