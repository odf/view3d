module View3d exposing
    ( Instance
    , Material
    , Model
    , Msg
    , Options
    , Outcome(..)
    , Vertex
    , defaultOptions
    , encompass
    , init
    , instance
    , lookAlong
    , mirrorInstanceAcross
    , placeInstanceIn
    , requestRedraw
    , rotateBy
    , rotateInstanceAround
    , scaleInstanceAbout
    , selection
    , setScene
    , setSelection
    , setSize
    , subscriptions
    , translateInstanceBy
    , translateInstanceIn
    , update
    , view
    )

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Bitwise
import Browser.Events as Events
import Color
import DOM
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Length exposing (Length)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Set exposing (Set)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import View3d.Camera as Camera
import View3d.EffectsRenderer as EffectsRenderer
import View3d.Picker as Picker
import View3d.SceneRenderer as SceneRenderer
import View3d.SimilarityTransform as Similarity exposing (SimilarityTransform)
import View3d.Types as Types
import WebGL



-- RE-EXPORTING


type alias Vertex coords =
    Types.Vertex coords


type alias Instance coords =
    Types.Instance coords


type alias Material =
    Types.Material


type alias Options =
    Types.Options



-- MODEL


type alias Position =
    { x : Float, y : Float }


type alias ModelImpl coords =
    Types.Model
        coords
        { requestRedraw : Bool
        , touchStart : Position
        , sceneMeshes : Array (SceneRenderer.Mesh coords)
        , effectsMeshes : Array EffectsRenderer.Mesh
        , pickingMeshes : Array Picker.Mesh
        }


type Model coords
    = Model (ModelImpl coords)


type Outcome
    = None
    | PickEmpty Touch.Keys
    | Pick Touch.Keys Int



-- INIT


init : Model coords
init =
    Model
        { size = { width = 0, height = 0 }
        , scene = []
        , selected = Set.empty
        , center = vec3 0 0 0
        , radius = 0
        , cameraState = Camera.initialState
        , requestRedraw = False
        , touchStart = { x = 0, y = 0 }
        , sceneMeshes = Array.empty
        , effectsMeshes = Array.empty
        , pickingMeshes = Array.empty
        }



-- SUBSCRIPTIONS


type alias Buttons =
    { left : Bool, right : Bool, middle : Bool }


type Msg
    = FrameMsg Float
    | MouseUpMsg Position Touch.Keys
    | MouseDownMsg Position Position Touch.Keys Buttons
    | MouseMoveMsg Position Touch.Keys
    | TouchStartMsg (List Position) Position
    | TouchMoveMsg (List Position)
    | TouchEndMsg
    | WheelMsg Float Touch.Keys


decodeModifiers : Decode.Decoder Touch.Keys
decodeModifiers =
    Decode.map3 (\alt ctrl shift -> { alt = alt, ctrl = ctrl, shift = shift })
        (Decode.at [ "altKey" ] Decode.bool)
        (Decode.at [ "ctrlKey" ] Decode.bool)
        (Decode.at [ "shiftKey" ] Decode.bool)


decodeButtons : Decode.Decoder Buttons
decodeButtons =
    Decode.map
        (\val ->
            { left = Bitwise.and val 1 > 0
            , right = Bitwise.and val 2 > 0
            , middle = Bitwise.and val 4 > 0
            }
        )
        (Decode.at [ "buttons" ] Decode.int)


decodePos : Decode.Decoder Position
decodePos =
    Decode.map2 (\x y -> { x = toFloat x, y = toFloat y })
        (Decode.at [ "clientX" ] Decode.int)
        (Decode.at [ "clientY" ] Decode.int)


decodeRelativePos : Decode.Decoder Position
decodeRelativePos =
    Decode.map2 (\x y -> { x = toFloat x, y = toFloat y })
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)


decodePosList : Decode.Decoder (List Position)
decodePosList =
    Decode.map
        (List.map (.clientPos >> (\( x, y ) -> { x = x, y = y })))
        (Decode.field
            "changedTouches"
            (Touch.touchListDecoder Touch.touchDecoder)
        )


decodeOffset : Decode.Decoder DOM.Rectangle
decodeOffset =
    DOM.target DOM.boundingClientRect


subscriptions : (Msg -> msg) -> Model coords -> Sub msg
subscriptions toMsg (Model model) =
    let
        frameEvent =
            if
                model.requestRedraw
                    || Camera.needsFrameEvents model.cameraState
            then
                Events.onAnimationFrameDelta FrameMsg

            else
                Sub.none

        decoder msg =
            Decode.map2 msg decodePos decodeModifiers

        mouseEvents =
            if Camera.needsMouseEvents model.cameraState then
                [ Events.onMouseMove (decoder MouseMoveMsg)
                , Events.onMouseUp (decoder MouseUpMsg)
                ]

            else
                []
    in
    (frameEvent :: mouseEvents)
        |> Sub.batch
        |> Sub.map toMsg



-- UPDATE


update : Msg -> Model coords -> ( Model coords, Outcome )
update msg (Model model) =
    ( updateModel msg model, outcome msg model )


updateModel : Msg -> ModelImpl coords -> Model coords
updateModel msg model =
    case msg of
        FrameMsg time ->
            updateCamera
                (Camera.nextFrame time)
                (Model { model | requestRedraw = False })

        MouseDownMsg pos posRel _ buttons ->
            if buttons.right then
                Model model

            else
                updateCamera
                    (Camera.startDragging pos)
                    (Model { model | touchStart = posRel })

        MouseUpMsg _ _ ->
            updateCamera Camera.finishDragging (Model model)

        MouseMoveMsg pos modifiers ->
            updateCamera
                (Camera.dragTo pos modifiers.shift)
                (Model model)

        TouchStartMsg posList offset ->
            touchStartUpdate posList offset (Model model)

        TouchMoveMsg posList ->
            touchMoveUpdate posList (Model model)

        TouchEndMsg ->
            updateCamera Camera.finishDragging (Model model)

        WheelMsg val modifiers ->
            updateCamera
                (Camera.updateZoom (wheelZoomFactor val) modifiers.shift)
                (Model model)


outcome : Msg -> ModelImpl coords -> Outcome
outcome msg model =
    if Camera.wasDragged model.cameraState then
        None

    else
        case msg of
            MouseUpMsg _ modifiers ->
                pickingOutcome model.touchStart modifiers (Model model)

            TouchEndMsg ->
                pickingOutcome
                    model.touchStart
                    { alt = True, ctrl = False, shift = False }
                    (Model model)

            _ ->
                None


pickingOutcome : Position -> Touch.Keys -> Model coords -> Outcome
pickingOutcome pos mods (Model model) =
    Camera.pickingRay pos model.cameraState model.center (3 * model.radius)
        |> Maybe.andThen
            (\r -> Picker.pick r model.pickingMeshes model.scene)
        |> Maybe.map (\index -> Pick mods index)
        |> Maybe.withDefault (PickEmpty mods)


avg : List Float -> Float
avg xs =
    case xs of
        [] ->
            0

        _ ->
            List.foldl (+) 0 xs / (List.length xs |> toFloat)


centerPosition : List Position -> Position
centerPosition posList =
    { x = List.map .x posList |> avg
    , y = List.map .y posList |> avg
    }


touchStartUpdate : List Position -> Position -> Model coords -> Model coords
touchStartUpdate posList offset (Model model) =
    case posList of
        pos :: [] ->
            { model
                | touchStart = { x = pos.x - offset.x, y = pos.y - offset.y }
            }
                |> Model
                |> updateCamera (Camera.startDragging pos)

        posA :: posB :: [] ->
            Model model
                |> updateCamera (Camera.startPinching posA posB)

        _ :: _ :: _ :: [] ->
            Model model
                |> updateCamera (Camera.startDragging <| centerPosition posList)

        _ ->
            Model model


touchMoveUpdate : List Position -> Model coords -> Model coords
touchMoveUpdate posList model =
    case posList of
        pos :: [] ->
            updateCamera (Camera.dragTo pos False) model

        posA :: posB :: [] ->
            updateCamera (Camera.pinchTo posA posB False) model

        _ :: _ :: _ :: [] ->
            updateCamera (Camera.dragTo (centerPosition posList) True) model

        _ ->
            model


wheelZoomFactor : Float -> Float
wheelZoomFactor wheelVal =
    if wheelVal > 0 then
        0.9

    else if wheelVal < 0 then
        1.0 / 0.9

    else
        1.0


updateCamera : (Camera.State -> Camera.State) -> Model coords -> Model coords
updateCamera fn (Model model) =
    { model | cameraState = fn model.cameraState } |> Model


lookAlong : Vec3 -> Vec3 -> Model coords -> Model coords
lookAlong axis up model =
    updateCamera (Camera.lookAlong axis up) model


rotateBy : Vec3 -> Float -> Model coords -> Model coords
rotateBy axis angle model =
    updateCamera (Camera.rotateBy axis angle) model


encompass : Model coords -> Model coords
encompass (Model model) =
    updateCamera (Camera.encompass model.center model.radius) (Model model)


setSize : Types.FrameSize -> Model coords -> Model coords
setSize size (Model model) =
    updateCamera (Camera.setFrameSize size) (Model { model | size = size })


setMeshes :
    List (TriangularMesh (Vertex coords))
    -> ModelImpl coords
    -> ModelImpl coords
setMeshes meshes model =
    { model
        | sceneMeshes =
            List.map SceneRenderer.convertMesh meshes |> Array.fromList
        , effectsMeshes =
            List.map EffectsRenderer.convertMesh meshes |> Array.fromList
        , pickingMeshes =
            List.map Picker.convertMesh meshes |> Array.fromList
    }


boundingSphere : Array Picker.Mesh -> List (Instance coords) -> ( Vec3, Float )
boundingSphere meshes scene =
    let
        fixRadius t r =
            let
                o =
                    Mat4.transform t (vec3 0 0 0)
            in
            [ Vec3.i, Vec3.j, Vec3.k ]
                |> List.map
                    (Vec3.scale r >> Mat4.transform t >> Vec3.distance o)
                |> List.maximum
                |> Maybe.withDefault 0

        hasIndex index (Types.Instance e) =
            e.idxMesh == index

        boundingSpheresForMesh index mesh =
            List.filter (hasIndex index) scene
                |> List.map
                    (\(Types.Instance inst) ->
                        ( Mat4.transform
                            (Similarity.matrix inst.transform)
                            mesh.centroid
                        , fixRadius
                            (Similarity.matrix inst.transform)
                            mesh.radius
                        )
                    )

        boundingSpheres =
            Array.toList meshes
                |> List.indexedMap boundingSpheresForMesh
                |> List.concat

        sceneCenter =
            boundingSpheres
                |> List.foldl (\( c, _ ) sum -> Vec3.add sum c) (vec3 0 0 0)
                |> Vec3.scale (1 / toFloat (List.length boundingSpheres))

        sceneRadius =
            boundingSpheres
                |> List.map (\( c, r ) -> r + Vec3.distance sceneCenter c)
                |> List.maximum
                |> Maybe.withDefault 0.0
    in
    ( sceneCenter, sceneRadius )


setScene :
    Maybe (List (TriangularMesh (Vertex coords)))
    -> List (Instance coords)
    -> Model coords
    -> Model coords
setScene maybeMeshes instances (Model model) =
    let
        modelWithMeshes =
            Maybe.map (flip setMeshes model) maybeMeshes
                |> Maybe.withDefault model

        ( sceneCenter, sceneRadius ) =
            boundingSphere modelWithMeshes.pickingMeshes instances
    in
    Model
        { modelWithMeshes
            | scene = instances
            , selected = Set.empty
            , center = sceneCenter
            , radius = sceneRadius
        }


setSelection : Set Int -> Model coords -> Model coords
setSelection selected (Model model) =
    Model { model | selected = selected }


selection : Model coords -> Set Int
selection (Model model) =
    model.selected


requestRedraw : Model coords -> Model coords
requestRedraw (Model model) =
    Model { model | requestRedraw = True }


instance : Types.Material -> Int -> Types.Instance coords
instance mat idxMesh =
    Types.Instance
        { material = mat
        , transform = Similarity.identity
        , idxMesh = idxMesh
        }


rotateInstanceAround :
    Axis3d Length.Meters coords
    -> Angle
    -> Types.Instance coords
    -> Types.Instance coords
rotateInstanceAround axis angle inst =
    updateInstance (Similarity.rotateAround axis angle) inst


translateInstanceBy :
    Vector3d Length.Meters coords
    -> Types.Instance coords
    -> Types.Instance coords
translateInstanceBy shift inst =
    updateInstance (Similarity.translateBy shift) inst


translateInstanceIn :
    Direction3d coords
    -> Length
    -> Types.Instance coords
    -> Types.Instance coords
translateInstanceIn dir dist inst =
    updateInstance (Similarity.translateIn dir dist) inst


mirrorInstanceAcross :
    Plane3d Length.Meters coords
    -> Types.Instance coords
    -> Types.Instance coords
mirrorInstanceAcross plane inst =
    updateInstance (Similarity.mirrorAcross plane) inst


scaleInstanceAbout :
    Point3d Length.Meters coords
    -> Float
    -> Types.Instance coords
    -> Types.Instance coords
scaleInstanceAbout center scale inst =
    updateInstance (Similarity.scaleAbout center scale) inst


placeInstanceIn :
    Similarity.Frame coords
    -> Types.Instance coords
    -> Types.Instance coords
placeInstanceIn frame =
    updateInstance (Similarity.placeIn frame)


updateInstance :
    (SimilarityTransform coords -> SimilarityTransform coords)
    -> Types.Instance coords
    -> Types.Instance coords
updateInstance fn (Types.Instance inst) =
    Types.Instance { inst | transform = fn inst.transform }



-- VIEW


view : (Msg -> msg) -> Model coords -> Options -> Html msg
view toMsg (Model model) options =
    let
        attributes =
            [ Html.Attributes.style "display" "block"
            , Html.Attributes.id "main-3d-canvas"
            , Html.Attributes.width (floor model.size.width)
            , Html.Attributes.height (floor model.size.height)
            , onMouseDown
                (\pos offset mods buttons ->
                    toMsg (MouseDownMsg pos offset mods buttons)
                )
            , onMouseWheel
                (\dy mods -> toMsg (WheelMsg dy mods))
            , onTouchStart
                (\posList offset -> toMsg (TouchStartMsg posList offset))
            , onTouchMove
                (toMsg << TouchMoveMsg)
            , onTouchEnd
                (toMsg TouchEndMsg)
            , onTouchCancel
                (toMsg TouchEndMsg)
            ]

        bgEntity =
            EffectsRenderer.backgroundEntity options.backgroundColor

        sceneEntities =
            SceneRenderer.entities model.sceneMeshes model options

        fogEntities =
            EffectsRenderer.entities model.effectsMeshes model options

        entities =
            bgEntity :: sceneEntities ++ fogEntities

        webGLOptions =
            [ WebGL.depth 1
            , WebGL.stencil 0
            , WebGL.alpha True
            , WebGL.clearColor 0 0 0 0
            , WebGL.antialias
            ]
    in
    WebGL.toHtmlWith webGLOptions attributes entities


defaultOptions : Options
defaultOptions =
    { orthogonalView = False
    , drawWires = False
    , fadeToBackground = 0.4
    , fadeToBlue = 0.1
    , backgroundColor = Color.black
    , addOutlines = False
    , outlineWidth = 0.0
    , outlineColor = Color.black
    , drawShadows = True
    }


onMouseDown :
    (Position -> Position -> Touch.Keys -> Buttons -> msg)
    -> Html.Attribute msg
onMouseDown toMsg =
    let
        toResult pos posRel mods buttons =
            { message = toMsg pos posRel mods buttons
            , stopPropagation = False
            , preventDefault = False
            }
    in
    Html.Events.custom
        "mousedown"
        (Decode.map4
            toResult
            decodePos
            decodeRelativePos
            decodeModifiers
            decodeButtons
        )


onMouseWheel : (Float -> Touch.Keys -> msg) -> Html.Attribute msg
onMouseWheel toMsg =
    let
        toResult dy mods =
            { message = toMsg dy mods
            , stopPropagation = True
            , preventDefault = True
            }
    in
    Html.Events.custom
        "wheel"
        (Decode.map2
            toResult
            (Decode.at [ "deltaY" ] Decode.float)
            decodeModifiers
        )


onTouchStart : (List Position -> Position -> msg) -> Html.Attribute msg
onTouchStart toMsg =
    let
        toResult posList { top, left } =
            { message = toMsg posList { x = left, y = top }
            , stopPropagation = True
            , preventDefault = True
            }
    in
    Html.Events.custom
        "touchstart"
        (Decode.map2 toResult decodePosList decodeOffset)


onTouchMove : (List Position -> msg) -> Html.Attribute msg
onTouchMove toMsg =
    Touch.onMove
        (.targetTouches
            >> List.map .clientPos
            >> List.map (\( x, y ) -> { x = x, y = y })
            >> toMsg
        )


onTouchEnd : msg -> Html.Attribute msg
onTouchEnd theMsg =
    Touch.onEnd (\_ -> theMsg)


onTouchCancel : msg -> Html.Attribute msg
onTouchCancel theMsg =
    Touch.onCancel (\_ -> theMsg)



-- General Helper Functions


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
