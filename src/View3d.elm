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
    , lookAlong
    , requestRedraw
    , rotateBy
    , selection
    , setScene
    , setSelection
    , setSize
    , subscriptions
    , update
    , view
    )

import Array exposing (Array)
import Bitwise
import Browser.Events as Events
import Color
import DOM
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)
import TriangularMesh exposing (TriangularMesh)
import View3d.Camera as Camera
import View3d.EffectsRenderer as EffectsRenderer
import View3d.Picker as Picker
import View3d.SceneRenderer as SceneRenderer
import View3d.Types as Types
import WebGL



-- RE-EXPORTING


type alias Vertex =
    Types.Vertex


type alias Instance =
    Types.Instance


type alias Material =
    Types.Material


type alias Options =
    Types.Options



-- MODEL


type alias Position =
    { x : Float, y : Float }


type alias ModelImpl =
    Types.Model
        { requestRedraw : Bool
        , touchStart : Position
        , sceneMeshes : Array SceneRenderer.Mesh
        , effectsMeshes : Array EffectsRenderer.Mesh
        , pickingMeshes : Array Picker.Mesh
        }


type Model
    = Model ModelImpl


type Outcome
    = None
    | PickEmpty Touch.Keys
    | Pick Touch.Keys Int



-- INIT


init : Model
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


subscriptions : (Msg -> msg) -> Model -> Sub msg
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


update : Msg -> Model -> ( Model, Outcome )
update msg (Model model) =
    ( updateModel msg model, outcome msg model )


updateModel : Msg -> ModelImpl -> Model
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


outcome : Msg -> ModelImpl -> Outcome
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


pickingOutcome : Position -> Touch.Keys -> Model -> Outcome
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


touchStartUpdate : List Position -> Position -> Model -> Model
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


touchMoveUpdate : List Position -> Model -> Model
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


updateCamera : (Camera.State -> Camera.State) -> Model -> Model
updateCamera fn (Model model) =
    { model | cameraState = fn model.cameraState } |> Model


lookAlong : Vec3 -> Vec3 -> Model -> Model
lookAlong axis up model =
    updateCamera (Camera.lookAlong axis up) model


rotateBy : Vec3 -> Float -> Model -> Model
rotateBy axis angle model =
    updateCamera (Camera.rotateBy axis angle) model


encompass : Model -> Model
encompass (Model model) =
    updateCamera (Camera.encompass model.center model.radius) (Model model)


setSize : Types.FrameSize -> Model -> Model
setSize size (Model model) =
    updateCamera (Camera.setFrameSize size) (Model { model | size = size })


setMeshes : List (TriangularMesh Vertex) -> ModelImpl -> ModelImpl
setMeshes meshes model =
    { model
        | sceneMeshes =
            List.map SceneRenderer.convertMesh meshes |> Array.fromList
        , effectsMeshes =
            List.map EffectsRenderer.convertMesh meshes |> Array.fromList
        , pickingMeshes =
            List.map Picker.convertMesh meshes |> Array.fromList
    }


boundingSphere : Array Picker.Mesh -> List Instance -> ( Vec3, Float )
boundingSphere meshes instances =
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

        boundingSpheresForMesh index mesh =
            List.filter (.idxMesh >> (==) index) instances
                |> List.map
                    (\{ transform } ->
                        ( Mat4.transform transform mesh.centroid
                        , fixRadius transform mesh.radius
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
    Maybe (List (TriangularMesh Vertex))
    -> List Instance
    -> Model
    -> Model
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


setSelection : Set ( Int, Int ) -> Model -> Model
setSelection selected (Model model) =
    Model { model | selected = selected }


selection : Model -> Set ( Int, Int )
selection (Model model) =
    model.selected


requestRedraw : Model -> Model
requestRedraw (Model model) =
    Model { model | requestRedraw = True }



-- VIEW


view : (Msg -> msg) -> Model -> Options -> Html msg
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
