module View3d exposing
    ( Instance
    , Material
    , Mesh
    , Model
    , Msg
    , Options
    , Outcome(..)
    , Vertex
    , encompass
    , init
    , lookAlong
    , requestRedraw
    , rotateBy
    , setScene
    , setSelection
    , setSize
    , subscriptions
    , surface
    , update
    , view
    )

import Array exposing (Array)
import Bitwise
import Browser.Events as Events
import DOM
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set exposing (Set)
import View3d.Camera as Camera
import View3d.Mesh as Mesh
import View3d.RendererCommon as RendererCommon
import View3d.RendererScene3d as RendererScene3d
import View3d.RendererWebGLEffects as RendererEffects
import WebGL



-- RE-EXPORTING


type alias Vertex =
    RendererCommon.Vertex


type alias Instance =
    RendererCommon.Instance


type alias Material =
    RendererCommon.Material


type alias Options =
    RendererCommon.Options


type alias Mesh =
    MeshImpl Vertex


surface : List vertex -> List (List Int) -> MeshImpl vertex
surface =
    Mesh.surface



-- MODEL


type alias Position =
    { x : Float, y : Float }


type alias MeshImpl a =
    Mesh.Mesh a


type alias PickingInfo =
    { centroid : Vec3
    , radius : Float
    , pickingMesh : MeshImpl Vec3
    }


type alias ModelImpl =
    RendererCommon.Model
        { requestRedraw : Bool
        , touchStart : Position
        , meshesScene3d : Array RendererScene3d.Mesh
        , meshesWebGLFog : Array RendererEffects.Mesh
        , pickingData : Array PickingInfo
        }


type Model
    = Model ModelImpl


type Outcome
    = None
    | PickEmpty Touch.Keys
    | Pick Touch.Keys { meshIndex : Int, instanceIndex : Int }



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
        , meshesScene3d = Array.empty
        , meshesWebGLFog = Array.empty
        , pickingData = Array.empty
        }


meshForPicking : Mesh -> MeshImpl Vec3
meshForPicking mesh =
    mesh
        |> Mesh.mapVertices (\v -> v.position)
        |> Mesh.resolved


centroid : MeshImpl { a | position : Vec3 } -> Vec3
centroid mesh =
    let
        vertices =
            List.map .position (Mesh.getVertices mesh)

        n =
            List.length vertices
    in
    vertices
        |> List.foldl Vec3.add (vec3 0 0 0)
        |> Vec3.scale (1 / toFloat n)


radius : MeshImpl { a | position : Vec3 } -> Float
radius mesh =
    let
        vertices =
            List.map .position (Mesh.getVertices mesh)

        c =
            centroid mesh
    in
    vertices
        |> List.map (\v -> Vec3.distance v c)
        |> List.maximum
        |> Maybe.withDefault 0.0


pick :
    Camera.Ray
    -> Array PickingInfo
    -> List Instance
    -> Maybe ( Int, Int )
pick ray pdata scene =
    let
        intersect =
            Mesh.mappedRayMeshIntersection ray.origin ray.direction

        step item bestSoFar =
            let
                mat =
                    Mat4.inverse item.transform

                pinfo =
                    Array.get item.idxMesh pdata

                mesh =
                    Maybe.map (\p -> p.pickingMesh) pinfo

                intersection =
                    Maybe.map3
                        (\t p m -> intersect t m p.centroid p.radius)
                        mat
                        pinfo
                        mesh
                        |> Maybe.andThen identity
            in
            case intersection of
                Nothing ->
                    bestSoFar

                Just tNew ->
                    case bestSoFar of
                        Nothing ->
                            Just ( tNew, item.idxMesh, item.idxInstance )

                        Just ( tOld, _, _ ) ->
                            if tNew < tOld then
                                Just ( tNew, item.idxMesh, item.idxInstance )

                            else
                                bestSoFar
    in
    scene
        |> List.foldl step Nothing
        |> Maybe.map (\( _, idxMesh, idxInstance ) -> ( idxMesh, idxInstance ))



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
    case msg of
        FrameMsg time ->
            ( updateCamera (Camera.nextFrame time)
                (Model { model | requestRedraw = False })
            , None
            )

        MouseDownMsg pos posRel _ buttons ->
            if buttons.right then
                ( Model model, None )

            else
                ( { model | touchStart = posRel }
                    |> Model
                    |> updateCamera (Camera.startDragging pos)
                , None
                )

        MouseUpMsg _ modifiers ->
            let
                outcome =
                    if Camera.wasDragged model.cameraState then
                        None

                    else
                        pickingOutcome model.touchStart modifiers (Model model)
            in
            ( updateCamera Camera.finishDragging (Model model), outcome )

        MouseMoveMsg pos modifiers ->
            ( updateCamera
                (Camera.dragTo pos modifiers.shift)
                (Model model)
            , None
            )

        TouchStartMsg posList offset ->
            ( touchStartUpdate posList offset (Model model), None )

        TouchMoveMsg posList ->
            ( touchMoveUpdate posList (Model model), None )

        TouchEndMsg ->
            let
                outcome =
                    if Camera.wasDragged model.cameraState then
                        None

                    else
                        pickingOutcome
                            model.touchStart
                            { alt = True, ctrl = False, shift = False }
                            (Model model)
            in
            ( updateCamera Camera.finishDragging (Model model), outcome )

        WheelMsg val modifiers ->
            ( updateCamera
                (Camera.updateZoom (wheelZoomFactor val) modifiers.shift)
                (Model model)
            , None
            )


pickingOutcome : Position -> Touch.Keys -> Model -> Outcome
pickingOutcome pos mods (Model model) =
    Camera.pickingRay pos model.cameraState model.center (3 * model.radius)
        |> Maybe.andThen
            (\r -> pick r model.pickingData model.scene)
        |> Maybe.map
            (\( m, i ) -> Pick mods { meshIndex = m, instanceIndex = i })
        |> Maybe.withDefault
            (PickEmpty mods)


centerPosition : List Position -> Position
centerPosition posList =
    let
        n =
            List.length posList |> max 1 |> toFloat

        sum =
            List.foldl (\p q -> { x = p.x + q.x, y = p.y + q.y })
                { x = 0, y = 0 }
                posList
    in
    { x = sum.x / n, y = sum.y / n }


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


setSize : RendererCommon.FrameSize -> Model -> Model
setSize size (Model model) =
    updateCamera (Camera.setFrameSize size) (Model { model | size = size })


setMeshes : List Mesh -> ModelImpl -> ModelImpl
setMeshes meshes model =
    let
        meshesScene3d =
            List.map
                (\mesh -> RendererScene3d.convertMeshForRenderer mesh)
                meshes

        meshesWebGLFog =
            List.map
                (\mesh -> RendererEffects.convertMeshForRenderer mesh)
                meshes

        pickingData =
            List.map
                (\mesh ->
                    { pickingMesh = meshForPicking mesh
                    , centroid = centroid mesh
                    , radius = radius mesh
                    }
                )
                meshes
    in
    { model
        | meshesScene3d = Array.fromList meshesScene3d
        , meshesWebGLFog = Array.fromList meshesWebGLFog
        , pickingData = Array.fromList pickingData
    }


setScene : Maybe (List Mesh) -> List Instance -> Model -> Model
setScene maybeMeshes instances (Model model) =
    let
        modelWithMeshes =
            case maybeMeshes of
                Nothing ->
                    model

                Just meshes ->
                    setMeshes meshes model

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

        boundingData =
            modelWithMeshes.pickingData
                |> Array.toList
                |> List.indexedMap
                    (\index p ->
                        instances
                            |> List.filter (\inst -> inst.idxMesh == index)
                            |> List.map
                                (\{ transform } ->
                                    ( Mat4.transform transform p.centroid
                                    , fixRadius transform p.radius
                                    )
                                )
                    )
                |> List.concat

        sceneCenter =
            boundingData
                |> List.foldl (\( c, _ ) sum -> Vec3.add sum c) (vec3 0 0 0)
                |> Vec3.scale (1 / toFloat (List.length boundingData))

        sceneRadius =
            boundingData
                |> List.map (\( c, r ) -> r + Vec3.distance sceneCenter c)
                |> List.maximum
                |> Maybe.withDefault 0.0
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
            RendererEffects.backgroundEntity options.backgroundColor

        sceneEntities =
            RendererScene3d.entities model.meshesScene3d model options

        fogEntities =
            RendererEffects.entities model.meshesWebGLFog model options

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
