module ProgOne exposing (..)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Animals exposing (..)
import Test exposing (addThree)
import Interop exposing (sendIncr, sendDecr, sendName, sendRadius)
import String exposing (append, toInt)
import Dom
import Task

main : Program Never Model Msg
main =
  --Html.beginnerProgram { model = model, view = view, update = update }
  Html.program {
    init = (model, Cmd.none),
    update = update,
    view = view, 
    subscriptions = subs
  }

type alias Model =
  { count: Int,
    content: String,
    thingName: String,
    panelWidth: Int,
    panelBorderRadius: Int,
    numberOfPanels: Int }

regularPanelWidth : Int
regularPanelWidth = 250

regularPanelHeight : Int
regularPanelHeight = 350

regularPanelHeaderFontSize : Int
regularPanelHeaderFontSize = 20

regularPanelBodyFontSize : Int 
regularPanelBodyFontSize = 15

normalScale : Float
normalScale = 1.0

miniMeScale : Float
miniMeScale = 0.6

model : Model
model =
  { count = 0,
    content = "hello",
    thingName = "thing",
    panelWidth = regularPanelWidth,
    panelBorderRadius = 20,
    numberOfPanels = 2 }

-- UPDATE
subs : Model -> Sub msg 
subs model = Sub.batch []

type Msg = 
      Increment 
    | Decrement 
    | ResetPanels
    | Change String 
    | ChangeThing String 
    | UpdateRadius String
    | AddPanel
    | NoOp

setFocus : Result Dom.Error () -> Msg
setFocus result = case result of
  Ok _  -> NoOp
  Err _ -> NoOp

blur : String -> Cmd Msg
blur id = 
  Task.attempt (\_ -> NoOp) (Dom.blur id)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      ({ model | count = model.count + 1 },
      (Cmd.batch [sendIncr (), blur "incr-button"]))
      -- also works as shorthand
      -- { model | count = model.count + 1 } ! 
      -- [ sendIncr(), blur "incr-button" ]

     
    Decrement ->
      ({ model | count = model.count - 1 }, sendDecr ())

    ResetPanels ->
      ({ model | numberOfPanels = 2 }, Cmd.none)

    Change newContent -> 
      ({ model | content = newContent }, sendName newContent)

    ChangeThing newThing ->
      ({ model | thingName = newThing }, Cmd.none)

    UpdateRadius newRadius -> 
      let updatedRadius = newRadius |> toInt |> Result.toMaybe |> Maybe.withDefault 0
      in ({ model | panelBorderRadius = updatedRadius }, sendRadius updatedRadius)

    AddPanel -> 
      ({ model | numberOfPanels = model.numberOfPanels + 1 }, Cmd.none)

    NoOp -> 
      (model, Cmd.none)

-- VIEW

getName : Animal -> String
getName animal =
  case animal of
    Cat name age ->
      name  
    Dog name age ->
      name

thing : Model -> Html Msg
thing model = 
  div [] 
    [ 
      div [] [text <| "I'm a " ++ model.thingName],
      div [] [input [value model.thingName, onInput ChangeThing] []],
      div [class "testing"] <| List.map (\n -> div [] [text <| "I'm a " ++ model.thingName]) (List.range 1 4)
    ]

styledButton : List (Html.Attribute msg) -> List (Html msg) -> Html msg
styledButton attrs children =
  let 
    btnStyle = Html.Attributes.class "btn btn-primary appButton" 
  in
    button (btnStyle :: attrs) children

buttonPanel : Html Msg 
buttonPanel = 
  div [] [
    styledButton [ id "incr-btn", onClick Increment ] [ text "Increment" ],
    styledButton [ onClick Decrement   ] [ text "Decrement" ],
    styledButton [ onClick AddPanel    ] [ text "Add Panel" ],
    styledButton [ onClick ResetPanels ] [ text "Reset Panels" ] 
  ]

asPixels : number -> String
asPixels numPixels = 
  let
    prependTo = flip append
  in
    numPixels |> toString |> prependTo "px"

minify : Int -> Int 
minify num = 
  num |> toFloat |> ((*) miniMeScale) |> round

miniPanel : Model -> Html Msg 
miniPanel model =
  let
    panelStyle = Html.Attributes.style [
      ("width", model.panelWidth |> minify |>asPixels),
      ("border-radius", model.panelBorderRadius |> minify |> asPixels),
      ("background-color", "white"),
      ("height", regularPanelHeight |> minify |>asPixels),
      ("overflow", "hidden"),
      ("margin", "10px 10px 10px 10px"),
      ("text-align", "center"),
      ("display", "inline-block")
    ]

    headerStyle = Html.Attributes.style [
      ("background-color", "#8eb9ff"),
      ("font-size", regularPanelHeaderFontSize |> minify |> asPixels)
    ]

    bodyStyle = Html.Attributes.style [
      ("font-size", regularPanelBodyFontSize |> minify |>asPixels)
    ]

    header = div [headerStyle] [text "Header Title"]
    body   = div [] [text "Body text goes here"]
  in
    div [panelStyle] [
      header,
      body
    ]



pPanel : Model -> Float -> Html Msg
pPanel model scale = 
  let
    -- Scales an integer value up or down, and then formats it 
    -- as a CSS pixel value string.
    scaledPixels : Int -> String
    scaledPixels val = val |> toFloat 
                        |> (*) scale 
                        |> round 
                        |> asPixels
    
    panelWidth        = model.panelWidth 
                        |> scaledPixels
    panelBorderRadius = model.panelBorderRadius 
                        |> scaledPixels
    panelHeight       = regularPanelHeight 
                        |> scaledPixels

    panelStyle = Html.Attributes.style [
      ("width",            panelWidth),
      ("border-radius",    panelBorderRadius),
      ("background-color", "white"),
      ("height",           panelHeight),
      ("overflow",         "hidden"),
      ("margin",           "10px 10px 10px 10px"),
      ("text-align",       "center"),
      ("display",          "inline-block")
    ]

    headerFontSize = regularPanelHeaderFontSize |> scaledPixels
    headerStyle = Html.Attributes.style [
      ("background-color", "#8eb9ff"),
      ("font-size"       , headerFontSize)
    ]

    bodyStyle = Html.Attributes.style [
      ("font-size", regularPanelBodyFontSize |> scaledPixels)
    ]

    header = div [headerStyle] [text "Header Title"]
    body   = div [] [text "Body text goes here"]
  in
    div [panelStyle] [
      header,
      body
    ] 

panelMaker : Model -> Float -> (a -> Html Msg)
panelMaker model scale = 
  let
    -- Scales an integer value up or down, and then formats it 
    -- as a CSS pixel value string.
    scaledPixels : Int -> String
    scaledPixels val = val |> toFloat 
                        |> (*) scale 
                        |> round 
                        |> asPixels
    
    panelWidth        = model.panelWidth 
                        |> scaledPixels
    panelBorderRadius = model.panelBorderRadius 
                        |> scaledPixels
    panelHeight       = regularPanelHeight 
                        |> scaledPixels

    panelStyle = Html.Attributes.style [
      ("animation-name", "dropHeader"),
      ("animation-iteration-count", "1"),
      ("animation-timing-function", "ease-in"),
      ("animation-duration", "0.4s"),
      ("width",            panelWidth),
      ("border-radius",    panelBorderRadius),
      ("background-color", "white"),
      ("height",           panelHeight),
      ("overflow",         "hidden"),
      ("margin",           "10px 10px 10px 10px"),
      ("text-align",       "center"),
      ("display",          "inline-block")
    ]

    headerFontSize = regularPanelHeaderFontSize |> scaledPixels
    headerStyle = Html.Attributes.style [
      ("background-color", "#8eb9ff"),
      ("font-size"       , headerFontSize)
    ]

    bodyStyle = Html.Attributes.style [
      ("font-size", regularPanelBodyFontSize |> scaledPixels)
    ]

    header = div [headerStyle] [text "Header Title"]
    body   = div [] [text "Body text goes here"]

    factory = \_ -> div [panelStyle , class "pricePanel"] [
                          header,
                          body
                    ] 
  in
    factory

view : Model -> Html Msg
view model =
  let 
    cat = Cat "Milo" 123
    dog = Dog "Otis" 456
    normalPanel = panelMaker model normalScale
    miniPanel   = panelMaker model miniMeScale
  in
    div [] [ 
        div [] [ text (toString model) ],
        buttonPanel,
        div [] [ text ("Current count is: " ++ (model.count |> toString) )],
        div [] [input  [ placeholder "Enter Something", onInput Change] []],
        div [] [ text ("Cat is really: " ++ (getName cat) ++ " and Dog is: " ++ (getName dog))],
        div [] [ text <| (\a -> "addThree result is: " ++ a) <| toString <| addThree 1 2 3 ],
        thing model,
        div [] [
          div [] [text "Radius:"],
          input [
            value (model.panelBorderRadius |> toString),
            type_ "range",
            Html.Attributes.min "0",
            Html.Attributes.max "100", 
            Html.Attributes.style [("width", 250 |> asPixels)],
            onInput UpdateRadius] []
        ],
        div [] ((List.map (\_ -> normalPanel []) (List.range 1 model.numberOfPanels))) --|> (flip List.append [miniPanel []]) )
      ] 