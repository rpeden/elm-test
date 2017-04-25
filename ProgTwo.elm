module ProgTwo exposing (..)

import Html exposing (Html, button, div, text, h3, input)
import Interop exposing (recvIncr, recvDecr, recvName, recvRadius)
import String exposing (append)

main : Program Never Model Msg
main =
  Html.program {
    init = (model, Cmd.none),
    update = update,
    view = view, 
    subscriptions = subs
  }
type alias Model =
  { count: Int, name: String, panelRadius: Int }

subs : Model -> Sub Msg
subs model = Sub.batch [ 
  recvIncr Incr,
  recvDecr Decr,
  recvName UpdateName,
  recvRadius UpdateRadius
  ] 

model : Model
model =
  { 
    count = 0, 
    name = "",
    panelRadius = 0
  }

-- UPDATE

type Msg = Increment | Decrement | Incr () | Decr () | UpdateName String | UpdateRadius Int

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of 
    Increment ->
      ({ model | count = model.count + 1 }, Cmd.none)
     
    Decrement ->
      ({ model | count = model.count - 1 }, Cmd.none)

    Incr i ->
      ({ model | count = model.count + 1 }, Cmd.none)

    Decr _ ->
      ({ model | count = model.count - 1}, Cmd.none)

    UpdateName n ->
      ({ model | name = n }, Cmd.none)

    UpdateRadius r -> 
      ({ model | panelRadius = r}, Cmd.none )




nameDiv : Model -> (t -> Html msg)
nameDiv model = 
  \_ -> div [] [model.name |> toString |> append "Name is:" |> text]

view : Model -> Html Msg
view model =
  div [] [
    h3  [] [text "second module"],
    div [] [text ("Count is: " ++ (model.count |> toString))],
    div [] [text ("Radius is: " ++ (model.panelRadius |> toString))],
    div [] <| List.map (nameDiv model) (List.range 1 1)
  ]