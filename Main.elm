import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Animals exposing (..)
import AppModel exposing (Model, model)
import Test exposing (addThree)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }
  
-- UPDATE

type Msg = Increment | Decrement | Change String | ChangeThing String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | count = model.count + 1 }
     
    Decrement ->
      { model | count = model.count - 1 }

    Change newContent -> 
      { model | content = newContent }

    ChangeThing newThing ->
      { model | thingName = newThing }
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
  let 
    things = List.map 
              (\n -> div [] [text <| "I'm a " ++ model.thingName]) 
              (List.range 1 4)
  in
    div [] 
    [ 
      div [] [text <| "I'm a " ++ model.thingName],
      div [] [input [value model.thingName, onInput ChangeThing] []],
      div [class "testing"] <| things
    ]

view : Model -> Html Msg
view model =
  let 
    cat = Cat "Milo" 123
    dog = Dog "Otis" 54 
  in
    div []
    [ button [ onClick Decrement ] [ text "-" ],
      div [] [ text (toString model) ],
      button [ onClick Increment ] [ text "+" ],
      div [] [ text ("Current count is: " ++ (model.count |> toString) )],
      div [] [input  [ placeholder "Enter Something", onInput Change] []],
      div [] [ text ("Cat is: " ++ (getName cat) ++ " and Dog is: " ++ (getName dog))],
      div [] [ text <| (\a -> "addThree result is: " ++ a) <| toString <| addThree 1 2 3 ],
      thing model
    ]