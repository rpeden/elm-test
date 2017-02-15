module Messages exposing (..)

type alias Stops = List String

type StopsAction = Update Stops | Clear

updateStops: Model -> StopsAction -> Model
updateStops model stopsAction =
  case stopsAction of
    Update stopList ->
      { model | stops = stopList }
    Clear ->
      { model | stops = [] }

type alias Shapes = List String

type ShapesUpdateAction = UpdateShapes Shapes

type ShapesClearAction = ClearShapes 

type ShapesAction = ShapesUpdateAction | ShapesClearAction


type Action = Stops StopsAction | Shapes ShapesAction

type alias Model = 
  { stops: Stops,
    shapes: Shapes}

update : Model -> Action -> Model
update model action = 
  case action of 
    Stops s ->
      updateStops model s
    Shapes s -> 
      model

aString : String
aString = 
  let a = "I'm a " 
  in
  a ++ "string!"




