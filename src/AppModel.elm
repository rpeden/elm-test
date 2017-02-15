module AppModel exposing (Model, model)
-- MODEL

type alias Model =
  { count: Int,
    content: String,
    thingName: String }

model : Model
model =
  { count = 0,
    content = "hello",
    thingName = "thing" }