import Array exposing (Array)
import Html exposing (Html, button)
import Html.Events exposing (onClick)
import Element exposing (..)
import Style



main = Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model = Array {
  rotate: (Int, Int, Int),
  translate: (Int, Int, Int)
  }

model : Model
model = Array.empty

-- UPDATE
type Msg
  = Add


update : Msg -> Model -> Model
update msg model =
  case msg of
    Add -> model

-- VIEW
styleSheet = Style.styleSheet [ Style.style [] [] ]
view : Model -> Html Msg
view model =
  Element.layout styleSheet <|
    column [] [] [ text "a", text "b" ]
  
