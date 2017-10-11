import Array exposing (Array)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html)
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
    Add -> Array.push {rotate = (0,0,0), translate = (0,0,0)} model

-- VIEW
styleSheet = Style.styleSheet [ ]
view : Model -> Html Msg
view model =
  Element.layout styleSheet <|
    row {} [padding 50, spacing 50]
      ([ button {} [padding 20, onClick Add] (text "a") ] ++ Array.toList (Array.map (text << toString) model))
  
