import Array exposing (Array)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html)
import Style



main = Html.beginnerProgram { model = model, view = view, update = update }


-- UTILS
remove : Int -> Array a -> Array a
remove i a =
  let
    a1 = Array.slice 0 i a
    a2 = Array.slice (i+1) (Array.length a) a
  in
    Array.append a1 a2



-- MODEL
type alias Model = Array {
  rotate: (Int, Int, Int),
  translate: (Int, Int, Int)
  }

model : Model
model = Array.empty

-- UPDATE
type Msg
  = Add | Remove Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    Add -> Array.push {rotate = (0,0,0), translate = (0,0,0)} model
    Remove i -> remove i model

-- VIEW
styleSheet = Style.styleSheet [ ]
view : Model -> Html Msg
view model =
  Element.layout styleSheet <|
    row {} [padding 50, spacing 50]
      ([ button {} [padding 20, onClick Add] (text "+") ] ++ Array.toList (Array.indexedMap viewItem model))


viewItem i item =
  column {} []
    [text <| toString item,
     button {} [onClick <| Remove i] (text "-")
    ]
  
