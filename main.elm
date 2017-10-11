import Array exposing (Array)
import Debug
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html)
import Style



main = Html.beginnerProgram { model = model, view = view, update = update }


-- UTILS
fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

remove : Int -> Array a -> Array a
remove i a =
  let
    a1 = Array.slice 0 i a
    a2 = Array.slice (i+1) (Array.length a) a
  in
    Array.append a1 a2

names = ["rotateX", "rotateY", "rotateZ", "translateX", "translateY", "translateZ"]

-- MODEL
type alias Model = Array Item
type alias Item = Array Int

model : Model
model = Array.fromList [Array.fromList [0,0,0,0,0,0]]

-- UPDATE
type Msg
  = Add | Remove Int | Modify Int Int Int


update : Msg -> Model -> Model
update msg model =
  case msg of
    Add -> Array.push (Array.fromList [0,0,0,0,0,0]) model
    Remove i -> remove i model
    Modify i j add ->
      let
        oldArray = fromJust <| Array.get i model
        oldVal = fromJust <| Array.get j oldArray
        newArray = Array.set j (oldVal+add) oldArray
      in
        Array.set i newArray model

-- VIEW
styleSheet = Style.styleSheet [ ]
view : Model -> Html Msg
view model =
  Element.layout styleSheet <|
    row {} [padding 50, spacing 50]
      ([ button {} [padding 20, onClick Add] (text "+") ] ++ Array.toList (Array.indexedMap viewItem model))

viewItem : Int -> Item -> Element {} variation Msg
viewItem i item =
  let
    numNameVal : List (Int, String, Int)
    numNameVal = List.map2 (\val (num, name) -> (num, name, val)) (Array.toList item) (List.indexedMap (,) names)

    showVal num val = row {} [spread] [
      button {} [onClick <| Modify i num -1] (text "-"),
      text <| toString val,
      button {} [onClick <| Modify i num  1] (text "+")
    ]
  in
    column {} [] (
      (List.concatMap (\(num, name, val) -> [text name, showVal num val]) numNameVal)
      ++ [button {} [onClick <| Remove i] (text "-")])
    
