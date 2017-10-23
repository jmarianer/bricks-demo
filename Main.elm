import Array
import Board exposing (Board, Orientation(..))
import Color exposing (rgb)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onInput)
import Board exposing (Board, Orientation(..))
import Time exposing (Time, second)
import ShowBoard
import SolveBoard
import Style
import Style.Color as Color
import Stylesheet exposing (cssString)

-- MODEL
type alias Model = {
  input : String,
  current : Board,
  steps : List Board,
  enabled : Bool
  }

default = "334;2112Y;2010Y;2200Y;2102Z"
initialModel : Model
initialModel = { input = "", current = Board.toBoard default, enabled = False, steps = [] }

-- UPDATE
type Value = Width | Height | Depth | Dummy --| X Int | Y Int | Z Int | Length Int
type Msg = Tick Time | Solve | Load | Input String | Set Value String

updateBoard : Board -> Value -> String -> Board
updateBoard board value string =
  let
    conversionResult = String.toInt string
  in
    case conversionResult of
      Ok newValue ->
        case value of
          Width  -> { board | width  = newValue }
          Height -> { board | height = newValue }
          Depth  -> { board | depth  = newValue }
          Dummy  -> board
      _ -> board

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    nextStep = case model.steps of
      []    -> { model | enabled = False }
      x::xs -> { model | steps = xs, current = x }

    newModel = case msg of
      Solve   -> { model | steps = SolveBoard.solveBoard model.current, enabled = True }
      Tick _  -> nextStep
      Load    -> { model | current = Board.toBoard model.input, enabled = False }
      Input s -> { model | input = s }
      Set v s -> { model | current = updateBoard model.current v s }
  in
    (newModel, Cmd.none)


-- VIEW
-- TODO: All these auxiliary functions need to go somewhere and also be documented
secondaryDivStyle = style [
  ("width", " 500px"),
  ("height", " 500px"),
  ("display", " flex"),
  ("justify-content", " center"),
  ("align-items", " center"),
  ("overflow", "none")
  ]

type MyStyles = None | Main | Other
styleSheet = Style.styleSheet [
  -- TODO: These colors are duplicated from Stylesheet.elm.
  Style.style Main  [ Color.background <| rgb 204 0 0 ],
  Style.style Other [ Color.background <| rgb 22 133 204 ]
  ]
numberInput value num =
  html <| Html.input [
    type_ "number", Html.Attributes.min "1", Html.Attributes.max "10", Html.Attributes.value (toString num), onInput <| Set value
      ] []
spacer = html <| Html.div [style [("width", "50px")]] []

view : Model -> Html Msg
view model =
  let
    showBoard =
      [Html.node "style" [] [Html.text cssString], ShowBoard.toHtml model.current]

    -- TODO: Rename
    makeColumn style block = column style [padding 5, spacing 10] [
      row None [] [text "X: ", numberInput Dummy block.x],
      row None [] [text "Y: ", numberInput Dummy block.y],
      row None [] [text "Z: ", numberInput Dummy block.z],
      row None [] [text "L: ", numberInput Dummy block.length]
    ]

    makeColumns =
      (makeColumn Main model.current.mainBlock) :: List.map (makeColumn Other) (Array.toList model.current.blocks)
  in
    layout styleSheet <|
      row None [center, width (percent 100), spacing 50] [
        column None [padding 50, spacing 50] [
          html <| Html.div [secondaryDivStyle] showBoard,
          row None [center] [
            text "Width: ",  numberInput Width  model.current.width, spacer,
            text "Height: ", numberInput Height model.current.height, spacer,
            text "Depth: ",  numberInput Depth  model.current.depth
          ]
        ],
        column None [paddingTop 100, spacing 50] [
          row None [spacing 10] makeColumns,
          button None [onClick Solve] (text "Solve"),
          Input.text None [] {
            onChange = Input,
            value = Board.serializeBoard model.current,
            label = Input.hiddenLabel "",
            options = []
          },
          button None [onClick Load] (text "Load")
        ]
      ]


-- MAIN
subscriptions : Model -> Sub Msg
subscriptions {enabled} =
  if enabled then Time.every second Tick else Sub.none

main =
  Html.program {
    init = (initialModel, Cmd.none),
    view = view,
    update = update,
    subscriptions = subscriptions
  }
