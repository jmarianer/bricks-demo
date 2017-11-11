import Array exposing (Array)
import Board exposing (Board, Block, Orientation(..))
import Color exposing (rgb)
import Html exposing (Html, div, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onInput)
import Http
import ShowBoard
import StyleUtils exposing (..)
import Time exposing (Time, second)

-- MODEL
type alias Model = {
  input : String,
  current : Board,
  steps : List Board,
  enabled : Bool
  }

default = "334;2112Y;2010Y;2200Y;2102Z"
initialModel : Model
initialModel = { input = default, current = fromJust <| Board.toBoard default, enabled = False, steps = [] }

-- UPDATE
type BlockValue = Orientation Orientation | Length
type Value = Width | Height | Depth | Block Int BlockValue
type Msg = Tick Time | Solve | Solution (Result Http.Error String) | Load | Save | Input String | Set Value String

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"
realGet i array = fromJust <| Array.get i array

updateBoard : Board -> Value -> String -> Board
updateBoard board value string =
  let
    conversionResult = String.toInt string
    blocks = Array.push board.mainBlock board.blocks

    blocksToBoard : Array Block -> Board
    blocksToBoard blocks =
      { board | mainBlock = realGet (Array.length blocks - 1) blocks,
                blocks    = Array.slice 0 -1 blocks }

    set : Block -> BlockValue -> Int -> Block
    set block blockValue newValue =
      case blockValue of
        Orientation X -> { block | x = newValue }
        Orientation Y -> { block | y = newValue }
        Orientation Z -> { block | z = newValue }
        Length        -> { block | length = newValue }
  in
    case conversionResult of
      Ok newValue ->
        case value of
          Width     -> { board | width  = newValue }
          Height    -> { board | height = newValue }
          Depth     -> { board | depth  = newValue }
          Block i v ->
            let
              newBlock = set (realGet i blocks) v newValue
              newBlocks = Array.set i newBlock blocks
            in
              blocksToBoard newBlocks
      _ -> board

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    nextStep = case model.steps of
      []    -> { model | enabled = False }
      x::xs -> { model | steps = xs, current = x }

    newModel = case msg of
      Solution (Ok s) ->
        let
          boards = String.split "\n" s
        in
          case boards of
            _ :: bs -> { model | steps = List.map (fromJust << Board.toBoard) bs, enabled = True }
            _       -> model
      Tick _      -> nextStep
      Load        -> case Board.toBoard model.input of
        Nothing -> model
        Just b  -> { model | current = b, enabled = False }
      Save        -> { model | input = Board.serializeBoard model.current }
      Input s     -> { model | input = s }
      Set v s     -> { model | current = updateBoard model.current v s }
      _           -> model

    command = case msg of
      Solve -> Http.send Solution (Http.getString <| "/solve/" ++ Board.serializeBoard model.current)
      _     -> Cmd.none
  in
    (newModel, command)


-- VIEW
{-
-- TODO More hardcoded styles. BAD.
textareaStyle = style [
  ("flex-grow", "1"),
  ("border-radius", "5px"),
  ("margin", "2.5px 2.5px 2.5px 1.5px"),
  ("min-height", "60px"),
  ("pointer-events", "auto")
  ]

-- TODO: Move to Stylesheet.elm (?)
type MyStyles = None | Button
styleSheet = Style.styleSheet [
  Style.style Button [ Border.all 1, Border.rounded 5, Style.prop "padding" "5px" ]
  ]

-- TODO: Extract?, document
numberInput value num =
  html <| Html.input [
    type_ "number", Html.Attributes.min "0", Html.Attributes.max "10", Html.Attributes.value (toString num), onInput <| Set value
      ] []
spacer = html <| Html.div [style [("width", "50px")]] []

view : Model -> Html Msg
view model =
  let
    showBoard = [ShowBoard.toHtml model.current]

    -- TODO: Rename
    -- TODO: className should be exposed from the style sheet rather than hardcoded at the callsite.
    makeColumn className (i,block) = column Button [paddingXY 10 12, spacing 10, class className] [
      row None [] [text "X: ", numberInput (Block i <| Orientation X) block.x],
      row None [] [text "Y: ", numberInput (Block i <| Orientation Y) block.y],
      row None [] [text "Z: ", numberInput (Block i <| Orientation Z) block.z],
      row None [] [text "L: ", numberInput (Block i Length)           block.length]
    ]

    firstColumn = (makeColumn "MainBlock" (Array.length model.current.blocks, model.current.mainBlock))
    makeColumns = firstColumn :: List.map (makeColumn "OtherBlock") (Array.toIndexedList model.current.blocks)
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
        column None [paddingTop 100, spacing 5] [
          row None [spacing 10] makeColumns,
          button Button [onClick Solve] (text "Solve"),
          Element.spacer 5,
          el Button [] (text <| Board.serializeBoard model.current),
          Element.spacer 5,
          row None [width (percent 100), spacing 5] [
            html <| Html.textarea [textareaStyle, onInput Input] [],
            column None [spacing 10] [
              button Button [onClick Load] (text "Load")
            ]
          ]
        ]
      ]
-}

numberInput label value num =
  Html.label [] [
    text (label ++ ": "),
    Html.input [
      type_ "number", Html.Attributes.min "0", Html.Attributes.max "10", Html.Attributes.value (toString num), onInput <| Set value
    ] []
  ]

singleBlockInput : CssClass -> (Int, Block) -> Html Msg
singleBlockInput className (i, block) =
  div [classes [className, SingleBlockInput]] [
    numberInput "X" (Block i <| Orientation X) block.x,
    numberInput "Y" (Block i <| Orientation Y) block.y,
    numberInput "Z" (Block i <| Orientation Z) block.z,
    numberInput "L" (Block i Length)           block.length
  ]


view : Model -> Html Msg
view model =
  let
    mainBlockInput = singleBlockInput MainBlock (Array.length model.current.blocks, model.current.mainBlock)
    otherBlockInputs = List.map (singleBlockInput OtherBlock) (Array.toIndexedList model.current.blocks)
  in
    div [classes [Main]] [
      div [] [
        div [classes [BlocksContainer]] [ShowBoard.toHtml model.current],
        div [classes [BoardSize]] [
          numberInput "Width"  Width  model.current.width,
          numberInput "Height" Height model.current.height,
          numberInput "Depth"  Depth  model.current.depth
        ]
      ],
      div [classes [RightSideControls]] [
        div [classes [BlockInputs]] (mainBlockInput :: otherBlockInputs),
        Html.button [onClick Solve] [text "Solve"],
        div [classes [CurrentState]] [text <| Board.serializeBoard model.current],
        div [classes [Load]] [
          Html.textarea [onInput Input] [],
          Html.button [onClick Load] [text "Load"]
        ]
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
