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

defaultBlock = { length = 0, x = 0, y = 0, z = 0, orientation = X }

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
type Msg = Tick Time | Solve | Solution (Result Http.Error String) | Load | Save | Input String | Set Value String | DeleteBlock Int | AddBlock

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

deleteBlock : Board -> Int -> Board
deleteBlock board i =
  let
    blocks = board.blocks
  in
    { board | blocks = Array.append (Array.slice 0 i blocks) (Array.slice (i+1) (Array.length blocks) blocks) }

addBlock : Board -> Board
addBlock board =
  let
    blocks = board.blocks
  in
    { board | blocks = Array.push defaultBlock blocks }

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
      DeleteBlock i -> { model | current = deleteBlock model.current i }
      AddBlock    -> { model | current = addBlock model.current }
      _           -> model

    command = case msg of
      Solve -> Http.send Solution (Http.getString <| "/solve/" ++ Board.serializeBoard model.current)
      _     -> Cmd.none
  in
    (newModel, command)


-- VIEW
numberInput label value num =
  Html.label [] [
    text (label ++ ": "),
    Html.input [
      type_ "number", Html.Attributes.min "0", Html.Attributes.max "10", Html.Attributes.value (toString num), onInput <| Set value
    ] []
  ]

inputElements i block = [
    numberInput "X" (Block i <| Orientation X) block.x,
    numberInput "Y" (Block i <| Orientation Y) block.y,
    numberInput "Z" (Block i <| Orientation Z) block.z,
    numberInput "L" (Block i Length)           block.length
  ]

singleBlockInput : CssClass -> Bool -> (Int, Block) -> Html Msg
singleBlockInput className allowDelete (i, block) =
  div [classes [className, SingleBlockInput]] (
    inputElements i block ++
    if allowDelete then [Html.img [Html.Attributes.src "Trash.svg", Html.Attributes.alt "delete", onClick (DeleteBlock i)] []] else []
  )

view : Model -> Html Msg
view model =
  let
    mainBlockInput = singleBlockInput MainBlock False (Array.length model.current.blocks, model.current.mainBlock)
    otherBlockInputs = List.map (singleBlockInput OtherBlock True) (Array.toIndexedList model.current.blocks)
    newBlock = div [classes [Disabled, SingleBlockInput], onClick AddBlock] <| inputElements -1 defaultBlock
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
        div [classes [BlockInputs]] (mainBlockInput :: otherBlockInputs ++ [newBlock]),
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
