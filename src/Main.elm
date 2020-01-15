-- Game of Ur
module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, text, div, button, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random

main : Program () Model Message
main = Browser.element { init=init, update=update, view=view, subscriptions=subscriptions }

type alias Model = { board : GameBoard, player : Player, selection : Maybe Int, dice : Dice }
type Message = ChangePlayer | SelectToken Int | RollDice | DiceOutcome Dice

type Player = Red | Blue

subscriptions : Model -> Sub Message
subscriptions m = Sub.none

init : () -> (Model, Cmd Message)
init _ =
  let
    command = Cmd.none
    state = { board = initialBoard
            , player = Red
            , selection = Nothing
            , dice = [Black, Black, White, White] }
  in (state, command)

dieGenerator : Random.Generator Die
dieGenerator = Random.map (\i -> if modBy 2 i == 0 then Black else White) (Random.int 0 1)

update : Message -> Model -> (Model, Cmd Message)
update msg s = case msg of
  DiceOutcome ds ->
    ( { s | dice = ds } , Cmd.none)
  RollDice ->
    (s, Random.generate DiceOutcome (Random.list 4 dieGenerator))
  ChangePlayer ->
    if s.player == Red
    then ({ s | player = Blue }, Cmd.none)
    else ({ s | player = Red  }, Cmd.none)
  SelectToken selectedId ->
    let
      updateLane : Lane -> Lane
      updateLane l =
        List.map (\mt -> Maybe.map (\t -> { t | selected = t.id == selectedId }) mt) l

      b = s.board
      rel = updateLane b.redEnterLane
      bel = updateLane b.blueEnterLane
      wl  = updateLane b.warLane
      rll = updateLane b.redLeaveLane
      bll = updateLane b.blueLeaveLane

      newBoard = { b | redEnterLane = rel
                     , blueEnterLane = bel
                     , warLane = wl
                     , redLeaveLane = rll
                     , blueLeaveLane = bll }
    in
      ({ s | selection = Just selectedId, board = newBoard }, Cmd.none)

view : Model -> Html Message
view m =
  div [] [
    div [ style "border" "2px solid black" ] [
      button [onClick ChangePlayer] [text "Next"],
      text ("Current Player: " ++ Debug.toString m) ],
    drawBoard m.board,
    drawDice (700, 500) m.dice
    ]

side = 100
offsetX = 200
offsetY = 200

drawBoard : GameBoard -> Html Message
drawBoard board =
  let
    rightEnterCol = List.indexedMap (\i -> square (offsetX, offsetY + (3 - i) * side )) board.redEnterLane
    leftEnterCol = List.indexedMap (\i -> square (offsetX + 2 * side, offsetY + (3 - i) * side)) board.blueEnterLane
    middleCol = List.indexedMap (\i -> square (offsetX + side, offsetY + i * side)) board.warLane
    rightLeaveCol = List.indexedMap (\i -> square (offsetX, offsetY + (8 - i) * side)) board.redLeaveLane
    leftLeaveCol = List.indexedMap (\i -> square (offsetX + 2 * side, offsetY + (8 - i) * side)) board.blueLeaveLane
  in
    div [] (rightEnterCol ++ rightLeaveCol ++ middleCol ++ leftLeaveCol ++ leftEnterCol)

square : (Int, Int) -> Maybe Token -> Html Message
square (x,y) mt =
  let borderWidth = 2
      borderLessSize = side - borderWidth
      token = mt
            |> Maybe.map (\t -> [circle t])
            |> Maybe.withDefault []
  in
    div [style "position" "absolute",
         style "border" (String.fromInt borderWidth ++ "px solid black"),
         style "width" (String.fromInt borderLessSize ++ "px"),
         style "height" (String.fromInt borderLessSize ++ "px"),
         style "left" (String.fromInt x ++ "px"),
         style "top" (String.fromInt y ++ "px")] token

circle : Token -> Html Message
circle token =
  let circleWidth = 75
      padding = (side - circleWidth) / 2
  in
    div [style "position" "relative",
         style "left" (String.fromFloat padding ++ "px"),
         style "top" (String.fromFloat padding ++ "px"),
         style "height" (String.fromInt circleWidth ++ "px"),
         style "width" (String.fromInt circleWidth ++ "px"),
         style "border-radius" "50%",
         style "background-color" (tokenToColor token),
         onClick (SelectToken token.id)] []

drawDice : (Int, Int) -> Dice -> Html Message
drawDice (x,y) dice =
  let
    diceValue = dice |> List.filter (\d -> d == White) |> List.length
    diceElems = dice |> List.indexedMap (\i d -> die (x + i * 60,y) d)
    rollDiceButton =
      button [style "position" "absolute",
              style "left" (String.fromInt (x + 100) ++ "px"),
              style "top" (String.fromInt (y + 100) ++ "px"),
              onClick RollDice] [text "Roll"]
    diceValueText =
      div [style "position" "absolute",
           style "left" (String.fromInt (x + 100) ++ "px"),
           style "top" (String.fromInt (y - 100) ++ "px")]
          [text ("Value: " ++ String.fromInt diceValue)]
  in
    div [] (rollDiceButton :: diceValueText :: diceElems)

die : (Int, Int) -> Die -> Html Message
die (x,y) d =
  let
    coloring = case d of
      White -> "#FFF"
      Black -> "#000"

    borderWidth = 3
    diceSize = 50
    borderLessSize = diceSize - borderWidth
  in
    div [style "position" "absolute",
         style "border" (String.fromInt borderWidth ++ "px solid black"),
         style "background-color" coloring,
         style "width" (String.fromInt borderLessSize ++ "px"),
         style "height" (String.fromInt borderLessSize ++ "px"),
         style "left" (String.fromInt x ++ "px"),
         style "top" (String.fromInt y ++ "px")] []

tokenToColor : Token -> Color
tokenToColor t = case t.player of
  Red  -> if t.selected then "#F00" else "#A00"
  Blue -> if t.selected then "#00F" else "#00A"

type alias Color = String
type alias Token = { id : Int, player : Player, selected : Bool }
type alias Lane = List (Maybe Token)
type alias GameBoard = {
  redEnterLane : Lane,
  blueEnterLane : Lane,
  warLane : Lane,
  redLeaveLane : Lane,
  blueLeaveLane : Lane }
type Die = White | Black
type alias Dice = List Die
type alias GameAction = { commandType : ActionType, player : Player, dice : Dice }
type ActionType = NewToken | MoveToken Int

initialBoard : GameBoard
initialBoard =
  let
    emptyLane : Int -> Lane
    emptyLane n = List.repeat n Nothing
  in {
    redEnterLane = emptyLane 4,
    blueEnterLane = Just { id = 2, player = Blue, selected = False } :: emptyLane 3,
    warLane = Just { id = 1, player = Red, selected = False } :: emptyLane 8,
    redLeaveLane = emptyLane 3,
    blueLeaveLane = emptyLane 3 }

updateBoard : Dice -> GameBoard -> GameBoard
updateBoard d s = s
