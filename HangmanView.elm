module HangmanView where

import Char
import String
import Text
import Graphics.Input as Input
import Signal
import Debug
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)

import Graphics.Element (..)
import List (..)

import HangmanModel (..)
import HangmanSignal (..)

hangmanWidth = 500

defaultSpacer : Element
defaultSpacer = spacer 0 30

reshape : Int -> List a -> List (List a)
reshape columns list =
  case list of
    [] -> []
    _ -> take columns list :: reshape columns(drop columns list)

displayKeyboardBtn : Char -> Element
displayKeyboardBtn char =
  (String.fromChar char)
  |> Text.fromString
  |> Text.height 21
  |> Text.leftAligned
  |> container 25 25 middle

keyboardBtn : State -> Char -> Element
keyboardBtn hangman char =
  if (letterGuessed hangman.guesses char)
  then spacer 25 25
  else Input.customButton (Signal.send guessesFromKeyboard (Just char)) (displayKeyboardBtn char) (displayKeyboardBtn char) (displayKeyboardBtn char)

keyboardRow : State -> List Char -> Element
keyboardRow hangman keys = flow right (map (keyboardBtn hangman) keys)

keyboard : State -> Element
keyboard hangman =
 reshape 5 ['A', 'B', 'C', 'D', 'E',
            'F', 'G', 'H', 'I', 'J',
            'K', 'L', 'M', 'N', 'O',
            'P', 'Q', 'R', 'S', 'T',
            'U', 'V', 'W', 'X', 'Y',
            'Z']
 |> map (keyboardRow hangman)
 |> flow down

displayLetter : List Char -> Char -> Char
displayLetter guesses letter =
  if not (letterGuessed guesses letter) && (not (letter == ' '))
  then '_'
  else letter

displayWord guesses word  =
 map (displayLetter guesses) (String.toList word)
 |> intersperse ' '
 |> String.fromList
 |> Text.fromString
 |> Text.height 34
 |> Text.centered

displayGuesses guesses word =
  guesses
  |> filter (not << wordHasLetter word)
  |> map String.fromChar
  |> String.join " "
  |> Text.fromString
  |> Text.height 21
  |> Text.centered

blackRect : Float -> Float -> Form
blackRect w h = rect w h |> filled black

gallowDrawSteps : List Form
gallowDrawSteps = [
    --gallows
    blackRect 300 15 |> move (0,-145),
    blackRect 15 300 |> move (-75, 0),
    blackRect 150 15 |> move (0, 130),
    blackRect 10 30 |> move (70, 110),
    --head
    circle 25 |> filled black |> move (70, 75),
    --body
    blackRect 10 50 |> move (70, 30),
    --legs
    blackRect 10 50 |> rotate (degrees 30) |> move (83, -10),
    blackRect 10 50 |> rotate (degrees -30) |> move (58, -10),
    --Arms
    blackRect 50 10 |> move (70, 30)
  ]

displayGallows hangman =
 collage 300 300 (take (misses hangman) gallowDrawSteps)
 |> container 300 300 middle
 |> color lightGrey

gallowsAndKeyboard : State -> Element
gallowsAndKeyboard hangman =
  flow right [
    flow down [
      previousGuesses hangman,
      spacer 0 10,
      displayGallows hangman
    ],
    container 185 340 midBottom (keyboard hangman)
  ]

previousGuesses : State -> Element
previousGuesses hangman =
  if (isEmpty hangman.guesses) || (misses hangman) == 0
  then defaultSpacer
  else displayGuesses hangman.guesses hangman.goalWord

guessesLeft : State -> Int
guessesLeft hangman = totalAllowedFailures - (misses hangman)

remainingGuesses : State -> Element
remainingGuesses hangman =
  Text.fromString ("You only have " ++ toString (guessesLeft hangman) ++ " incorrect guesses left!")
  |> Text.centered

header : Int -> Element
header w =
  Text.fromString "Hangman"
    |> Text.height 55
    |> Text.centered
    |> width w

displayDeadScene : State -> Int -> Element
displayDeadScene hangman w =
  flow down [
    displayGallows hangman |> containerAutoHeight w middle,
    defaultSpacer,
    Text.plainText "Sorry you are DEAD, the word was..."
      |> containerAutoHeight w middle,
    hangman.goalWord
      |> Text.fromString
      |> Text.height 34
      |> Text.centered
      |> containerAutoHeight w middle
  ]

containerAutoHeight : Int -> Position -> Element -> Element
containerAutoHeight width position elm = container width (heightOf elm) position elm

displayPlayingScene : State -> Int -> Element
displayPlayingScene hangman w =
  flow down [
    gallowsAndKeyboard hangman |> containerAutoHeight w middle,
    defaultSpacer,
    displayWord hangman.guesses hangman.goalWord |> containerAutoHeight w middle,
    defaultSpacer,
    remainingGuesses hangman |> containerAutoHeight w middle
  ]

displayWonScene : State -> Int -> Element
displayWonScene hangman w =
  flow down [
    defaultSpacer,
    Text.plainText "Congratulations! You got the word!"
      |> containerAutoHeight w middle,
    defaultSpacer,
    displayWord hangman.guesses hangman.goalWord
      |> containerAutoHeight w middle
    ]

displayBody : State -> Int -> Element
displayBody hangman w =
  if
    | dead hangman -> displayDeadScene hangman w
    | won hangman -> displayWonScene hangman w
    | otherwise -> displayPlayingScene hangman w

scene : State -> (Int, Int) -> Element
scene hangman (w, h) =
  let pos = midTopAt (relative 0.5) (absolute 40) in
    flow down [
        header w,
        containerAutoHeight w middle <| displayBody hangman w
      ]
      |> container w h pos
