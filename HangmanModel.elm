module HangmanModel where

import Char
import String
import List (..)

totalAllowedFailures : Int
totalAllowedFailures = 9

type alias State =
    { goalWord: String
    , guesses: List Char
    }

initalHangmanState : State
initalHangmanState = {
    goalWord="Awesome",
    guesses=[]
  }

beenGuessed : List Char -> Char -> Bool
beenGuessed guesses letter = member letter guesses

winningGuesses : String -> List Char
winningGuesses goalWord = filter ((/=) ' ') (String.toList (String.toUpper goalWord))

dead : State -> Bool
dead hangman = (misses hangman) >= totalAllowedFailures

won : State -> Bool
won hangman = all (beenGuessed hangman.guesses) (winningGuesses hangman.goalWord)

letterGuessed : List Char -> Char -> Bool
letterGuessed guesses letter = member (Char.toLower letter) guesses || member (Char.toUpper letter) guesses

wordHasLetter : String -> Char -> Bool
wordHasLetter word letter = String.contains (String.toUpper (String.fromChar letter)) (String.toUpper word)

misses : State -> Int
misses hangman =
 hangman.guesses
 |> filter (not << wordHasLetter hangman.goalWord)
 |> length
