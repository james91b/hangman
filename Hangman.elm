module Hangman where

import Window
import Signal
import Signal ( (<~), (~) )

import HangmanModel (..)
import HangmanUpdate (..)
import HangmanView (..)
import HangmanSignal (..)

--Wire The Application
hangman = initalHangmanState

hangmanState : Signal.Signal State
hangmanState = Signal.foldp step hangman (Signal.subscribe guessesFromKeyboard)

main = scene <~ hangmanState ~ Window.dimensions
