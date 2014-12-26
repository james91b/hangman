module HangmanSignal where

import Signal
import Char

guessesFromKeyboard : Signal.Channel (Maybe Char)
guessesFromKeyboard = Signal.channel Nothing
