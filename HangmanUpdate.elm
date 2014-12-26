module HangmanUpdate where

import Set
import String
import Char
import List (..)

import HangmanModel (..)

step : Maybe Char -> State -> State
step guess state =
  if (misses state) >= 9
  then
   state
  else
    case guess of
      Just letter ->
        { state | guesses <- letter :: state.guesses }
      Nothing ->
        state
