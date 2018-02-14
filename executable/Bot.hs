module Bot (myBot) where

import Protocol


myBot :: GameState Action
myBot = do
  return Pass
