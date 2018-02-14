module Bot (myBot) where

import Protocol

import Control.Monad.IO.Class (liftIO)


myBot :: GameState Action
myBot = do
  return Pass
