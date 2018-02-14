{-# LANGUAGE MultiWayIf #-}
module Main where

import Protocol
import Bot

import System.IO
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List              (isInfixOf)


main :: IO ()
main = do
  zipWithM_ hSetBuffering
    [stdin, stdout, stderr]
    (repeat LineBuffering)

  void . runGameState' . forever $ do
    upd <- liftIO getLine
    parseUpdate $ words upd

    when ("action move " `isInfixOf` upd)
      $ myBot >>= liftIO . print
