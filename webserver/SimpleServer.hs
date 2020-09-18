#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package text --package scotty --package atomic-primops

{- 
download stack using either:
* brew install haskell-stack
OR:
* curl -sSL https://get.haskellstack.org/ | sh
-}

{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe
import           Data.Monoid        ((<>))
import           Data.Text.Lazy     (Text)
import           Data.Text.Lazy
import           Web.Scotty         (ActionM, ScottyM, scotty)
import           Web.Scotty.Trans
import           Control.Monad.IO.Class
import           Data.Atomics.Counter

main :: IO ()
main = do
  counter <- newCounter 0
  scotty 8080 (get "/" $ (serveGreeting counter))

serveGreeting :: AtomicCounter -> ActionM ()
serveGreeting counter = do
    liftIO $ incrCounter_ 1 counter
    counterVal <- liftIO $ readCounter counter
    liftIO $ putStrLn $ "Server request number: " ++ show counterVal
    text "Hello from the server!"