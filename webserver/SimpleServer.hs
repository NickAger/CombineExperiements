#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package text --package scotty

{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe
import           Data.Monoid        ((<>))
import           Data.Text.Lazy     (Text)
import           Data.Text.Lazy
import           Web.Scotty         (ActionM, ScottyM, scotty)
import           Web.Scotty.Trans
import           Control.Monad.IO.Class

main :: IO ()
main = do
  scotty 8080 (get "/" $ serveGreeting)

serveGreeting :: ActionM ()
serveGreeting = do
    liftIO $ putStrLn "request made for content"
    text "Hello from the server!"