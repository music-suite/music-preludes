
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-
  A webb with a Music Suite-aware interpreter
  Requires threepenny-gui-0.4.2.0
-}
module Main where

import           Data.Either
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Traversable
import           Music.Prelude hiding ((#), element, set, text, option, pre, never)
import qualified Music.Score               as Score
-- import qualified Data.List
-- import System.Random

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements

main :: IO ()
main = do
    startGUI defaultConfig
        { tpPort       = Just 1032
        , tpStatic     = Nothing
        } 
        -- setup
        (runR $ buttonR "Test")


-- Data that:
--   Can be used to create some GUI (in UI)
--   May vary depending on user input (in UI)
data R a = R {
    getR :: Element -> UI (Event a)
  }
buttonR :: String -> R ()
buttonR t = R $ \p -> do
  e <- set text t $ button
  return p #+ [return e]
  return $ fmap (const ()) $ click e


-- rowR :: [R a] -> R a
-- rowR rs = R (\e -> fmap (($ e) . getR) rs)

runR :: Show a => R a -> Window -> UI ()
runR (R r) window = do
  p <- getBody window
  e <- r p
  stop <- liftIO $ register e print
  -- stop is never called
  return ()
  











setup :: Window -> UI ()
setup window = do 
  body <- getBody window

  presets <- select
  return presets #+ [option # set text "Foo", option # set text "Bar"]
  input   <- textarea # set text "" # set rows "20" # set cols "80"
  button  <- button   # set text "Compile"
  result1 <- pre
  (accumB "|" $ fmap (const (\x -> x ++ "|")) $ valueChange input) >>= \presetB ->
    sink text presetB $ return result1

  return body #+ [column [
    return presets,
    return input, 
    return button,
    return result1
    ]]  
  return ()
