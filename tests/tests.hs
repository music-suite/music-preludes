
module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.Process

main = defaultMain golden


testMusicFile name = testGroup name [
  testMusicFileAs "mid" name,
  testMusicFileAs "xml" name,
  testMusicFileAs "ly"  name
  ]

testMusicFileAs ext name =
  goldenVsFile 
    (name ++ "." ++ ext)
    (name ++ "." ++ ext)
    (name ++ "." ++ ext) -- TODO use the golden file
    (rawSystem "" [] >> return ())

testCheckSum =
  goldenVsFile
    "Sanity"
      "golden/sum.sha"
      "current/sum.sha"
      (system "shasum *.music | shasum > current/sum.sha" >> return ())

golden = testGroup "Regression tests" [
  testCheckSum,
  testMusicFile "articulation_all_accents",
  testMusicFile "articulation_all_separations",
  testMusicFile "articulation_legato"
  ]