
module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.Process

main = defaultMain $ testGroup "All tests" [sanity, golden]

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

{-
  This test will always fail if the .music files have been edited.
  If you intend to commit your changes, please run
    make generate-checksums
-}
testMusicFileCheckSum =
  goldenVsFile
      "Test files OK"
      "golden/sum.sha"
      "current/sum.sha"
      (system "shasum *.music | shasum > current/sum.sha" >> return ())

sanity = testGroup "Sanity checks" [
  testMusicFileCheckSum
  ]

golden = testGroup "Regression tests" [
  testMusicFile "articulation_all_accents",
  testMusicFile "articulation_all_separations",
  testMusicFile "articulation_legato"
  ]