
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

  If you intend to make permanent changes to the test files, run

      $ make generate-checksums
  
  and check in the resulting .sha file to ensure that this test
  will continoue to work in the future.

-}
testMusicFileCheckSum =
  goldenVsFile
      "Test files OK"
      "reference_sum.sha"
      "actual_sum.sha"
      (system "shasum *.music | shasum > actual_sum.sha" >> return ())

sanity = testGroup "Sanity checks" [
  testMusicFileCheckSum
  ]

golden = testGroup "Regression tests" [
  testMusicFile "articulation_all_accents",
  testMusicFile "articulation_all_separations",
  testMusicFile "articulation_legato"
  ]