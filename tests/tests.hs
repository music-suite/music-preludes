
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

converter ext = case ext of
  "mid" -> "music2midi"
  "xml" -> "music2musicxml"
  "ly"  -> "music2ly"

testMusicFileAs ext name =
  goldenVsFile 
    (name ++ "." ++ ext)
    ("golden/" ++ name ++ "." ++ ext)
    ("current/" ++ name ++ "." ++ ext)
    ((system $ converter ext ++" -o current/"++name++"."++ext++" "++name++".music") 
      >> return ())

{-
  This test will always fail if the .music files have been edited.

  If you intend to make permanent changes to the test files, run

      $ make generate-checksums
  
  and check in the resulting .sha file to ensure that this test
  will continoue to work in the future.

-}
testMusicFileCheckSum =
  goldenVsFile
      "Original file checksums"
      "originals_ref.sha"
      "originals_check.sha"
      (system "shasum *.music | shasum > originals_check.sha" >> return ())

testGoldenFileChecksum =
  goldenVsFile
      "Generated file checksums"
      "generated_ref.sha"
      "generated_check.sha"
      (system "shasum golden/* | shasum > generated_check.sha" >> return ())

sanity = testGroup "Sanity checks" [
  testMusicFileCheckSum,
  testGoldenFileChecksum
  ]

golden = testGroup "Regression tests" [
  testMusicFile "articulation_all_accents",
  testMusicFile "articulation_all_separations",
  testMusicFile "articulation_legato"
  ]