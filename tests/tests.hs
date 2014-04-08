
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
  This test will always fail if the test files have been edited.

  If you have edited one or more test files and DEFINITELY intend to make 
  your changes permanent, please assure that you are in a clean working
  directory (except for your edits), and then run:

      $ make generate
  
  You should commit all the resulting files along with your edits.
-}
sanity = testGroup "Sanity checks" [
  testMusicFileCheckSum,
  testGoldenFileChecksum
  ]

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

golden = testGroup "Regression tests" [
  testMusicFile "articulation_all_accents",
  testMusicFile "articulation_all_separations",
  testMusicFile "articulation_legato",
  testMusicFile "articulation_portato",
  testMusicFile "articulation_staccato",
  testMusicFile "dynamics_constant",
  testMusicFile "melody_chords",
  testMusicFile "meta_annotations",
  testMusicFile "meta_clef1",
  testMusicFile "meta_composer",
  testMusicFile "meta_time_signature",
  testMusicFile "misc_counterpoint",
  testMusicFile "octaves",
  testMusicFile "overlay_chords",
  testMusicFile "overlay_voices",
  testMusicFile "pitch_inv",
  testMusicFile "sharpen",
  testMusicFile "simple_figure",
  testMusicFile "simple_start_later",
  testMusicFile "single_note",
  testMusicFile "special_gliss",
  testMusicFile "special_harmonics",
  testMusicFile "special_text",
  testMusicFile "special_tremolo",
  testMusicFile "stretch_single_note1",
  testMusicFile "stretch_single_note2",
  testMusicFile "stretch_single_note3",
  testMusicFile "times",
  testMusicFile "track_single",
  testMusicFile "voice_single"
  ]