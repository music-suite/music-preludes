\include "lilypond-book-preamble.ly"
\paper {
  #(define dump-extents #t)

  indent = 0\mm
  line-width = 210\mm - 2.0 * 0.4\in
  ragged-right = ##t
  force-assignment = #""
  line-width = #(- line-width (* mm  3.000000))
}
\header {
  title = ""
  composer = ""
}
\layout {
}

<<
    \new Staff {   \set Staff.instrumentName = "" \set Staff.shortInstrumentName = "" \time 5/8 \clef treble {   c'8( d'8 e'8 f'8 g'8)
                                                                                                             }
               }
    \new Staff {   \set Staff.instrumentName = "" \set Staff.shortInstrumentName = "" \time 5/8 \clef treble {   c'8-. d'8-. e'8-. f'8-. g'8-.
                                                                                                             }
               }
    \new Staff {   \set Staff.instrumentName = "" \set Staff.shortInstrumentName = "" \time 5/8 \clef treble {   c'8(-. d'8-. e'8-. f'8-. g'8-.)
                                                                                                             }
               }
    \new Staff {   \set Staff.instrumentName = "" \set Staff.shortInstrumentName = "" \time 5/8 \clef treble {   c'8-- d'8-- e'8-- f'8-- g'8--
                                                                                                             }
               }
    \new Staff {   \set Staff.instrumentName = "" \set Staff.shortInstrumentName = "" \time 5/8 \clef treble {   c'8-_ d'8-_ e'8-_ f'8-_ g'8-_
                                                                                                             }
               }
    \new Staff {   \set Staff.instrumentName = "" \set Staff.shortInstrumentName = "" \time 5/8 \clef treble {   c'8-| d'8-| e'8-| f'8-| g'8-|
                                                                                                             }
               }
>>