
clean:	
	rm -f tests/golden/*.mid
	rm -f tests/golden/*.xml
	rm -f tests/golden/*.ly
	rm tests/originals_ref.sha
	rm tests/generated_ref.sha

generate: generate-checksums

generate-checksums: generate-originals
	shasum tests/*.music  | shasum > tests/originals_ref.sha
	shasum tests/golden/* | shasum > tests/generated_ref.sha

generate-originals:
	music2midi	-o tests/golden/articulation_all_accents.mid		tests/articulation_all_accents.music
	music2ly	-o tests/golden/articulation_all_accents.ly		tests/articulation_all_accents.music
	music2musicxml	-o tests/golden/articulation_all_accents.xml		tests/articulation_all_accents.music
	music2midi	-o tests/golden/articulation_all_separations.mid    	tests/articulation_all_separations.music
	music2ly	-o tests/golden/articulation_all_separations.ly		tests/articulation_all_separations.music
	music2musicxml	-o tests/golden/articulation_all_separations.xml	tests/articulation_all_separations.music
	music2midi	-o tests/golden/articulation_legato.mid     		tests/articulation_legato.music
	music2ly	-o tests/golden/articulation_legato.ly			tests/articulation_legato.music
	music2musicxml	-o tests/golden/articulation_legato.xml			tests/articulation_legato.music
	music2midi	-o tests/golden/articulation_portato.mid    		tests/articulation_portato.music
	music2ly	-o tests/golden/articulation_portato.ly			tests/articulation_portato.music
	music2musicxml	-o tests/golden/articulation_portato.xml		tests/articulation_portato.music
	music2midi	-o tests/golden/articulation_staccato.mid   		tests/articulation_staccato.music
	music2ly	-o tests/golden/articulation_staccato.ly		tests/articulation_staccato.music
	music2musicxml	-o tests/golden/articulation_staccato.xml		tests/articulation_staccato.music
	music2midi	-o tests/golden/dynamics_constant.mid       		tests/dynamics_constant.music
	music2ly	-o tests/golden/dynamics_constant.ly			tests/dynamics_constant.music
	music2musicxml	-o tests/golden/dynamics_constant.xml			tests/dynamics_constant.music
	music2midi	-o tests/golden/melody_chords.mid   			tests/melody_chords.music
	music2ly	-o tests/golden/melody_chords.ly			tests/melody_chords.music
	music2musicxml	-o tests/golden/melody_chords.xml			tests/melody_chords.music
	music2midi	-o tests/golden/meta_annotations.mid			tests/meta_annotations.music
	music2ly	-o tests/golden/meta_annotations.ly			tests/meta_annotations.music
	music2musicxml	-o tests/golden/meta_annotations.xml			tests/meta_annotations.music
	music2midi	-o tests/golden/meta_clef1.mid				tests/meta_clef1.music
	music2ly	-o tests/golden/meta_clef1.ly				tests/meta_clef1.music
	music2musicxml	-o tests/golden/meta_clef1.xml				tests/meta_clef1.music
	music2midi	-o tests/golden/meta_composer.mid			tests/meta_composer.music
	music2ly	-o tests/golden/meta_composer.ly			tests/meta_composer.music
	music2musicxml	-o tests/golden/meta_composer.xml			tests/meta_composer.music
	music2midi	-o tests/golden/meta_time_signature.mid			tests/meta_time_signature.music
	music2ly	-o tests/golden/meta_time_signature.ly			tests/meta_time_signature.music
	music2musicxml	-o tests/golden/meta_time_signature.xml			tests/meta_time_signature.music
	music2midi	-o tests/golden/misc_counterpoint.mid			tests/misc_counterpoint.music
	music2ly	-o tests/golden/misc_counterpoint.ly			tests/misc_counterpoint.music
	music2musicxml	-o tests/golden/misc_counterpoint.xml			tests/misc_counterpoint.music
	music2midi	-o tests/golden/octaves.mid				tests/octaves.music
	music2ly	-o tests/golden/octaves.ly				tests/octaves.music
	music2musicxml	-o tests/golden/octaves.xml				tests/octaves.music
	music2midi	-o tests/golden/overlay_chords.mid			tests/overlay_chords.music
	music2ly	-o tests/golden/overlay_chords.ly			tests/overlay_chords.music
	music2musicxml	-o tests/golden/overlay_chords.xml			tests/overlay_chords.music
	music2midi	-o tests/golden/overlay_voices.mid			tests/overlay_voices.music
	music2ly	-o tests/golden/overlay_voices.ly			tests/overlay_voices.music
	music2musicxml	-o tests/golden/overlay_voices.xml			tests/overlay_voices.music
	music2midi	-o tests/golden/pitch_inv.mid				tests/pitch_inv.music
	music2ly	-o tests/golden/pitch_inv.ly				tests/pitch_inv.music
	music2musicxml	-o tests/golden/pitch_inv.xml				tests/pitch_inv.music
	music2midi	-o tests/golden/sharpen.mid				tests/sharpen.music
	music2ly	-o tests/golden/sharpen.ly				tests/sharpen.music
	music2musicxml	-o tests/golden/sharpen.xml				tests/sharpen.music
	music2midi	-o tests/golden/simple_figure.mid			tests/simple_figure.music
	music2ly	-o tests/golden/simple_figure.ly			tests/simple_figure.music
	music2musicxml	-o tests/golden/simple_figure.xml			tests/simple_figure.music
	music2midi	-o tests/golden/simple_start_later.mid			tests/simple_start_later.music
	music2ly	-o tests/golden/simple_start_later.ly			tests/simple_start_later.music
	music2musicxml	-o tests/golden/simple_start_later.xml			tests/simple_start_later.music
	music2midi	-o tests/golden/single_note.mid				tests/single_note.music
	music2ly	-o tests/golden/single_note.ly				tests/single_note.music
	music2musicxml	-o tests/golden/single_note.xml				tests/single_note.music
	music2midi	-o tests/golden/special_gliss.mid			tests/special_gliss.music
	music2ly	-o tests/golden/special_gliss.ly			tests/special_gliss.music
	music2musicxml	-o tests/golden/special_gliss.xml			tests/special_gliss.music
	music2midi	-o tests/golden/special_harmonics.mid			tests/special_harmonics.music
	music2ly	-o tests/golden/special_harmonics.ly			tests/special_harmonics.music
	music2musicxml	-o tests/golden/special_harmonics.xml			tests/special_harmonics.music
	music2midi	-o tests/golden/special_text.mid			tests/special_text.music
	music2ly	-o tests/golden/special_text.ly				tests/special_text.music
	music2musicxml	-o tests/golden/special_text.xml			tests/special_text.music
	music2midi	-o tests/golden/special_tremolo.mid			tests/special_tremolo.music
	music2ly	-o tests/golden/special_tremolo.ly			tests/special_tremolo.music
	music2musicxml	-o tests/golden/special_tremolo.xml			tests/special_tremolo.music
	music2midi	-o tests/golden/stretch_single_note1.mid		tests/stretch_single_note1.music
	music2ly	-o tests/golden/stretch_single_note1.ly			tests/stretch_single_note1.music
	music2musicxml	-o tests/golden/stretch_single_note1.xml		tests/stretch_single_note1.music
	music2midi	-o tests/golden/stretch_single_note2.mid		tests/stretch_single_note2.music
	music2ly	-o tests/golden/stretch_single_note2.ly			tests/stretch_single_note2.music
	music2musicxml	-o tests/golden/stretch_single_note2.xml		tests/stretch_single_note2.music
	music2midi	-o tests/golden/stretch_single_note3.mid		tests/stretch_single_note3.music
	music2ly	-o tests/golden/stretch_single_note3.ly			tests/stretch_single_note3.music
	music2musicxml	-o tests/golden/stretch_single_note3.xml		tests/stretch_single_note3.music
	music2midi	-o tests/golden/times.mid				tests/times.music
	music2ly	-o tests/golden/times.ly				tests/times.music
	music2musicxml	-o tests/golden/times.xml				tests/times.music
	music2midi	-o tests/golden/track_single.mid			tests/track_single.music
	music2ly	-o tests/golden/track_single.ly				tests/track_single.music
	music2musicxml	-o tests/golden/track_single.xml			tests/track_single.music
	music2midi	-o tests/golden/voice_single.mid			tests/voice_single.music
	music2ly	-o tests/golden/voice_single.ly				tests/voice_single.music
	music2musicxml	-o tests/golden/voice_single.xml			tests/voice_single.music

