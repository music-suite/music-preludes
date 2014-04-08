
clean:	
	rm -f golden/*.mid
	rm -f golden/*.xml
	rm -f golden/*.ly

generate-checksums:
	shasum *.music | shasum > reference_sum.sha

generate-originals:
	music2midi	-o golden/articulation_all_accents.mid	articulation_all_accents.music
	music2ly	-o golden/articulation_all_accents.ly		articulation_all_accents.music
	music2musicxml	-o golden/articulation_all_accents.xml		articulation_all_accents.music
	music2midi	-o golden/articulation_all_separations.mid    articulation_all_separations.music
	music2ly	-o golden/articulation_all_separations.ly	articulation_all_separations.music
	music2musicxml	-o golden/articulation_all_separations.xml	articulation_all_separations.music
	music2midi	-o golden/articulation_legato.mid     	articulation_legato.music
	music2ly	-o golden/articulation_legato.ly		articulation_legato.music
	music2musicxml	-o golden/articulation_legato.xml		articulation_legato.music
	music2midi	-o golden/articulation_portato.mid    	articulation_portato.music
	music2ly	-o golden/articulation_portato.ly		articulation_portato.music
	music2musicxml	-o golden/articulation_portato.xml		articulation_portato.music
	music2midi	-o golden/articulation_staccato.mid   	articulation_staccato.music
	music2ly	-o golden/articulation_staccato.ly		articulation_staccato.music
	music2musicxml	-o golden/articulation_staccato.xml		articulation_staccato.music
	music2midi	-o golden/dynamics_constant.mid       	dynamics_constant.music
	music2ly	-o golden/dynamics_constant.ly			dynamics_constant.music
	music2musicxml	-o golden/dynamics_constant.xml		dynamics_constant.music
	music2midi	-o golden/melody_chords.mid   		melody_chords.music
	music2ly	-o golden/melody_chords.ly			melody_chords.music
	music2musicxml	-o golden/melody_chords.xml			melody_chords.music
	music2midi	-o golden/meta_annotations.mid		meta_annotations.music
	music2ly	-o golden/meta_annotations.ly			meta_annotations.music
	music2musicxml	-o golden/meta_annotations.xml			meta_annotations.music
	music2midi	-o golden/meta_clef1.mid			meta_clef1.music
	music2ly	-o golden/meta_clef1.ly			meta_clef1.music
	music2musicxml	-o golden/meta_clef1.xml			meta_clef1.music
	music2midi	-o golden/meta_composer.mid			meta_composer.music
	music2ly	-o golden/meta_composer.ly			meta_composer.music
	music2musicxml	-o golden/meta_composer.xml			meta_composer.music
	music2midi	-o golden/meta_time_signature.mid		meta_time_signature.music
	music2ly	-o golden/meta_time_signature.ly		meta_time_signature.music
	music2musicxml	-o golden/meta_time_signature.xml		meta_time_signature.music
	music2midi	-o golden/misc_counterpoint.mid		misc_counterpoint.music
	music2ly	-o golden/misc_counterpoint.ly			misc_counterpoint.music
	music2musicxml	-o golden/misc_counterpoint.xml		misc_counterpoint.music
	music2midi	-o golden/octaves.mid				octaves.music
	music2ly	-o golden/octaves.ly				octaves.music
	music2musicxml	-o golden/octaves.xml				octaves.music
	music2midi	-o golden/overlay_chords.mid			overlay_chords.music
	music2ly	-o golden/overlay_chords.ly			overlay_chords.music
	music2musicxml	-o golden/overlay_chords.xml			overlay_chords.music
	music2midi	-o golden/overlay_voices.mid			overlay_voices.music
	music2ly	-o golden/overlay_voices.ly			overlay_voices.music
	music2musicxml	-o golden/overlay_voices.xml			overlay_voices.music
	music2midi	-o golden/pitch_inv.mid			pitch_inv.music
	music2ly	-o golden/pitch_inv.ly				pitch_inv.music
	music2musicxml	-o golden/pitch_inv.xml			pitch_inv.music
	music2midi	-o golden/sharpen.mid				sharpen.music
	music2ly	-o golden/sharpen.ly				sharpen.music
	music2musicxml	-o golden/sharpen.xml				sharpen.music
	music2midi	-o golden/simple_figure.mid			simple_figure.music
	music2ly	-o golden/simple_figure.ly			simple_figure.music
	music2musicxml	-o golden/simple_figure.xml			simple_figure.music
	music2midi	-o golden/simple_start_later.mid		simple_start_later.music
	music2ly	-o golden/simple_start_later.ly		simple_start_later.music
	music2musicxml	-o golden/simple_start_later.xml		simple_start_later.music
	music2midi	-o golden/single_note.mid			single_note.music
	music2ly	-o golden/single_note.ly			single_note.music
	music2musicxml	-o golden/single_note.xml			single_note.music
	music2midi	-o golden/special_gliss.mid			special_gliss.music
	music2ly	-o golden/special_gliss.ly			special_gliss.music
	music2musicxml	-o golden/special_gliss.xml			special_gliss.music
	music2midi	-o golden/special_harmonics.mid		special_harmonics.music
	music2ly	-o golden/special_harmonics.ly			special_harmonics.music
	music2musicxml	-o golden/special_harmonics.xml		special_harmonics.music
	music2midi	-o golden/special_text.mid			special_text.music
	music2ly	-o golden/special_text.ly			special_text.music
	music2musicxml	-o golden/special_text.xml			special_text.music
	music2midi	-o golden/special_tremolo.mid			special_tremolo.music
	music2ly	-o golden/special_tremolo.ly			special_tremolo.music
	music2musicxml	-o golden/special_tremolo.xml			special_tremolo.music
	music2midi	-o golden/stretch_single_note1.mid		stretch_single_note1.music
	music2ly	-o golden/stretch_single_note1.ly		stretch_single_note1.music
	music2musicxml	-o golden/stretch_single_note1.xml		stretch_single_note1.music
	music2midi	-o golden/stretch_single_note2.mid		stretch_single_note2.music
	music2ly	-o golden/stretch_single_note2.ly		stretch_single_note2.music
	music2musicxml	-o golden/stretch_single_note2.xml		stretch_single_note2.music
	music2midi	-o golden/stretch_single_note3.mid		stretch_single_note3.music
	music2ly	-o golden/stretch_single_note3.ly		stretch_single_note3.music
	music2musicxml	-o golden/stretch_single_note3.xml		stretch_single_note3.music
	music2midi	-o golden/times.mid				times.music
	music2ly	-o golden/times.ly				times.music
	music2musicxml	-o golden/times.xml				times.music
	music2midi	-o golden/track_single.mid			track_single.music
	music2ly	-o golden/track_single.ly			track_single.music
	music2musicxml	-o golden/track_single.xml			track_single.music
	music2midi	-o golden/track_single.mid			track_single.music
	music2ly	-o golden/voice_single.ly			voice_single.music
	music2musicxml	-o golden/voice_single.xml			voice_single.music

