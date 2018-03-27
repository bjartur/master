from sys import stderr
from nox_reader.nox_recording_class import Recording
from nox_reader import cm
from extract_nadirs import extract_nadirs
from typing import List

stderr.write('--Breath by Breath Nadir--\r\n');


def split_recording_into_breaths(recording: Recording) -> List[cm.Period]:
    rip_header = recording.get_signal_header_for_signal('RIP Sum');
    breaths = recording.get_breath_events(rip_header, recording.analysis_period);
    return breaths;


extract_nadirs(split_recording_into_breaths, "BbB");
