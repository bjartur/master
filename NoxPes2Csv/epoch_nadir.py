from sys import stderr
from nox_reader.nox_recording_class import Recording
from nox_reader import cm
from extract_nadirs import extract_nadirs
from typing import Iterator

stderr.write('--Epoch by Epoch Nadir--\r\n');


def split_recording_into_epochs(recording: Recording) -> Iterator[cm.Period]:
    epoch_markers = recording.get_sleep_events(whole_recording=False);
    return (epoch_marker.Period for epoch_marker in epoch_markers);


extract_nadirs(split_recording_into_epochs, "epoch");
