from sys import stderr, argv
from nox_reader.nox_recording_class import Recording
from nox_reader import cm, get_python_date
from extract_nadirs import extract_nadirs
from typing import List

stderr.write('--Breath by Breath Nadir--\r\n');


def split_recording_into_breaths(recording: Recording) -> List[cm.Period]:
    # PES 3 (or RIP Sum) results in way to short 'breaths'.
    # RIP Sum couldn't be expected to indicate breathing efforts during paradoxical movements, anyway.
    # Thorax is much better, but joins some breaths when the Thorax is almost still.
    # Using the Abdomen belt enabled the most accurate scoring of Pes events.
    rip_header = recording.get_signal_header_for_signal('Abdomen');
    breaths = recording.get_breath_events(rip_header, recording.analysis_period);

    epochs = recording.get_sleep_events();
    breath_number = 0;
    while breath_number < len(breaths):
        while epochs[0].Period.get_To() < breaths[breath_number].get_From():
            epochs.pop(0);
        assert epochs[0].Period.get_From() <= breaths[breath_number].get_From();
        sleep_stage = epochs[0].get_Key().Type.__str__();
        if sleep_stage == 'sleep-wake':
            while breath_number < len(breaths) and breaths[breath_number].get_From() < epochs[0].Period.get_To():
                breaths.pop(breath_number);
        else:
            while breath_number < len(breaths) and breaths[breath_number].get_From() < epochs[0].Period.get_To():
                breath_number += 1;
        epochs.pop(0);

    return breaths;


extract_nadirs(argv[1:], split_recording_into_breaths, "BbB");
