# encoding: UTF-8
from datetime import datetime
from glob import glob
from nox_reader import cm
from nox_reader import Recording
from nox_reader.nox_recording_class import Recording
from sys import stderr
from os import path

def mark_events(recording: Recording, signal='PES 3', log=False):
    with open('..\csv-to-score\output', 'r', encoding='ascii') as lines:
        scoring_names = recording.get_all_scoring_names()
        scoring_name = 'Sigga' if 'Sigga' in scoring_names else scoring_names[0]
        if log:
            stderr.write("base scoring: {}\n".format(scoring_name))
        recording.set_active_scoring_group(scoring_name)

        for line in lines:
            row = line.strip().split(',')
            beginning, end = [datetime.strptime(cell, '%Y-%m-%d %H:%M:%S.%f') for cell in row]
            duration = end - beginning
            start_time = row[0].replace(' ', 'T')[:-3]
            pes = recording.get_signal_header_for_signal(signal)
            if log:
                stderr.write("Beginning: {}\n".format(beginning))
                stderr.write("End:       {}\n".format(end))
                stderr.write("Duration:              {}\n".format(duration))
            recording.add_marker_to_active_scoring(event_type="autopes", start_time=start_time,
                                             signal_header=pes, duration=duration.total_seconds(), artifact=False)
        recording.save_scoring("Fyrsta hækkun")


if __name__ == '__main__':
    paths = ["autoscored/" + path.basename(original) for original in glob("../NoxPes2score/VSN-14-080/*/")]
    if(False):
        stderr.write("Does the recording {} exist? ".format(filepath))
        if(path.isdir(filepath)):
            stderr.write("Yes\n")
        else:
            stderr.write("No\n")
    for filepath in paths:
        recording = Recording(filepath, False)
        mark_events(recording, best_signal(filepath), True)


def best_signal(measurement_name: str) -> str:
    return 'PES 2' if measurement_name.endswith('VSN-14-080-006/') else 'PES 3'
