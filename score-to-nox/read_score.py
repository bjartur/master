# encoding: UTF-8
from datetime import datetime
from nox_reader import cm
from nox_reader import Recording
from nox_reader.nox_recording_class import Recording
from sys import stderr
from os import path

def mark_first_events(recording: Recording, log=False):
    with open('..\csv-to-score\output') as input:
        arbitrarily_chosen_scoring_name = recording.get_all_scoring_names()[0]
        if (log):
            stderr.write("base scoring: {}\n".format(arbitrarily_chosen_scoring_name))
        recording.set_active_scoring_group(arbitrarily_chosen_scoring_name)

        for _ in range(10):
            row = input.readline().strip().split(',')
            beginning, end = [datetime.strptime(cell, '%Y-%m-%d %H:%M:%S.%f') for cell in row]
            duration = end - beginning
            start_time = row[0].replace(' ', 'T')[:-3]
            pes = recording.get_signal_header_for_signal('PES 3')
            if(log):
                stderr.write("Beginning: {}\n".format(beginning))
                stderr.write("End:       {}\n".format(end))
                stderr.write("Duration:              {}\n".format(duration))
            recording.add_marker_to_active_scoring(event_type="test1", start_time=start_time,
                                             signal_header=pes, duration=duration.total_seconds(), artifact=False)
        recording.save_scoring("Fyrsta hækkun")


if __name__ == '__main__':
    filepath = 'PES PSG rescore\\20141218T214507 - eb35b'
    if(False):
        stderr.write("Does the recording {} exist? ".format(filepath))
        if(path.isdir(filepath)):
            stderr.write("Yes\n")
        else:
            stderr.write("No\n")
    recording = Recording(filepath, False)
    mark_first_events(recording, True)
