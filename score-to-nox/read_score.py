# encoding: UTF-8
from datetime import datetime
from glob import glob
from nox_reader import cm
from nox_reader import Recording
from nox_reader.nox_recording_class import Recording
from sys import stderr
from os import path

def mark_events(recording: Recording, score: str, base_scoring='PSGPes', signal='PES 3', log=False):
    with open(score, 'r', encoding='ascii') as lines:
        if log:
            stderr.write("base scoring: {}\n".format(base_scoring))
        recording.set_active_scoring_group(base_scoring)

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
        recording.save_scoring("Bjartur")


with open('../best_signal.py', 'rb') as file:
    exec(file.read()); #def best_signal(name: str)


if __name__ == '__main__':
    paths = glob("autoscored\\*\\")
    if(False):
        stderr.write("Does the recording {} exist? ".format(filepath))
        if(path.isdir(filepath)):
            stderr.write("Yes\n")
        else:
            stderr.write("No\n")
    for filepath in paths:
        recording = Recording(filepath, False)
        name = path.basename(path.dirname(filepath))
        base_scoring = 'PSGPes' if name != 'VSN-14-080-006' else 'PSG-Marta'
        score = '..\\csv-to-score\\output\\' + name + '.txt'
        mark_events(recording, score, base_scoring, best_signal(name), True)

