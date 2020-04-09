from sys import argv
from nox_reader.nox_recording_class import Recording
from nox_reader import cm, get_python_date
from typing import List, Tuple
from nox_reader import cm, get_python_date, get_data_for_period
from numpy import array, ndarray
from datetime import datetime
from extract import extract

def middle_of(period: cm.Period) -> datetime:
    return get_python_date(period.From) + (get_python_date(period.To) - get_python_date(period.From))/2;

def nadir(pes_header: cm.ISignal, period: cm.Period) -> Tuple[datetime, float]:
    data = (effort for effort in get_data_for_period(pes_header, period) if effort != 0e1);
    greatest_effort = max(data);
    least_pressure = -greatest_effort;
    return middle_of(period), least_pressure;

def zero(period: cm.Period) -> Tuple[datetime, float]:
    return middle_of(period), 0e1

def split_recording_into_breaths(recording: Recording, signal: cm.ISignal) -> List[Tuple[datetime, float]]:
    # PES 3 (or RIP Sum) results in way to short 'breaths'.
    # RIP Sum couldn't be expected to indicate breathing efforts during paradoxical movements, anyway.
    # Thorax is much better, but joins some breaths when the Thorax is almost still.
    # Using the Abdomen belt enabled the most accurate scoring of Pes events.
    rip_header = recording.get_signal_header_for_signal('Abdomen');
    pes_header = recording.get_signal_header_for_signal(signal);
    breaths = recording.get_breath_events(rip_header, recording.analysis_period);
    breaths_during_sleep = [];

    epochs = recording.get_sleep_events();
    while len(breaths) > 0:
        while epochs[0].Period.get_To() < breaths[0].get_From():
            epochs.pop(0);

        while len(breaths) > 0 and breaths[0].get_From() < epochs[0].Period.get_From():
            # no sleep-wake stage scored
            breaths_during_sleep.append(zero(breaths.pop(0)));

        if epochs[0].get_Key().Type.__str__() == 'sleep-wake':
            while len(breaths) > 0 and breaths[0].get_From() < epochs[0].Period.get_To():
                breaths_during_sleep.append(zero(breaths.pop(0)));
        else:
            while len(breaths) > 0 and breaths[0].get_From() < epochs[0].Period.get_To():
                breaths_during_sleep.append(nadir(pes_header, breaths.pop(0)));
        epochs.pop(0);

    return array(breaths_during_sleep);


extract(argv[1:], "nadir/" + "BbB", split_recording_into_breaths, "%s,%.18e");
