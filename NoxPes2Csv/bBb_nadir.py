from sys import argv
from nox_reader.nox_recording_class import Recording
from typing import List, Tuple
from nox_reader import cm, get_python_date, get_data_for_period
from numpy import array, ndarray
from typing import Optional
from datetime import datetime
from extract import extract

def middle_of(period: cm.Period) -> datetime:
    return get_python_date(period.From) + (get_python_date(period.To) - get_python_date(period.From))/2;

def nadir(pes_header: cm.ISignal, n: int, accumulator: Optional[float], recent_expiration: float, period: cm.Period) -> Tuple[datetime, float]:
    data = [effort for effort in get_data_for_period(pes_header, period) if effort != 0e1];
    greatest_effort = max(data);
    expiration = min(data);
    swing = greatest_effort - expiration;
    if expiration < recent_expiration and swing > 4*accumulator:
        return zero(period), accumulator, recent_expiration
    if None is accumulator:
        overestimated_mean_swing = 3*swing;
    else:
        overestimated_mean_swing = mean(n=n+1, accumulator=accumulator, next=swing);
    least_pressure = -greatest_effort;
    return (middle_of(period), least_pressure), overestimated_mean_swing, expiration;

def zero(period: cm.Period) -> Tuple[datetime, float]:
    return middle_of(period), 0e1

def split_recording_into_breaths(recording: Recording, signal: cm.ISignal) -> List[Tuple[datetime, float]]:
    #movements = recording.get_special_events("Activity");
    # PES 3 (or RIP Sum) results in way to short 'breaths'.
    # RIP Sum couldn't be expected to indicate breathing efforts during paradoxical movements, anyway.
    # Thorax is much better, but joins some breaths when the Thorax is almost still.
    # Using the Abdomen belt enabled the most accurate scoring of Pes events.
    rip_header = recording.get_signal_header_for_signal('Abdomen');
    pes_header = recording.get_signal_header_for_signal(signal);
    breaths = recording.get_breath_events(rip_header, recording.analysis_period);
    breaths_during_sleep = [];
    swing = None;
    recent_expiration = float('-inf');

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
                breath, swing, recent_expiration = nadir(pes_header=pes_header, n=len(breaths_during_sleep), accumulator=swing, recent_expiration=recent_expiration, period=breaths.pop(0));
                breaths_during_sleep.append(breath);
        epochs.pop(0);

    return array(breaths_during_sleep);

# n is the number of samples seen, including next.
# accumulator is the mean of the first n-1 samples.
# next is a sample not factored into accumulator.
# The return value is the mean of the first n samples, including next.
def mean(n: int, accumulator: float, next: float):
    return accumulator + (next - accumulator)/n;

extract(argv[1:], "nadir/" + "filtered", split_recording_into_breaths, "%s,%.18e");
