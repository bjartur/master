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

def nadir(pes_header: cm.ISignal, n: int, accumulator: Optional[float], recent_expiration: float, period: cm.Period) -> Tuple[Tuple[datetime, float], float, float]:
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

def drop(until, markers: List[cm.Marker]):
    while len(markers) > 0 and markers[0].Period.get_To() < until:
        markers.pop(0);

def exclude_next_breath(input_breaths: List[cm.Periods], output_pressures: List[Tuple[datetime, float]]):
    output_pressures.append(zero(input_breaths.pop(0)));

def is_next_breath_before(input_breaths: List[cm.Periods], until):
    return len(input_breaths) > 0 and input_breaths[0].get_From() < until;

def excluder(input_breaths: List[cm.Periods], output_pressures: List[Tuple[datetime, float]]):
    def exclude(until):
        while is_next_breath_before(input_breaths=input_breaths, until=until):
            exclude_next_breath(input_breaths=input_breaths, output_pressures=output_pressures);
    return exclude;

def split_recording_into_breaths(recording: Recording, signal: cm.ISignal) -> ndarray:
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
    movements = recording.get_special_events("Activity");
    exclude = excluder(input_breaths=breaths, output_pressures=breaths_during_sleep);
    while len(breaths) > 0:
        # I := breaths is a chronological list of a breath or consecutive breaths in the analysis period, ...
        #  ... breaths_during_sleep is empty or a chronological list of the breaths preceding those in breaths, ...
        #  ... all breaths in breaths_during_sleep are either from epochs labeled as sleep and ...
        #  ... have pressure data available, or ...
        #  ... have their pressure reported as positive zero (no less and no more).
        drop(until=breaths[0].get_From(), markers=epochs);
        # I and epochs[0] starts at the same time or later than breaths[0] (either labeled sleep or labeled as wake).
        exclude(until=epochs[0].Period.get_From()); # crashes if the last breaths have not be sleep-wake staged
        # I and breaths[0] is in epochs[0], which is either labeled as sleep or labeled as wake.

        if epochs[0].get_Key().Type.__str__() == 'sleep-wake':
            exclude(until=epochs[0].Period.get_To());
            # epochs[0] is labeled as wake and I.
        else:
            # I and S := breaths[0] is in epochs[0] and epochs[0] is labeled as sleep.
            while is_next_breath_before(breaths, epochs[0].Period.get_To()):
                # I and S
                drop(until=breaths[0].get_From(), markers=movements);
                # I, S, and movements is empty or movements[0] starts at the same or later than the next breath.
                if 0 == len(movements) or breaths[0].get_To() < movements[0].Period.get_From():
                    # I, S, and movements is empty or breaths[0] ends before movements[0] begins.
                    breath, swing, recent_expiration = nadir(pes_header=pes_header, n=len(breaths_during_sleep), accumulator=swing, recent_expiration=recent_expiration, period=breaths.pop(0));
                    breaths_during_sleep.append(breath);
                else:
                    # I, S, and movements[0] starts during the next breath.
                    exclude(until=movements[0].Period.get_To());
                    # I and movements[0] ends at the same time as breaths[0] or later.
                    if len(breaths) > 0 and movements[0].Period.get_To() == breaths[0].get_To():
                        exclude_next_breath(breaths, breaths_during_sleep);
        epochs.pop(0);

    return array(breaths_during_sleep);

# n is the number of samples seen, including next.
# accumulator is the mean of the first n-1 samples.
# next is a sample not factored into accumulator.
# The return value is the mean of the first n samples, including next.
def mean(n: int, accumulator: float, next: float):
    return accumulator + (next - accumulator)/n;

extract(argv[1:], "nadir/" + "filtered", split_recording_into_breaths, "%s,%.18e");
