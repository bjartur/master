from nox_reader import cm, get_python_date, get_data_for_period
from numpy import array, ndarray
from datetime import datetime
from extract import extract
from typing import Tuple


def nadir(pes_header: cm.ISignal, period: cm.Period) -> Tuple[datetime, float]:
    data = [effort for effort in get_data_for_period(pes_header, period) if effort != 0e1];
    time = get_python_date(period.To);
    greatest_effort = max(data);
    least_pressure = -greatest_effort;
    return time, least_pressure;

def get_nadirs(recording, periods, signal='PES 3') -> ndarray:
    pes_header = recording.get_signal_header_for_signal(signal);
    nadirs = array([
        nadir(pes_header, period) for period in periods
    ]);
    return nadirs;


def extract_nadirs(measurements, splitting_method, splitting_method_name) -> None:
    """
    :type measurements:          List[str]
    :type splitting_method:      Callable[[Recording], List[cm.Period]]
    :type splitting_method_name: str
    """
    extract(measurements, splitting_method, "nadir/" + splitting_method_name, get_nadirs, "%s,%.18e");
