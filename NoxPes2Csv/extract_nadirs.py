from nox_reader import cm, get_python_date, get_data_for_period
from numpy import array, ndarray
from datetime import datetime
from extract import extract
from typing import Tuple


def nadir(pes_header: cm.ISignal, period: cm.Period) -> Tuple[datetime, float]:
    data = get_data_for_period(pes_header, period);
    time = get_python_date(period.To);
#   data is proportional to effort so we flip the sign to get pressure
    return time, -max(data);

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
