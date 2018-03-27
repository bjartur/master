from nox_reader import cm, get_python_date, get_data_for_period
from numpy import array, ndarray
from datetime import datetime
from extract import extract
from typing import Tuple


def nadir(pes_header: cm.ISignal, period: cm.Period) -> Tuple[datetime, float]:
    data = get_data_for_period(pes_header, period);
    time = get_python_date(period.To);
    return time, min(data);

def get_nadirs(recording, periods) -> ndarray:
    pes_header = recording.get_signal_header_for_signal('PES 3');
    nadirs = array([
        nadir(pes_header, period) for period in periods
    ]);
    return nadirs;


def extract_nadirs(splitting_method, splitting_method_name) -> None:
    extract(splitting_method, "nadir/" + splitting_method_name, get_nadirs, "%s,%.18e");
