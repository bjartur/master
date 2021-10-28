from os import path, makedirs
from sys import stderr
from nox_reader import get_recording_with_derived
from nox_reader.nox_recording_class import Recording
from nox_reader import cm
from typing import List, Callable
from numpy import ndarray, save, savetxt


with open('../best_signal.py', 'rb') as file:
    exec(file.read()); #def best_signal(name: str); def best_scoring_group


def extract(measurements, splitting_method_name, statistic, fmt) -> None:
    """
    :type measurements:          List[str]
    :type splitting_method_name: str
    :type statistic:             Callable[[Recording, List[cm.Period], str], ndarray])
    :type fmt:                   str
    """
    stderr.write('--Starting {}--\r\n'.format(splitting_method_name));
    try:
        for measurement in measurements:
            if  path.basename(path.dirname(measurement)) in ("VSN-14-080-030", "VSN-14-080-031", "VSN-14-080-002_needsfix"):
                continue;
            recording = get_recording_with_derived(measurement);
            measurement_name = path.basename(path.dirname(measurement));
            best_scoring_group(recording, measurement_name=measurement_name, log=True);
            statistics = statistic(recording, best_signal(path.dirname(measurement)));
            write(measurement_name, splitting_method_name, statistics, fmt);
    finally:
        stderr.write('--Terminating--\r\n');


def csvwriter(filepath: str, data: ndarray, fmt: str="%.18e") -> Callable[[str], None]:
    def savecsv(filename_extension: str):
        savetxt(filepath + filename_extension, data, fmt=fmt);
    return savecsv;


def write(measurement_name: str, path: str, statistics: ndarray, fmt: str):
    makedirs(path, mode=0o007, exist_ok=True);

    stderr.write("{0} has dimensions {1}.\r\n".format(measurement_name, statistics.shape));
    filename = "{0}/{1}".format(path, measurement_name);
    stderr.write('Saving ' + filename + '.npy...');
    save(filename, statistics);
    savecsv = csvwriter(filename, statistics, fmt);
    stderr.write(' Done.\r\nSaving ' + filename + '.txt.gz...');
    savecsv(".txt.gz");
    stderr.write(' Done.\r\nSaving ' + filename + '.txt...');
    savecsv(".txt");
    stderr.write(' Done.\r\n');
