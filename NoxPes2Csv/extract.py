from glob import glob
from os import path, makedirs
from sys import stderr
from nox_reader import get_recording_with_derived
from nox_reader.nox_recording_class import Recording
from nox_reader import cm
from typing import List, Callable
from numpy import ndarray, save, savetxt


def extract(splitting_method, splitting_method_name, statistic, fmt) -> None:
    """
    :type splitting_method:      Callable[[Recording], List[cm.Period]]
    :type splitting_method_name: str
    :type statistic:             Callable[[Recording, List[cm.Period], str], ndarray])
    :type fmt:                   str
    """
    try:
        measurements = glob("VSN-14-080\\*\\");
        print(measurements)
        for measurement in measurements:
            if  path.basename(path.dirname(measurement)) in ("VSN-14-080-031", "VSN-14-080-002_needsfix"):
                continue;
            recording = get_recording_with_derived(measurement);
            periods = splitting_method(recording);
            statistics = statistic(recording, periods, signal(measurement));
            measurement_name = path.basename(path.dirname(measurement));
            write(measurement_name, splitting_method_name, statistics, fmt);
    finally:
        stderr.write('--Terminating--\r\n');


def signal(measurement_name: str) -> str:
    return 'PES 2' if measurement_name.endswith('VSN-14-080-006/') else 'PES 3'


def csvwriter(filepath: str, data: ndarray, fmt: str="%.18e") -> Callable[[str], None]:
    def savecsv(filename_extension: str):
        savetxt(filepath + filename_extension, data, fmt=fmt)
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
    savecsv(".txt")
    stderr.write(' Done.\r\n');
