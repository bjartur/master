from glob import glob
from os import path
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
    :type statistic:             Callable[[Recording, List[cm.Period]], ndarray])
    :type fmt:                   str
    """
    try:
        measurements = glob("PES PSG rescore/*");
        for measurement in measurements:
            if " - 097ae" in measurement:
                continue;
            recording = get_recording_with_derived(measurement);
            periods = splitting_method(recording);
            statistics = statistic(recording, periods);
            measurement_name = path.basename(measurement);
            write(measurement_name, splitting_method_name, statistics, fmt);
    finally:
        stderr.write('--Terminating--\r\n');


def csvwriter(filepath: str, data: ndarray, fmt: str="%.18e") -> Callable[[str], None]:
    def savecsv(filename_extension: str):
        savetxt(filepath + filename_extension, data, fmt=fmt)
    return savecsv;


def write(measurement_name: str, path: str, statistics: ndarray, fmt: str):
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
