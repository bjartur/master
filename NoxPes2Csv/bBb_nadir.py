from glob import glob
from os import path
from sys import stderr
import sys
from nox_reader import *
from numpy import *

try:
    stderr.write('----Running----\r\n');
    labels = [];

    measurements = glob("PES PSG rescore/*");
    for measurement in measurements:
        r = get_recording_with_derived(measurement);
        rip_header = r.get_signal_header_for_signal('RIP Sum');
        breaths = r.get_breath_events(rip_header, r.analysis_period);
        pes_header = r.get_signal_header_for_signal('PES 3');
        nadir_per_breath = array([
            min(get_data_for_period(pes_header, breath)) for breath in breaths
        ]);
        time_series = array(nadir_per_breath);
        measurement_name = path.basename(measurement);
        stderr.write(measurement_name, time_series.shape);
        filename = "nadir/BbB/" + measurement_name;
        stderr.write('Saving ' + filename + '.npy...');
        save(filename, time_series);
        stderr.write(' Done.\r\nSaving ' + filename + '.txt.gz...');
        savetxt(filename + '.txt.gz', time_series);
        stderr.write(' Done.\r\nSaving ' + filename + '.txt...');
        savetxt(filename + '.txt', time_series);
        stderr.write(' Done.\r\n');
finally:
    stderr.write('--Terminating--\r\n');
