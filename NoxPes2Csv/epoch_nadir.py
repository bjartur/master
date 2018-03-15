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
        epoch_markers = r.get_sleep_events();
        pes_header = r.get_signal_header_for_signal('PES 3');
        nadir_per_epoch = array([
            min(get_data_for_period(pes_header, epoch.Period)) for epoch in epoch_markers
        ]);
        time_series = array(nadir_per_epoch);
        measurement_name = path.basename(measurement);
        stderr.write(measurement_name, time_series.shape);
        filename = "nadir/epoch/" + measurement_name;
        stderr.write('Saving ' + filename + '.npy...');
        save(filename, time_series);
        stderr.write(' Done.\r\nSaving ' + filename + '.txt.gz...');
        savetxt(filename + '.txt.gz', time_series);
        stderr.write(' Done.\r\nSaving ' + filename + '.txt...');
        savetxt(filename + '.txt', time_series);
        stderr.write(' Done.\r\n');
finally:
    stderr.write('--Terminating--\r\n');
