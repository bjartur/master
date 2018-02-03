from nox_reader import *
from glob import glob
import sys
from os import path
from numpy import *

try:
	sys.stderr.write('----Running----\r\n');
	labels = [];
	
	measurements = glob("* */*");
        for measurement in measurements:
            r = get_recording_with_derived(measurement);
            signal_header = r.get_signal_header_for_signal('PES 1');
            epochs = r.get_sleep_events();
            data_per_epoch = [get_data_for_period(signal_header, epoch.Period) for epoch in epochs];
            time_series = array(data_per_epoch);
            filename = "serwatko/" + path.basename(measurement);
            sys.stderr.write('Saving ' + filename + '.npy...');
            save(filename, time_series);
            sys.stderr.write(' Done.\r\nSaving ' + filename + '.txt.gz...');
            savetxt(filename + '.txt.gz', time_series);
            sys.stderr.write(' Done.\r\nSaving ' + filename + '.txt...');
            savetxt(filename + '.txt', time_series);
            sys.stderr.write(' Done.\r\n');
finally:
	sys.stderr.write('--Terminating--\r\n');
