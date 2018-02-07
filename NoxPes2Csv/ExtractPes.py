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
	    rip_header = r.get_signal_header_for_signal('RIP Sum');
	    breaths = r.get_breath_events(rip_header, r.analysis_period);
	    pes_header = r.get_signal_header_for_signal('PES 3');
	    max_pes_per_breath = array(min(get_data_for_period(pes_header, breath)) for breath in breaths);
	    time_series = array(pes_per_breath);
	    print(time_series.shape);
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
