.ONESHELL:
all: nadir/BbB/VSN-14-080-*.*
nadir/BbB/VSN-14-080-%.txt : ../best_signal.py BbB_nadir.py extract.py VSN-14-080/VSN-14-080-%/
	source activate master
	python BbB_nadir.py VSN-14-080/VSN-14-080-$*/
.PHONY: all
