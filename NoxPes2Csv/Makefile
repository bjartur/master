.ONESHELL:
all: nadir/filtered/VSN-14-080-*.txt
nadir/filtered/VSN-14-080-%.txt : ../best_signal.py bBb_nadir.py extract.py VSN-14-080/VSN-14-080-%/
	source activate master
	python bBb_nadir.py VSN-14-080/VSN-14-080-$*/
.PHONY: all
