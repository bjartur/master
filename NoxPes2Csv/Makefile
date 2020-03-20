.ONESHELL:
all: nadir/BbB/VSN-14-080-*.*
nadir/BbB/VSN-14-080-%.txt : BbB_nadir.py extract.py extract_nadirs.py # VSN-14-080/VSN-14-080-%/*
	source activate master
	python BbB_nadir.py VSN-14-080/VSN-14-080-$*/
.PHONY: all
