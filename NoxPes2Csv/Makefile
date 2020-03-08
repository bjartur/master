.ONESHELL:
all: nadir/BbB/VSN-14-080-*.*
nadir/BbB/VSN-14-080-*.* &: BbB_nadir.py extract.py extract_nadirs.py
	source activate master
	python BbB_nadir.py
.PHONY: all
