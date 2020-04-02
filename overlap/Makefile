all : output/*.txt
overlap : *.hs
	stack build
output/%.txt : SHELL := /bin/bash
output/%.txt : overlap ../csv-to-score/output/%.txt ../Nox2score/output/%.csv
	mkdir -p output
	stack exec overlap ../csv-to-score/output/"$*".txt ../Nox2score/output/"$*".csv > output/"$*".txt
.PHONY : overlap all
