all : output/*.txt
output/%.txt : SHELL := /bin/bash
output/%.txt : Main.hs overlap.cabal package.yaml stack.yaml ../csv-to-score/output/%.txt ../Nox2score/output/%.csv
	stack build
	mkdir -p output
	stack exec overlap ../csv-to-score/output/"$*".txt ../Nox2score/output/"$*".csv > output/"$*".txt
.PHONY : overlap all
