overlap : *.hs
	stack build
output/%.txt : SHELL := /bin/bash
output/%.txt : overlap ../csv-to-score/output/*.txt
	mkdir -p output
	recordings=($*)
	stack exec overlap ${recordings[@]/%/.txt} > output/"$*"
all : output/*.txt
.PHONY : overlap all
