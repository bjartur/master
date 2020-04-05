all: output/VSN-14-080-0??.txt
output/%.txt: ../NoxPes2Csv/nadir/BbB/%.txt src/*.hs cmd/*hs package.yaml stack.yaml csv-to-score.cabal
	stack build --test
	stack exec csv-to-score output/ $<
.PHONY: all
