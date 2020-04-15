all: output/breaths/*/VSN-14-080-0??.txt
test_results.txt: src/*.hs cmd/*hs package.yaml stack.yaml csv-to-score.cabal
	stack build --test >test_results.txt
	tail -n2 test_results.txt
output/breaths/*/%.txt: ../NoxPes2Csv/nadir/BbB/%.txt test_results.txt
	mkdir -p output/breaths/{2,3,4,5}
	stack exec csv-to-score output/ $<
.PHONY: all
