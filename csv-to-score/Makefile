#all: output/breaths/*/VSN-14-080-0??.txt
all: output/baseline/*/VSN-14-080-0??.txt
test_results.txt: src/*.hs cmd/*hs package.yaml stack.yaml csv-to-score.cabal
	stack build --test >test_results.txt
	tail -n2 test_results.txt
output/breaths/%/:
	mkdir -p output/breaths/{2,3,4,5}
output/baseline/%/:
	mkdir -p output/baseline/{2,3,4,5}

numbers:=2 3 4 5

define crescendo
output/breaths/$(number)/%.txt : ../NoxPes2Csv/nadir/BbB/%.txt test_results.txt | output/breaths/$(number)/
	stack exec csv-to-score output/ "$$<"
output/baseline/$(number)/%.txt : ../NoxPes2Csv/nadir/BbB/%.txt test_results.txt | output/baseline/$(number)/
	stack exec csv-to-score output/ "$$<"
endef

$(foreach number,$(numbers), \
    $(eval $(crescendo)) \
)

.PHONY: all
