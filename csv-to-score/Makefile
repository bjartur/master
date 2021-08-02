#all: output/breaths/*/VSN-14-080-0??.txt
methods := unabrupt reversal baseline
all: $(foreach method,$(methods), \
		$(wildcard output/$(method)/[2-5]/VSN-14-080-001.txt) \
		$(wildcard output/$(method)/[2-5]/VSN-14-080-0??.txt) \
		$(wildcard output/unabrupt/10sec/VSN-14-080-0??.txt) \
	)
test_results.txt: src/*.hs cmd/*hs package.yaml stack.yaml csv-to-score.cabal
	stack build --test >test_results.txt
	tail -n2 test_results.txt
output/unabrupt/%/:
	mkdir -p output/unabrupt/{2,3,4,5}
output/unabrupt/10sec/:
	mkdir -p output/unabrupt/10sec
output/reversal/%/:
	mkdir -p output/reversal/{2,3,4,5}
output/baseline/%/:
	mkdir -p output/baseline/{2,3,4,5}

numbers:=2 3 4 5

define crescendo
output/unabrupt/$(number)/%.txt : ../NoxPes2Csv/nadir/BbB/%.txt test_results.txt | output/unabrupt/$(number)/
	stack exec -- csv-to-score --unabrupt -n$(number) output/unabrupt "$$<"
output/reversal/$(number)/%.txt : ../NoxPes2Csv/nadir/BbB/%.txt test_results.txt | output/reversal/$(number)/
	stack exec -- csv-to-score --reversal -n$(number) output/reversal "$$<"
output/baseline/$(number)/%.txt : ../NoxPes2Csv/nadir/BbB/%.txt test_results.txt | output/baseline/$(number)/
	stack exec -- csv-to-score --baseline -n$(number) output/baseline "$$<"
endef

$(foreach number,$(numbers), \
    $(eval $(crescendo)) \
)

output/unabrupt/10sec/%.txt : output/unabrupt/2/%.txt | output/unabrupt/10sec/
	<"$<" stack exec -- 10sec >"$@"

.PHONY: all
