#all: output/breaths/*/VSN-14-080-0??.txt
methods := unabrupt dip reversal abrupt baseline baserev complex
all: $(foreach method,$(methods), \
		$(wildcard output/$(method)/[2-5]/VSN-14-080-001.txt) \
		$(wildcard output/$(method)/[2-5]/VSN-14-080-0??.txt) \
		$(wildcard output/unabrupt/10sec/VSN-14-080-0??.txt) \
	)
test_results.txt: test/	*.hs src/*.hs cmd/*hs package.yaml stack.yaml csv-to-score.cabal
	stack build --test >test_results.txt
	tail -n2 test_results.txt
output/dip/%/:
	mkdir -p output/dip/{2,3,4,5}
output/unabrupt/%/:
	mkdir -p output/unabrupt/{2,3,4,5}
output/unabrupt/10sec/:
	mkdir -p output/unabrupt/10sec
output/reversal/%/:
	mkdir -p output/reversal/{2,3,4,5}
output/abrupt/%/:
	mkdir -p output/abrupt/{2,3,4,5}
output/baseline/%/:
	mkdir -p output/baseline/{2,3,4,5}
output/baserev/%/:
	mkdir -p output/baserev/{2,3,4,5}
output/complex/%/:
	mkdir -p output/complex/{2,3,4,5}

numbers:=2 3 4 5

define crescendo
output/dip/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/dip/$(number)/
	stack exec -- csv-to-score --dip -n$(number) output/dip "$$<"
output/unabrupt/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/unabrupt/$(number)/
	stack exec -- csv-to-score --unabrupt -n$(number) output/unabrupt "$$<"
output/reversal/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/reversal/$(number)/
	stack exec -- csv-to-score --reversal -n$(number) output/reversal "$$<"
output/abrupt/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/abrupt/$(number)/
	stack exec -- csv-to-score --abrupt -n$(number) output/abrupt "$$<"
output/baseline/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/baseline/$(number)/
	stack exec -- csv-to-score --baseline -n$(number) output/baseline "$$<"
output/baserev/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/baserev/$(number)/
	stack exec -- csv-to-score --baseline --reversal -n$(number) output/baserev "$$<"
output/complex/$(number)/%.txt : ../NoxPes2Csv/nadir/filtered/%.txt test_results.txt | output/complex/$(number)/
	stack exec -- csv-to-score --baseline --abrupt -n$(number) output/complex "$$<"
endef

$(foreach number,$(numbers), \
    $(eval $(crescendo)) \
)

output/unabrupt/10sec/%.txt : output/unabrupt/2/%.txt | output/unabrupt/10sec/
	<"$<" stack exec -- 10sec >"$@"

.PHONY: all
