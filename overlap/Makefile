numbers:=2 3 4 5

define compare_to_manual
output/breaths/$(number)/%.txt :  test_results.txt ../csv-to-score/output/breaths/$(number)/%.txt ../Nox2score/output/%.csv
	mkdir -p output/breaths/$(number)/
	stack exec overlap ../csv-to-score/output/breaths/$(number)/"$$*".txt ../Nox2score/output/"$$*".csv > output/breaths/$(number)/"$$*".txt
endef

define compare_to_3breaths
output/breaths/$(number)/%-3.txt :  test_results.txt ../csv-to-score/output/breaths/$(number)/%.txt ../csv-to-score/output/breaths/3/%.txt
	mkdir -p output/breaths/$(number)/
	stack exec overlap ../csv-to-score/output/breaths/$(number)/"$$*".txt ../csv-to-score/output/breaths/3/"$$*".txt  >output/breaths/$(number)/"$$*"-3.txt
endef

#$(foreach number,$(numbers), \
#    $(eval $(compare_to_manual)) \
#    $(eval $(compare_to_3breaths)) \
#)

all : test_results.txt
	stack exec overlap -- --leave1out
test_results.txt : Main.hs Plot.hs overlap.cabal package.yaml stack.yaml ../lib/src/Bjartur/Records.hs
	stack build --test >test_results.txt
.PHONY : all
