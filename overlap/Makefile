report.txt : test_results.txt
	stack exec overlap -- --leave1out >report.txt
test_results.txt : Tests.hs Main.hs Plot.hs Histogram.hs overlap.cabal package.yaml stack.yaml ../lib/src/Bjartur/Records.hs
	stack build --test >test_results.txt
fast : report.txt
	stack exec overlap
.PHONY : fast
