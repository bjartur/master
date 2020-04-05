$(wildcard output/VSN-14-080-0??.csv) &: Manual/*.csv Main.hs nox2score.cabal package.yaml stack.yaml
	stack build --test
	stack exec nox2score output/ Manual/*
.PHONY : all
