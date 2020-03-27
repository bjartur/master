all : output/*.csv
output/*.csv &: Manual/*.csv Main.hs *.cabal *.yaml
	stack build
	stack exec nox2score output/ Manual/*
.PHONY : all
