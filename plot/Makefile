all: tally.heat.svg distribution.heat.svg

tally.heat.svg distribution.heat.svg &: Main.hs
	stack build
	stack exec plot >report.txt

.PHONY: all
