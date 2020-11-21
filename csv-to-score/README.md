A P<sub>es</sub> crescendo is a breathing pattern. `csv-to-score` is a program for matching supplied esophageal pressure data against one or more builtin P<sub>es</sub> crescendos, outputting the timestamps of matches. `csv-to-score` translates a CSV with peak inspiratory pressures and breath timestamps into a CSV with the start and end timestamps matching a "P<sub>es</sub> crescendo."

# Build

To compile, install [Stack](https://docs.haskellstack.org/en/stable/README/) and then execute:
```
stack build
```

# Run

The program can be executed using `stack exec --`. For example:
```
stack exec -- csv-to-score --help
```
<pre>
Usage: csv2score [--unabrupt|--reversal|--baseline] [-nN] [--] DESTINATION FILE...
If no method is specified, baseline is used by default.
Reversal drops the requirement that every nadir be under baseline.
Unabrupt additionally drops the requirement that a crescendo be followed by an nadir above baseline.
The only legal value for N is 0 which represents varying the the minimum number of increases in negative pressure from 2 to 5.
</pre>

## Methods
Three sets of P<sub>es</sub> crescendos are built in. Each P<sub>es</sub> crescendo is defined by a set of criteria: `--unabrupt` (or `--simple`) by a dropping pressures; `--reversal` (or `--medium`) by dropping pressures followed by a resurge in pressure; and `--baseline` (or `--complex`) by a maximum pressure, dropping pressures and a resurge in pressure. These sets of criteria are sub- and supersets of each other, but do not match sub- and supersets of breathing, compared to each other, due to variations in what breathing is considered not matching the P<sub>es</sub> crescendo and thus suitable for use as a baseline.

| Method | `--unabrupt`         | `--reversal`                                  | `--baseline` |
|:------:|:--------------------:|:---------------------------------------------:|:------------:|
| **Alternative name** | `--simple`           | `--medium`                                    | `--complex` |
| **Description** | At least _N_ breaths with successively lower peak inspiratory pressures | followed by an abrupt reversal above baseline | where the peak inspiratory pressure in each of the at least _N_ breaths preceding the reversal is below baseline |

## Minimum length

## Destination
As a commandline argument, please supply a directory where output directories and files can be created.

## Input files
An example of an input CSV file with peak inspiratory pressures and breath timestamps can be found in ../NoxPes2Csv/nadir/BbB/. Provide the path to each input file as a commandline argument.

# Test
Modifications can be regression tested using

```
stack test
```

