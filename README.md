This software was composed for scoring esophageal pressure swings, namely Pes crescendos, in 26 polysomnograms comprising a specific, pre-existing, dataset as part of a scientific study. If you want to use it for another dataset, some adaptions will be needed. **This documentation, data, grapics and source code is provided "as is" and is not merchantable without further modifications. In no event shall the authors be liable for any damages, direct or indirect, arising in connection with the use of this information or this software. The authors disclaim all warranties with regards to this information or this software, including implied warranties.** Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted.

If you have sleep-wake scored polysomnograms with esophageal manometry in the proprietary Noxturnal format, and a proprietary library from Nox Medical, you can use this software with modifications to score swings in esophageal pressure known as Pes crescendos in your polysomnograms. Alternatively, you supply your dataset in the format of CSV containing little but the peak-negative pressure of each breath. See NoxPes2Csv/nadir/BbB/\*.txt for examples of such CSVs. Replace them with CSVs based on your data. In special, NoxPes2Csv is hardcoded to exclude a polysomnogram named VSN-14-080-002. Additionally, the definition of the function score in the file lib/src/Records.hs needs to be revised to match the dataset. Currently, score uses the function forbid to exclude polysomnograms named VSN-14-080-013 and VSN-14-080-014.

Full data flow
---
If your polysomnograms are in the proprietary Noxturnal format, you can adapt the NoxPes2Csv/Makefile and the script NoxPes2Csv/BbB_nadir.py and its dependencies for your dataset. The Makefile mentions the VSN-14-080 dataset explicitly, and the Python script is hardcoded to exclude polysomnogram number 2 (from the VSN-14-080 set of polysomnograms). Change both to match your dataset.

### Polysomnogram with scored esophageal manometry
Polysomnograms ⇉|NoxPes2Csv|⇉ CSVs breaths ⇉|csv-to-score|⇉ CSVs of Pes crescendos ⇉|score-to-nox|⇉ Polysomnograms
<dl>
  <dt>CSV of breaths</dt><dd> has two columns: the datetime and the peak-negative pressure of each breath. The datetime can be any phase of the breath, e.g. the highest pressure or the lowest pressure, as long as it is consistently the same phase both within and across CSVs.
  <dt>CSV of Pes crescendos</dt><dd> has two columns: the datetime of the beginning of a Pes crescendo and the datetime of its end. A datetime in a CSV of Pes crescendos is always a verbatim copy of a datetime in a CSV of breaths. The output of `score-to-nox` is twelve copies of each Noxturnal format polysomnogram, with Pes crescendos marked on the esophageal pressure signal in each polysomnogram according to one of the 12 respiratory effort patterns implemented by `csv-to-score`.</dd>
</dl>

### Numerical report
A numerical report lists statistics on how frequently breathing matching a respiratory pattern also matched other respiratory patterns. `overlap` writes the numerical report is written to standard output.

Polysomnograms ⇉|NoxPes2Csv|⇉ CSVs of breaths ⇉|csv-to-score|⇉ CSVs of Pes crescendos ⇉|overlap|⇉ Numerical report  ⇇|overlap|⇇ CSVs of timestamped Pes crescendos ⇇|nox2score|⇇ Polysomnograms with differently prescored Pes crescendos (e.g. manually scored)

### Heatmaps
A heatmap tabulates the of the frequency of Pes crescendos by polysomnogram and the respiratory pattern defining a "Pes crescendo" chromatically.

Polysomnograms ⇉|NoxPes2Csv|⇉ CSVs of breaths ⇉|csv-to-score|⇉ CSVs of Pes crescendos ⇉|plot|⇉ Heatmaps


Short data flow
---
In practice, your polysomnograms may be in a different format. In this case, prepare your dataset as CSVs, one CSV per polysomnogram, containing little but the peak-negative pressure of each breath, timestamped. See NoxPes2Csv/nadir/BbB/\*.txt for examples of such CSVs. Make note of the big-endian datetime format. Choose which phase of a breath to timestamp freely but consistently. and replace them with CSVs based on your data.In special, NoxPes2Csv is hardcoded to exclude a polysomnogram named VSN-14-080-013. Additionally, the definition of the function score in the file lib/src/Records.hs needs to be revised to match the dataset. Currently, score uses the function forbid to exclude polysomnograms named VSN-14-080-013 and VSN-14-080-014. Consult the Makefiles for usage examples, but do not hesitate to modify them to refer to your dataset.

### Numerical report
CSVs of breaths ⇉|csv-to-score|⇉ CSVs of Pes crescendos ⇉|overlap|⇉ Numerical report

### Heatmaps
CSVs of breaths ⇉|csv-to-score|⇉ CSVs of Pes crescendos ⇉|plot|⇉ Heatmaps


Dependencies
---
* Haskell (Haskell Platform, including Stack; versions nightly-2019-09-21, lts-14.27, and lts-16.12 recommended)
  - plots-0.1.1.2
  - hashable-1.2.7.0
  - data-interval-2.0.1
  - extended-reals-0.2.4.0
* Make (GNU Make 4, optional)

### If using polysomnograms in the Noxturnal format
* Python (CPython 3.5.3)
* Microsoft Windows (with a .NET runtime)
* Nox Reader library
