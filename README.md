This software was made for scoring esophageal pressure swings, namely Pes crescendos, in a single dataset of 26 polysomnograms. If you want to use it for another dataset, some adaptions will be needed.

If you have sleep-wake scored polysomnograms with esophageal manometry in the proprietary Noxturnal format, and a proprietary library from Nox Medical, to you can use this software with modifications to score swings in esophageal pressure known as Pes crescendos in your polysomnograms. Alternatively, you supply your dataset in the format of CSV containing little but the peak-negative pressure of each breath. See NoxPes2Csv/nadir/BbB/*.txt for examples of such CSVs. Replace them with CSVs based on your data. In special, NoxPes2Csv is hardcoded to exclude a polysomnogram named VSN-14-080-002. Additionally, the definition of the function score in the file lib/src/Records.hs needs to be revised to match the dataset. Currently, score uses the function forbid to exclude polysomnograms named VSN-14-080-013 and VSN-14-080-014.

Full data flow
---
If your polysomnograms are in the proprietary Noxturnal format, you can adapt the NoxPes2Csv/Makefile and the script NoxPes2Csv/BbB_nadir.py and its dependencies for your dataset. The Makefile mentions the VSN-14-080 dataset explicitly, and the Python script is hardcoded to exclude polysomnogram number 2 (from the VSN-14-080 set of polysomnograms).

### Polysomnogram with scored esophageal manometry
Polysomnograms ⇉|NoxPes2Csv|⇉ CSVs of timestamped breaths with the peak-negative pressure of each breath ⇉|csv-to-score|⇉ CSVs of timestamped Pes crescendos ⇉|score-to-nox|⇉ Twelve copies of each polysomnogram, with Pes crescendos marked on the esophageal pressure signal in each polysomnogram according to one of the 12 respiratory effort patterns.

### Numerical report
Polysomnograms ⇉|NoxPes2Csv|⇉ CSVs of timestamped breaths with the peak-negative pressure of each breath ⇉|csv-to-score|⇉ CSVs of timestamped Pes crescendos ⇉|overlap|⇉ Numerical report on how frequently breathing matching a respiratory pattern also matched other respiratory patterns. ⇇|overlap|⇇ CSVs of timestamped Pes crescendos ⇇|nox2score|⇇ Polysomnograms with differently prescored Pes crescendos (e.g. manually scored)

### Heatmaps
Polysomnograms ⇉|NoxPes2Csv|⇉ CSVs of timestamped breaths with the peak-negative pressure of each breath ⇉|csv-to-score|⇉ CSVs of Pes crescendos ⇉|plot|⇉ Heatmaps of the frequency of Pes crescendos by polysomnogram and the respiratory pattern defining a "Pes crescendo"

Short data flow
---
In practice, your polysomnograms may be in a different format. In this case, prepare your dataset as CSVs, one CSV per polysomnogram, containing little but the peak-negative pressure of each breath, timestamped. See NoxPes2Csv/nadir/BbB/*.txt for examples of such CSVs. Make note of the big-endian datetime format. Choose which phase of a breath to timestamp freely but consistently. and replace them with CSVs based on your data.In special, NoxPes2Csv is hardcoded to exclude a polysomnogram named VSN-14-080-013. Additionally, the definition of the function score in the file lib/src/Records.hs needs to be revised to match the dataset. Currently, score uses the function forbid to exclude polysomnograms named VSN-14-080-013 and VSN-14-080-014. Consult the Makefiles for usage examples, but do not hesitate to modify them to refer to your dataset.

Dependencies
---
* Haskell (Haskell Platform, including Stack, circa 2019) 
* Make (GNU Make 4, optional)

### If using polysomnograms in the Noxturnal format
* Python (CPython 3.5.3)
* Microsoft Windows (with a .NET runtime)
* Nox Reader library
