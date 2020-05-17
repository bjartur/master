Translate a CSV with peak-inspiratory pressures and breath timestamps into a CSV with the start and end timestamps of "pes crescendo" (or decrescendo, to be more precise) events for various definitions of "pes crescendo".

For synopsis, execute:
```
stack build
stack exec -- csv-to-score --help
```

Any modifications can be regression tested using
```
stack test
```

See the reference Makefile in this directory for usage examples.
