Gas parameter diff
==================

This script can be used to compare the results of several runs of
Snoop parameter inference. It is useful in particular for detecting
regressions in the carbonated part of the Tezos protocol.

Usage
-----

The script takes as input n CSV files produced by `tezos-snoop infer
parameters ... --dump-csv ...`. It outputs (on the standard output
channel) a CSV file containing (n+1) columns. The first column
contains the names of the gas parameters, the other n columns contain
the inferred values for the parameters; each column corresponding to
an input CSV file.

If a gas parameter is not present in one of the input files, the
corresponding cell in the output is blank.

To call the script, call `dune exec gas_parameter_diff -- <file1.csv> <file2.csv> ...`.
