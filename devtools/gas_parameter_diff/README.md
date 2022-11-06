Gas parameter diff
==================

This script can be used to compare the results of two runs of Snoop
parameter inference. It is useful in particular for detecting
regressions in the carbonated part of the Tezos protocol.

Usage
-----

The script takes as input two CSV files produced by `tezos-snoop infer
parameters ... --dump-csv ...`. It outputs (on the standard output
channel) a third CSV file containing three columns: name of the
constants, value in the first CSV file, value in the second CSV file.

If a constant is only present in one of the files a warning is
produced on the standard error channel.

To call the script, call `dune exec gas_parameter_diff -- <file1.csv> <file2.csv>`.



