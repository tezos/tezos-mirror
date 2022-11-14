Gas parameter diff
==================

This script can be used to compare the results of several runs of
Snoop parameter inference. It is useful in particular for detecting
regressions in the carbonated part of the Tezos protocol.

Usage
-----

The script takes as input n CSV files produced by `tezos-snoop infer
parameters ... --dump-csv ...`. It outputs (on the standard output
channel) a CSV file containing the following columns:
- the first column contains the names of the gas parameters,
- the next n columns contain, on their first row the path to one of the input CSV files, and on the
following rows the inferred values for the parameters found in that CSV file,
- the next column contains the minimum value for the parameter,
- the next column contains the maximum value for the parameter,
- the next column contains the difference between the maximum value and the minimum value,
- the next and final column contains the ratio, expressed in
  percentage, between the difference and the maximum value. If all
  inferred values for the parameter are null, this ratio is also
  defined to be null.

If a gas parameter is not present in one of the input files, the
corresponding cell in the output is blank and ignored when computing
the minimum and maximum values of the parameter.

To call the script, call `dune exec gas_parameter_diff -- <file1.csv> ... <fileN.csv>`.
