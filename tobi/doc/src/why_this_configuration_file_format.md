# Tobi: why this configuration file format

## Concatenating files

The [low-level semantics](configuration_file.md) makes it so that
concatenating two configuration files is equivalent to merging the two
corresponding mappings. In other words, we get this nice composition property:

    meaning_of(concat(file1, file2)) = merge(meaning_of(file1), meaning_of(file2))

Imagine for instance that you want to generate parts of the configuration,
but still be able to add some entries manually.
You could generate `file1`, write `file2` manually, and produce `tobi/config`
with a simple `cat file1 file2 > tobi/config`.

## Why not JSON

JSON does not have the composition property defined in the previous section
(one cannot concatenate two JSON files).

Also, JSON is not a very good configuration file because it does not support comments.

## Why not YAML

See [the YAML document from hell](https://ruudvanasseldonk.com/2023/01/11/the-yaml-document-from-hell).
