# EVM Evaluation

## Preliminary step

Clone [ethereum/tests](https://github.com/ethereum/tests).

Since Rust has a small minimum stack size, some tests will fail.
To prevent that run the following command:

```
$ ulimit -s 20000
```

## Execute the assessor

The EVM evaluation assessor can be launch by simply running:

```
$ evm-evaluation-assessor
```

It will try to make use of the locally cloned `ethereum/tests` repository at the location where `evm-evaluation-assessor` is used.

Specify `ethereum/tests` location:

```
$ evm-evaluation-assessor --eth-tests <ethereum-tests>
# or
$ evm-evaluation-assessor -d <ethereum-tests>
```

Specify the sub directory of tests you want to execute:

```
$ evm-evaluation-assessor --sub-directory <sub-dir>
# or
$ evm-evaluation-assessor -s <sub-dir>
```

Specify the name of the test to execute:

```
$ evm-evaluation-assessor --test <test>
# or
$ evm-evaluation-assessor -t <test>
```

Specify the file where the logs will be outputed:

```
$ evm-evaluation-assessor --output <output>
# or
$ evm-evaluation-assessor -o <output>
```

Specify if you only want to output the final report:

```
$ evm-evaluation-assessor --report-only
# or
$ evm-evaluation-assessor -r
```

Overwrite the target file and start the report from scratch:

```
$ evm-evaluation-assessor --from-scratch
# or
$ evm-evaluation-assessor -h
```

Dump a result of an evaluation and use it to produce a diff in the log file after making some changes:

```
$ evm-evaluation-assessor --result // By default the execution will be outputed into `evm_evaluation.result`
$ evm-evaluation-assessor --diff <result-file> // By default the execution will be outputed into `evm_evaluation.diff`
```

Specify the path where the tool needs to retrieve its resources from:

```
$ evm-evaluation-assessor --resources // By default its tezos' directory (./etherlink/kernel_evm/evm_evaluation/resources)
```

To make the tool act as a non-regression job in the CI:

```
$ evm-evaluation-assessor --ci-mode
# or
$ evm-evaluation-assessor -c
```

Multiple options can be combined:

```
$ evm-evaluation-assessor --eth-tests <ethereum-tests> --sub-directory <sub-dir> --test <test> --output <output>
# or
$ evm-evaluation-assessor -d <ethereum-tests> -s <sub-dir> -t <test> -o <output>
```
