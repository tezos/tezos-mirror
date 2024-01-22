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


These can all be combined:

```
$ evm-evaluation-assessor --eth-tests <ethereum-tests> --sub-directory <sub-dir> --test <test> --output <output>
# or
$ evm-evaluation-assessor -d <ethereum-tests> -s <sub-dir> -t <test> -o <output>
```
