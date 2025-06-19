# Memstat

Memstat is a tool for Linux to measure memory usage of a process.
It does not work on MacOS, but it does work in Docker running a Linux container
on a MacOS host.

## How to build and run

Build it with `dune build scripts/memstat` from the root of the repository.
Then run with `_build/default/scripts/memstat/main.exe`.

In the next sections we assume you have copied this executable
into your `PATH` as `memstat`, or that you use an alias named `memstat`
to run this executable. For the latter, run the following command
from the root of the repository:
```
alias memstat=$PWD/_build/default/scripts/memstat/main.exe
```
See the list of command-line options for each command with `--help`:
```
memstat --help
memstat get --help
memstat run --help
```

## Measure current memory usage of an existing process

To measure the current memory usage of a process with PID 1234, run:
```
memstat get 1234
```
This only gives you the memory usage of 1234 itself.
To also count its children recursively, use `-r`, and `-v` to get the detail:
```
memstat get 1234 -rv
```
You can repeat this measurement with `-w`.
```
memstat get 1234 -rvw
```
In this scenario `memstat` runs forever.
Use Ctrl+C to exit.

## Run a process and measure its memory usage

You can ask memstat to start a program and to measure its memory usage
repeatedly until it exits. For instance, to run 'git --help', run:
```
memstat run git \\--help
```
This displays peak memory usage at the end.

Arguments that you want to pass to the command to run (`git` in this example)
must be prefixed by a backslash if they start with a dash.
Most shells require you to escape this backslash, hence the double backslash
before `--help`. Otherwise `memstat` believes the argument is for itself.

Like `get`, you can ask to measure recursively with `-r`,
and display the details of each measurement with `-v`.
```
memstat run git \\--help -rv
```

## How it reads memory

The script works by reading:
- `/proc/PID/smaps` (specifically the `Pss` field) to read memory usage of a single process
- `/proc/PID/comm` to read the name of a process
- `/proc/PID/task/*/children` to recursively get the children of a process

PSS stands for Proportional Set Size.
The `Pss` field contains memory usage divided by the number of processes
that share this particular memory page.
This is so that shared pages are only counted once in total.

For instance, imagine a process P1 that forks a process P2, such that:
- P1 and P2 share 100 MB of memory;
- P1 uses 40 MB that it does not share with P2;
- P2 uses 10 MB that it does not share with P1.

In this example:
- the PSS of P1 is (100 / 2) + 40 = 90;
- the PSS of P2 is (100 / 2) + 10 = 60;
- total amount of memory used is 150, which is 100 + 40 + 10, but also 90 + 60.
