# `git-gas-diff`

`git-gas-diff` is a tool that makes a synthesis of gas changes from a git diff on regression traces.

## Build and run

Compile with `make`.

Run with `dune exec git-gas-diff [file]`. [file] is optional; if absent, the standard input will be read.

Typical usage is to pipe a `git diff` with the tool:
```
git diff HEAD^ HEAD | dune exec git-gas-diff
```
if the top commit is an update of the gas for regression traces.

See `bin/main.ml` for more details.

## Wrapper

`git-gas-diff.sh` is a wrapper script that simplifies the typical usage of invoking
```
git diff [commit1] [commit2] | dune exec git-gas-diff
```
by simply running `./git-gas-diff.sh [commit1] [commit2]`. It works on Tezos regression directories, sets a few options to reduce the size of the embedded `git diff`, and can also be called with a single commit argument that will be then compared to `HEAD`, or even no argument at all to compare the last merged commit with `HEAD`.

See `git-gas-diff.sh` for more details.

## Example

(When run on a clone of https://gitlab.com/nomadic-labs/tezos/-/tree/master.)

```
$ ./git-gas-diff.sh 4a6c91ee 1a28a48ed2c9fa654ed7d601302d551332b52e77
-- LINE NUMBERS REFER TO TEMPORARY FILE /tmp/tmp.j8YRu0Y9kY. --

* Could not parse line 6 `-      Origination:`.
* Could not parse line 7 `+      Internal Origination:`.
* Could not parse line 13 `+expru9ZhqLGykU3yRksBDWe4uDwxavSM19B8WfKukQA3fkgok4hGTE	[CONTRACT_PATH]/mini_scenarios/add_clear_tickets.tz`.
* Could not parse line 38 `-      Transaction:`.
* Could not parse line 39 `+      Internal Transaction:`.
* Could not parse line 668 `-      Delegation:`.
* Could not parse line 669 `+      Internal Delegation:`.
* Could not parse line 671 `-      Delegation:`.
* Could not parse line 672 `+      Internal Delegation:`.
* Could not parse line 708 `-      Transaction:`.
* Could not parse line 709 `+      Internal Transaction:`.
* Could not parse line 736 `-      Transaction:`.
* Could not parse line 737 `+      Internal Transaction:`.
* Could not parse line 872 `-      Transaction:`.
* Could not parse line 873 `+      Internal Transaction:`.
* Could not parse line 892 `-      Transaction:`.
* Could not parse line 893 `+      Internal Transaction:`.
* Could not parse line 2294 `-  Internal origination:`.
* Could not parse line 2295 `+  Internal Origination:`.
* Could not parse line 7592 `-      Transaction:`.
* Could not parse line 7593 `+      Internal Transaction:`.
* Could not parse line 7595 `-      Transaction:`.
* Could not parse line 7596 `+      Internal Transaction:`.
* Could not parse line 7737 `-      Transaction:`.
* Could not parse line 7738 `+      Internal Transaction:`.
* Could not parse line 7740 `-      Transaction:`.
* Could not parse line 7741 `+      Internal Transaction:`.
* Could not parse line 7743 `-      Transaction:`.
* Could not parse line 7744 `+      Internal Transaction:`.
* Could not parse line 7762 `-      Transaction:`.
* Could not parse line 7763 `+      Internal Transaction:`.
* Could not parse line 7765 `-      Transaction:`.
* Could not parse line 7766 `+      Internal Transaction:`.
* Could not parse line 7789 `-      Transaction:`.
* Could not parse line 7790 `+      Internal Transaction:`.
* Could not parse line 7792 `-      Transaction:`.
* Could not parse line 7793 `+      Internal Transaction:`.
* Could not parse line 7895 `-      Transaction:`.
* Could not parse line 7896 `+      Internal Transaction:`.
* Could not parse line 7898 `-      Transaction:`.
* Could not parse line 7899 `+      Internal Transaction:`.
* Could not parse line 7904 `-      Transaction:`.
* Could not parse line 7905 `+      Internal Transaction:`.

Lines with `Consumed gas: `:
  (Better means the value must decrease.)
  Accumulated value before: 79902.496
  Accumulated value now:    75571.261
  Total gain: ~5%
  Maximum loss on a line: 0
  Maximum gain on a line: 590.205 (~18%, line 848)
  Number of lines with a change:      37
  Number of lines with a degradation: 0

Lines with `Estimated gas: `:
  (Better means the value must decrease.)
  Accumulated value before: 104066.032
  Accumulated value now:    99734.797
  Total gain: ~4%
  Maximum loss on a line: 0
  Maximum gain on a line: 903.300 (~11%, line 7878)
  Number of lines with a change:      32
  Number of lines with a degradation: 0

Lines with `Fee to the baker: ꜩ`:
  (Better means the value must decrease.)
  Accumulated value before: 0.012461
  Accumulated value now:    0.012024
  Total gain: ~3%
  Maximum loss on a line: 0
  Maximum gain on a line: 0.000090 (~7%, line 7881)
  Number of lines with a change:      19
  Number of lines with a degradation: 0

Lines with `Gas limit: `:
  (Better means the value must decrease.)
  Accumulated value before: 97918
  Accumulated value now:    93584
  Total gain: ~4%
  Maximum loss on a line: 0
  Maximum gain on a line: 903 (~11%, line 7884)
  Number of lines with a change:      27
  Number of lines with a degradation: 0

Lines with `Gas remaining: `:
  (Better means the value must increase.)
  Accumulated value before: 92553530.176
  Accumulated value now:    92554374.016
  Total gain: ~0%
  Maximum loss on a line: 0.584 (~0%, line 661)
  Maximum gain on a line: 429.240 (~0%, line 486)
  Number of lines with a change:      89
  Number of lines with a degradation: 3

Lines with `payload fees(the block proposer) ....... +ꜩ`:
  (Better means the value must decrease.)
  Accumulated value before: 0.012461
  Accumulated value now:    0.012024
  Total gain: ~3%
  Maximum loss on a line: 0
  Maximum gain on a line: 0.000090 (~7%, line 7888)
  Number of lines with a change:      19
  Number of lines with a degradation: 0

Lines with `remaining gas: `:
  (Better means the value must increase.)
  Accumulated value before: 2265669583.210
  Accumulated value now:    2265676771.345
  Total gain: ~0%
  Maximum loss on a line: 0
  Maximum gain on a line: 5.445 (~0%, line 4546)
  Number of lines with a change:      2179
  Number of lines with a degradation: 0

Total number of lines with a change: 2421.
Total number of lines with a degradation: 3.

Lines with the following strings were not changed:
  `Estimated storage: `
  `Storage limit: `
  `Storage size: `
  `fee = `
  `storage fees ........................... +ꜩ`

19 line(s) with the following strings have changed:
  `CONTRACT_HASH`
  `New contract`
  `Operation hash`
  `PUBLIC_KEY_HASH`
  `Parameter: `
  `To: `
  `tezos-client`

-- LINE NUMBERS REFER TO TEMPORARY FILE /tmp/tmp.j8YRu0Y9kY. --
```
