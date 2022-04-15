`git-gas-diff` is a tool that makes a synthesis of gas changes from a git diff on regression traces.

Compile with `make`.

Run with `dune exec git-gas-diff [file]`. [file] is optional; if absent, the standard input will be read.

Typical usage is to pipe a `git diff` with the tool:
```
git diff HEAD^ HEAD | dune exec git-gas-diff
```
if the top commit is an update of the gas for regression traces.

See `bin/main.ml` for more details.

Example of a run and its output:
```
$ git diff 530825d255c3a8a6c9777ff910a3deb5ffe15261 cc6a393d71007599c600afbc82ccea66a8517070 | dune exec git-gas-diff
* Could not parse `-            fee_first_transfer = 394`.
* Could not parse `-            fee_second_transfer = 298`.
* Could not parse `+            fee_first_transfer = 397`.
* Could not parse `+            fee_second_transfer = 301`.
* Could not parse `-          KT1TLT2cXZCtenEAXzkndiAQGJXWenkwThRv`.
* Could not parse `+          KT1LfQjDNgPpdwMHbhzyQcD8GTE2L4rwxxpN`.
* Could not parse `-      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000591`.
* Could not parse `+      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.00059`.
* Could not parse `-          KT1LvTWVW1cc4DeSPCseMsiPdDDkTKvMrNZb`.
* Could not parse `+          KT1RdnquZZf4Y4ZDJvaEuY4cbam3xor3CffU`.
* Could not parse `-      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000795`.
* Could not parse `+      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000748`.
* Could not parse `-        KT1LvTWVW1cc4DeSPCseMsiPdDDkTKvMrNZb ... +ꜩ1`.
* Could not parse `+        KT1RdnquZZf4Y4ZDJvaEuY4cbam3xor3CffU ... +ꜩ1`.
* Could not parse `-      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000467`.
* Could not parse `+      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000472`.
* Could not parse `-        KT1TLT2cXZCtenEAXzkndiAQGJXWenkwThRv ... +ꜩ1`.
* Could not parse `+        KT1LfQjDNgPpdwMHbhzyQcD8GTE2L4rwxxpN ... +ꜩ1`.
* Could not parse `-      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000927`.
* Could not parse `+      tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ0.000749`.
* Could not parse `-        KT1LvTWVW1cc4DeSPCseMsiPdDDkTKvMrNZb ... +ꜩ1`.
* Could not parse `+        KT1RdnquZZf4Y4ZDJvaEuY4cbam3xor3CffU ... +ꜩ1`.

Lines with `Estimated gas: `:
  (Better means the value must decrease.)
  Accumulated value before: 364093.096
  Accumulated value now:    345025.713
  Total gain: ~5%
  Maximum loss on a line: 155 (~10%)
  Maximum gain on a line: 1779.637 (~126%)
  Number of lines with a change:      123
  Number of lines with a degradation: 75

Lines with `Consumed gas: `:
  (Better means the value must decrease.)
  Accumulated value before: 360694.441
  Accumulated value now:    341627.058
  Total gain: ~5%
  Maximum loss on a line: 155 (~10%)
  Maximum gain on a line: 1779.637 (~126%)
  Number of lines with a change:      140
  Number of lines with a degradation: 80

Lines with `Gas remaining: `:
  (Better means the value must increase.)
  Accumulated value before: 328622803.888
  Accumulated value now:    328631151.408
  Total gain: ~0%
  Maximum loss on a line: 1 (~0%)
  Maximum gain on a line: 1929.380 (~0%)
  Number of lines with a change:      316
  Number of lines with a degradation: 3

Lines with `Gas limit: `:
  (Better means the value must decrease.)
  Accumulated value before: 371942
  Accumulated value now:    352873
  Total gain: ~5%
  Maximum loss on a line: 155 (~10%)
  Maximum gain on a line: 1779 (~117%)
  Number of lines with a change:      120
  Number of lines with a degradation: 75

Lines with `remaining gas: `:
  (Better means the value must increase.)
  Accumulated value before: 6310916745.321
  Accumulated value now:    6311167310.034
  Total gain: ~0%
  Maximum loss on a line: 1 (~0%)
  Maximum gain on a line: 1304.820 (~0%)
  Number of lines with a change:      6069
  Number of lines with a degradation: 83

Lines with `Fee to the baker: ꜩ`:
  (Better means the value must decrease.)
  Accumulated value before: 0.168683
  Accumulated value now:    0.166774
  Total gain: ~1%
  Maximum loss on a line: 0.000015 (~3%)
  Maximum gain on a line: 0.000178 (~47%)
  Number of lines with a change:      105
  Number of lines with a degradation: 75

Lines with `payload fees(the block proposer) ....... +ꜩ`:
  (Better means the value must decrease.)
  Accumulated value before: 0.168683
  Accumulated value now:    0.166774
  Total gain: ~1%
  Maximum loss on a line: 0.000015 (~3%)
  Maximum gain on a line: 0.000178 (~47%)
  Number of lines with a change:      105
  Number of lines with a degradation: 75

Lines with `fee = `:
  (Better means the value must decrease.)
  Accumulated value before: 0.001372
  Accumulated value now:    0.001318
  Total gain: ~3%
  Maximum loss on a line: 0.000002 (~0%)
  Maximum gain on a line: 0.000056 (~7%)
  Number of lines with a change:      2
  Number of lines with a degradation: 1

Lines with the following strings were ignored:
  `PUBLIC_KEY_HASH`
  `CONTRACT_HASH`
  `tezos-client`
  `Operation hash`
  `New contract`
  `To: `
  `Parameter: `
```
