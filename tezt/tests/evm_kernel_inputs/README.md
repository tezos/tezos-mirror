# How to generate inputs

Inputs like in `./inputs.json` have been produced by [wagyu](https://github.com/AleoHQ/wagyu).
It is used to create the raw transaction and sign it:
```
$ wagyu ethereum transaction --createrawtransaction '{"to":"0xB53dc01974176E5dFf2298C5a94343c2585E3c54", "value":"1000000000000000000", "gas":"21000", "gasPrice":"1250", "nonce":1, "network":"1337"}'

      Transaction Hex      0xe9018204e282520894b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080018080

$ wagyu ethereum transaction --signrawtransaction 0xe9018204e282520894b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080018080 9722f6cc9ff938e63f8ccb74c3daa6b45837e5c5e3835ac08c44c50ab5f39dc0

      Transaction Id       0xc139fbf9a050a3745ea49ef55692dd6bf11d1e708091fc436ab5ac59f68659e8
      Transaction Hex      0xf869018204e282520894b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a76400008026a0e05675c80f386c2c3e52db9b4a8b32773b5828bcec5dc9387c4a7ec109f01686a0192d4db23677d74299b9a5892db9b4e97896bdcb1c165513abaaa50f791faab9
```

You then need to add the smart rollup address and the transaction hash to the raw transaction:
```
Raw transaction: 0xf869018204e282520894b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a76400008026a0e05675c80f386c2c3e52db9b4a8b32773b5828bcec5dc9387c4a7ec109f01686a0192d4db23677d74299b9a5892db9b4e97896bdcb1c165513abaaa50f791faab9

External message:
    Smart rollup address (20 bytes): 00 * 20
    Transaction hash (32 bytes): 00 * 32
    Raw transaction: f869018204e282520894b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a76400008026a0e05675c80f386c2c3e52db9b4a8b32773b5828bcec5dc9387c4a7ec109f01686a0192d4db23677d74299b9a5892db9b4e97896bdcb1c165513abaaa50f791faab9
```

# Smart contracts inputs

To deploy a solidity contract on the rollup two files are necessary:
- the ABI (interface), in a JSON format,
- the binary as a hex string (without the `0x` prefix).

Those files can be generated from the solidity source code using Ethereum
standard tooling. For example [Remix](https://remix.ethereum.org/) (online IDE)
or other tools.

The contracts source code can be found in `src/kernel_evm/solidity_examples`.