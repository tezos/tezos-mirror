# Threshold encryption

This crate contains high-level crypto primitives as well as common structures for integrating threshold encryption into Etherlink.  
It is aimed to be used by Etherlink kernel, DSN node (sidecar), and other tooling.

## About

This is an implementation of the [Baek-Zheng threshold cryptosystem](https://cpb-us-w2.wpmucdn.com/sites.uab.edu/dist/a/68/files/2020/01/globecom03-p1491.pdf) (BZTE) for the [BLS12-381](https://hackmd.io/@benjaminion/bls12-381) curve with several modifications:
- The payload is first encrypted with a symmetric cipher (ChaCha), and symmetric key (constant size) is encrypted with BZTE;
- The BLS signature (ciphertext component) binds both the encrypted key and original payload to prevent replay attacks;
- This is a variant where both public keys and decryption shares are from G1

The crate contains several optimizations to tackle spamming attacks and to improve the overall performance:
- Partial deserialization to avoid unnecessary computations in case of an early exit
- Batch verification of ciphertexts / decryption shares

The crate reuses most of the functionality from the [`blsttc`](https://github.com/maidsafe/blsttc) crate by MaidSafe.  
It is also inspired by [Ferveo](https://github.com/anoma/ferveo), especially wrt batch optimizations.  

Check out the full scheme description: https://hackmd.io/@m-kus/rJXTo9_pT

## Misc

### Compiling `blst` on MacOS

1. Install llvm via homebrew 
    ```
    brew install llvm
    export PATH="/opt/homebrew/opt/llvm/bin/:$PATH"
    ```
2. Ensure clang and archiver point to the right binaries:
    ```sh
    export CC=/opt/homebrew/opt/llvm/bin/clang
    export AR=/opt/homebrew/opt/llvm/bin/llvm-ar
    ```
3. Check yourself (should have `wasm32` in the output)
    ```sh
    llc --version
    ```
