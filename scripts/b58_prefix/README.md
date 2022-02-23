## Base58 prefix

An address is a Blake2B hash of a certain public key, i.e. a list of bytes. To
get a readable representation, we encode the hash with a specific encoding,
[Base58CheckEncoding](https://en.bitcoin.it/wiki/Base58Check_encoding),
the same as Bitcoin does, based on an alphabet of 58 characters.
The one used in Tezos is
`123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz` (other DLT project
uses different encoding, like Ripple).

In reality, when you have the address `tz2JmrN5LtfkYZFCQnWQtwpd9u7Fq3Dc4n6E`, it is the Base58CheckEncoding of the bytes
`b"r\185\224.sx\154\215\182\216\226\172\230\252\156p1\138\231K"`.

```
# Run dune utop in src/lib_crypto
open Tezos_crypto;;
open Tezos_error_monad.Error_monad;;
# From bytes
let (Ok pkh) = Secp256k1.Public_key_hash.of_bytes (Bytes.of_string "r\185\224.sx\154\215\182\216\226\172\230\252\156p1\138\231K");;
Secp256k1.Public_key_hash.to_b58check pkh;;
# - : string = "tz2JmrN5LtfkYZFCQnWQtwpd9u7Fq3Dc4n6E"
# From the encoded address
let (Ok pkh) = Secp256k1.Public_key_hash.of_b58check "tz2JmrN5LtfkYZFCQnWQtwpd9u7Fq3Dc4n6E";;
# - : bytes = Bytes.of_string "r\185\224.sx\154\215\182\216\226\172\230\252\156p1\138\231K"
```

Note that any sequence of bytes can be converted into a
`Secp256k1.Public_key_hash.t` value (using `Public_key_hash.of_bytes` and
`Public_key_hash.to_b58check`), but **it does not mean it is a Tezos address
derived from a secret key!!**. For instance, let's change the first byte
(here `r`) into a `a`.

```
open Tezos_crypto;;
open Tezos_error_monad.Error_monad;;
let (Ok pkh) = Secp256k1.Public_key_hash.of_bytes
(*                 "r\185\224.sx\154\215\182\216\226\172\230\252\156p1\138\231K" *)
(*                  |                                                            *)
  (Bytes.of_string "a\185\224.sx\154\215\182\216\226\172\230\252\156p1\138\231K");;
Secp256k1.Public_key_hash.to_b58check pkh;;
(* - : string = "tz2HDxspCVakXgHwNfNqngKZxChJsSpJ7wvg" *)
```

Also notice that
- the Base58Check encoding does still start with `tz2`.
- the resulting string is completely different, even if we change only one byte.
- the result is a 36-long string.

The idea behind the Base58CheckEncoding algorithm is to get more security when typing or
copying an address, i.e. if one character of the Tezos address does change, it
gives an invalid address.

```
# Let's change the 4th characters (J) in t.
open Tezos_crypto;;
open Tezos_error_monad.Error_monad;;
#                                                        tz2JmrN5LtfkYZFCQnWQtwpd9u7Fq3Dc4n6E
#                                                           |
let (Error pkh) = Secp256k1.Public_key_hash.of_b58check "tz2tmrN5LtfkYZFCQnWQtwpd9u7Fq3Dc4n6E";;
```

In addition to that, the result of the Base58CheckEncoding algorithm must give a
36 characters long Base58 encoded string starting with `tz2` (called the version bytes
sometimes). The initial bytes are called the payload.

`b58_prefix.py` computes the additional bytes to add to the class of payloads of a certain
length to get a base58 string starting with a certain prefix.
It is important to understand that there may exist multiple solutions!

Executing the script gives for `tz2` the bytes `\006\161\161`.
Let's see how the Base58CheckEncoding algorithm works for an secp256k1 address (20 bytes):
1. Prefix the version byte (`\006\161\161`) with the payload (20 bytes)
```
 Version byte                                             Address bytes
-------------- ------------------------------------------------------------------------------------------------
|            | |                                                                                              |
006 161 161 114 185 \224 \46 \115 \120 \154 \215 \182 \216 \226 \172 \230 \252 \156 \112 \49 \138 \231 \75
```

2. Hash twice with SHA256 the result of 1, take the 4 first bytes (called the
   checksum), and add it at the end of 1). Let's say it is C1C2C3C4.

```
 Version byte                                             Address bytes                                            Checksum
-------------- ------------------------------------------------------------------------------------------------  -----------
|            | |                                                                                              |  |         |
\006 \161 \161 \114 \185 \224 \46 \115 \120 \154 \215 \182 \216 \226 \172 \230 \252 \156 \112 \49 \138 \231 \75  C1 C2 C3 C4
```

3. The public key hash is then the result of the encoding in base58.


## Usage

```
# Install the dependencies
poetry install
```

```
# A tz1 address is generated from a fixed size hash of 20 characters.
poetry run python b58_prefix.py --prefix tz1 --length 20
# Output: Base58 size: 36. Version bytes: [6, 161, 159]
# A block is generated from a fixed size hash of 32 characters.
poetry run python b58_prefix.py --prefix B --length 32
# Output: Base58 size: 51. Version bytes: [1, 43]
```

## Linting and running test

```
# Install the dev tools
poetry install
poetry run pylint b58_prefix.py --disable=missing-docstring --disable=invalid-name
poetry run pytest test_b58_prefix.py -v
```
