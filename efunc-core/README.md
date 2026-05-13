# Efunc

Efunc is an OCaml Ethereum client.

📚 API documentation is available here:  
https://functori.gitlab.io/dev/efunc/efunc/Eth/index.html

## Install

Efunc can be installed through [opam](https://opam.ocaml.org/doc/Install.html):

```bash
opam pin add efunc.~dev https://gitlab.com/functori/dev/efunc
```

---

## Usage

At the low level, this library reproduces the behavior of the [Ethereum JSON-RPC API](https://ethereum.org/en/developers/docs/apis/json-rpc) through `post`:

```ocaml
val post : ?id:int -> ?base:EzAPI.base_url -> 'a meth -> ('a, error) result
```

### High-level helpers

- `send` — send a signed transaction to the Ethereum network.

  ```ocaml
  val send :
    ?base:EzAPI.base_url ->
    sk:Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t ->
    data_input transaction_input ->
    (b, error) result Lwt.t
  ```

  Use `ti` to build `transaction_input`:

  ```ocaml
  val ti :
    ?chain_id:int ->
    ?nonce:int ->
    ?gas_limit:int ->
    ?max_priority_fee:Z.t ->
    ?max_fee:Z.t ->
    ?value:Z.t ->
    ?data:'a ->
    ?access_list:(address * b) list ->
    ?signature:signature ->
    address ->
    'a transaction_input
  ```

- `call` — call a contract entrypoint (read-only or simulated execution).

  ```ocaml
  val call :
    ?base:EzAPI.base_url ->
    ?level:level_id ->
    ?src:address ->
    ?typ:evm_type ->
    data_input transaction_input ->
    (evm_value, error) result Lwt.t
  ```

- `events` — fetch event logs.

  ```ocaml
  val events :
    ?base:EzAPI.base_url ->
    ?from:level_id ->
    ?to_:level_id ->
    ?addresses:address list ->
    event ->
    (evm_value, error) result Lwt.t
  ```

### Other modules

- `Crypto`: Keccak and secp256k1 utilities
- `Evm`: EVM value encoding/decoding
- `Forge`: transaction forging with RLP
- `Abi`: ABI JSON structure support
- `Utils`: convenience functions (`balance`, `logs`, `sign`, `prepare`, ...)

---

## Security check (new recommended preflight)

Before broadcasting a transaction, **validate fee settings and chain id** to avoid accidental replay or invalid-fee transactions:

```ocaml
let validate_tx ~expected_chain_id tx =
  match tx.chain_id, tx.max_priority_fee, tx.max_fee with
  | Some cid, Some p, Some m when cid = expected_chain_id && Z.leq p m -> Ok ()
  | Some cid, _, _ when cid <> expected_chain_id ->
      Error (`Msg "Invalid chain_id: possible replay-risk configuration")
  | _, Some p, Some m when Z.gt p m ->
      Error (`Msg "Invalid EIP-1559 fees: max_priority_fee > max_fee")
  | _ -> Ok ()
```

Recommended flow:
1. Build with `ti`.
2. Run `validate_tx`.
3. Optionally simulate with `call`.
4. Send with `send`.

---

## PPX

This library also provides `efunc.ppx` for ABI-driven bindings:

```ocaml
let interface = {%abi|contracts/contracts_erc20_sol_ERC20.abi|}
```

This generates typed helpers for:
- contract calls (`balanceOf`, `symbol`, `totalSupply`, ...)
- transaction senders (`transfer`, `approve`, ...)
- event filters/listeners (`transfer_event`, `listen_transfer`, ...)
- payload encoders/decoders (`produce_*`, `extract_*`)

(Generated signature omitted here for brevity; it follows the ABI exactly.)

---

## JavaScript / WASM

For JavaScript usage, download the WASM + JS glue files from:  
https://gitlab.com/nomadic-labs/ocaml-secp256k1-internal/-/packages/6003907

Load them **before** your main JS bundle.

### Node.js

```javascript
function load() {
  const _secp256k1 = require('@nomadic-labs/secp256k1-wasm');
  return _secp256k1()
    .then((SECP256K1) => {
      global._SECP256K1 = SECP256K1;
    })
    .catch(console.error);
}

load().then(yourMainFunction);
```

### Browser

Load `secp256k1.js` first, then run:

```javascript
function load(src) {
  _SECP256K1()
    .then((secp256k1) => {
      _SECP256K1 = secp256k1;
      const elt = document.createElement('script');
      elt.src = src;
      document.body.appendChild(elt);
    })
    .catch(console.error);
}

load('your_main_file.js');
```

---

## License

Copyright © 2024, Functori <contact@functori.com>.  
Released under the [MIT License](https://gitlab.com/functori/efunc/-/blob/master/LICENSE).