# Efunc

Efunc is an ocaml ethereum client.
The documentation of the library can be found [here](https://functori.gitlab.io/dev/efunc/efunc/Eth/index.html).

## Install

Efunc can be installed throught [opam](https://opam.ocaml.org/doc/Install.html):
```bash
opam pin add efunc.~dev https://gitlab.com/functori/dev/efunc
```

## Use

The lower level of this library reproduces the behaviour of the [ethereum json rpc client](https://ethereum.org/en/developers/docs/apis/json-rpc) through the `post` function:
```ocaml
val post : ?id:int -> ?base:EzAPI.base_url -> 'a meth -> ('a, error) result
```

This library also provides some higher level tools:
- `send`:
    ```ocaml
    val send : ?base:EzAPI.base_url -> sk:Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t
               -> data_input transaction_input -> (b, error) result Lwt.t
    ```
    sending a transaction on the ethereum blockchain.
    A function `ti`  helps to build the `transaction_input` value:
    ```ocaml
    val ti : ?chain_id:int -> ?nonce:int -> ?gas_limit:int -> ?max_priority_fee:Z.t
             -> ?max_fee:Z.t -> ?value:Z.t -> ?data:'a -> ?access_list:(address * b) list
             -> ?signature:signature -> address -> 'a transaction_input
    ```

- `call`:
    ```ocaml
    val call : ?base:EzAPI.base_url -> ?level:level_id -> ?src:address -> ?typ:evm_type
               -> data_input transaction_input -> (evm_value, error) result Lwt.t
    ```
    calling a contract entrypoint.

- `events`:
    ```ocaml
    val events : ?base:EzAPI.base_url -> ?from:level_id -> ?to_:level_id -> ?addresses:address list
                 -> event -> (evm_value, error) result Lwt.t
    ```
    getting event logs.

Other lower level tools can be found in different modules:
- `Crypto`: keccak and secp256k1 tools.
- `Evm`: encoding and decoding EVM values.
- `Forge`: forge transactions using RLP encoding.
- `Abi`: ABI json structure.
- `Utils`: other practical functions like (`balance`, `logs`, `sign`, `prepare`, ...)

## PPX
This library also proposes a ppx `efunc.ppx` to use ABI file of ethereum contracts:
```ocaml
let interface = {%abi|contracts/contracts_erc20_sol_ERC20.abi|}
val interface : <
  allowance : ?base:EzAPI.base_url ->
              ?chain_id:bint ->
              ?nonce:bint ->
              ?gas_limit:bint ->
              ?max_priority_fee:bz ->
              ?max_fee:bz ->
              ?value:bz ->
              ?access_list:(address * b list) list ->
              ?level:level_id ->
              ?src:address ->
              dst:address option ->
              address * address -> (evm_value, error) result Lwt.t;
  allowance_inputs : evm_type list;
  allowance_outputs : evm_type;
  approval_event : evm_value list option list -> event;
  approval_event_inputs : evm_type list;
  approval_filter : evm_value list option list -> topic list;
  approve : ?base:EzAPI.base_url ->
            ?chain_id:bint ->
            ?nonce:bint ->
            ?gas_limit:bint ->
            ?max_priority_fee:bz ->
            ?max_fee:bz ->
            ?access_list:(address * b list) list ->
            sk:Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t ->
            dst:address option ->
            (< amount : bz; spender : address; .. > as 'a) ->
            (b, error) result Lwt.t;
  approve_inputs : evm_type list;
  approve_outputs : evm_type;
  balanceOf : ?base:EzAPI.base_url ->
              ?chain_id:bint ->
              ?nonce:bint ->
              ?gas_limit:bint ->
              ?max_priority_fee:bz ->
              ?max_fee:bz ->
              ?value:bz ->
              ?access_list:(address * b list) list ->
              ?level:level_id ->
              ?src:address ->
              dst:address option ->
              address -> (evm_value, error) result Lwt.t;
  balanceOf_inputs : evm_type list;
  balanceOf_outputs : evm_type;
  burn : ?base:EzAPI.base_url ->
         ?chain_id:bint ->
         ?nonce:bint ->
         ?gas_limit:bint ->
         ?max_priority_fee:bz ->
         ?max_fee:bz ->
         ?access_list:(address * b list) list ->
         sk:Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t ->
         dst:address option -> bz -> (b, error) result Lwt.t;
  burn_inputs : evm_type list;
  decimals : ?base:EzAPI.base_url ->
             ?chain_id:bint ->
             ?nonce:bint ->
             ?gas_limit:bint ->
             ?max_priority_fee:bz ->
             ?max_fee:bz ->
             ?value:bz ->
             ?access_list:(address * b list) list ->
             ?level:level_id ->
             ?src:address ->
             dst:address option ->
             none -> (evm_value, error) result Lwt.t;
  decimals_outputs : evm_type; extract_allowance : b -> bz;
  extract_approve : b -> bool; extract_balanceOf : b -> bz;
  extract_decimals : b -> bz; extract_name : b -> string;
  extract_symbol : b -> string; extract_totalSupply : b -> bz;
  extract_transfer : b -> bool; extract_transferFrom : b -> bool;
  listen_approval : ?base:EzAPI.base_url ->
                    ?from:level_id ->
                    to_:level_id option ->
                    ?addresses:address list ->
                    evm_value list option list ->
                    (evm_value log list, error) result Lwt.t;
  listen_transfer : ?base:EzAPI.base_url ->
                    ?from:level_id ->
                    to_:level_id option ->
                    ?addresses:address list ->
                    evm_value list option list ->
                    (evm_value log list, error) result Lwt.t;
  mint : ?base:EzAPI.base_url ->
         ?chain_id:bint ->
         ?nonce:bint ->
         ?gas_limit:bint ->
         ?max_priority_fee:bz ->
         ?max_fee:bz ->
         ?access_list:(address * b list) list ->
         sk:Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t ->
         dst:address option -> bz -> (b, error) result Lwt.t;
  mint_inputs : evm_type list;
  name : ?base:EzAPI.base_url ->
         ?chain_id:bint ->
         ?nonce:bint ->
         ?gas_limit:bint ->
         ?max_priority_fee:bz ->
         ?max_fee:bz ->
         ?value:bz ->
         ?access_list:(address * b list) list ->
         ?level:level_id ->
         ?src:address ->
         dst:address option ->
         none -> (evm_value, error) result Lwt.t;
  name_outputs : evm_type;
  produce_allowance : address * address -> b; produce_approve : 'a -> b;
  produce_balanceOf : address -> b; produce_burn : bz -> b;
  produce_mint : bz -> b;
  produce_transfer : (< amount : bz; recipient : address; .. > as 'b) -> b;
  produce_transferFrom : (< amount : bz; recipient : address;
                            sender : address; .. >
                          as 'c) ->
                         b;
  symbol : ?base:EzAPI.base_url ->
           ?chain_id:bint ->
           ?nonce:bint ->
           ?gas_limit:bint ->
           ?max_priority_fee:bz ->
           ?max_fee:bz ->
           ?value:bz ->
           ?access_list:(address * b list) list ->
           ?level:level_id ->
           ?src:address ->
           dst:address option ->
           none -> (evm_value, error) result Lwt.t;
  symbol_outputs : evm_type;
  totalSupply : ?base:EzAPI.base_url ->
                ?chain_id:bint ->
                ?nonce:bint ->
                ?gas_limit:bint ->
                ?max_priority_fee:bz ->
                ?max_fee:bz ->
                ?value:bz ->
                ?access_list:(address * b list) list ->
                ?level:level_id ->
                ?src:address ->
                dst:address option ->
                none -> (evm_value, error) result Lwt.t;
  totalSupply_outputs : evm_type;
  transfer : ?base:EzAPI.base_url ->
             ?chain_id:bint ->
             ?nonce:bint ->
             ?gas_limit:bint ->
             ?max_priority_fee:bz ->
             ?max_fee:bz ->
             ?access_list:(address * b list) list ->
             sk:Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t ->
             dst:address option -> 'b -> (b, error) result Lwt.t;
  transferFrom : ?base:EzAPI.base_url ->
                 ?chain_id:bint ->
                 ?nonce:bint ->
                 ?gas_limit:bint ->
                 ?max_priority_fee:bz ->
                 ?max_fee:bz ->
                 ?access_list:(address * b list) list ->
                 sk:Libsecp256k1.External.Key.secret
                    Libsecp256k1.External.Key.t ->
                 dst:address option -> 'c -> (b, error) result Lwt.t;
  transferFrom_inputs : evm_type list;
  transferFrom_outputs : evm_type;
  transfer_event : evm_value list option list -> event;
  transfer_event_inputs : evm_type list;
  transfer_filter : evm_value list option list -> topic list;
  transfer_inputs : evm_type list;
  transfer_outputs : evm_type
>
```

## JS
For javascript use, download the wasm and js glue available [here](https://gitlab.com/nomadic-labs/ocaml-secp256k1-internal/-/packages/6003907).
These files need to be loaded before your main js file using this library:
- on node:
    ```javascript
    function load() {
      var _secp256k1 = require('@nomadic-labs/secp256k1-wasm');
      return _secp256k1().then(function (SECP256K1) { global._SECP256K1 = SECP256K1 }).catch(e => console.log(e))
    }
    load().then(your_main_function)
    ```
- on web:
    load the secp256k1.js file with a script tag and then a preloader file containing:
    ```javascript
    function load(src) {
      _SECP256K1().then(function (secp256k1) {
        _SECP256K1 = secp256k1;
        var elt = document.createElement('script');
        elt.src = src;
        document.body.appendChild(elt);
      }).catch(console.log);
    }
    load("your_main_file.js")
    ```



## License
Copyright © 2024, Functori <contact@functori.com>. Released under the [MIT License](https://gitlab.com/functori/efunc/-/blob/master/LICENSE).
