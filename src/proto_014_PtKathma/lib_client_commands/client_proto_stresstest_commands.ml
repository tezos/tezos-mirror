(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
module Smart_contracts = Client_proto_stresstest_contracts

type transfer_strategy =
  | Fixed_amount of {mutez : Tez.t}  (** Amount to transfer *)
  | Evaporation of {fraction : float}
      (** Maximum fraction of current wealth to transfer.
          Minimum amount is 1 mutez regardless of total wealth. *)

type limit =
  | Abs of int  (** Absolute level at which we should stop  *)
  | Rel of int  (** Relative number of levels before stopping *)

type parameters = {
  seed : int;
  fresh_probability : float;
      (** Per-transfer probability that the destination will be fresh *)
  tps : float;  (** Transaction per seconds target *)
  strategy : transfer_strategy;
  regular_transfer_fee : Tez.t;
      (** fees for each transfer (except for transfers to smart contracts), in mutez *)
  regular_transfer_gas_limit : Gas.Arith.integral;
      (** gas limit per operation (except for transfers to smart contracts) *)
  storage_limit : Z.t;  (** storage limit per operation *)
  account_creation_storage : Z.t;
      (** upper bound on bytes consumed when creating a tz1 account *)
  total_transfers : int option;
      (** total number of transfers to perform; unbounded if None *)
  level_limit : limit option;
      (** total number of levels during which the stresstest is run; unbounded if None *)
  smart_contracts : Smart_contracts.t;
      (** An opaque type that stores all the information that is necessary for
    efficient sampling of smart contract calls. *)
}

type origin = Explicit | Wallet_pkh | Wallet_alias of string

type source = {
  pkh : public_key_hash;
  pk : public_key;
  sk : Tezos_crypto.Signature.V0.secret_key;
}

type source_with_uri = {
  pkh : public_key_hash;
  pk : public_key;
  pk_uri : Client_keys_v0.pk_uri;
  sk : Tezos_crypto.Signature.V0.secret_key;
  sk_uri : Client_keys_v0.sk_uri;
}

type input_source =
  | Explicit of source
  | Wallet_alias of string
  | Wallet_pkh of public_key_hash

type source_origin = {source : source; origin : origin}

(** Destination of a call: either an implicit contract or an originated one
   with all the necessary data (entrypoint and the argument). *)
type destination =
  | Implicit of Tezos_crypto.Signature.V0.Public_key_hash.t
  | Originated of Smart_contracts.invocation_parameters

type transfer = {
  src : source;
  dst : destination;
  fee : Tez.t;
  gas_limit : Gas.Arith.integral;
  amount : Tez.t;
  counter : Z.t option;
  fresh_dst : bool;
}

type state = {
  rng_state : Random.State.t;
  current_head_on_start : Block_hash.t;
  mutable pool : source_origin list;
  mutable pool_size : int;
  mutable shuffled_pool : source list;
  mutable revealed : Tezos_crypto.Signature.V0.Public_key_hash.Set.t;
  mutable last_block : Block_hash.t;
  mutable last_level : int;
  mutable target_block : Block_hash.t;
      (** The block on top of which we are injecting transactions (HEAD~2). *)
  new_block_condition : unit Lwt_condition.t;
  injected_operations : Operation_hash.t list Block_hash.Table.t;
}

(** Cost estimations for every kind of transaction used in the stress test.
   *)
type transaction_costs = {
  regular : Gas.Arith.integral;  (** Cost of a regular transaction. *)
  smart_contracts : (string * Gas.Arith.integral) list;
      (** Cost of a smart contract call (per contract alias). *)
}

type verbosity = Notice | Info | Debug

let verbosity = ref Notice

let log level msg =
  match (level, !verbosity) with
  | Notice, _ | Info, Info | Info, Debug | Debug, Debug -> msg ()
  | _ -> Lwt.return_unit

let pp_sep ppf () = Format.fprintf ppf ",@ "

let default_parameters =
  {
    seed = 0x533D;
    fresh_probability = 0.001;
    tps = 5.0;
    strategy = Fixed_amount {mutez = Tez.one};
    regular_transfer_fee = Tez.of_mutez_exn 2_000L;
    regular_transfer_gas_limit = Gas.Arith.integral_of_int_exn 1_600;
    (* [gas_limit] corresponds to a slight overapproximation of the
       gas needed to inject an operation. It was obtained by simulating
       the operation using the client. *)
    storage_limit = Z.zero;
    account_creation_storage = Z.of_int 300;
    (* [account_creation_storage] corresponds to a slight overapproximation
       of the storage consumed when allocating a new implicit account.
       It was obtained by simulating the operation using the client. *)
    total_transfers = None;
    level_limit = None;
    smart_contracts = Smart_contracts.no_contracts;
  }

let input_source_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"explicit"
        (Tag 0)
        (obj3
           (req "pkh" Tezos_crypto.Signature.V0.Public_key_hash.encoding)
           (req "pk" Tezos_crypto.Signature.V0.Public_key.encoding)
           (req "sk" Tezos_crypto.Signature.V0.Secret_key.encoding))
        (function Explicit {pkh; pk; sk} -> Some (pkh, pk, sk) | _ -> None)
        (fun (pkh, pk, sk) -> Explicit {pkh; pk; sk});
      case
        ~title:"alias"
        (Tag 1)
        (obj1 (req "alias" Data_encoding.string))
        (function Wallet_alias alias -> Some alias | _ -> None)
        (fun alias -> Wallet_alias alias);
      case
        ~title:"pkh"
        (Tag 2)
        (obj1 (req "pkh" Tezos_crypto.Signature.V0.Public_key_hash.encoding))
        (function Wallet_pkh pkh -> Some pkh | _ -> None)
        (fun pkh -> Wallet_pkh pkh);
    ]

let input_source_list_encoding = Data_encoding.list input_source_encoding

let injected_operations_encoding =
  let open Data_encoding in
  list
    (obj2
       (req "block_hash_when_injected" Block_hash.encoding)
       (req "operation_hashes" (list Operation_hash.encoding)))

let transaction_costs_encoding =
  let open Data_encoding in
  conv
    (fun {regular; smart_contracts} -> (regular, smart_contracts))
    (fun (regular, smart_contracts) -> {regular; smart_contracts})
    (obj2
       (req "regular" Gas.Arith.n_integral_encoding)
       (req "smart_contracts" (assoc Gas.Arith.n_integral_encoding)))

let destination_to_contract dst =
  match dst with
  | Implicit x -> Contract.Implicit x
  | Originated x -> x.destination

let parse_strategy s =
  match String.split ~limit:1 ':' s with
  | ["fixed"; parameter] -> (
      match int_of_string parameter with
      | exception _ -> Error "invalid integer literal"
      | mutez when mutez <= 0 -> Error "negative amount"
      | mutez -> (
          match Tez.of_mutez (Int64.of_int mutez) with
          | None -> Error "invalid mutez"
          | Some mutez -> Ok (Fixed_amount {mutez})))
  | ["evaporation"; parameter] -> (
      match float_of_string parameter with
      | exception _ -> Error "invalid float literal"
      | fraction when fraction < 0.0 || fraction > 1.0 ->
          Error "invalid evaporation rate"
      | fraction -> Ok (Evaporation {fraction}))
  | _ -> Error "invalid argument"

(** This command uses two different data structures for sources:
    - The in-output files one,
    - The normalized one.

    The data structure used for in-output files does not directly contain the
    data required to forge operations. For efficiency purposes, the sources are
    converted into a normalized data structure that contains all the required
    data to forge operations and the format originally used to be able to
    revert this conversion. *)

(** [normalize_source cctxt src] converts [src] from in-output data structure
    to normalized one. If the conversion fails, [None] is returned and a
    warning message is printed in [cctxt].

    Only unencrypted and encrypted sources from the wallet of [cctxt] are
    supported. *)
let normalize_source cctxt =
  let sk_of_sk_uri sk_uri =
    match
      Tezos_crypto.Signature.V0.Secret_key.of_b58check
        (Uri.path (sk_uri : Client_keys_v0.sk_uri :> Uri.t))
    with
    | Ok sk -> Lwt.return_some sk
    | Error _ -> (
        Tezos_signer_backends.Encrypted.decrypt cctxt sk_uri >|= function
        | Error _ -> None
        | Ok sk -> Tezos_crypto.Signature.V0.Of_V_latest.secret_key sk)
  in
  let key_from_alias alias =
    let warning msg alias =
      cctxt#warning msg alias >>= fun () -> Lwt.return_none
    in
    ( Client_keys_v0.alias_keys cctxt alias >>= function
      | Error _ | Ok None ->
          warning "Alias \"%s\" not found in the wallet" alias
      | Ok (Some (_, None, _)) | Ok (Some (_, _, None)) ->
          warning
            "Alias \"%s\" does not contain public or secret key and could not \
             be used for stresstest"
            alias
      | Ok (Some (pkh, Some pk, Some sk_uri)) -> (
          sk_of_sk_uri sk_uri >>= function
          | None ->
              warning
                "Cannot extract the secret key form the alias \"%s\" of the \
                 wallet"
                alias
          | Some sk ->
              Lwt.return_some
                {source = {pkh; pk; sk}; origin = Wallet_alias alias}) )
    >>= function
    | None -> warning "Source given as alias \"%s\" ignored" alias
    | key -> Lwt.return key
  in
  let key_from_wallet pkh =
    let warning msg pkh =
      cctxt#warning msg Tezos_crypto.Signature.V0.Public_key_hash.pp pkh
      >>= fun () -> Lwt.return_none
    in
    ( Client_keys_v0.get_key cctxt pkh >>= function
      | Error _ -> warning "Pkh \"%a\" not found in the wallet" pkh
      | Ok (alias, pk, sk_uri) -> (
          sk_of_sk_uri sk_uri >>= function
          | None ->
              cctxt#warning
                "Cannot extract the secret key form the pkh \"%a\" (alias: \
                 \"%s\") of the wallet"
                Tezos_crypto.Signature.V0.Public_key_hash.pp
                pkh
                alias
              >>= fun () -> Lwt.return_none
          | Some sk ->
              Lwt.return_some {source = {pkh; pk; sk}; origin = Wallet_pkh}) )
    >>= function
    | None -> warning "Source given as pkh \"%a\" ignored" pkh
    | key -> Lwt.return key
  in
  function
  | Explicit source -> Lwt.return_some {source; origin = Explicit}
  | Wallet_alias alias -> key_from_alias alias
  | Wallet_pkh pkh -> key_from_wallet pkh

(** [unnormalize_source src_org] converts [src_org] from normalized data
    structure to in-output one. *)
let unnormalize_source src_org =
  match src_org.origin with
  | Explicit -> Explicit src_org.source
  | Wallet_pkh -> Wallet_pkh src_org.source.pkh
  | Wallet_alias alias -> Wallet_alias alias

(** Samples from [state.pool]. Used to generate the destination of a
   transfer. *)
let sample_any_source_from_pool state =
  let idx = Random.State.int state.rng_state state.pool_size in
  match List.nth state.pool idx with
  | None -> assert false
  | Some src_org -> Lwt.return src_org.source

(** Takes and returns a source from [state.shuffled_pool]. Waits for a
   new block if no source is available. *)
let rec get_source_from_shuffled_pool state
    (cctxt : Protocol_client_context.full) =
  match state.shuffled_pool with
  | source :: l ->
      state.shuffled_pool <- l ;
      log Debug (fun () ->
          cctxt#message
            "sample_transfer: %d unused sources for the block next to %a"
            (List.length l)
            Block_hash.pp
            state.last_block)
      >>= fun () -> Lwt.return source
  | [] ->
      cctxt#message
        "all available sources have been used for block next to %a"
        Block_hash.pp
        state.last_block
      >>= fun () ->
      Lwt_condition.wait state.new_block_condition >>= fun () ->
      get_source_from_shuffled_pool state cctxt

let random_seed rng =
  Bytes.init 32 (fun _ -> Char.chr (Random.State.int rng 256))

let generate_fresh_source state =
  let seed = random_seed state.rng_state in
  let pkh, pk, sk = Tezos_crypto.Signature.V0.generate_key ~seed () in
  let fresh = {source = {pkh; pk; sk}; origin = Explicit} in
  state.pool <- fresh :: state.pool ;
  state.pool_size <- state.pool_size + 1 ;
  fresh.source

(* [heads_iter cctxt f] calls [f head] each time there is a new head received
   by the streamed RPC /monitor/heads/main and returns [promise, stopper].
   [promise] resolved when the stream is closed. [stopper ()] closes the
   stream. *)
let heads_iter (cctxt : Protocol_client_context.full)
    (f : Block_hash.t * Tezos_base.Block_header.t -> unit tzresult Lwt.t) :
    (unit tzresult Lwt.t * Tezos_rpc.Context.stopper) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* heads_stream, stopper = Shell_services.Monitor.heads cctxt `Main in
  let rec loop () : unit tzresult Lwt.t =
    let*! block_hash_and_header = Lwt_stream.get heads_stream in
    match block_hash_and_header with
    | None -> Error_monad.failwith "unexpected end of block stream@."
    | Some ((new_block_hash, _block_header) as block_hash_and_header) ->
        Lwt.catch
          (fun () ->
            let*! () =
              log Debug (fun () ->
                  cctxt#message
                    "heads_iter: new block received %a@."
                    Block_hash.pp
                    new_block_hash)
            in
            let* protocols =
              Shell_services.Blocks.protocols
                cctxt
                ~block:(`Hash (new_block_hash, 0))
                ()
            in
            if Protocol_hash.(protocols.current_protocol = Protocol.hash) then
              let* () = f block_hash_and_header in
              loop ()
            else
              let*! () =
                log Debug (fun () ->
                    cctxt#message
                      "heads_iter: new block on protocol %a. Stopping \
                       iteration.@."
                      Protocol_hash.pp
                      protocols.current_protocol)
              in
              return_unit)
          (fun exn ->
            Error_monad.failwith
              "An exception occurred on a function bound on new heads : %s@."
              (Printexc.to_string exn))
  in
  let promise = loop () in
  let*! () =
    log Debug (fun () ->
        cctxt#message
          "head iteration for proto %a stopped@."
          Protocol_hash.pp
          Protocol.hash)
  in
  return (promise, stopper)

let sample_smart_contracts smart_contracts rng_state =
  let smart_contract =
    Smart_contracts.select smart_contracts (Random.State.float rng_state 1.0)
  in
  Option.map
    (fun invocation_parameters ->
      ( Originated invocation_parameters,
        invocation_parameters.fee,
        invocation_parameters.gas_limit ))
    smart_contract

(* We perform rejection sampling of valid sources.
   We could maintain a local cache of existing contracts with sufficient balance. *)
let rec sample_transfer (cctxt : Protocol_client_context.full) chain block
    (parameters : parameters) (state : state) =
  get_source_from_shuffled_pool state cctxt >>= fun src ->
  Alpha_services.Contract.balance
    cctxt
    (chain, block)
    (Contract.Implicit src.pkh)
  >>=? fun tez ->
  if Tez.(tez = zero) then
    log Debug (fun () ->
        cctxt#message
          "sample_transfer: invalid balance %a"
          Tezos_crypto.Signature.V0.Public_key_hash.pp
          src.pkh)
    >>= fun () ->
    (* Sampled source has zero balance: the transfer that created that
       address was not included yet. Retry *)
    sample_transfer cctxt chain block parameters state
  else
    let fresh =
      Random.State.float state.rng_state 1.0 < parameters.fresh_probability
    in
    (match
       sample_smart_contracts parameters.smart_contracts state.rng_state
     with
    | None ->
        (if fresh then Lwt.return (generate_fresh_source state)
         else sample_any_source_from_pool state)
        >|= fun dest ->
        Ok
          ( Implicit dest.pkh,
            parameters.regular_transfer_fee,
            parameters.regular_transfer_gas_limit )
    | Some v -> return v)
    >>=? fun (dst, fee, gas_limit) ->
    let amount =
      match parameters.strategy with
      | Fixed_amount {mutez} -> mutez
      | Evaporation {fraction} ->
          let mutez = Int64.to_float (Tez.to_mutez tez) in
          let max_fraction = Int64.of_float (mutez *. fraction) in
          let amount =
            if max_fraction = 0L then 1L
            else max 1L (Random.State.int64 state.rng_state max_fraction)
          in
          Tez.of_mutez_exn amount
    in
    return {src; dst; fee; gas_limit; amount; counter = None; fresh_dst = fresh}

let inject_contents (cctxt : Protocol_client_context.full) branch sk contents =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({branch}, Contents_list contents)
  in
  let signature =
    Some
      (Tezos_crypto.Signature.V0.sign
         ~watermark:Tezos_crypto.Signature.V0.Generic_operation
         sk
         bytes)
  in
  let op : _ Operation.t =
    {shell = {branch}; protocol_data = {contents; signature}}
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn Operation.encoding (Operation.pack op)
  in
  Shell_services.Injection.operation cctxt bytes

(* counter _must_ be set before calling this function *)
let manager_op_of_transfer parameters
    {src; dst; fee; gas_limit; amount; counter; fresh_dst} =
  let source = src.pkh in
  let storage_limit =
    if fresh_dst then
      Z.add parameters.account_creation_storage parameters.storage_limit
    else parameters.storage_limit
  in
  let operation =
    let parameters =
      let open Tezos_micheline in
      Script.lazy_expr
        (match dst with
        | Implicit _ ->
            Micheline.strip_locations
              (Prim (0, Michelson_v1_primitives.D_Unit, [], []))
        | Originated x -> x.arg)
    in
    let entrypoint =
      match dst with
      | Implicit _ -> Entrypoint.default
      | Originated x -> x.entrypoint
    in
    let destination = destination_to_contract dst in
    Transaction {amount; parameters; entrypoint; destination}
  in
  match counter with
  | None -> assert false
  | Some counter ->
      Manager_operation
        {source; fee; counter; operation; gas_limit; storage_limit}

let cost_of_manager_operation = Gas.Arith.integral_of_int_exn 1_000

let inject_transfer (cctxt : Protocol_client_context.full) parameters state
    transfer =
  Shell_services.Blocks.hash cctxt () >>=? fun branch ->
  Alpha_services.Contract.counter cctxt (`Main, `Head 0) transfer.src.pkh
  >>=? fun current_counter ->
  (if
     Tezos_crypto.Signature.V0.Public_key_hash.Set.mem
       transfer.src.pkh
       state.revealed
   then return true
   else (
     (* Either the [manager_key] RPC tells us the key is already
        revealed, or we immediately inject a reveal operation: in any
        case the key is revealed in the end. *)
     state.revealed <-
       Tezos_crypto.Signature.V0.Public_key_hash.Set.add
         transfer.src.pkh
         state.revealed ;
     Alpha_services.Contract.manager_key cctxt (`Main, `Head 0) transfer.src.pkh
     >>=? fun pk_opt -> return (Option.is_some pk_opt)))
  >>=? fun already_revealed ->
  (if not already_revealed then
     let reveal_counter = Z.succ current_counter in
     let transf_counter = Z.succ reveal_counter in
     let reveal =
       Manager_operation
         {
           source = transfer.src.pkh;
           fee = Tez.zero;
           counter = reveal_counter;
           gas_limit = cost_of_manager_operation;
           storage_limit = Z.zero;
           operation = Reveal transfer.src.pk;
         }
     in
     let manager_op =
       manager_op_of_transfer
         parameters
         {transfer with counter = Some transf_counter}
     in
     let list = Cons (reveal, Single manager_op) in
     log Info (fun () ->
         cctxt#message
           "injecting reveal+transfer from %a (counters=%a,%a) to %a"
           Tezos_crypto.Signature.V0.Public_key_hash.pp
           transfer.src.pkh
           Z.pp_print
           reveal_counter
           Z.pp_print
           transf_counter
           Contract.pp
           (destination_to_contract transfer.dst))
     >>= fun () ->
     (* NB: regardless of our best efforts to keep track of counters, injection can fail with
        "counter in the future" if a block switch happens in between the moment we
        get the branch and the moment we inject, and the new block does not include
        all the operations we injected. *)
     inject_contents cctxt state.target_block transfer.src.sk list
   else
     let transf_counter = Z.succ current_counter in
     let manager_op =
       manager_op_of_transfer
         parameters
         {transfer with counter = Some transf_counter}
     in
     let list = Single manager_op in
     log Info (fun () ->
         cctxt#message
           "injecting transfer from %a (counter=%a) to %a"
           Tezos_crypto.Signature.V0.Public_key_hash.pp
           transfer.src.pkh
           Z.pp_print
           transf_counter
           Contract.pp
           (destination_to_contract transfer.dst))
     >>= fun () ->
     (* See comment above. *)
     inject_contents cctxt state.target_block transfer.src.sk list)
  >>= function
  | Ok op_hash ->
      log Debug (fun () ->
          cctxt#message
            "inject_transfer: op injected %a"
            Operation_hash.pp
            op_hash)
      >>= fun () ->
      let ops =
        Option.value
          ~default:[]
          (Block_hash.Table.find state.injected_operations branch)
      in
      Block_hash.Table.replace state.injected_operations branch (op_hash :: ops) ;
      return_unit
  | Error e ->
      log Debug (fun () ->
          cctxt#message
            "inject_transfer: error, op not injected: %a"
            Error_monad.pp_print_trace
            e)
      >>= fun () -> return_unit

let save_injected_operations (cctxt : Protocol_client_context.full) state =
  let json =
    Data_encoding.Json.construct
      injected_operations_encoding
      (Block_hash.Table.fold
         (fun k v acc -> (k, v) :: acc)
         state.injected_operations
         [])
  in
  let path =
    Filename.temp_file "client-stresstest-injected_operations-" ".json"
  in
  cctxt#message "writing injected operations in file %s" path >>= fun () ->
  Lwt_utils_unix.Json.write_file path json >>= function
  | Error e ->
      cctxt#message
        "could not write injected operations json file: %a"
        Error_monad.pp_print_trace
        e
  | Ok _ -> Lwt.return_unit

let stat_on_exit (cctxt : Protocol_client_context.full) state =
  let ratio_injected_included_op () =
    Shell_services.Blocks.hash cctxt () >>=? fun current_head_on_exit ->
    let inter_cardinal s1 s2 =
      Operation_hash.Set.cardinal
        (Operation_hash.Set.inter
           (Operation_hash.Set.of_list s1)
           (Operation_hash.Set.of_list s2))
    in
    let get_included_ops older_block =
      let rec get_included_ops block acc_included_ops =
        if block = older_block then return acc_included_ops
        else
          Shell_services.Chain.Blocks.Operation_hashes.operation_hashes_in_pass
            cctxt
            ~chain:`Main
            ~block:(`Hash (block, 0))
            3
          >>=? fun included_ops ->
          Shell_services.Blocks.list
            cctxt
            ~chain:`Main
            ~heads:[block]
            ~length:2
            ()
          >>=? function
          | [[current; predecessor]] when current = block ->
              get_included_ops
                predecessor
                (List.append acc_included_ops included_ops)
          | _ -> cctxt#error "Error while computing stats: invalid block list"
      in
      get_included_ops current_head_on_exit []
    in
    let injected_ops =
      Block_hash.Table.fold
        (fun k l acc ->
          (* The operations injected during the last block are ignored because
             they should not be currently included. *)
          if current_head_on_exit <> k then List.append acc l else acc)
        state.injected_operations
        []
    in
    get_included_ops state.current_head_on_start >>=? fun included_ops ->
    let included_ops_count = inter_cardinal injected_ops included_ops in
    log Debug (fun () ->
        cctxt#message
          "injected : [%a]@.included: [%a]"
          (Format.pp_print_list ~pp_sep Operation_hash.pp)
          injected_ops
          (Format.pp_print_list ~pp_sep Operation_hash.pp)
          included_ops)
    >>= fun () ->
    let injected_ops_count = List.length injected_ops in
    cctxt#message
      "%s of the injected operations have been included (%d injected, %d \
       included). Note that the operations injected during the last block are \
       ignored because they should not be currently included."
      (if Int.equal injected_ops_count 0 then "N/A"
       else Format.sprintf "%d%%" (included_ops_count * 100 / injected_ops_count))
      injected_ops_count
      included_ops_count
    >>= fun () -> return_unit
  in
  ratio_injected_included_op ()

let launch (cctxt : Protocol_client_context.full) (parameters : parameters)
    state save_pool_callback =
  let injected = ref 0 in
  let target_level =
    match parameters.level_limit with
    | None -> None
    | Some (Abs target) -> Some target
    | Some (Rel offset) -> Some (state.last_level + offset)
  in
  let dt = 1. /. parameters.tps in
  let terminated () =
    if
      match parameters.total_transfers with
      | None -> false
      | Some bound -> bound <= !injected
    then
      cctxt#message
        "Stopping after %d injections (target %a)."
        !injected
        Format.(pp_print_option pp_print_int)
        parameters.total_transfers
      >>= fun () -> Lwt.return_true
    else
      match target_level with
      | None -> Lwt.return_false
      | Some target ->
          if target <= state.last_level then
            cctxt#message
              "Stopping at level %d (target level: %d)."
              state.last_level
              target
            >>= fun () -> Lwt.return_true
          else Lwt.return_false
  in

  let rec loop () =
    terminated () >>= fun terminated ->
    if terminated then
      save_pool_callback () >>= fun () ->
      save_injected_operations cctxt state >>= fun () ->
      stat_on_exit cctxt state
    else
      let start = Mtime_clock.counter () in
      log Debug (fun () -> cctxt#message "launch.loop: invoke sample_transfer")
      >>= fun () ->
      sample_transfer cctxt cctxt#chain cctxt#block parameters state
      >>=? fun transfer ->
      log Debug (fun () -> cctxt#message "launch.loop: invoke inject_transfer")
      >>= fun () ->
      inject_transfer cctxt parameters state transfer >>=? fun () ->
      incr injected ;
      let elapsed = Time.Monotonic.Span.to_float_s (Mtime_clock.count start) in
      let remaining = dt -. elapsed in
      (if remaining <= 0.0 then
         cctxt#warning
           "warning: tps target could not be reached, consider using a lower \
            value for --tps"
       else Lwt_unix.sleep remaining)
      >>= loop
  in
  let on_new_head :
      Block_hash.t * Tezos_base.Block_header.t -> unit tzresult Lwt.t =
    (* Because of how Tenderbake works the target block should stay 2
       blocks in the past because this guarantees that we are targeting a
       block that is decided. *)
    let update_target_block () =
      Shell_services.Blocks.hash cctxt ~block:(`Head 2) ()
      >>=? fun target_block ->
      state.target_block <- target_block ;
      return_unit
    in
    fun (new_block_hash, new_block_header) ->
      update_target_block () >>=? fun () ->
      if not (Block_hash.equal new_block_hash state.last_block) then (
        state.last_block <- new_block_hash ;
        state.last_level <- Int32.to_int new_block_header.shell.level ;
        state.shuffled_pool <-
          List.shuffle
            ~rng:state.rng_state
            (List.map (fun src_org -> src_org.source) state.pool)) ;
      Lwt_condition.broadcast state.new_block_condition () ;
      return_unit
  in
  heads_iter cctxt on_new_head >>=? fun (heads_iteration, stopper) ->
  (* The head iteration stops at protocol change. *)
  Lwt.pick [loop (); heads_iteration] >>=? fun () ->
  (match Lwt.state heads_iteration with Lwt.Return _ -> () | _ -> stopper ()) ;
  return_unit

let group =
  Tezos_clic.
    {name = "stresstest"; title = "Commands for stress-testing the network"}

type pool_source =
  | From_string of {json : Ezjsonm.value}
  | From_file of {path : string; json : Ezjsonm.value}

let json_of_pool_source = function
  | From_string {json} | From_file {json; _} -> json

let json_file_or_text_parameter =
  Tezos_clic.parameter (fun _ p ->
      match String.split ~limit:1 ':' p with
      | ["text"; text] -> return (From_string {json = Ezjsonm.from_string text})
      | ["file"; path] ->
          Lwt_utils_unix.Json.read_file path >|=? fun json ->
          From_file {path; json}
      | _ -> (
          if Sys.file_exists p then
            Lwt_utils_unix.Json.read_file p >|=? fun json ->
            From_file {path = p; json}
          else
            try return (From_string {json = Ezjsonm.from_string p})
            with Ezjsonm.Parse_error _ ->
              failwith "Neither an existing file nor valid JSON: '%s'" p))

let seed_arg =
  let open Tezos_clic in
  arg
    ~long:"seed"
    ~placeholder:"int"
    ~doc:"random seed"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match int_of_string s with
         | exception _ ->
             cctxt#error
               "While parsing --seed: could not convert argument to int"
         | i -> return i))

let tps_arg =
  let open Tezos_clic in
  arg
    ~long:"tps"
    ~placeholder:"float"
    ~doc:"transactions per seconds target"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match float_of_string s with
         | exception _ ->
             cctxt#error
               "While parsing --tps: could not convert argument to float"
         | f when f < 0.0 ->
             cctxt#error "While parsing --tps: negative argument"
         | f -> return f))

let fresh_probability_arg =
  let open Tezos_clic in
  arg
    ~long:"fresh-probability"
    ~placeholder:"float in [0;1]"
    ~doc:
      (Format.sprintf
         "Probability for each transaction's destination to be a fresh \
          account. The default value is %g. This new account may then be used \
          as source or destination of subsequent transactions, just like the \
          accounts that were initially provided to the command. Note that when \
          [--single-op-per-pkh-per-block] is set, the new account will not be \
          used as source until the head changes."
         default_parameters.fresh_probability)
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match float_of_string s with
         | exception _ ->
             cctxt#error
               "While parsing --fresh-probability: could not convert argument \
                to float"
         | f when f < 0.0 || f > 1.0 ->
             cctxt#error "While parsing --fresh-probability: invalid argument"
         | f -> return f))

let smart_contract_parameters_arg =
  let open Tezos_clic in
  arg
    ~long:"smart-contract-parameters"
    ~placeholder:"JSON file with smart contract parameters"
    ~doc:
      (Format.sprintf
         "A JSON object that maps smart contract aliases to objects with three \
          fields: probability in [0;1], invocation_fee, and \
          invocation_gas_limit.")
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match Data_encoding.Json.from_string s with
         | Ok json ->
             return
               (Data_encoding.Json.destruct
                  Smart_contracts.contract_parameters_collection_encoding
                  json)
         | Error _ ->
             cctxt#error
               "While parsing --smart-contract-parameters: invalid JSON %s"
               s))

let strategy_arg =
  let open Tezos_clic in
  arg
    ~long:"strategy"
    ~placeholder:"fixed:mutez | evaporation:[0;1]"
    ~doc:"wealth redistribution strategy"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match parse_strategy s with
         | Error msg -> cctxt#error "While parsing --strategy: %s" msg
         | Ok strategy -> return strategy))

let gas_limit_arg =
  let open Tezos_clic in
  let gas_limit_kind =
    parameter (fun _ s ->
        try
          let v = Z.of_string s in
          return (Gas.Arith.integral_exn v)
        with _ -> failwith "invalid gas limit (must be a positive number)")
  in
  arg
    ~long:"gas-limit"
    ~short:'G'
    ~placeholder:"amount"
    ~doc:
      (Format.asprintf
         "Set the gas limit of the transaction instead of using the default \
          value of %a"
         Gas.Arith.pp_integral
         default_parameters.regular_transfer_gas_limit)
    gas_limit_kind

let storage_limit_arg =
  let open Tezos_clic in
  let storage_limit_kind =
    parameter (fun _ s ->
        try
          let v = Z.of_string s in
          assert (Compare.Z.(v >= Z.zero)) ;
          return v
        with _ ->
          failwith "invalid storage limit (must be a positive number of bytes)")
  in
  arg
    ~long:"storage-limit"
    ~short:'S'
    ~placeholder:"amount"
    ~doc:
      (Format.asprintf
         "Set the storage limit of the transaction instead of using the \
          default value of %a"
         Z.pp_print
         default_parameters.storage_limit)
    storage_limit_kind

let transfers_arg =
  let open Tezos_clic in
  arg
    ~long:"transfers"
    ~placeholder:"integer"
    ~doc:"total number of transfers to perform, unbounded if not specified"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match int_of_string s with
         | exception _ ->
             cctxt#error "While parsing --transfers: invalid integer literal"
         | i when i <= 0 ->
             cctxt#error "While parsing --transfers: negative integer"
         | i -> return i))

let level_limit_arg =
  let open Tezos_clic in
  arg
    ~long:"level-limit"
    ~placeholder:"integer | +integer"
    ~doc:
      "Level at which the stresstest will stop (if prefixed by '+', the level \
       is relative to the current head)"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match int_of_string s with
         | exception _ ->
             cctxt#error "While parsing --levels: invalid integer literal"
         | i when i <= 0 ->
             cctxt#error "While parsing --levels: negative integer or zero"
         | i -> if String.get s 0 = '+' then return (Rel i) else return (Abs i)))

let verbose_arg =
  Tezos_clic.switch
    ~long:"verbose"
    ~short:'v'
    ~doc:"Display detailed logs of the injected operations"
    ()

let debug_arg =
  Tezos_clic.switch ~long:"debug" ~short:'V' ~doc:"Display debug logs" ()

let set_option opt f x = Option.fold ~none:x ~some:(f x) opt

let save_pool_callback (cctxt : Protocol_client_context.full) pool_source state
    =
  let json =
    Data_encoding.Json.construct
      input_source_list_encoding
      (List.map unnormalize_source state.pool)
  in
  let catch_write_error = function
    | Error e ->
        cctxt#message
          "could not write back json file: %a"
          Error_monad.pp_print_trace
          e
    | Ok () -> Lwt.return_unit
  in
  match pool_source with
  | From_string _ ->
      (* If the initial pool was given directly as json, save pool to
         a temp file. *)
      let path = Filename.temp_file "client-stresstest-pool-" ".json" in
      cctxt#message "writing back address pool in file %s" path >>= fun () ->
      Lwt_utils_unix.Json.write_file path json >>= catch_write_error
  | From_file {path; _} ->
      (* If the pool specification was a json file, save pool to
         the same file. *)
      cctxt#message "writing back address pool in file %s" path >>= fun () ->
      Lwt_utils_unix.Json.write_file path json >>= catch_write_error

let generate_random_transactions =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Generate random transactions"
    (args12
       seed_arg
       tps_arg
       fresh_probability_arg
       smart_contract_parameters_arg
       strategy_arg
       Client_proto_args.fee_arg
       gas_limit_arg
       storage_limit_arg
       transfers_arg
       level_limit_arg
       verbose_arg
       debug_arg)
    (prefixes ["stresstest"; "transfer"; "using"]
    @@ param
         ~name:"sources.json"
         ~desc:
           {|List of accounts from which to perform transfers in JSON format. The input JSON must be an array of objects of the form {"pkh":"<pkh>","pk":"<pk>","sk":"<sk>"} or  {"alias":"<alias from wallet>"} or {"pkh":"<pkh from wallet>"} with the pkh, pk and sk encoded in B58 form."|}
         json_file_or_text_parameter
    @@ stop)
    (fun ( seed,
           tps,
           freshp,
           smart_contract_parameters,
           strat,
           fee,
           gas_limit,
           storage_limit,
           transfers,
           level_limit,
           verbose_flag,
           debug_flag )
         sources_json
         (cctxt : Protocol_client_context.full)
       ->
      (verbosity :=
         match (debug_flag, verbose_flag) with
         | true, _ -> Debug
         | false, true -> Info
         | false, false -> Notice) ;
      Smart_contracts.init
        cctxt
        (Option.value ~default:[] smart_contract_parameters)
      >>=? fun smart_contracts ->
      let parameters =
        {default_parameters with smart_contracts}
        |> set_option seed (fun parameter seed -> {parameter with seed})
        |> set_option tps (fun parameter tps -> {parameter with tps})
        |> set_option freshp (fun parameter fresh_probability ->
               {parameter with fresh_probability})
        |> set_option strat (fun parameter strategy ->
               {parameter with strategy})
        |> set_option fee (fun parameter regular_transfer_fee ->
               {parameter with regular_transfer_fee})
        |> set_option gas_limit (fun parameter regular_transfer_gas_limit ->
               {parameter with regular_transfer_gas_limit})
        |> set_option storage_limit (fun parameter storage_limit ->
               {parameter with storage_limit})
        |> set_option transfers (fun parameter transfers ->
               {parameter with total_transfers = Some transfers})
        |> set_option level_limit (fun parameter level_limit ->
               {parameter with level_limit = Some level_limit})
      in
      match
        Data_encoding.Json.destruct
          input_source_list_encoding
          (json_of_pool_source sources_json)
      with
      | exception _ -> cctxt#error "Could not decode list of sources"
      | [] -> cctxt#error "It is required to provide sources"
      | sources ->
          log Info (fun () -> cctxt#message "starting to normalize sources")
          >>= fun () ->
          List.filter_map_s (normalize_source cctxt) sources >>= fun sources ->
          log Info (fun () -> cctxt#message "all sources have been normalized")
          >>= fun () ->
          let sources =
            List.sort_uniq
              (fun src1 src2 ->
                Tezos_crypto.Signature.V0.Secret_key.compare
                  src1.source.sk
                  src2.source.sk)
              sources
          in
          let rng_state = Random.State.make [|parameters.seed|] in
          Shell_services.Blocks.hash cctxt () >>=? fun current_head_on_start ->
          Shell_services.Blocks.Header.shell_header cctxt ()
          >>=? fun header_on_start ->
          (if header_on_start.level <= 2l then
             cctxt#error
               "The level of the head (%a) needs to be greater than 2 and is \
                actually %ld."
               Block_hash.pp
               current_head_on_start
               header_on_start.level
           else return_unit)
          >>=? fun () ->
          Shell_services.Blocks.hash cctxt ~block:(`Head 2) ()
          >>=? fun current_target_block ->
          let state =
            {
              rng_state;
              current_head_on_start;
              pool = sources;
              pool_size = List.length sources;
              shuffled_pool =
                List.shuffle
                  ~rng:rng_state
                  (List.map (fun src_org -> src_org.source) sources);
              revealed = Tezos_crypto.Signature.V0.Public_key_hash.Set.empty;
              last_block = current_head_on_start;
              last_level = Int32.to_int header_on_start.level;
              target_block = current_target_block;
              new_block_condition = Lwt_condition.create ();
              injected_operations = Block_hash.Table.create 1023;
            }
          in
          let exit_callback_id =
            Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _retcode ->
                stat_on_exit cctxt state >>= function
                | Ok () -> Lwt.return_unit
                | Error e ->
                    cctxt#message "Error: %a" Error_monad.pp_print_trace e)
          in
          let save_pool () = save_pool_callback cctxt sources_json state in
          (* Register a callback for saving the pool when the tool is interrupted
             through ctrl-c *)
          let exit_callback_id =
            Lwt_exit.register_clean_up_callback
              ~loc:__LOC__
              ~after:[exit_callback_id]
              (fun _retcode -> save_pool ())
          in
          let save_injected_operations () =
            save_injected_operations cctxt state
          in
          ignore
            (Lwt_exit.register_clean_up_callback
               ~loc:__LOC__
               ~after:[exit_callback_id]
               (fun _retcode -> save_injected_operations ())) ;
          launch cctxt parameters state save_pool)

let estimate_transaction_cost ?smart_contracts
    (cctxt : Protocol_client_context.full) : Gas.Arith.integral tzresult Lwt.t =
  normalize_source cctxt (Wallet_alias "bootstrap1") >>= fun src ->
  normalize_source cctxt (Wallet_alias "bootstrap2") >>= fun dst ->
  let rng_state = Random.State.make [|default_parameters.seed|] in
  (match (src, dst) with
  | Some src, Some dst -> return (src, dst)
  | _ ->
      cctxt#error "Cannot find bootstrap1 or bootstrap2 accounts in the wallet.")
  >>=? fun (src, dst) ->
  let chain = cctxt#chain in
  let block = cctxt#block in
  let selected_smart_contract =
    Option.bind smart_contracts (fun smart_contracts ->
        sample_smart_contracts smart_contracts rng_state)
  in
  let dst, fee, gas_limit =
    Option.value
      selected_smart_contract
      ~default:
        ( Implicit dst.source.pkh,
          default_parameters.regular_transfer_fee,
          default_parameters.regular_transfer_gas_limit )
  in
  Alpha_services.Contract.counter cctxt (chain, block) src.source.pkh
  >>=? fun current_counter ->
  let transf_counter = Z.succ current_counter in
  let transfer =
    {
      src = src.source;
      dst;
      fee;
      gas_limit;
      amount = Tez.of_mutez_exn (Int64.of_int 1);
      counter = Some transf_counter;
      fresh_dst = false;
    }
  in
  let manager_op =
    manager_op_of_transfer
      {
        default_parameters with
        regular_transfer_gas_limit =
          Default_parameters.constants_mainnet.hard_gas_limit_per_operation;
      }
      transfer
  in
  Injection.simulate cctxt ~chain ~block (Single manager_op)
  >>=? fun (_oph, op, result) ->
  match result.contents with
  | Single_result (Manager_operation_result {operation_result; _}) -> (
      match operation_result with
      | Applied
          (Transaction_result (Transaction_to_contract_result {consumed_gas; _}))
        ->
          return (Gas.Arith.ceil consumed_gas)
      | _ ->
          (match operation_result with
          | Failed (_, errors) ->
              Error_monad.pp_print_trace
                Format.err_formatter
                (Environment.wrap_tztrace errors)
          | _ -> assert false) ;
          cctxt#error
            "@[<v 2>Simulation result:@,%a@]"
            Operation_result.pp_operation_result
            (op.protocol_data.contents, result.contents))

let estimate_transaction_costs : Protocol_client_context.full Tezos_clic.command
    =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Output gas estimations for transactions that stresstest uses"
    no_options
    (prefixes ["stresstest"; "estimate"; "gas"] @@ stop)
    (fun () cctxt ->
      estimate_transaction_cost cctxt >>=? fun regular ->
      Smart_contracts.with_every_known_smart_contract
        cctxt
        (fun smart_contracts ->
          estimate_transaction_cost ~smart_contracts cctxt)
      >>=? fun smart_contracts ->
      let transaction_costs : transaction_costs = {regular; smart_contracts} in
      let json =
        Data_encoding.Json.construct
          transaction_costs_encoding
          transaction_costs
      in
      Format.printf "%a" Data_encoding.Json.pp json ;
      return_unit)

(* Returns a list of transfers from each element of [sources]. *)
let generate_transfers ~sources ~amount ~parameters ~entrypoint ~fee ~gas_limit
    ~storage_limit =
  List.map
    (fun dst ->
      let destination = Contract.Implicit dst.pkh in
      let transfer =
        Client_proto_context.build_transaction_operation
          ~amount
          ~parameters
          ~entrypoint
          ~fee
          ~gas_limit
          ~storage_limit
          destination
      in
      Annotated_manager_operation.Annotated_manager_operation transfer)
    sources

(* Returns a list of reveals from each element of [sources]. *)
let generate_reveals ~sources ~fee ~gas_limit ~storage_limit =
  List.map
    (fun dst ->
      let reveal =
        Client_proto_context.build_reveal_operation
          ~fee
          ~gas_limit
          ~storage_limit
          dst.pk
      in
      (dst, Annotated_manager_operation.Annotated_manager_operation reveal))
    sources

(* Given a list of [sources], it returns
   - a list of batches of transfers where each batch has a maximum of
     [batch_size] operation, for each element of [sources],
   - a list of reveals, for each element of [sources].

    [sources] is the list of "starter" accounts, used to fund all
    accounts in a exponential way.
*)
let generate_starter_ops ~sources ~amount ~batch_size =
  let fee = Tez.of_mutez_exn 1_000L in
  let gas_limit = Gas.Arith.integral_of_int_exn 1_040 in
  let storage_limit = Z.of_int 257 in
  let parameters =
    let open Tezos_micheline in
    Script.lazy_expr
      (Micheline.strip_locations
         (Prim (0, Michelson_v1_primitives.D_Unit, [], [])))
  in
  let entrypoint = Entrypoint.default in
  let txs_ops =
    generate_transfers
      ~sources
      ~amount
      ~parameters
      ~entrypoint
      ~fee
      ~gas_limit
      ~storage_limit
  in
  let reveal_ops = generate_reveals ~sources ~fee ~gas_limit ~storage_limit in
  let rec split n acc = function
    | [] -> acc
    | l ->
        let current, next = List.rev_split_n n l in
        let batch = Annotated_manager_operation.manager_of_list current in
        split n (batch :: acc) next
  in
  (* Split the list of transfers into N batches containing a maximum
     of [batch_size] operations. *)
  let txs_batch_l = split batch_size [] txs_ops in
  (txs_batch_l, reveal_ops)

(* Returns a list of list of batch. A list of batch consists of N
   batches, depending on the number of [starter_sources]. The top
   level list can be seen a block partition, so that the 1M
   restriction is ensured. *)
let generate_account_funding_batches (starter_sources : source_with_uri list)
    (empty_accounts : source_with_uri list) ~batch_size ~amount =
  let open Lwt_result_syntax in
  let nb_sources = List.length starter_sources in
  let fee = Tez.of_mutez_exn 1_000L in
  let gas_limit = Gas.Arith.integral_of_int_exn 1_040 in
  let storage_limit = Z.of_int 257 in
  let parameters =
    let open Tezos_micheline in
    Script.lazy_expr
      (Micheline.strip_locations
         (Prim (0, Michelson_v1_primitives.D_Unit, [], [])))
  in
  let entrypoint = Entrypoint.default in
  let to_batch candidates emiters =
    (* For each [emiters], it generates [batch_size] transactions from
       it, and to [batch_size] candidates.*)
    let rec aux acc (candidates : source_with_uri list)
        (emiters : source_with_uri list) =
      match emiters with
      | [] -> return acc
      | source :: next_sources ->
          let current, next_candidates =
            List.rev_split_n batch_size candidates
          in
          let txs =
            generate_transfers
              ~sources:current
              ~amount
              ~parameters
              ~entrypoint
              ~fee
              ~gas_limit
              ~storage_limit
          in
          let batch = Annotated_manager_operation.manager_of_list txs in
          (*Avoid the generation of empty batches*)
          if next_candidates = [] then return ((source, batch) :: acc)
          else aux ((source, batch) :: acc) next_candidates next_sources
    in
    aux [] candidates emiters
  in
  let rec aux acc = function
    | [] -> return acc
    | empty_accounts ->
        let candidates, rest =
          List.rev_split_n (batch_size * nb_sources) empty_accounts
        in
        let* batch = to_batch candidates starter_sources in
        aux (batch :: acc) rest
  in
  let* res = aux [] empty_accounts in
  return res

(* Loads a wallet by reading directly the files to speed up things. *)
let load_wallet cctxt ~source_pkh =
  let open Lwt_result_syntax in
  let* keys = Client_keys_v0.get_keys cctxt in
  (* Convert loaded and filter identities. We want to ban activator
     and bootstrap<1-5> in sandbox, as well as the "faucet source" on
     test networks. *)
  let to_ban =
    ["activator"; "bootstrap"]
    @ WithExceptions.Result.get_ok
        ~loc:__LOC__
        (List.init ~when_negative_length:"error" 5 (fun i ->
             Format.sprintf "bootstrap%d" (i + 1)))
  in
  let rec aux acc = function
    | [] -> return acc
    | (alias, pkh, _, _) :: tl
      when List.exists (String.equal alias) to_ban
           || Tezos_crypto.Signature.V0.Public_key_hash.equal pkh source_pkh ->
        aux acc tl
    | (_, pkh, pk, sk_uri) :: tl ->
        let* pk_uri = Client_keys_v0.neuterize sk_uri in
        let payload =
          Uri.path (sk_uri : Tezos_signer_backends.Unencrypted.sk_uri :> Uri.t)
        in
        let sk = Tezos_crypto.Signature.V0.Secret_key.of_b58check_exn payload in
        aux ({pkh; pk; pk_uri; sk; sk_uri} :: acc) tl
  in
  aux [] keys

let source_key_arg =
  let open Tezos_clic in
  param
    ~name:"source_key_arg"
    ~desc:
      "Source key public key hash from which the tokens will be transferred to \
       start the funding."
    (parameter (fun _ s ->
         let r = Tezos_crypto.Signature.V0.Public_key_hash.of_b58check s in
         match r with
         | Ok pkh -> Lwt_result_syntax.return pkh
         | Error e ->
             failwith
               "Cannot read public key hash: %a"
               Error_monad.pp_print_trace
               e))

let batch_size_arg =
  let open Tezos_clic in
  default_arg
    ~long:"batch-size"
    ~placeholder:"integer"
    ~doc:
      "Maximum number of operations that can be put into a single batch (250 \
       by default)"
    ~default:"250"
    (parameter (fun _ s ->
         match int_of_string_opt s with
         | Some i when i > 0 -> Lwt_result_syntax.return i
         | Some _ -> failwith "Integer must be positive."
         | None -> failwith "Cannot read integer"))

let batches_per_block_arg =
  let open Tezos_clic in
  default_arg
    ~long:"batches-per-block"
    ~placeholder:"integer"
    ~doc:
      "Maximum number of batches that can be put into a single block (100 by \
       default)"
    ~default:"100"
    (parameter (fun _ s ->
         match int_of_string_opt s with
         | Some i when i > 0 -> Lwt_result_syntax.return i
         | Some _ -> failwith "Integer must be positive."
         | None -> failwith "Cannot read integer"))

let initial_amount_arg =
  let open Tezos_clic in
  default_arg
    ~long:"initial-amount"
    ~placeholder:"integer"
    ~doc:
      "Number of token, in μtz, that will be funded on each of the accounts to \
       fund (1 by default)"
    ~default:"1_000_000"
    (parameter (fun _ s ->
         match Int64.of_string_opt s with
         | Some i when i > 0L -> (
             try Lwt_result_syntax.return (Tez.of_mutez_exn i)
             with e ->
               failwith "Cannot convert to Tez.t:%s" (Printexc.to_string e))
         | Some _ -> failwith "Integer must be positive."
         | None -> failwith "Cannot read integer"))

(* Monitors the node's head to inject transaction batches. *)
let inject_batched_txs cctxt (source_pkh, source_pk, source_sk)
    ~(starter_batch : Annotated_manager_operation.packed_annotated_list list)
    ~fee ~gas_limit ~storage_limit ~fee_parameter batches_per_block =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let* heads_stream, stop = Shell_services.Monitor.heads cctxt chain in
  let rec aux stream
      (sources_ops : Annotated_manager_operation.packed_annotated_list list) =
    let*! v = Lwt_stream.get stream in
    match v with
    | Some (_block_hash, _) -> (
        match sources_ops with
        | [] ->
            stop () ;
            return []
        | sources_ops ->
            let now, next = List.rev_split_n batches_per_block sources_ops in
            let* () =
              List.iter_ep
                (fun batch ->
                  let (Annotated_manager_operation.Manager_list contents) =
                    batch
                  in
                  let* _results =
                    Injection.inject_manager_operation
                      cctxt
                      ~chain:cctxt#chain
                      ~block:cctxt#block
                      ?confirmations:cctxt#confirmations
                      ~dry_run:false
                      ~verbose_signing:false
                      ~simulation:false
                      ~force:false
                      ~source:source_pkh
                      ~fee:(Limit.of_option fee)
                      ~gas_limit:(Limit.of_option gas_limit)
                      ~storage_limit:(Limit.of_option storage_limit)
                      ~src_pk:source_pk
                      ~src_sk:source_sk
                      ~replace_by_fees:false
                      ~fee_parameter
                      contents
                  in
                  return_unit)
                now
            in
            aux stream next)
    | None ->
        let*! () = Lwt_unix.sleep 0.5 in
        aux stream sources_ops
  in
  let* _ = aux heads_stream starter_batch in
  return_unit

(* Monitors the node's head to inject reveal batches. *)
let inject_batched_reveals cctxt
    ~(starter_reveals :
       (source_with_uri * Annotated_manager_operation.packed) list) ~fee
    ~gas_limit ~storage_limit ~fee_parameter batches_per_block =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let* heads_stream, stop = Shell_services.Monitor.heads cctxt chain in
  let rec aux stream
      (sources_ops :
        (source_with_uri * Annotated_manager_operation.packed) list) =
    let*! v = Lwt_stream.get stream in
    match v with
    | Some (_block_hash, _) -> (
        match sources_ops with
        | [] ->
            stop () ;
            return []
        | sources_ops ->
            let now, next = List.rev_split_n batches_per_block sources_ops in
            let* () =
              List.iter_ep
                (fun (source, op) ->
                  let (Annotated_manager_operation.Manager_list contents) =
                    Annotated_manager_operation.manager_of_list [op]
                  in
                  let* _ =
                    Injection.inject_manager_operation
                      cctxt
                      ~chain:cctxt#chain
                      ~block:cctxt#block
                      ?confirmations:cctxt#confirmations
                      ~dry_run:false
                      ~verbose_signing:false
                      ~simulation:false
                      ~force:false
                      ~source:source.pkh
                      ~fee:(Limit.of_option fee)
                      ~gas_limit:(Limit.of_option gas_limit)
                      ~storage_limit:(Limit.of_option storage_limit)
                      ~src_pk:source.pk
                      ~src_sk:source.sk_uri
                      ~replace_by_fees:false
                      ~fee_parameter
                      contents
                  in
                  return_unit)
                now
            in
            aux stream next)
    | None ->
        let*! () = Lwt_unix.sleep 0.5 in
        aux stream sources_ops
  in
  let* _ = aux heads_stream starter_reveals in
  return_unit

(* Monitors the node's head to inject transaction batches. *)
let inject_funding_batches cctxt
    ~(funding_batches :
       (source_with_uri * Annotated_manager_operation.packed_annotated_list)
       list
       list) ~fee ~gas_limit ~storage_limit ~fee_parameter batches_per_block =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let* heads_stream, stop = Shell_services.Monitor.heads cctxt chain in
  let rec aux stream
      (sources_ops :
        (source_with_uri * Annotated_manager_operation.packed_annotated_list)
        list
        list) =
    let*! v = Lwt_stream.get stream in
    match v with
    | Some (_block_hash, _) -> (
        match sources_ops with
        | [] ->
            stop () ;
            return []
        | block_ops :: tl ->
            let now, next = List.rev_split_n batches_per_block block_ops in
            let* () =
              List.iter_ep
                (fun (source, batch) ->
                  let (Annotated_manager_operation.Manager_list contents) =
                    batch
                  in
                  let* _results =
                    Injection.inject_manager_operation
                      cctxt
                      ~chain:cctxt#chain
                      ~block:cctxt#block
                      ?confirmations:cctxt#confirmations
                      ~dry_run:false
                      ~verbose_signing:false
                      ~simulation:false
                      ~force:false
                      ~source:source.pkh
                      ~fee:(Limit.of_option fee)
                      ~gas_limit:(Limit.of_option gas_limit)
                      ~storage_limit:(Limit.of_option storage_limit)
                      ~src_pk:source.pk
                      ~src_sk:source.sk_uri
                      ~replace_by_fees:false
                      ~fee_parameter
                      contents
                  in
                  return_unit)
                now
            in
            if next = [] then aux stream tl else aux stream (next :: tl))
    | None ->
        let*! () = Lwt_unix.sleep 0.5 in
        aux stream sources_ops
  in
  let* _ = aux heads_stream funding_batches in
  return_unit

(* This command aims to fund accounts to be used in pair with the
   stresstest transfer command. To do so, it will proceed in the
   following steps:
   - takes all the identities found in a given wallet,
   - chooses [batch_size] identities as starters ,
   - funds the starters with some funds (using source account),
   - reveal the starters (using source account),
   - makes and injects batches so that the starters uses their funds to
     fund the [nb_identities - nb_starters] remaining accounts.

   These steps allows to minimize the number of
   transfers/operations/blocks to fund many accounts.

   As parameters, it is possible to chose:
   - batch_size: number of operations into a single batch,
   - batches_per_block: number of batches/operations per block,
   - initial_amount: number of token distributed to each accounts.
   It also allows to define additional parameters, such as fee, gas
   and storage limit.
*)
let fund_accounts_from_source : Protocol_client_context.full Tezos_clic.command
    =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Funds all the given accounts"
    (args7
       batch_size_arg
       batches_per_block_arg
       initial_amount_arg
       Client_proto_args.default_fee_arg
       Client_proto_args.default_gas_limit_arg
       Client_proto_args.default_storage_limit_arg
       Client_proto_args.fee_parameter_args)
    (prefixes ["stresstest"; "fund"; "accounts"; "from"]
    @@ source_key_arg @@ stop)
    (fun ( batch_size,
           batches_per_block,
           initial_amount,
           fee,
           gas_limit,
           storage_limit,
           fee_parameter )
         source_pkh
         (cctxt : Protocol_client_context.full)
       ->
      let open Lwt_result_syntax in
      let* source_pk, source_sk =
        let* _, src_pk, src_sk = Client_keys_v0.get_key cctxt source_pkh in
        return (src_pk, src_sk)
      in
      let*! () = log Notice (fun () -> cctxt#message "@.") in
      let*! () =
        log Notice (fun () ->
            cctxt#message
              "Starting funding from %a with parameters:@.- batch_size %d@.- \
               batches_per_block %d@.- initial_amount %a@."
              Tezos_crypto.Signature.V0.Public_key_hash.pp
              source_pkh
              batch_size
              batches_per_block
              Tez.pp
              initial_amount)
      in
      (* All generated sources *)
      let* new_sources = load_wallet cctxt ~source_pkh in
      (* Starter sources used to initiate the "exponential"
         funding. *)
      let nb_starters =
        let l = List.length new_sources in
        (l / batch_size) + if l mod batch_size = 0 then 0 else 1
      in
      let starter_sources, empty_accounts =
        List.rev_split_n nb_starters new_sources
      in
      let*! () =
        log Notice (fun () ->
            cctxt#message
              "Funding %d accounts using %d starters@."
              (List.length new_sources)
              nb_starters)
      in
      (* Initial amount that is sent to starters to allow them to fund
         other accounts. This is an over approximation. *)
      let starter_initial_amount =
        (* over approximation of the max number of operation that a
           starter may inject. We add one to leave the starter account
           with it's own initial amount. *)
        let max_nb_transfers = batch_size + 1 in
        (* Fees are: reveal + max_nb_transfers * manager_fees
                   = reveal + max_nb_transfers * (storage_fees + tx fees)
                   = 0.001tz + max_nb_transfers * (0.06425tz + 0.001tz)
                   =~ max_nb_transfers * 0.1 tz *)
        let fees_approx = Tez.of_mutez_exn 100_000L in
        let amount =
          WithExceptions.Result.get_ok
            ~loc:__LOC__
            Tez.(initial_amount +? fees_approx)
        in
        Tez.mul_exn amount max_nb_transfers
      in
      let*! () =
        log Notice (fun () ->
            cctxt#message
              "Sending %a tz to starter accounts@."
              Tez.pp
              starter_initial_amount)
      in
      let* source_balance =
        Alpha_services.Contract.balance
          cctxt
          (cctxt#chain, cctxt#block)
          (Contract.Implicit source_pkh)
      in
      let* () =
        let req_balance = Tez.mul_exn starter_initial_amount nb_starters in
        if Tez.(source_balance < req_balance) then
          failwith
            "Not enough funds to init starter accounts: %a are needed, only %a \
             is available on %a@."
            Tez.pp
            source_balance
            Tez.pp
            req_balance
            Tezos_crypto.Signature.V0.Public_key_hash.pp
            source_pkh
        else
          let*! () =
            log Notice (fun () ->
                cctxt#message
                  "Transfering %a tz from %a (out of %a)@."
                  Tez.pp
                  req_balance
                  Tezos_crypto.Signature.V0.Public_key_hash.pp
                  source_pkh
                  Tez.pp
                  source_balance)
          in
          return_unit
      in
      let*! () =
        log Notice (fun () ->
            cctxt#message "Generating starter transactions and reveals@.")
      in
      let starter_batch, starter_reveals =
        generate_starter_ops
          ~sources:starter_sources
          ~amount:starter_initial_amount
          ~batch_size
      in
      (* Inject generated batches and reveals for the starters. *)
      let*! () =
        log Notice (fun () ->
            cctxt#message "Injecting starter transfer batches@.")
      in
      let* () =
        inject_batched_txs
          cctxt
          (source_pkh, source_pk, source_sk)
          ~starter_batch
          ~fee
          ~gas_limit
          ~storage_limit
          ~fee_parameter
          batches_per_block
      in
      let*! () =
        log Notice (fun () ->
            cctxt#message "Injecting starter reveal batches@.")
      in
      let* () =
        inject_batched_reveals
          cctxt
          ~starter_reveals
          ~fee
          ~gas_limit
          ~storage_limit
          ~fee_parameter
          batches_per_block
      in
      let*! () =
        log Notice (fun () -> cctxt#message "Generating funding batches@.")
      in
      let* funding_batches =
        generate_account_funding_batches
          starter_sources
          empty_accounts
          ~batch_size
          ~amount:initial_amount
      in
      let*! () =
        log Notice (fun () -> cctxt#message "Injecting funding batches@.")
      in
      let* () =
        inject_funding_batches
          cctxt
          ~funding_batches
          ~fee
          ~gas_limit
          ~storage_limit
          ~fee_parameter
          batches_per_block
      in
      let*! () = log Notice (fun () -> cctxt#message "Done.@.") in
      return_unit)

let commands =
  [
    generate_random_transactions;
    estimate_transaction_costs;
    Smart_contracts.originate_command;
    fund_accounts_from_source;
  ]

let commands network () =
  (* Stresstest should not be used on mainnet. If the client is running with
     the yes-crypto activated, operations won't be considered valid and should
     not endanger the network nor the users funds. *)
  match Sys.getenv_opt Tezos_crypto.Helpers.yes_crypto_environment_variable with
  | Some _ -> commands
  | None -> (
      match network with
      | Some `Mainnet -> []
      | Some `Testnet | None -> commands)
