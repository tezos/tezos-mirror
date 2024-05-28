(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
  single_op_per_pkh_per_block : bool;
      (** if true, a single operation will be injected by pkh by block to
          improve the chance for the injected operations to be included in the
          next block *)
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
  current_head_on_start : Block_hash.t;
  counters :
    (Block_hash.t * Z.t) Tezos_crypto.Signature.V0.Public_key_hash.Table.t;
  mutable pool : source_origin list;
  mutable pool_size : int;
      (** [Some l] if [single_op_per_pkh_per_block] is true *)
  mutable shuffled_pool : source list option;
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

let verbose = ref false

let debug = ref false

let debug_msg msg = if !debug then msg () else Lwt.return_unit

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
    single_op_per_pkh_per_block = false;
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
  | Implicit x -> Contract.implicit_contract x
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
    (Client_keys_v0.alias_keys cctxt alias >>= function
     | Error _ | Ok None -> warning "Alias \"%s\" not found in the wallet" alias
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
               {source = {pkh; pk; sk}; origin = Wallet_alias alias}))
    >>= function
    | None -> warning "Source given as alias \"%s\" ignored" alias
    | key -> Lwt.return key
  in
  let key_from_wallet pkh =
    let warning msg pkh =
      cctxt#warning msg Tezos_crypto.Signature.V0.Public_key_hash.pp pkh
      >>= fun () -> Lwt.return_none
    in
    (Client_keys_v0.get_key cctxt pkh >>= function
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
             Lwt.return_some {source = {pkh; pk; sk}; origin = Wallet_pkh}))
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
    transfer, and its source only when [state.shuffled_pool] is [None]
    meaning that [--single-op-per-pkh-per-block] is not set. *)
let sample_any_source_from_pool state rng =
  let idx = Random.State.int rng state.pool_size in
  match List.nth state.pool idx with
  | None -> assert false
  | Some src_org -> Lwt.return src_org.source

(** Generates the source of a transfer. If [state.shuffled_pool] has a
    value (meaning that [--single-op-per-pkh-per-block] is active) then
    it is sampled from there, otherwise from [state.pool]. *)
let rec sample_source_from_pool state rng (cctxt : Protocol_client_context.full)
    =
  match state.shuffled_pool with
  | None -> sample_any_source_from_pool state rng
  | Some (source :: l) ->
      state.shuffled_pool <- Some l ;
      debug_msg (fun () ->
          cctxt#message
            "sample_transfer: %d unused sources for the block next to %a"
            (List.length l)
            Block_hash.pp
            state.last_block)
      >>= fun () -> Lwt.return source
  | Some [] ->
      cctxt#message
        "all available sources have been used for block next to %a"
        Block_hash.pp
        state.last_block
      >>= fun () ->
      Lwt_condition.wait state.new_block_condition >>= fun () ->
      sample_source_from_pool state rng cctxt

let random_seed rng =
  Bytes.init 32 (fun _ -> Char.chr (Random.State.int rng 256))

let generate_fresh_source pool rng =
  let seed = random_seed rng in
  let pkh, pk, sk = Tezos_crypto.Signature.V0.generate_key ~seed () in
  let fresh = {source = {pkh; pk; sk}; origin = Explicit} in
  pool.pool <- fresh :: pool.pool ;
  pool.pool_size <- pool.pool_size + 1 ;
  fresh.source

(* [on_new_head cctxt f] calls [f head] each time there is a new head
   received by the streamed RPC /monitor/heads/main *)
let on_new_head (cctxt : Protocol_client_context.full) f =
  Shell_services.Monitor.heads cctxt `Main >>=? fun (heads_stream, stopper) ->
  Lwt_stream.iter_s f heads_stream >>= fun () ->
  stopper () ;
  return_unit

(* We perform rejection sampling of valid sources.
   We could maintain a local cache of existing contracts with sufficient balance. *)
let rec sample_transfer (cctxt : Protocol_client_context.full) chain block
    (parameters : parameters) (state : state) rng =
  sample_source_from_pool state rng cctxt >>= fun src ->
  Alpha_services.Contract.balance
    cctxt
    (chain, block)
    (Contract.implicit_contract src.pkh)
  >>=? fun tez ->
  if Tez.(tez = zero) then
    debug_msg (fun () ->
        cctxt#message
          "sample_transfer: invalid balance %a"
          Tezos_crypto.Signature.V0.Public_key_hash.pp
          src.pkh)
    >>= fun () ->
    (* Sampled source has zero balance: the transfer that created that
             address was not included yet. Retry *)
    sample_transfer cctxt chain block parameters state rng
  else
    let fresh = Random.State.float rng 1.0 < parameters.fresh_probability in
    (match
       Smart_contracts.select
         parameters.smart_contracts
         (Random.State.float rng 1.0)
     with
    | None ->
        (if fresh then Lwt.return (generate_fresh_source state rng)
        else sample_any_source_from_pool state rng)
        >|= fun dest ->
        Ok
          ( Implicit dest.pkh,
            parameters.regular_transfer_fee,
            parameters.regular_transfer_gas_limit )
    | Some invocation_parameters ->
        return
          ( Originated invocation_parameters,
            invocation_parameters.fee,
            invocation_parameters.gas_limit ))
    >>=? fun (dst, fee, gas_limit) ->
    let amount =
      match parameters.strategy with
      | Fixed_amount {mutez} -> mutez
      | Evaporation {fraction} ->
          let mutez = Int64.to_float (Tez.to_mutez tez) in
          let max_fraction = Int64.of_float (mutez *. fraction) in
          let amount =
            if max_fraction = 0L then 1L
            else max 1L (Random.State.int64 rng max_fraction)
          in
          Tez.of_mutez_exn amount
    in
    return {src; dst; fee; gas_limit; amount; counter = None; fresh_dst = fresh}

let inject_contents (cctxt : Protocol_client_context.full) chain branch sk
    contents =
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
  Shell_services.Injection.operation cctxt ~chain bytes

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
    let destination = Destination.Contract (destination_to_contract dst) in
    Transaction {amount; parameters; entrypoint; destination}
  in
  match counter with
  | None -> assert false
  | Some counter ->
      Manager_operation
        {source; fee; counter; operation; gas_limit; storage_limit}

let cost_of_manager_operation = Gas.Arith.integral_of_int_exn 1_000

let inject_transfer (cctxt : Protocol_client_context.full) parameters state rng
    chain block transfer =
  Alpha_services.Contract.counter cctxt (chain, block) transfer.src.pkh
  >>=? fun pcounter ->
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun branch ->
  (* If there is a new block refresh the fresh_pool *)
  if not (Block_hash.equal branch state.last_block) then (
    state.last_block <- branch ;
    (* Because of how Tenderbake works the target block should stay 2
       blocks in the past because this guarantees that we are targeting a
       block that is decided. *)
    Shell_services.Blocks.hash cctxt ~chain ~block:(`Head 2) ()
    >>=? fun target_block ->
    state.target_block <- target_block ;
    if Option.is_some state.shuffled_pool then
      state.shuffled_pool <-
        Some
          (List.shuffle
             ~rng
             (List.map (fun src_org -> src_org.source) state.pool)) ;
    return ())
  else
    return () >>=? fun () ->
    let freshest_counter =
      match
        Tezos_crypto.Signature.V0.Public_key_hash.Table.find
          state.counters
          transfer.src.pkh
      with
      | None ->
          (* This is the first operation we inject for this pkh: the counter given
             by the RPC _must_ be the freshest one. *)
          pcounter
      | Some (previous_branch, previous_counter) ->
          if Block_hash.equal branch previous_branch then
            (* We already injected an operation on top of this block: the one stored
               locally is the freshest one. *)
            previous_counter
          else
            (* It seems the block changed since we last injected an operation:
               this invalidates the previously stored counter. We return the counter
               given by the RPC. *)
            pcounter
    in
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
      Alpha_services.Contract.manager_key cctxt (chain, block) transfer.src.pkh
      >>=? fun pk_opt -> return (Option.is_some pk_opt)))
    >>=? fun already_revealed ->
    (if not already_revealed then (
     let reveal_counter = Z.succ freshest_counter in
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
     Tezos_crypto.Signature.V0.Public_key_hash.Table.remove
       state.counters
       transfer.src.pkh ;
     Tezos_crypto.Signature.V0.Public_key_hash.Table.add
       state.counters
       transfer.src.pkh
       (branch, transf_counter) ;
     (if !verbose then
      cctxt#message
        "injecting reveal+transfer from %a (counters=%a,%a) to %a"
        Tezos_crypto.Signature.V0.Public_key_hash.pp
        transfer.src.pkh
        Z.pp_print
        reveal_counter
        Z.pp_print
        transf_counter
        Contract.pp
        (destination_to_contract transfer.dst)
     else Lwt.return_unit)
     >>= fun () ->
     (* NB: regardless of our best efforts to keep track of counters, injection can fail with
        "counter in the future" if a block switch happens in between the moment we
        get the branch and the moment we inject, and the new block does not include
        all the operations we injected. *)
     inject_contents cctxt chain state.target_block transfer.src.sk list)
    else
      let transf_counter = Z.succ freshest_counter in
      let manager_op =
        manager_op_of_transfer
          parameters
          {transfer with counter = Some transf_counter}
      in
      let list = Single manager_op in
      Tezos_crypto.Signature.V0.Public_key_hash.Table.remove
        state.counters
        transfer.src.pkh ;
      Tezos_crypto.Signature.V0.Public_key_hash.Table.add
        state.counters
        transfer.src.pkh
        (branch, transf_counter) ;
      (if !verbose then
       cctxt#message
         "injecting transfer from %a (counter=%a) to %a"
         Tezos_crypto.Signature.V0.Public_key_hash.pp
         transfer.src.pkh
         Z.pp_print
         transf_counter
         Contract.pp
         (destination_to_contract transfer.dst)
      else Lwt.return_unit)
      >>= fun () ->
      (* See comment above. *)
      inject_contents cctxt chain state.target_block transfer.src.sk list)
    >>= function
    | Ok op_hash ->
        debug_msg (fun () ->
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
        Block_hash.Table.replace
          state.injected_operations
          branch
          (op_hash :: ops) ;
        return_unit
    | Error e ->
        debug_msg (fun () ->
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
    debug_msg (fun () ->
        cctxt#message
          "injected : %a\nincluded: %a"
          (Format.pp_print_list Operation_hash.pp)
          injected_ops
          (Format.pp_print_list Operation_hash.pp)
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
    state rng save_pool_callback =
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
      debug_msg (fun () -> cctxt#message "launch.loop: invoke sample_transfer")
      >>= fun () ->
      sample_transfer cctxt cctxt#chain cctxt#block parameters state rng
      >>=? fun transfer ->
      debug_msg (fun () -> cctxt#message "launch.loop: invoke inject_transfer")
      >>= fun () ->
      inject_transfer
        cctxt
        parameters
        state
        rng
        cctxt#chain
        cctxt#block
        transfer
      >>=? fun () ->
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
  (* True, if and only if [single_op_per_pkh_per_block] is true. *)
  if Option.is_some state.shuffled_pool then
    dont_wait
      (fun () ->
        on_new_head cctxt (fun (block, new_block_header) ->
            if not (Block_hash.equal block state.last_block) then (
              state.last_block <- block ;
              state.last_level <- Int32.to_int new_block_header.shell.level ;
              state.shuffled_pool <-
                Some
                  (List.shuffle
                     ~rng
                     (List.map (fun src_org -> src_org.source) state.pool))) ;
            Lwt_condition.broadcast state.new_block_condition () ;
            Lwt.return_unit))
      (fun trace ->
        ignore
          (cctxt#error
             "an error while getting the new head has been returned: %a"
             Error_monad.pp_print_trace
             trace))
      (fun exn ->
        ignore
          (cctxt#error
             "an exception while getting the new head has been raised: %s"
             (Printexc.to_string exn))) ;
  loop ()

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

let single_op_per_pkh_per_block_arg =
  Tezos_clic.switch
    ~long:"single-op-per-pkh-per-block"
    ~doc:
      "ensure that the operations are not rejected by limiting the injection \
       to 1 operation per public_key_hash per block."
    ()

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
    ~doc:"Display detailed logs of the injected operations"
    ()

let debug_arg = Tezos_clic.switch ~long:"debug" ~doc:"Display debug logs" ()

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
    (args13
       seed_arg
       tps_arg
       fresh_probability_arg
       smart_contract_parameters_arg
       strategy_arg
       Client_proto_args.fee_arg
       gas_limit_arg
       storage_limit_arg
       transfers_arg
       single_op_per_pkh_per_block_arg
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
           single_op_per_pkh_per_block,
           level_limit,
           verbose_flag,
           debug_flag )
         sources_json
         (cctxt : Protocol_client_context.full) ->
      verbose := verbose_flag ;
      debug := debug_flag ;
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
        |> fun parameter ->
        {parameter with single_op_per_pkh_per_block}
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
          (if !verbose then cctxt#message "starting to normalize sources"
          else Lwt.return_unit)
          >>= fun () ->
          List.filter_map_s (normalize_source cctxt) sources >>= fun sources ->
          (if !verbose then cctxt#message "all sources have been normalized"
          else Lwt.return_unit)
          >>= fun () ->
          let counters =
            Tezos_crypto.Signature.V0.Public_key_hash.Table.create 1023
          in
          let rng = Random.State.make [|parameters.seed|] in
          Shell_services.Blocks.hash cctxt () >>=? fun current_head_on_start ->
          Shell_services.Blocks.hash cctxt ~block:(`Head 2) ()
          >>=? fun current_target_block ->
          Shell_services.Blocks.Header.shell_header cctxt ()
          >>=? fun header_on_start ->
          let state =
            {
              current_head_on_start;
              counters;
              pool = sources;
              pool_size = List.length sources;
              shuffled_pool =
                (if parameters.single_op_per_pkh_per_block then
                 Some
                   (List.shuffle
                      ~rng
                      (List.map (fun src_org -> src_org.source) sources))
                else None);
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
          launch cctxt parameters state rng save_pool)

let estimate_transaction_cost parameters (cctxt : Protocol_client_context.full)
    : Gas.Arith.integral tzresult Lwt.t =
  let sources_json =
    From_string
      {
        json =
          `A
            [
              `O [("alias", `String "bootstrap1")];
              `O [("alias", `String "bootstrap2")];
            ];
      }
  in
  match
    Data_encoding.Json.destruct
      input_source_list_encoding
      (json_of_pool_source sources_json)
  with
  | exception _ -> cctxt#error "Could not decode list of sources"
  | [] -> cctxt#error "It is required to provide sources"
  | sources0 -> (
      List.filter_map_p (normalize_source cctxt) sources0 >>= fun sources ->
      Protocol_client_context.Alpha_block_services.header cctxt ()
      >>=? fun header_on_start ->
      let current_head_on_start = header_on_start.hash in
      let counters =
        Tezos_crypto.Signature.V0.Public_key_hash.Table.create 1023
      in
      Shell_services.Blocks.hash cctxt ~block:(`Head 2) ()
      >>=? fun current_target_block ->
      let state =
        {
          current_head_on_start;
          counters;
          pool = sources;
          pool_size = List.length sources;
          shuffled_pool = None;
          revealed = Tezos_crypto.Signature.V0.Public_key_hash.Set.empty;
          last_block = current_head_on_start;
          last_level = Int32.to_int header_on_start.shell.level;
          target_block = current_target_block;
          new_block_condition = Lwt_condition.create ();
          injected_operations = Block_hash.Table.create 1023;
        }
      in
      let rng = Random.State.make [|parameters.seed|] in
      let chain = cctxt#chain in
      let block = cctxt#block in
      sample_transfer cctxt chain block parameters state rng
      >>=? fun transfer ->
      Alpha_services.Contract.counter cctxt (chain, block) transfer.src.pkh
      >>=? fun current_counter ->
      let transf_counter = Z.succ current_counter in
      let manager_op =
        manager_op_of_transfer
          {
            parameters with
            regular_transfer_gas_limit =
              Default_parameters.constants_mainnet.hard_gas_limit_per_operation;
          }
          {transfer with counter = Some transf_counter}
      in
      Injection.simulate cctxt ~chain ~block (Single manager_op)
      >>=? fun (_oph, op, result) ->
      match result.contents with
      | Single_result (Manager_operation_result {operation_result; _}) -> (
          match operation_result with
          | Applied
              (Transaction_result
                (Transaction_to_contract_result {consumed_gas; _})) ->
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
                (op.protocol_data.contents, result.contents)))

let estimate_transaction_costs : Protocol_client_context.full Tezos_clic.command
    =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Output gas estimations for transactions that stresstest uses"
    no_options
    (prefixes ["stresstest"; "estimate"; "gas"] @@ stop)
    (fun () cctxt ->
      estimate_transaction_cost default_parameters cctxt >>=? fun regular ->
      Smart_contracts.with_every_known_smart_contract
        cctxt
        (fun smart_contracts ->
          let params = {default_parameters with smart_contracts} in
          estimate_transaction_cost params cctxt)
      >>=? fun smart_contracts ->
      let transaction_costs : transaction_costs = {regular; smart_contracts} in
      let json =
        Data_encoding.Json.construct
          transaction_costs_encoding
          transaction_costs
      in
      Format.printf "%a" Data_encoding.Json.pp json ;
      return_unit)

let commands =
  [
    generate_random_transactions;
    estimate_transaction_costs;
    Smart_contracts.originate_command;
  ]

let commands network () =
  match network with Some `Mainnet -> [] | Some `Testnet | None -> commands
