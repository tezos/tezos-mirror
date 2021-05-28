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

type transfer_strategy =
  | Fixed_amount of {mutez : Tez.t}  (** Amount to transfer *)
  | Evaporation of {fraction : float}
      (** Maximum fraction of current wealth to transfer.
          Minimum amount is 1 mutez regardless of total wealth. *)

type parameters = {
  seed : int;
  fresh_probability : float;
      (** Per-transfer probability that the destination will be fresh *)
  tps : float;  (** Transaction per seconds target *)
  strategy : transfer_strategy;
  fee_mutez : Tez.t;  (** fees for each transfer, in mutez *)
  gas_limit : Gas.Arith.integral;  (** gas limit per operation *)
  storage_limit : Z.t;  (** storage limit per operation *)
  account_creation_storage : Z.t;
      (** upper bound on bytes consumed when creating a tz1 account *)
  total_transfers : int option;
      (** total number of transfers to perform; unbounded if None *)
}

type source = {
  pkh : public_key_hash;
  pk : public_key;
  sk : Signature.secret_key;
}

type transfer = {
  src : source;
  dst : public_key_hash;
  fee : Tez.t;
  amount : Tez.t;
  counter : Z.t option;
  fresh_dst : bool;
}

type state = {
  counters : (Block_hash.t * Z.t) Signature.Public_key_hash.Table.t;
  mutable pool : source list;
  mutable pool_size : int;
}

let verbose = ref false

let default_parameters =
  {
    seed = 0x533D;
    fresh_probability = 0.001;
    tps = 5.0;
    strategy = Fixed_amount {mutez = Tez.one};
    fee_mutez = Tez.of_mutez_exn 2_000L;
    gas_limit = Gas.Arith.integral_of_int_exn 1_600;
    (* [gas_limit] corresponds to a slight overapproximation of the
       gas needed to inject an operation. It was obtained by simulating
       the operation using the client. *)
    storage_limit = Z.zero;
    account_creation_storage = Z.of_int 300;
    (* [account_creation_storage] corresponds to a slight overapproximation
       of the storage consumed when allocating a new implicit account.
       It was obtained by simulating the operation using the client. *)
    total_transfers = None;
  }

let source_encoding =
  let open Data_encoding in
  conv
    (fun {pkh; pk; sk} -> (pkh, pk, sk))
    (fun (pkh, pk, sk) -> {pkh; pk; sk})
    (obj3
       (req "pkh" Signature.Public_key_hash.encoding)
       (req "pk" Signature.Public_key.encoding)
       (req "sk" Signature.Secret_key.encoding))

let source_list_encoding =
  let open Data_encoding in
  list source_encoding

let parse_strategy s =
  match String.split ~limit:1 ':' s with
  | ["fixed"; parameter] -> (
    match int_of_string parameter with
    | exception _ ->
        Error "invalid integer literal"
    | mutez when mutez <= 0 ->
        Error "negative amount"
    | mutez -> (
      match Tez.of_mutez (Int64.of_int mutez) with
      | None ->
          Error "invalid mutez"
      | Some mutez ->
          Ok (Fixed_amount {mutez}) ) )
  | ["evaporation"; parameter] -> (
    match float_of_string parameter with
    | exception _ ->
        Error "invalid float literal"
    | fraction when fraction < 0.0 || fraction > 1.0 ->
        Error "invalid evaporation rate"
    | fraction ->
        Ok (Evaporation {fraction}) )
  | _ ->
      Error "invalid argument"

let sample_from_pool state rng_state =
  let idx = Random.State.int rng_state state.pool_size in
  match List.nth state.pool idx with None -> assert false | Some src -> src

let random_seed rng_state =
  Bytes.init 32 (fun _ -> Char.chr (Random.State.int rng_state 256))

let generate_fresh pool rng_state =
  let seed = random_seed rng_state in
  let (pkh, pk, sk) = Signature.generate_key ~seed () in
  let fresh = {pkh; pk; sk} in
  pool.pool <- fresh :: pool.pool ;
  pool.pool_size <- pool.pool_size + 1 ;
  fresh

(* We perform rejection sampling of valid sources.
   We could maintain a local cache of existing contracts with sufficient balance.
*)
let rec sample_transfer (cctxt : Protocol_client_context.full)
    (parameters : parameters) (state : state) rng_state =
  let src = sample_from_pool state rng_state in
  Alpha_services.Contract.balance
    cctxt
    (cctxt#chain, cctxt#block)
    (Contract.implicit_contract src.pkh)
  >>=? fun tez ->
  if Tez.(tez = zero) then
    (* Sampled source has zero balance: the transfer that created that
       address was not included yet. Retry *)
    sample_transfer cctxt parameters state rng_state
  else
    let fresh =
      Random.State.float rng_state 1.0 < parameters.fresh_probability
    in
    let dest =
      if fresh then generate_fresh state rng_state
      else sample_from_pool state rng_state
    in
    let amount =
      match parameters.strategy with
      | Fixed_amount {mutez} ->
          mutez
      | Evaporation {fraction} ->
          let mutez = Int64.to_float (Tez.to_mutez tez) in
          let max_fraction = Int64.of_float (mutez *. fraction) in
          let amount =
            if max_fraction = 0L then 1L
            else max 1L (Random.State.int64 rng_state max_fraction)
          in
          Tez.of_mutez_exn amount
    in
    let fee = parameters.fee_mutez in
    return
      {src; dst = dest.pkh; fee; amount; counter = None; fresh_dst = fresh}

let inject_contents (cctxt : Protocol_client_context.full) chain branch sk
    contents =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      ({branch}, Contents_list contents)
  in
  let signature =
    Some (Signature.sign ~watermark:Signature.Generic_operation sk bytes)
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
    {src; dst; fee; amount; counter; fresh_dst} =
  let source = src.pkh in
  let gas_limit = parameters.gas_limit in
  let storage_limit =
    if fresh_dst then
      Z.add parameters.account_creation_storage parameters.storage_limit
    else parameters.storage_limit
  in
  let operation =
    let parameters =
      let open Tezos_micheline in
      Script.lazy_expr
      @@ Micheline.strip_locations
           (Prim (0, Michelson_v1_primitives.D_Unit, [], []))
    in
    let entrypoint = "default" in
    let destination = Contract.implicit_contract dst in
    Transaction {amount; parameters; entrypoint; destination}
  in
  match counter with
  | None ->
      assert false
  | Some counter ->
      Manager_operation
        {source; fee; counter; operation; gas_limit; storage_limit}

let cost_of_manager_operation = Gas.Arith.integral_of_int_exn 1_000

let inject_transfer (cctxt : Protocol_client_context.full) parameters state
    chain block transfer =
  Alpha_services.Contract.counter cctxt (chain, block) transfer.src.pkh
  >>=? fun pcounter ->
  Shell_services.Blocks.hash cctxt ~chain ~block ()
  >>=? fun branch ->
  let freshest_counter =
    match
      Signature.Public_key_hash.Table.find state.counters transfer.src.pkh
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
  Alpha_services.Contract.manager_key cctxt (chain, block) transfer.src.pkh
  >>=? function
  | None -> (
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
      Signature.Public_key_hash.Table.remove state.counters transfer.src.pkh ;
      Signature.Public_key_hash.Table.add
        state.counters
        transfer.src.pkh
        (branch, transf_counter) ;
      ( if !verbose then
        cctxt#message
          "injecting reveal+transfer from %a (counters=%a,%a) to %a"
          Signature.Public_key_hash.pp
          transfer.src.pkh
          Z.pp_print
          reveal_counter
          Z.pp_print
          transf_counter
          Signature.Public_key_hash.pp
          transfer.dst
      else Lwt.return_unit )
      >>= fun () ->
      (* NB: regardless of our best efforts to keep track of counters, injection can fail with
         "counter in the future" if a block switch happens in between the moment we
         get the branch and the moment we inject, and the new block does not include
         all the operations we injected. *)
      inject_contents cctxt chain branch transfer.src.sk list
      >>= function Error _ | Ok _ -> return_unit )
  | Some _ -> (
      let transf_counter = Z.succ freshest_counter in
      let manager_op =
        manager_op_of_transfer
          parameters
          {transfer with counter = Some transf_counter}
      in
      let list = Single manager_op in
      Signature.Public_key_hash.Table.remove state.counters transfer.src.pkh ;
      Signature.Public_key_hash.Table.add
        state.counters
        transfer.src.pkh
        (branch, transf_counter) ;
      ( if !verbose then
        cctxt#message
          "injecting transfer from %a (counter=%a) to %a"
          Signature.Public_key_hash.pp
          transfer.src.pkh
          Z.pp_print
          transf_counter
          Signature.Public_key_hash.pp
          transfer.dst
      else Lwt.return_unit )
      >>= fun () ->
      (* See comment above. *)
      inject_contents cctxt chain branch transfer.src.sk list
      >>= function Error _ | Ok _ -> return_unit )

let launch (cctxt : Protocol_client_context.full) (parameters : parameters)
    state save_pool_callback =
  let injected = ref 0 in
  let dt = 1. /. parameters.tps in
  let terminated () =
    match parameters.total_transfers with
    | None ->
        false
    | Some bound ->
        bound <= !injected
  in
  let rng_state = Random.State.make [|parameters.seed|] in
  let rec loop () =
    if terminated () then save_pool_callback () >|= ok
    else
      let start = Mtime_clock.elapsed () in
      sample_transfer cctxt parameters state rng_state
      >>=? fun transfer ->
      inject_transfer cctxt parameters state cctxt#chain cctxt#block transfer
      >>=? fun () ->
      incr injected ;
      let stop = Mtime_clock.elapsed () in
      let elapsed = Mtime.Span.(to_s stop -. to_s start) in
      let remaining = dt -. elapsed in
      ( if remaining <= 0.0 then
        cctxt#warning
          "warning: tps target could not be reached, consider using a lower \
           value for --tps"
      else Lwt_unix.sleep remaining )
      >>= loop
  in
  loop ()

let group =
  Clic.{name = "stresstest"; title = "Commands for stress-testing the network"}

type pool_source =
  | From_string of {json : Ezjsonm.value}
  | From_file of {path : string; json : Ezjsonm.value}

let json_of_pool_source = function
  | From_string {json} | From_file {json; _} ->
      json

let json_file_or_text_parameter =
  Clic.parameter (fun _ p ->
      match String.split ~limit:1 ':' p with
      | ["text"; text] ->
          return (From_string {json = Ezjsonm.from_string text})
      | ["file"; path] ->
          Lwt_utils_unix.Json.read_file path
          >|=? fun json -> From_file {path; json}
      | _ -> (
          if Sys.file_exists p then
            Lwt_utils_unix.Json.read_file p
            >|=? fun json -> From_file {path = p; json}
          else
            try return (From_string {json = Ezjsonm.from_string p})
            with Ezjsonm.Parse_error _ ->
              failwith "Neither an existing file nor valid JSON: '%s'" p ))

let seed_arg =
  let open Clic in
  arg
    ~long:"seed"
    ~placeholder:"int"
    ~doc:"random seed"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match int_of_string s with
         | exception _ ->
             cctxt#error
               "While parsing --seed: could not convert argument to int"
         | i ->
             return i))

let tps_arg =
  let open Clic in
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
         | f ->
             return f))

let fresh_probability_arg =
  let open Clic in
  arg
    ~long:"fresh-probability"
    ~placeholder:"float in [0;1]"
    ~doc:"probability for a destination to be a fresh account"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match float_of_string s with
         | exception _ ->
             cctxt#error
               "While parsing --fresh-probability: could not convert argument \
                to float"
         | f when f < 0.0 || f > 1.0 ->
             cctxt#error "While parsing --fresh-probability: invalid argument"
         | f ->
             return f))

let strategy_arg =
  let open Clic in
  arg
    ~long:"strategy"
    ~placeholder:"fixed:mutez | evaporation:[0;1]"
    ~doc:"wealth redistribution strategy"
    (parameter (fun (cctxt : Protocol_client_context.full) s ->
         match parse_strategy s with
         | Error msg ->
             cctxt#error "While parsing --strategy: %s" msg
         | Ok strategy ->
             return strategy))

let gas_limit_arg =
  let open Clic in
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
         default_parameters.gas_limit)
    gas_limit_kind

let storage_limit_arg =
  let open Clic in
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
  let open Clic in
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
         | i ->
             return i))

let verbose_arg =
  Clic.switch
    ~long:"verbose"
    ~doc:"Display detailed logs of the injected operations"
    ()

let set_option opt f x = Option.fold ~none:x ~some:(f x) opt

let save_pool_callback (cctxt : Protocol_client_context.full) pool_source state
    =
  let json = Data_encoding.Json.construct source_list_encoding state.pool in
  let catch_write_error = function
    | Error _ ->
        cctxt#message "could not write back json file"
    | Ok () ->
        Lwt.return_unit
  in
  match pool_source with
  | From_string _ ->
      (* If the initial pool was given directly as json, save pool to
         a temp file. *)
      let path = Filename.temp_file "client-stresstest-pool" "json" in
      cctxt#message "writing back address pool in file %s" path
      >>= fun () ->
      Lwt_utils_unix.Json.write_file path json >>= catch_write_error
  | From_file {path; _} ->
      (* If the pool specification was a json file, save pool to
         the same file. *)
      cctxt#message "writing back address pool in file %s" path
      >>= fun () ->
      Lwt_utils_unix.Json.write_file path json >>= catch_write_error

let generate_random_transactions =
  let open Clic in
  command
    ~group
    ~desc:"Generate random transactions"
    (args9
       seed_arg
       tps_arg
       fresh_probability_arg
       strategy_arg
       Client_proto_args.fee_arg
       gas_limit_arg
       storage_limit_arg
       transfers_arg
       verbose_arg)
    ( prefixes ["stresstest"; "transfer"; "using"]
    @@ param
         ~name:"sources.json"
         ~desc:
           "List of accounts from which to perform transfers in JSON format. \
            The input JSON must be an array of objects of the form \
            '[{\"pkh\":pkh;\"pk\":pk;\"sk\":sk}; ...]' with the pkh, pk and \
            sk encoded in B58 form."
         json_file_or_text_parameter
    @@ stop )
    (fun ( seed,
           tps,
           freshp,
           strat,
           fee,
           gas_limit,
           storage_limit,
           transfers,
           verbose_flag )
         sources_json
         (cctxt : Protocol_client_context.full) ->
      verbose := verbose_flag ;
      let parameters =
        default_parameters
        |> set_option seed (fun parameter seed -> {parameter with seed})
        |> set_option tps (fun parameter tps -> {parameter with tps})
        |> set_option freshp (fun parameter fresh_probability ->
               {parameter with fresh_probability})
        |> set_option strat (fun parameter strategy ->
               {parameter with strategy})
        |> set_option fee (fun parameter fee_mutez ->
               {parameter with fee_mutez})
        |> set_option gas_limit (fun parameter gas_limit ->
               {parameter with gas_limit})
        |> set_option storage_limit (fun parameter storage_limit ->
               {parameter with storage_limit})
        |> set_option transfers (fun parameter transfers ->
               {parameter with total_transfers = Some transfers})
      in
      match
        Data_encoding.Json.destruct
          source_list_encoding
          (json_of_pool_source sources_json)
      with
      | exception _ ->
          cctxt#error "Could not decode list of sources"
      | sources ->
          let counters = Signature.Public_key_hash.Table.create 1023 in
          let state =
            {counters; pool = sources; pool_size = List.length sources}
          in
          let save_pool () = save_pool_callback cctxt sources_json state in
          (* Register a callback for saving the pool when the tool is interrupted
             through ctrl-c *)
          ignore
            (Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _retcode ->
                 save_pool ())) ;
          launch cctxt parameters state save_pool)

let commands network () =
  match network with
  | Some `Mainnet ->
      []
  | Some `Testnet | None ->
      [generate_random_transactions]
