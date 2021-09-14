(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let genesis =
  {Test_utils.genesis with protocol = Tezos_protocol_genesis.Protocol.hash}

let genesis_parameters =
  ( "sandbox_parameter",
    `O
      [
        ( "genesis_pubkey",
          `String "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2" );
      ] )

let default_genesis_accounts =
  let open Tezos_protocol_alpha.Protocol in
  let open Alpha_context in
  let initial_accounts =
    [
      ( "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh" );
      ( "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
        "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo" );
      ( "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV",
        "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ" );
      ( "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv",
        "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU",
        "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3" );
      ( "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n",
        "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm" );
    ]
  in
  let default_amount = Tez.of_mutez_exn 4_000_000_000_000L in
  let open Alpha_utils.Account in
  let to_account (pkh, pk, sk) =
    {
      pkh = Signature.Public_key_hash.of_b58check_exn pkh;
      pk = Signature.Public_key.of_b58check_exn pk;
      sk = Signature.Secret_key.of_b58check_exn sk;
    }
  in
  let accounts = List.map to_account initial_accounts in
  List.iter add_account accounts ;
  List.map (fun acc -> account_to_bootstrap (acc, default_amount)) accounts

(* Inlined from Tezos_shell.Patch_context to avoid cyclic dependencies *)
let patch_context ctxt =
  let key_json = Some genesis_parameters in
  (match key_json with
  | None -> Lwt.return ctxt
  | Some (key, json) ->
      Tezos_context.Context.add
        ctxt
        [key]
        (Data_encoding.Binary.to_bytes_exn Data_encoding.json json))
  >>= fun ctxt ->
  Tezos_protocol_updater.Registered_protocol.get_result genesis.protocol
  >>=? fun proto ->
  let module Proto = (val proto) in
  let ctxt = Tezos_shell_context.Shell_context.wrap_disk_context ctxt in
  Proto.init
    ctxt
    {
      level = 0l;
      proto_level = 0;
      predecessor = genesis.block;
      timestamp = genesis.time;
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.empty;
      fitness = [];
      context = Context_hash.zero;
    }
  >>=? fun {context; _} ->
  return (Tezos_shell_context.Shell_context.unwrap_disk_context context)

let make_legacy_store ?(user_activated_upgrades = [])
    ?(user_activated_protocol_overrides = []) ~legacy_store_builder_exe
    ~history_mode ~genesis_block store genesis (blocks : Store.Block.t list)
    ~output_dir =
  let legacy_dir = output_dir in
  let proc =
    let exe = legacy_store_builder_exe in
    if not (Sys.file_exists exe) then
      Format.ksprintf Stdlib.invalid_arg "File %s does not exist" exe ;
    Lwt_process.open_process_full (exe, [|Filename.basename exe; legacy_dir|])
  in
  let proc_reader =
    let rec loop () =
      External_validation.recv proc#stdout Data_encoding.string >>= fun l ->
      Format.printf "[Legacy store builder] %s@." l ;
      loop ()
    in
    Lwt.catch
      (fun () -> loop ())
      (function
        | End_of_file ->
            Format.printf "[Legacy store builder] Terminating@.@." ;
            Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  Lwt.async (fun () -> proc_reader) ;
  (* Initialize the legacy state *)
  External_validation.send
    proc#stdin
    (Data_encoding.tup2
       History_mode.Legacy.encoding
       External_validation.parameters_encoding)
    ( history_mode,
      {
        External_validation.context_root = Filename.concat legacy_dir "context";
        protocol_root = Filename.concat legacy_dir "protocol";
        sandbox_parameters = Some (snd genesis_parameters);
        genesis;
        user_activated_upgrades;
        user_activated_protocol_overrides;
      } )
  >>= fun () ->
  (* Start sending blocks *)
  let chain_store = Store.main_chain_store store in
  let chain_id = Store.Chain.chain_id chain_store in
  Lwt_list.fold_left_s
    (fun pred_block block ->
      let block_header = Store.Block.header block in
      let operations = Store.Block.operations block in
      let max_operations_ttl =
        (WithExceptions.Option.get
           ~loc:__LOC__
           (Store.Unsafe.repr_of_block pred_block).Block_repr.metadata)
          .max_operations_ttl
      in
      let predecessor_ops_metadata_hash =
        Store.Block.all_operations_metadata_hash pred_block
      in
      External_validation.send
        proc#stdin
        External_validation.request_encoding
        (Validate
           {
             chain_id;
             block_header;
             predecessor_block_header = Store.Block.header pred_block;
             predecessor_block_metadata_hash =
               Store.Block.block_metadata_hash pred_block;
             predecessor_ops_metadata_hash;
             operations;
             max_operations_ttl;
           })
      >>= fun () -> Lwt.return block)
    genesis_block
    blocks
  >>= fun _ ->
  External_validation.send
    proc#stdin
    External_validation.request_encoding
    Terminate
  >>= fun () ->
  Lwt.join [proc_reader] >>= fun () ->
  proc#status >>= fun status ->
  Assert.is_true ~msg:"legacy builder exited" (status = Unix.(WEXITED 0)) ;
  Lwt.return_unit

open Filename.Infix

let fitness_from_int64 fitness =
  (* definition taken from src/proto_alpha/lib_protocol/src/constants_repr.ml *)
  let version_number = "\000" in
  (* definitions taken from src/proto_alpha/lib_protocol/src/fitness_repr.ml *)
  let int64_to_bytes i =
    let b = Bytes.create 8 in
    TzEndian.set_int64 b 0 i ;
    b
  in
  [Bytes.of_string version_number; int64_to_bytes fitness]

let make_block_header shell command =
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn
      Tezos_protocol_genesis.Protocol.Data.Command.encoding
      command
  in
  ({shell; protocol_data} : Block_header.t)

let make_activation_block genesis_block =
  let open Tezos_protocol_genesis.Protocol.Data.Command in
  let fitness = fitness_from_int64 1L in
  let protocol_parameters =
    Data_encoding.Binary.to_bytes_exn
      Data_encoding.json
      (Data_encoding.Json.construct
         Tezos_protocol_alpha.Protocol.Alpha_context.Parameters.encoding
         {
           Alpha_utils.default_genesis_parameters with
           bootstrap_accounts = default_genesis_accounts;
         })
  in
  let command =
    Activate
      {
        protocol = Tezos_protocol_alpha.Protocol.hash;
        fitness;
        protocol_parameters;
      }
  in
  let operations_hash = Operation_list_list_hash.compute [] in
  let shell_header =
    ({
       (Alpha_utils.Forge.make_shell
          ~level:(Int32.succ (Store.Block.level genesis_block))
          ~predecessor:(Store.Block.hash genesis_block)
          ~timestamp:
            (Time.Protocol.add (Store.Block.timestamp genesis_block) 60L)
          ~fitness
          ~operations_hash
          ~proto_level:1)
       with
       validation_passes = 0;
     }
      : Block_header.shell_header)
  in
  let block_header = make_block_header shell_header command in
  ( block_header,
    ({command; signature = Signature.zero}
      : Tezos_protocol_genesis.Protocol.block_header_data) )

let init_store ~base_dir ~patch_context ~history_mode k =
  let new_store_dir = base_dir // "new_store" in
  let store_dir = new_store_dir // "store" in
  let context_dir = new_store_dir // "context" in
  Store.init
    ~patch_context
    ~history_mode
    ~store_dir
    ~context_dir
    ~allow_testchains:false
    genesis
  >>=? fun store -> k (store_dir, context_dir) store

let build_new_store nb_blocks ~base_dir ~patch_context ~history_mode =
  let k (_store_dir, _context_dir) store =
    let chain_store = Store.main_chain_store store in
    let genesis = Store.Chain.genesis chain_store in
    Store.Chain.genesis_block chain_store >>= fun genesis_block ->
    let chain_id = Store.Chain.chain_id chain_store in
    (* Make the activation block with "dummy" context_hash and protocol_data *)
    let (activation_block, protocol_data) =
      make_activation_block genesis_block
    in
    Store.Block.context chain_store genesis_block >>=? fun genesis_ctxt ->
    (* Craft a block in order to get a right context_hash *)
    Tezos_protocol_genesis.Protocol.begin_construction
      ~chain_id
      ~predecessor_context:
        (Tezos_shell_context.Shell_context.wrap_disk_context genesis_ctxt)
      ~predecessor_timestamp:(Store.Block.timestamp genesis_block)
      ~predecessor_level:(Store.Block.level genesis_block)
      ~predecessor_fitness:(Store.Block.fitness genesis_block)
      ~predecessor:genesis.block
      ~timestamp:(Time.Protocol.add (Store.Block.timestamp genesis_block) 60L)
      ~protocol_data
      ()
    >>= fun res ->
    Lwt.return (Tezos_protocol_environment_genesis.Environment.wrap_error res)
    >>=? fun vs ->
    Tezos_protocol_genesis.Protocol.finalize_block vs >>= fun res ->
    Lwt.return (Tezos_protocol_environment_genesis.Environment.wrap_error res)
    >>=? fun ({context; _}, _meta) ->
    Tezos_protocol_alpha.Protocol.Main.init context activation_block.shell
    >>= fun res ->
    Lwt.return (Tezos_protocol_environment_alpha.Environment.wrap_tzresult res)
    >>=? fun vr ->
    let ctxt_hash =
      Tezos_context.Context.hash
        ~time:activation_block.shell.timestamp
        (Tezos_shell_context.Shell_context.unwrap_disk_context vr.context)
    in
    (* Use the right context hash in activation block *)
    let shell = {activation_block.shell with context = ctxt_hash} in
    let activation_block = {activation_block with shell} in
    let block =
      Tezos_protocol_genesis.Protocol.Data.Command.forge
        shell
        protocol_data.command
    in
    let sk =
      Signature.Secret_key.of_b58check_exn
        "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"
    in
    (* Sign the block with activator key *)
    let signature =
      Signature.sign ~watermark:Signature.(Block_header chain_id) sk block
    in
    (* Protocol_data is the signed block *)
    let protocol_data =
      Signature.concat
        (Data_encoding.Binary.to_bytes_exn
           Tezos_protocol_genesis.Protocol.Data.Command.encoding
           protocol_data.command)
        signature
    in
    (* Use the right protocol data *)
    let activation_block = {activation_block with protocol_data} in
    (* Regular apply + store *)
    let apply_environment =
      {
        Block_validation.max_operations_ttl = 0;
        chain_id;
        predecessor_block_header = Store.Block.header genesis_block;
        predecessor_context = genesis_ctxt;
        predecessor_block_metadata_hash = None;
        predecessor_ops_metadata_hash = None;
        user_activated_upgrades = [];
        user_activated_protocol_overrides = [];
      }
    in
    Block_validation.apply apply_environment activation_block [] ~cache:`Lazy
    >>=? fun (bv, _) ->
    (Store.Block.store_block
       chain_store
       ~block_header:activation_block
       ~operations:[]
       bv
     >>=? function
     | Some b -> return b
     | None -> assert false)
    >>=? fun block ->
    Lwt.finalize
      (fun () ->
        match nb_blocks with
        | `Cycle n -> Alpha_utils.bake_until_n_cycle_end chain_store n block
        | `Blocks n -> Alpha_utils.bake_n chain_store n block)
      (fun () ->
        let bs = Store.Unsafe.get_block_store chain_store in
        Block_store.await_merging bs >>= fun _ -> Lwt.return_unit)
    >>=? fun (blocks, _last_head) ->
    return (store, genesis, genesis_block, block :: blocks)
  in
  init_store ~base_dir ~patch_context ~history_mode k

let build_old_store ~genesis ~genesis_block ~legacy_history_mode:history_mode
    ~store ~legacy_data_dir ~legacy_store_builder_exe blocks =
  make_legacy_store
    ~legacy_store_builder_exe
    ~genesis_block
    ~history_mode
    store
    genesis
    blocks
    ~output_dir:legacy_data_dir
  >>= fun () ->
  Legacy_state.init
    ~readonly:false
    ~history_mode
    ~store_root:(legacy_data_dir // "store")
    ~context_root:(legacy_data_dir // "context")
    genesis

let store_builder ?(legacy_history_mode = History_mode.Legacy.Full)
    ?(nb_blocks = `Cycle 8) ~base_dir ~legacy_store_builder_exe () =
  let history_mode = History_mode.convert legacy_history_mode in
  build_new_store nb_blocks ~base_dir ~patch_context ~history_mode
  >>=? fun (store, genesis, genesis_block, blocks) ->
  let legacy_data_dir = base_dir // "store_to_upgrade" in
  build_old_store
    ~genesis
    ~genesis_block
    ~legacy_history_mode
    ~store
    ~legacy_data_dir
    ~legacy_store_builder_exe
    blocks
  >>=? fun (legacy_state, _, _, _) ->
  return (store, (legacy_data_dir, legacy_state), blocks)

type test = {
  name : string;
  speed : [`Quick | `Slow];
  legacy_history_mode : History_mode.Legacy.t;
  nb_blocks : [`Cycle of int | `Blocks of int];
  test :
    Store.t ->
    string * Legacy_state.t ->
    Store.Block.t list ->
    unit tzresult Lwt.t;
}

let wrap_test_legacy ?(keep_dir = false) test : string Alcotest_lwt.test_case =
  let {name; speed; legacy_history_mode; nb_blocks; test} = test in
  let prefix_dir = "tezos_indexed_store_test_" in
  let with_dir f =
    if not keep_dir then
      Lwt_utils_unix.with_tempdir prefix_dir (fun base_dir ->
          Lwt.catch
            (fun () -> f base_dir)
            (fun exn ->
              Lwt_utils_unix.remove_dir base_dir >>= fun () -> fail_with_exn exn))
    else
      let base_dir = Filename.temp_file prefix_dir "" in
      Lwt_unix.unlink base_dir >>= fun () ->
      Lwt_unix.mkdir base_dir 0o700 >>= fun () ->
      Format.printf "@\nPersisting dir %s for test.@." base_dir ;
      f base_dir
  in
  let test _ legacy_store_builder_exe =
    with_dir (fun base_dir ->
        store_builder
          ~base_dir
          ~legacy_history_mode
          ~nb_blocks
          ~legacy_store_builder_exe
          ()
        >>=? fun (store, (legacy_store_dir, legacy_state), blocks) ->
        Lwt.finalize
          (fun () ->
            protect
              ~on_error:(fun err ->
                Store.make_pp_store store >>= fun pp_store ->
                Format.eprintf "@[<v>DEBUG:@ %a@]@." pp_store () ;
                Lwt.return (Error err))
              (fun () -> test store (legacy_store_dir, legacy_state) blocks))
          (fun () ->
            Legacy_state.close legacy_state >>= fun () ->
            Store.close_store store))
  in
  Alcotest_lwt.test_case name speed (fun x exe ->
      test x exe >>= function
      | Ok () -> Lwt.return_unit
      | Error errs ->
          Format.printf
            "@\nError while running test:@\n%a@."
            Error_monad.pp_print_trace
            errs ;
          Lwt.fail Alcotest.Test_error)
