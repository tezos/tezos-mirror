(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Dal_helpers
module Dal = Dal_common

module Dal_RPC = struct
  include Dal.RPC

  (* We override call_xx RPCs in Dal.RPC to use a DAL node in this file. *)
  include Dal.RPC.Local
end

let trim_trailing_zeros str =
  let rec trim idx =
    if idx < 0 then ""
    else if str.[idx] = '0' then trim (idx - 1)
    else String.sub str 0 (idx + 1)
  in
  let last_index = String.length str - 1 in
  trim last_index

let make_even_length str =
  if String.length str mod 2 == 1 then str ^ "0" else str

(* Publish a slot, hopefully at level [level]. It's not clear though when the
   slot will be included in a block, that is, at which level it will actually be
   included. *)
let publish_slot dal_node client source ~slot_size ~level ~slot_index =
  let slot_content =
    Format.sprintf "DATA for level %d with index %d" level slot_index
  in
  Log.info "Publishing slot data '%s'..." slot_content ;
  let slot = Dal.Helpers.make_slot ~slot_size slot_content in
  let* commitment, proof =
    Dal.Helpers.store_slot (Either.Left dal_node) ~with_proof:true slot
  in
  let commitment_hash =
    match Dal.Cryptobox.Commitment.of_b58check_opt commitment with
    | None -> assert false
    | Some hash -> hash
  in
  let proof = Dal.Commitment.proof_of_string proof in
  Operation.Manager.(
    inject
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6127
       Think of a better strategy to push slots *)
      ~force:true
      [
        make ~source
        @@ dal_publish_commitment
             ~index:slot_index
             ~commitment:commitment_hash
             ~proof;
      ]
      client)

(* This function attempts to publish a slot at every level, from the current one
   up to [last_level]. *)
let publish_at_levels node dal_node client ~slot_size ~last_level publisher
    ~slot_index =
  let rec publish () =
    let* current_level = Node.get_level node in
    if current_level >= last_level then return ()
    else
      let* _op_hash =
        publish_slot
          dal_node
          client
          publisher
          ~slot_size
          ~level:(current_level + 1)
          ~slot_index
      in
      let* _ = Node.wait_for_level node (current_level + 1) in
      publish ()
  in
  publish ()

(* [check_attestations level] checks that the attested slot indexes posted to
   L1 at level [level] matches the slot indexes classified as attestable by
   the DAL node.  Returns a pair of (number of published slot headers, number
   of attested slot indexes) at the given level. *)
let check_attestations node dal_node ~lag ~number_of_slots ~published_level =
  let module Map = Map.Make (String) in
  let* slot_headers =
    Dal_RPC.(call dal_node @@ get_published_level_headers published_level)
  in
  let map =
    List.fold_left
      (fun acc Dal_RPC.{status; slot_index; _} ->
        Map.update
          status
          (function
            | None -> Some (1, [slot_index])
            | Some (c, indexes) -> Some (c + 1, slot_index :: indexes))
          acc)
      Map.empty
      slot_headers
  in
  let pp_map =
    let open Format in
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      (fun fmt (status, (c, _indexes)) -> Format.fprintf fmt "%d %s" c status)
  in
  let attested_level = string_of_int (published_level + lag) in
  let* metadata =
    Node.RPC.(call node @@ get_chain_block_metadata ~block:attested_level ())
  in
  let pp_array fmt a =
    for i = 0 to Array.length a - 1 do
      let b = if a.(i) then 1 else 0 in
      Format.fprintf fmt "%d" b
    done
  in
  let proto_attestation =
    match metadata.dal_attestation with
    | None -> Array.make number_of_slots false
    | Some x ->
        let len = Array.length x in
        if len < number_of_slots then (
          let a = Array.make number_of_slots false in
          for i = 0 to number_of_slots - 1 do
            if i < len then a.(i) <- x.(i)
          done ;
          a)
        else x
  in
  let num_attested, indexes =
    match Map.find_opt "attested" map with
    | None -> (0, [])
    | Some (c, l) -> (c, l)
  in
  let node_attestation =
    (* build array from list *)
    let a = Array.make number_of_slots false in
    List.iter (fun i -> a.(i) <- true) indexes ;
    a
  in
  if proto_attestation <> node_attestation then
    Test.fail
      "At published_level %d, attestations in the L1 and DAL nodes differ %a \
       vs %a"
      published_level
      pp_array
      proto_attestation
      pp_array
      node_attestation ;
  let num_published = List.length slot_headers in
  let* ops =
    Node.RPC.(
      call node
      @@ get_chain_block_operations_validation_pass
           ~block:attested_level
           ~validation_pass:0
           ())
  in
  let attestations =
    List.filter
      (fun op ->
        let op_type = JSON.(op |-> "contents" |=> 0 |-> "kind" |> as_string) in
        String.equal op_type "dal_attestation")
      (JSON.as_list ops)
  in
  Log.info
    "At published_level %d, published slots: %d, status: %a (%d attestations)"
    published_level
    (List.length slot_headers)
    pp_map
    (Map.bindings map)
    (List.length attestations) ;
  return (num_published, num_attested)

(* This scenario starts a L1 node and a DAL node on the given testnet (Dailynet
   or Weeklynet), and it publishes slots for a number of levels and a number of
   slot producers (both given as arguments to the test). At the end of the test,
   the average number of published respectively attested slots are shown (with
   Log.info).

   To run the test, one can use:

   dune exec src/bin_testnet_scenarios/main.exe -- dal dailynet simple -a load -a save -a num_accounts=5 -a levels=10 -i

   Use the arguments:
   - `load`: to load an existing data-dir saved (with `save`, see next) in a previous run of the script
   - `save`: to save the current data-dir after the L1 node is synced and at the end of the test
   - `num_accounts=<int>`: to specify the number of slot producers
   - `levels`: to specify for how many levels to publish slots
   - `peers`: to specify additional DAL bootstrap peers
*)
let scenario_without_rollup_node node dal_node client _network_name
    proto_parameters num_levels keys =
  let num_accounts = List.length keys in
  let key_indices = range 0 (num_accounts - 1) in
  let dal_parameters =
    Dal.Parameters.from_protocol_parameters proto_parameters
  in
  let cryptobox = dal_parameters.cryptobox in
  let number_of_slots = dal_parameters.number_of_slots in
  let lag = dal_parameters.attestation_lag in

  let* first_level =
    let* level = Node.wait_for_level node 0 in
    1 + level |> return
  in

  let last_level = first_level + num_levels in
  let publish_for_index key_index =
    let slot_index = key_index mod number_of_slots in
    let publisher = List.nth keys key_index in
    publish_at_levels
      node
      dal_node
      client
      publisher
      ~slot_size:cryptobox.slot_size
      ~last_level
      ~slot_index
  in
  let* () = Lwt_list.iter_p (fun i -> publish_for_index i) key_indices in
  let last_level = first_level + num_levels + lag in
  Log.info
    "Waiting for level %d, that is, for the attestation period to pass..."
    last_level ;
  let* _level = Node.wait_for_level node last_level in
  Log.info
    "Stats on attestations at levels %d to %d:"
    first_level
    (first_level + num_levels - 1) ;
  let* published, attested =
    Lwt_list.fold_left_s
      (fun (total_published, total_attested) level ->
        let* published, attested =
          check_attestations
            node
            dal_node
            ~lag
            ~number_of_slots
            ~published_level:level
        in
        return (total_published + published, total_attested + attested))
      (0, 0)
      (range first_level (first_level + num_levels - 1))
  in
  let avg_pub, avg_att =
    let n = float_of_int num_levels in
    (float_of_int published /. n, float_of_int attested /. n)
  in
  Log.info
    "With %d accounts, average slots per level over %d levels: published = \
     %.2f attested = %.2f"
    num_accounts
    num_levels
    avg_pub
    avg_att ;
  unit

let prepare_installer_kernel rollup_node =
  Sc_rollup_helpers.prepare_installer_kernel
    ~preimages_dir:
      (Filename.concat (Sc_rollup_node.data_dir rollup_node) "wasm_2_0_0")
    Constant.WASM.dal_echo_kernel

(* Originate a rollup with alias [rollup_alias] running the "dal_echo_kernel" on
   the given [rollup_node]. *)
let originate_rollup client rollup_node rollup_alias =
  let* {boot_sector; _} = prepare_installer_kernel rollup_node in
  let* rollup_address =
    Client.Sc_rollup.originate
      ~force:true (* because the alias might have already been used *)
      ~wait:"1"
      ~burn_cap:Tez.(of_int 10)
      ~alias:rollup_alias
      ~src:Wallet.Airdrop.giver_alias
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"unit"
      client
  in
  Log.info "Originated rollup %s with address: %s" rollup_alias rollup_address ;
  unit

(* This scenario is similar to [scenario_without_rollup_node], while in addition
   it also deploys a rollup node running the "dal_echo_roll_kernel" and checks
   that its storage contains the published slots.

   To run the test, one can use:

   dune exec src/bin_testnet_scenarios/main.exe -- dal dailynet rollup -a load -a save -a num_accounts=5 -a levels=10 -a originate -i

   The additional `originate` argument is used to specify whether the rollup
   should be re-originated (if given) or not (if missing).
*)
let scenario_with_rollup_node node dal_node client network_name proto_parameters
    num_levels keys =
  let dal_parameters =
    Dal.Parameters.from_protocol_parameters proto_parameters
  in
  let cryptobox = dal_parameters.cryptobox in
  let lag = dal_parameters.attestation_lag in
  let number_of_slots = dal_parameters.number_of_slots in

  let originate =
    Cli.get ~default:None (fun _ -> Some (Some ())) "originate"
    |> Option.is_some
  in
  let rollup_alias = "dal_echo_rollup" in
  let rollup_node =
    Sc_rollup_node.create
      ~dal_node
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:Wallet.Airdrop.giver_alias
  in

  let rollup_backup =
    Filename.get_temp_dir_name () // (network_name ^ "-rollup")
  in
  let tezt_rollup_data_dir = Sc_rollup_node.data_dir rollup_node in
  let* () =
    match Cli.get ~default:None (fun _ -> Some (Some ())) "load" with
    | Some () when not originate ->
        if Sys.file_exists rollup_backup then (
          Log.info
            "Loading rollup data-dir from %s in current tezt workspace..."
            rollup_backup ;
          Process.run "cp" ["-rT"; rollup_backup; tezt_rollup_data_dir])
        else (
          Log.info "Expected rollup data-dir %s does not exist." rollup_backup ;
          unit)
    | _ -> unit
  in
  let* () =
    if originate then originate_rollup client rollup_node rollup_alias
    else
      (* We still need to let the installer prepare the reveal preimages for the
         DAL echo kernel to be correctly executed: recall that we originated a
         rollup running the installer kernel, not the echo kernel. *)
      let* _boot_sector = prepare_installer_kernel rollup_node in
      unit
  in
  let* () = Sc_rollup_node.run rollup_node rollup_alias [Log_kernel_debug] in
  let* first_level =
    let* crt = Node.get_level node in
    crt + 1 |> return
  in
  Log.info "Monitoring the rollup node starting with level %d" first_level ;
  let* _level = Sc_rollup_node.wait_for_level rollup_node first_level in

  let slot_producer = List.hd keys in
  let last_level = first_level + num_levels in
  let publish () =
    publish_at_levels
      node
      dal_node
      client
      slot_producer
      ~slot_size:cryptobox.slot_size
      ~last_level
      ~slot_index:0
  in

  let rec get_stored_slot published_level =
    if published_level >= first_level + num_levels + 2 then unit
    else
      let queried_level = published_level + lag in
      let* _level =
        (* Wait one more level to be sure the rollup node processed the
           block. *)
        Sc_rollup_node.wait_for_level rollup_node (queried_level + 1)
      in
      let* value =
        Sc_rollup_node.RPC.call rollup_node
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~block:(string_of_int queried_level)
             ~pvm_kind:"wasm_2_0_0"
             ~operation:Value
             ~key:"/output/slot-0"
             ()
      in
      (match value with
      | None ->
          Log.info
            "For published level %d, there is no stored data"
            published_level
      | Some s ->
          let decode s = Hex.to_string (`Hex s) in
          let slot = s |> trim_trailing_zeros |> make_even_length |> decode in
          Log.info
            "For published level %d, the stored slot data is: '%s'"
            published_level
            slot) ;
      get_stored_slot (published_level + 1)
  in

  let rec check_slots_attested published_level =
    if published_level >= first_level + num_levels + 2 then return ()
    else
      let attested_level = published_level + lag in
      let* _level =
        (* Wait 1 level for the block to be final, wait another one to be sure
           the DAL node processed it. *)
        Node.wait_for_level node (attested_level + 2)
      in
      let* _ =
        check_attestations node dal_node ~lag ~number_of_slots ~published_level
      in
      check_slots_attested (published_level + 1)
  in

  let* () =
    Lwt.join
      [
        publish (); get_stored_slot first_level; check_slots_attested first_level;
      ]
  in
  match Cli.get ~default:None (fun _ -> Some (Some ())) "save" with
  | None -> unit
  | Some () -> Process.run "cp" ["-rT"; tezt_rollup_data_dir; rollup_backup]

(* [load_and_save] returns two functions [load] and [save]. [save] can be used
   to save the current L1 and DAL nodes data-dirs in predefined locations, while
   [load] is used to copy the data-dirs from these predefined locations to the
   current data-dirs. *)
let load_and_save node dal_node network_name network_arg load_arg save_arg =
  let tezt_data_dir = Node.data_dir node in
  let tezt_dal_data_dir = Dal_node.data_dir dal_node in
  let l1_backup = Filename.get_temp_dir_name () // network_name in
  let dal_backup = Filename.get_temp_dir_name () // (network_name ^ "-dal") in
  let load () =
    let load_l1_dir, load_dal_dir =
      match load_arg with
      | Some () ->
          let load_l1, load_dal =
            if Sys.file_exists l1_backup then
              if Sys.file_exists dal_backup then (true, true)
              else (
                Log.info "Expected DAL data-dir %s does not exist." dal_backup ;
                (true, false))
            else (
              Log.info "Expected L1 data-dir %s does not exist." l1_backup ;
              (* Even if the DAL backup exists, we don't load it, because the DAL
                 config depends on the L1 config. *)
              (false, false))
          in
          (load_l1, load_dal)
      | None -> (false, false)
    in
    let* () =
      if load_l1_dir then (
        Log.info
          "Loading L1 node's data-dir from %s in current tezt workspace..."
          l1_backup ;
        let* () = Process.run "cp" ["-rT"; l1_backup; tezt_data_dir] in
        unit)
      else (
        Log.info "Initializing L1 node..." ;
        Node.config_init
          node
          [
            network_arg;
            Expected_pow 26;
            Synchronisation_threshold 2;
            (* use the archive mode so that the rollup node can retrieve the
               inbox for old blocks *)
            History_mode Archive;
          ])
    in
    if load_dal_dir then (
      Log.info
        "Loading DAL node's data-dir from %s in current tezt workspace..."
        dal_backup ;
      let* () = Process.run "cp" ["-rT"; dal_backup; tezt_dal_data_dir] in
      (* We delete the config file, because it is not useful for subsequent runs
         and may be confusing (it contains in particular the net/rpc addresses
         which should not be reused by mistake). *)
      let config_file = tezt_dal_data_dir // "config.json" in
      if Sys.file_exists config_file then Process.run "rm" [config_file]
      else unit)
    else unit
  in
  let save ~restart =
    match save_arg with
    | None -> return ()
    | Some () ->
        Log.info
          "Save the current data-dirs into %s and %s..."
          l1_backup
          dal_backup ;
        let* () = Node.terminate node in
        let* () = Process.run "cp" ["-rT"; tezt_data_dir; l1_backup] in
        let* () = Process.run "cp" ["-rT"; tezt_dal_data_dir; dal_backup] in
        if restart then (
          Log.info "Restart L1 node and wait for it to sync..." ;
          let wait_for_sync_promise = wait_for_sync node in
          let* () = Node.run node [network_arg] in
          let* () = Node.wait_for_ready node in
          let* () = wait_for_sync_promise in
          unit)
        else unit
  in
  (load, save)

let run_scenario network kind scenario =
  let net_name = Network.short_name network in
  let kind_tag, kind_str =
    match kind with
    | `Simple -> ("simple", "without rollup node")
    | `With_rollup -> ("rollup", "with rollup node")
  in
  Test.register
    ~__FILE__
    ~title:(sf "Produce slots on %s %s" net_name kind_str)
    ~tags:[Tag.tezos2; "dal"; net_name; kind_tag]
  @@ fun () ->
  let load = Cli.get ~default:None (fun _ -> Some (Some ())) "load" in
  let save = Cli.get ~default:None (fun _ -> Some (Some ())) "save" in
  let dal_peers = Cli.get ~default:[] (fun str -> Some [str]) "peers" in
  let num_accounts =
    Cli.get ~default:5 (fun str -> Some (int_of_string str)) "num_accounts"
  in
  let num_levels =
    Cli.get ~default:10 (fun str -> Some (int_of_string str)) "levels"
  in
  Log.info
    "Using %d keys for slot production, publishing for %d levels"
    num_accounts
    num_levels ;
  let key_indices = range 0 (num_accounts - 1) in

  let network_name = Network.name network in
  let network_url = Format.sprintf "https://teztnets.com/%s" network_name in
  let network_arg = Node.Network network_url in
  let network_baker =
    (* a bootstrap delegate for both Dailynet and Weeklynet *)
    "tz1foXHgRzdYdaLgX6XhpZGxbBv42LZ6ubvE"
  in

  let node = Node.create [] in
  let dal_node = Dal_node.create ~node () in
  let client =
    Client.create
      ~base_dir:(Wallet.default_wallet network)
      ~endpoint:(Node node)
      ()
  in
  let load, save =
    load_and_save node dal_node network_name network_arg load save
  in

  let* () = load () in

  Log.info "Initializing the DAL node..." ;
  let* () =
    Dal_node.init_config
      ~peers:dal_peers
      ~expected_pow:26.
      ~producer_profiles:key_indices
      ~attester_profiles:[network_baker]
      dal_node
  in
  Log.info "Run L1 node and wait for it to sync..." ;
  let wait_for_sync_promise = wait_for_sync node in
  let* () = Node.run node [network_arg] in
  let* () = Node.wait_for_ready node in
  let* () = wait_for_sync_promise in
  let* () = save ~restart:true in

  Log.info "Make sure the wallet is usable." ;
  let aliases =
    List.map (fun i -> "slot-producer-" ^ string_of_int i) key_indices
  in
  let* keys = Wallet.load_wallet network client aliases in
  let* () = Wallet.Airdrop.distribute_money client keys in

  let* () = Dal_node.run dal_node in
  let* proto_parameters =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in

  Log.info "Setup finished. Now running the scenario-specific part." ;
  let* () =
    scenario node dal_node client network_name proto_parameters num_levels keys
  in

  save ~restart:false

let register () =
  run_scenario Dailynet `Simple scenario_without_rollup_node ;
  run_scenario Weeklynet `Simple scenario_without_rollup_node ;
  run_scenario Dailynet `With_rollup scenario_with_rollup_node ;
  run_scenario Weeklynet `With_rollup scenario_with_rollup_node
