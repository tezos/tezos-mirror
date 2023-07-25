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

(* This scenario starts a L1 node and a DAL node on the given testnet (Dailynet
   or Mondaynet), and it publishes slots for a number of levels and a number of
   slot producers (both given as arguments to the test). At the end of the test,
   the average number of published respectively attested slots are shown (with
   Log.info).

   To run the test, one can use:

   dune exec src/bin_testnet_scenarios/main.exe -- dal dailynet -a load -a save -a num_accounts=5 -a levels=10

   Use the arguments:
   - `load`: to load an existing data-dir saved (with `save`, see next) in a previous run of the script
   - `save`: to save the current data-dir after the L1 node is synced and at the end of the test
   - `num_account=<int>`: to specify the number of slot producers
   - `levels`: to specify for how many levels to publish slots
*)
let scenario network =
  let net_name = Network.short_name network in
  Test.register
    ~__FILE__
    ~title:(sf "Produce slots on %s" net_name)
    ~tags:["dal"; net_name]
  @@ fun () ->
  let load = Cli.get ~default:None (fun _ -> Some (Some ())) "load" in
  let save = Cli.get ~default:None (fun _ -> Some (Some ())) "save" in
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

  let node = Node.create [] in
  let tezt_data_dir = Node.data_dir node in
  let network_name = Network.name network in
  let network_url = Format.sprintf "https://teztnets.xyz/%s" network_name in
  let network_arg = Node.Network network_url in
  let backup = Filename.get_temp_dir_name () // network_name in
  let load () =
    let do_load =
      match load with
      | Some () ->
          if Sys.file_exists backup then true
          else (
            Log.info
              "Expected data-dir %s does not exist in current tezt workspace"
              backup ;
            false)
      | None -> false
    in
    if do_load then (
      Log.info "Loading data-dir from %s in current tezt workspace..." backup ;
      Process.run "cp" ["-rT"; backup; tezt_data_dir])
    else (
      Log.info "Initializing L1 node..." ;
      Node.config_init
        node
        [network_arg; Expected_pow 26; Synchronisation_threshold 2])
  in
  let* () = load () in
  Log.info "Run L1 node and wait for it to sync..." ;
  let* () = Node.run node [network_arg] in
  let* () = Node.wait_for_ready node in
  let* () = wait_for_sync node in
  let client =
    Client.create
      ~base_dir:(Wallet.default_wallet network)
      ~endpoint:(Node node)
      ()
  in
  let key_indices = range 0 (num_accounts - 1) in
  let aliases =
    List.map (fun i -> "slot-producer-" ^ string_of_int i) key_indices
  in
  let* keys = Wallet.load_wallet network client aliases in
  let save ~restart =
    match save with
    | None -> return ()
    | Some () ->
        Log.info "Save the current data-dir into %s..." backup ;
        let* () = Node.terminate node in
        let* () = Process.run "cp" ["-rT"; tezt_data_dir; backup] in
        if restart then (
          Log.info "Restart L1 node and wait for it to sync..." ;
          let* () = Node.run node [network_arg] in
          let* () = Node.wait_for_ready node in
          wait_for_sync node)
        else return ()
  in
  let* () = save ~restart:true in
  let* () = Wallet.Airdrop.distribute_money client keys in
  let dal_node = Dal_node.create ~node ~client () in
  let attestor_profiles =
    match network with
    | Dailynet ->
        [(* the one and only delegate *) "tz1foXHgRzdYdaLgX6XhpZGxbBv42LZ6ubvE"]
    | Mondaynet -> []
  in
  let* () =
    Dal_node.init_config
      ~expected_pow:26.
      ~producer_profiles:key_indices
      ~attestor_profiles
      dal_node
  in
  let* () = Dal_node.run dal_node in
  let* proto_parameters =
    RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
  in
  let block_times =
    JSON.(
      proto_parameters |-> "minimal_block_delay" |> as_string |> int_of_string)
  in
  let dal_parameters =
    Rollup.Dal.Parameters.from_protocol_parameters proto_parameters
  in
  let cryptobox = dal_parameters.cryptobox in
  let number_of_slots = dal_parameters.number_of_slots in
  let lag = dal_parameters.attestation_lag in
  let first_level = 1 + Node.get_level node in

  (* [publish_slot key_index] publishes a slot from the [key_index+1]-th account
     in [keys] for every level from current level to [first_level + num_levels -
     1]. *)
  let rec publish_slot key_index =
    let current_level = Node.get_level node in
    if current_level >= first_level + num_levels then return ()
    else
      let slot =
        String.init 30 (fun _i ->
            let x = Random.int 26 in
            Char.code 'a' + x |> Char.chr)
        |> Rollup.Dal.make_slot ~slot_size:cryptobox.slot_size
      in
      let slot_index = key_index mod number_of_slots in

      Log.info
        "publishing slot at level %d with index %d..."
        current_level
        slot_index ;
      let* commitment =
        RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot)
      in
      let* () =
        RPC.call dal_node
        @@ Rollup.Dal.RPC.put_commitment_shards ~with_proof:true commitment
      in
      let commitment_hash =
        match Rollup.Dal.Cryptobox.Commitment.of_b58check_opt commitment with
        | None -> assert false
        | Some hash -> hash
      in
      let* proof =
        let* proof =
          RPC.call dal_node @@ Rollup.Dal.RPC.get_commitment_proof commitment
        in
        return
          (Data_encoding.Json.destruct
             Rollup.Dal.Cryptobox.Commitment_proof.encoding
             (`String proof))
      in
      let source = List.nth keys key_index in
      let* _ =
        Operation.Manager.(
          inject
            [
              make ~source
              @@ dal_publish_slot_header
                   ~index:slot_index
                   ~commitment:commitment_hash
                   ~proof;
            ]
            client)
      in
      let* _ = Node.wait_for_level node (current_level + 1) in
      publish_slot key_index
  in
  let* () = Lwt_list.iter_p (fun i -> publish_slot i) key_indices in
  Log.info "Waiting for the attestation period to pass..." ;
  let* _ =
    Lwt_unix.sleep
      (5. +. float_of_int (dal_parameters.attestation_lag * block_times))
  in

  (* [check_attestations level] checks that the attested slot indexes posted to
     L1 at level [level] matches the slot indexes classified as attestable by
     the DAL node.  Returns a pair of (number of published slot headers, number
     of attested slot indexes) at the given level. *)
  let check_attestations level =
    let module Map = Map.Make (String) in
    let* slot_headers =
      RPC.call dal_node (Rollup.Dal.RPC.get_published_level_headers level)
    in
    let map =
      List.fold_left
        (fun acc Rollup.Dal.RPC.{status; slot_index; _} ->
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
    let* metadata =
      RPC.call
        node
        (RPC.get_chain_block_metadata ~block:(string_of_int (level + lag)) ())
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
        "At level %d, attestations in the L1 and DAL nodes differ %a vs %a"
        level
        pp_array
        proto_attestation
        pp_array
        node_attestation ;
    let num_published = List.length slot_headers in
    Log.info
      "At level %d, published slots: %d, status: %a"
      level
      (List.length slot_headers)
      pp_map
      (Map.bindings map) ;
    return (num_published, num_attested)
  in

  let last_level = Node.get_level node in
  let expected_min_level = first_level + num_levels + lag in
  Check.(
    (last_level >= expected_min_level)
      int
      ~error_msg:"Node level is %L, expected to be at least %R") ;

  Log.info
    "Stats on attestations at levels %d to %d:"
    first_level
    (first_level + num_levels - 1) ;
  let* published, attested =
    Lwt_list.fold_left_s
      (fun (total_published, total_attested) level ->
        let* published, attested = check_attestations level in
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

  save ~restart:false

let register () =
  scenario Dailynet ;
  scenario Mondaynet
