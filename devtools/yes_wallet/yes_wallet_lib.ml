(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(*
   dune exec devtools/yes_wallet/yes-wallet.exe

   Given a list of aliases and public key hashes:
   - encodes each public key as a fake secret key that can be used
     with the yes-node.patch
   - creates a 'yes-wallet' directory to be passed to octez-client -d option
 *)

let pp_protocol ppf (module P : Sigs.PROTOCOL) = Protocol_hash.pp ppf P.hash

let pkh_json (alias, pkh, _pk, _ck) =
  Ezjsonm.(dict [("name", string alias); ("value", string pkh)])

let pk_json (alias, _pkh, pk, _ck) =
  Ezjsonm.(
    dict
      [
        ("name", string alias);
        ( "value",
          dict [("locator", string @@ "unencrypted:" ^ pk); ("key", string pk)]
        );
      ])

(* P-256 pk : 33+1 bytes
   ed25519 pk sk : 32+1 bytes
*)

let fake_sk_of_pk (pk_s : string) : string =
  let open Tezos_crypto.Signature.V_latest in
  let pk = Public_key.of_b58check_exn pk_s in
  let pk_b = Data_encoding.Binary.to_bytes_exn Public_key.encoding pk in
  let sk : Signature.Secret_key.t =
    match pk with
    | Ed25519 _ | Secp256k1 _ | P256 _ ->
        let sk_b = Bytes.sub pk_b 0 33 in
        (* Extracting fake secret key from the associated public key.*)
        Data_encoding.Binary.of_bytes_exn Secret_key.encoding sk_b
    | Mldsa44 _ ->
        let _pkh, _pk, sk = Signature.Mldsa44.generate_key () in
        Mldsa44 sk
    | Bls _ ->
        (* For BLS we cannot easily encode secret key from public key bytes. It
           is simpler to generate a new random secret key with the public key as
           seed. *)
        let sk = Bls12_381_signature.generate_sk pk_b in
        Bls sk
  in
  let sk_s = Secret_key.to_b58check sk in
  sk_s

let sk_json (alias, _pkh, pk, _ck) =
  Ezjsonm.(
    dict
      [
        ("name", string alias);
        ("value", string @@ "unencrypted:" ^ fake_sk_of_pk pk);
      ])

let ck_json (alias, pkh, _pk, ck) =
  match ck with
  | Some (cpkh, cpk) ->
      Ezjsonm.(
        dict
          [
            ("name", string alias);
            ("public_key_hash", string pkh);
            ("consensus_public_key_hash", string cpkh);
            ("consensus_public_key", string cpk);
          ])
  | None -> Ezjsonm.unit ()

let map_bind_to_json f list = Ezjsonm.list f list

let pkh_list_json list = map_bind_to_json pkh_json list

let pk_list_json list = map_bind_to_json pk_json list

let sk_list_json list = map_bind_to_json sk_json list

let ck_list_json list =
  map_bind_to_json
    ck_json
    (List.filter (fun (_, _, _, v) -> Option.is_some v) list)

let json_to_file json file =
  let chan = open_out file in
  Ezjsonm.to_channel ~minify:false chan json ;
  close_out chan

let json_of_file file =
  let chan = open_in file in
  let json = Ezjsonm.from_channel chan in
  close_in chan ;
  json

let pk_of_json (json : Ezjsonm.value) =
  match json with
  | `O
      [
        ("name", `String alias);
        ("value", `O [("locator", _); ("key", `String pk_s)]);
      ] ->
      (alias, pk_s)
  | `O [("name", `String alias); ("value", `String locator)] -> (
      match String.split_on_char ':' locator with
      | ["unencrypted"; pk_s] -> (alias, pk_s)
      | _ ->
          raise (Failure ("unsupported locator in public key file" ^ locator)))
  | json ->
      raise
        (Failure
           ("unsupported public key file format: "
           ^ Ezjsonm.decode_string_exn json))

let map_bind_of_json f (list : Ezjsonm.t) =
  match list with
  | `O _ -> raise (Failure "not a list")
  | `A val_lst -> List.map f val_lst

let pk_list_of_json = map_bind_of_json pk_of_json

let pk_list_of_file file = pk_list_of_json @@ json_of_file file

let sk_list_of_pk_file file =
  let list = pk_list_of_file file in
  Format.printf "found %d keys@." (List.length list) ;
  map_bind_to_json
    (fun (alias, pk_s) -> sk_json (alias, alias, pk_s, None))
    list

let load_alias_file chn =
  let alias_of_json (json : Ezjsonm.value) =
    match json with
    | `O
        [
          ("alias", `String alias);
          ("address", `String pkh);
          ("publicKey", `String pk);
        ] ->
        (alias, pkh, pk)
    | _json -> raise (Failure "Unsupported alias file format: ")
  in
  map_bind_of_json alias_of_json @@ json_of_file chn

(* Create a yes-wallet in [dest] using the alias list [alias_pkh_pk_ck_list].
   If [write_consensus_keys] is true, the consensus key mapping file will be
   written, even if it is an empty list. *)
let write_yes_wallet dest alias_pkh_pk_ck_list =
  let pkh_filename = Filename.concat dest "public_key_hashs" in
  let pk_filename = Filename.concat dest "public_keys" in
  let sk_filename = Filename.concat dest "secret_keys" in
  let ck_filename = Filename.concat dest "consensus_keys_mapping" in
  if not (Sys.file_exists dest) then Unix.mkdir dest 0o750 ;
  json_to_file (pkh_list_json alias_pkh_pk_ck_list) pkh_filename ;
  json_to_file (pk_list_json alias_pkh_pk_ck_list) pk_filename ;
  json_to_file (sk_list_json alias_pkh_pk_ck_list) sk_filename ;
  json_to_file (ck_list_json alias_pkh_pk_ck_list) ck_filename

(** Assuming that the [keys_list] is sorted in descending order, the
    function extracts the first keys until reaching the limit of
    [total_stake] by a percentage of [share]. *)
let filter_up_to_staking_share share total_stake to_mutez keys_list =
  let total_stake = to_mutez total_stake in
  match share with
  | None ->
      List.map
        (fun (pkh, pk, consensus_key, companion_key, stb, frz, unstk_frz) ->
          ( pkh,
            pk,
            consensus_key,
            companion_key,
            to_mutez stb,
            to_mutez frz,
            to_mutez unstk_frz ))
        keys_list
  | Some share ->
      let staking_amount_limit =
        Int64.add (Int64.mul (Int64.div total_stake 100L) share) 100L
      in
      Format.printf
        "@[<v>@[<h>Total staking amount:@;<7 0>%Ld mutez@]@,"
        total_stake ;
      Format.printf
        "@[Staking amount limit (%Ld%%): ~%Ld mutez@]@]@."
        share
        staking_amount_limit ;
      let rec loop ((keys_acc, stb_acc) as acc) = function
        | [] -> acc
        | (pkh, pk, consensus_key, companion_key, stb, frz, unstk_frz) :: l ->
            if Compare.Int64.(stb_acc > staking_amount_limit) then acc
              (* Stop whenever the limit is exceeded. *)
            else
              loop
                ( ( pkh,
                    pk,
                    consensus_key,
                    companion_key,
                    to_mutez stb,
                    to_mutez frz,
                    to_mutez unstk_frz )
                  :: keys_acc,
                  Int64.add (to_mutez stb) stb_acc )
                l
      in
      loop ([], 0L) keys_list |> fst |> List.rev

let get_consensus_keys (type ctxt pkh)
    (module P : Sigs.PROTOCOL
      with type context = ctxt
       and type Signature.public_key_hash = pkh) (ctxt : ctxt) pk (pkh : pkh) =
  let open Lwt_result_syntax in
  let* cpk, pending_cpks = P.Delegate.consensus_keys ctxt pkh in
  let cpk = P.Signature.To_latest.public_key cpk in
  let pending_cpks =
    List.map
      (fun pending_pk ->
        let pending_pk = P.Signature.To_latest.public_key pending_pk in
        let pending_pkh = Tezos_crypto.Signature.Public_key.hash pending_pk in
        (pending_pkh, pending_pk))
      pending_cpks
  in
  if Tezos_crypto.Signature.Public_key.equal pk cpk then
    return (None, pending_cpks)
  else
    let cpkh = Tezos_crypto.Signature.Public_key.hash cpk in
    return (Some (cpkh, cpk), pending_cpks)

let get_companion_keys (type ctxt pkh)
    (module P : Sigs.PROTOCOL
      with type context = ctxt
       and type Signature.public_key_hash = pkh) (ctxt : ctxt) (pkh : pkh) =
  let open Lwt_result_syntax in
  let* cpk, pending_cpks = P.Delegate.companion_keys ctxt pkh in
  let pending_cpks =
    List.map
      (fun pending_pk ->
        let pending_pkh =
          Tezos_crypto.Signature.Bls.Public_key.hash pending_pk
        in
        (pending_pkh, pending_pk))
      pending_cpks
  in
  match cpk with
  | Some cpk ->
      let cpkh = Tezos_crypto.Signature.Bls.Public_key.hash cpk in
      return (Some (cpkh, cpk), pending_cpks)
  | None -> return (None, pending_cpks)

let get_delegates_and_accounts (module P : Sigs.PROTOCOL) context
    (header : Block_header.shell_header) active_bakers_only staking_share_opt
    accounts_pkh_lists =
  let open Lwt_result_syntax in
  let level = header.Block_header.level in
  let predecessor_timestamp = header.timestamp in
  let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
  let* ctxt =
    P.prepare_context context ~level ~predecessor_timestamp ~timestamp
  in
  let* accounts =
    List.filter_map_es
      (fun pkh ->
        let*! pk_opt =
          Option.map_es
            (P.Contract.get_manager_key ctxt)
            (P.Signature.Of_latest.public_key_hash pkh)
        in
        match pk_opt with
        | Ok (Some pk) ->
            return (Some (pkh, P.Signature.To_latest.public_key pk))
        | Ok None | Error _ -> return_none)
      accounts_pkh_lists
  in
  (* Loop on delegates to extract keys and compute the total stake. *)
  let* delegates, total_stake =
    P.Delegate.fold
      ctxt
      ~order:`Sorted
      ~init:(Ok ([], P.Tez.zero))
      ~f:(fun pkh acc ->
        let* pk =
          P.Delegate.pubkey ctxt pkh
          |> Lwt_result.map P.Signature.To_latest.public_key
        in
        let* active_consensus_key, pending_consensus_keys =
          get_consensus_keys (module P) ctxt pk pkh
        in
        let* active_companion_key, pending_companion_keys =
          get_companion_keys (module P) ctxt pkh
        in
        let*? key_list_acc, staking_balance_acc = acc in
        let* staking_balance = P.Delegate.staking_balance ctxt pkh in
        let* frozen_deposits = P.Delegate.current_frozen_deposits ctxt pkh in
        let* unstaked_frozen_deposits =
          P.Delegate.unstaked_frozen_deposits ctxt pkh
        in
        let*? updated_staking_balance_acc =
          P.Tez.(staking_balance_acc +? staking_balance)
        in
        let staking_balance_info :
            Signature.public_key_hash
            * Signature.public_key
            * ((Signature.public_key_hash * Signature.public_key) option
              * (Signature.public_key_hash * Signature.public_key) list)
            * ((Signature.Bls.Public_key_hash.t * Bls12_381_signature.MinPk.pk)
               option
              * (Signature.Bls.Public_key_hash.t * Bls12_381_signature.MinPk.pk)
                list)
            * P.Tez.t
            * P.Tez.t
            * P.Tez.t =
          ( P.Signature.To_latest.public_key_hash pkh,
            pk,
            (active_consensus_key, pending_consensus_keys),
            (active_companion_key, pending_companion_keys),
            staking_balance,
            frozen_deposits,
            unstaked_frozen_deposits )
        in
        (* Filter deactivated bakers if required *)
        if active_bakers_only then
          let* b = P.Delegate.deactivated ctxt pkh in
          match b with
          (* Ignore the baker. *)
          | true -> return (key_list_acc, staking_balance_acc)
          (* Consider the baker. *)
          | false ->
              return
                ( staking_balance_info :: key_list_acc,
                  updated_staking_balance_acc )
        else
          return
            (staking_balance_info :: key_list_acc, updated_staking_balance_acc))
  in
  return
    ( filter_up_to_staking_share staking_share_opt total_stake P.Tez.to_mutez
      @@
      (* By swapping x and y we do a descending sort *)
      List.sort
        (fun (_, _, _, _, x, _, _) (_, _, _, _, y, _, _) -> P.Tez.compare y x)
        delegates,
      accounts )

type contract_info = {
  address : string;
  balance : int64;
  frozen_bonds : int64;
  staked_balance : int64;
  unstaked_frozen_balance : int64;
  unstaked_finalizable_balance : int64;
}

let get_contracts (module P : Sigs.PROTOCOL) ?dump_contracts context
    (header : Block_header.shell_header) =
  let open Lwt_result_syntax in
  let level = header.Block_header.level in
  let predecessor_timestamp = header.timestamp in
  let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
  let* ctxt =
    P.prepare_context context ~level ~predecessor_timestamp ~timestamp
  in
  (* Loop on commitments to compute the total unclaimed funds *)
  let* total_commitments, nb_commitments =
    P.Commitment.fold
      ctxt
      ~order:`Undefined
      ~init:(Ok (0L, 0))
      ~f:(fun _ r acc ->
        let*? acc, nb_commitments = acc in
        return @@ (Int64.add r acc, nb_commitments + 1))
  in
  Format.printf "@[<v>Read %d commitments@]@." nb_commitments ;
  (* Loop on contracts to compute the total supply in contracts *)
  let* contracts, total_info, nb_account, nb_failures =
    P.Contract.fold
      ctxt
      ~init:
        (Ok
           ( [],
             {
               address = "Total supply";
               balance = 0L;
               frozen_bonds = 0L;
               staked_balance = 0L;
               unstaked_frozen_balance = 0L;
               unstaked_finalizable_balance = 0L;
             },
             0,
             0 ))
      ~f:(fun contract acc ->
        let*? contract_list_acc, total_info_acc, nb_account, nb_failures =
          acc
        in
        try
          let address = P.Contract.contract_address contract in
          let* balance = P.Contract.balance ctxt contract in
          let* frozen_bonds = P.Contract.frozen_bonds ctxt contract in
          let* staked_balance_opt =
            P.Contract.get_staked_balance ctxt contract
          in
          let* unstaked_frozen_balance_opt =
            P.Contract.get_unstaked_frozen_balance ctxt contract
          in
          let* unstaked_finalizable_balance_opt =
            P.Contract.get_unstaked_finalizable_balance ctxt contract
          in
          let staked_balance =
            match staked_balance_opt with
            | None -> 0L
            | Some staked_balance_opt -> P.Tez.to_mutez staked_balance_opt
          in
          let unstaked_frozen_balance =
            match unstaked_frozen_balance_opt with
            | None -> 0L
            | Some unstaked_frozen_balance_opt ->
                P.Tez.to_mutez unstaked_frozen_balance_opt
          and unstaked_finalizable_balance =
            match unstaked_finalizable_balance_opt with
            | None -> 0L
            | Some unstaked_finalizable_balance_opt ->
                P.Tez.to_mutez unstaked_finalizable_balance_opt
          in
          (* Unused but make sure the call works *)
          ignore (P.Contract.get_full_balance ctxt contract) ;
          let total_info_acc =
            {
              address = "Total supply";
              balance =
                Int64.add (P.Tez.to_mutez balance) total_info_acc.balance;
              frozen_bonds =
                Int64.add
                  (P.Tez.to_mutez frozen_bonds)
                  total_info_acc.frozen_bonds;
              staked_balance =
                Int64.add staked_balance total_info_acc.staked_balance;
              unstaked_frozen_balance =
                Int64.add
                  unstaked_frozen_balance
                  total_info_acc.unstaked_frozen_balance;
              unstaked_finalizable_balance =
                Int64.add
                  unstaked_finalizable_balance
                  total_info_acc.unstaked_finalizable_balance;
            }
          in

          let total_supply_info : contract_info =
            {
              address;
              balance = P.Tez.to_mutez balance;
              frozen_bonds = P.Tez.to_mutez frozen_bonds;
              staked_balance;
              unstaked_frozen_balance;
              unstaked_finalizable_balance;
            }
          in
          return
            ( total_supply_info :: contract_list_acc,
              total_info_acc,
              nb_account + 1,
              nb_failures )
        with _ ->
          return (contract_list_acc, total_info_acc, nb_account, nb_failures + 1))
  in
  let circulating_supply =
    Int64.(
      add
        total_info.balance
        (add total_info.unstaked_finalizable_balance total_commitments))
  in
  let computed_frozen_supply =
    Int64.(
      add
        total_info.frozen_bonds
        (add total_info.staked_balance total_info.unstaked_frozen_balance))
  in
  let total_computed_supply =
    Int64.(add circulating_supply computed_frozen_supply)
  in
  let* estimated_total_supply = P.Contract.total_supply ctxt in

  Format.printf
    "@[<v>Read %d contracts with %d failures @]@;\n\
     @[<v>Computed total commitments: .................. %16Ld@]@;\
     @[<v>Computed total spendable balance: ............ %16Ld@]@;\
     @[<v>Computed total unstaked finalizable balance: . %16Ld@]@;\
     @[----------------------------------------------------------------@]@;\
     @[<v>Computed circulating supply: ................. %16Ld@]@;\
     @\n\
     @[<v>Computed total frozen bonds: ................. %16Ld@]@;\
     @[<v>Computed total staked balance: ............... %16Ld@]@;\
     @[<v>Computed total unstaked frozen balance: ...... %16Ld@]@;\
     @[----------------------------------------------------------------@]@;\
     @[<v>Computed frozen supply: ...................... %16Ld@]@;\
     @\n\
     @[----------------------------------------------------------------@]@;\
     @[<v>Computed total supply: ....................... %16Ld@]@;\
     @[<v>Estimated total supply: ...................... %16Ld@]@;\
     @[----------------------------------------------------------------@]@;\
     @\n\
     @[<v>Computed - Estimated total supply: ........... %16Ld@]@."
    nb_account
    nb_failures
    total_commitments
    total_info.balance
    total_info.unstaked_finalizable_balance
    circulating_supply
    total_info.frozen_bonds
    total_info.staked_balance
    total_info.unstaked_frozen_balance
    computed_frozen_supply
    total_computed_supply
    (P.Tez.to_mutez estimated_total_supply)
    (Int64.sub total_computed_supply (P.Tez.to_mutez estimated_total_supply)) ;
  match dump_contracts with
  | Some _ -> return @@ (total_info :: contracts)
  | None -> return []

let protocol_of_hash protocol_hash =
  List.find
    (fun (module P : Sigs.PROTOCOL) -> Protocol_hash.equal P.hash protocol_hash)
    (Known_protocols.get_all ())

let supported_networks =
  List.map
    (fun (alias, config) ->
      (alias, config.Octez_node_config.Config_file.genesis))
    Octez_node_config.Config_file.builtin_blockchain_networks

(** load mainnet store from [base_dir]
*)
let genesis ~network =
  let open Lwt_result_syntax in
  if String.starts_with ~prefix:"http" network then
    let* {genesis; _} =
      Octez_node_config.Shared_arg.load_config_from_url (Uri.of_string network)
    in
    return genesis
  else
    Option.value_f ~default:(fun () ->
        Stdlib.failwith
        @@ Format.asprintf
             "@[Unkown network alias %s.@,Known networks are @[%a @]@]"
             network
             Format.(pp_print_list pp_print_string)
             (List.map (fun (alias, _) -> alias) supported_networks))
    @@ List.assoc ~equal:String.equal network supported_networks
    |> return

let get_context ?level ~network_opt base_dir =
  let open Lwt_result_syntax in
  let open Tezos_store in
  let* genesis = genesis ~network:network_opt in
  let* store =
    Lwt.catch
      (fun () ->
        Tezos_store.Store.init
          ~store_dir:(Filename.concat base_dir "store")
          ~data_dir:base_dir
          ~allow_testchains:true
          ~readonly:true
          genesis)
      (fun exn ->
        Format.eprintf
          "An error occured while initialising the store. It usually happens \
           when using the wrong network alias.\n\
           Network alias used was \"%s\".@."
          network_opt ;
        tzfail (Exn exn))
  in
  let main_chain_store = Store.main_chain_store store in
  let*! block =
    match level with
    | None -> Tezos_store.Store.Chain.current_head main_chain_store
    | Some level -> (
        Printf.printf "Loading block at level %ld@." level ;
        let*! block =
          Store.Block.read_block_by_level_opt main_chain_store level
        in
        match block with
        | None ->
            Printf.printf "Level %ld not found" level ;
            exit 1
        | Some block -> Lwt.return block)
  in
  Format.printf
    "@[<h>Head block:@;<17 0>%a@] @.@[<h>Head block level:@;<11 0>%ld@] @."
    Block_hash.pp
    (Tezos_store.Store.Block.hash block)
    (Tezos_store.Store.Block.level block) ;
  let header = Store.Block.header block in
  let*! context =
    let*! r = Store.Block.context_exn main_chain_store block in
    Lwt.return r
  in
  let*! protocol_hash = Store.Block.protocol_hash_exn main_chain_store block in
  let header = header.shell in
  return (protocol_hash, context, header, store, block)

let load_cache (module P : Sigs.PROTOCOL) context store block =
  let open Lwt_result_syntax in
  let open Tezos_store in
  let predecessor_context = context in
  let chain_store = Store.main_chain_store store in
  let chain_id = Store.Chain.chain_id chain_store in
  let Block_header.
        {
          timestamp = predecessor_timestamp;
          level = predecessor_level;
          fitness = predecessor_fitness;
          _;
        } =
    Store.Block.shell_header block
  in
  let predecessor = Store.Block.hash block in
  let timestamp = Time.System.to_protocol (Time.System.now ()) in
  let* value_of_key =
    P.value_of_key
      ~chain_id
      ~predecessor_context
      ~predecessor_timestamp
      ~predecessor_level
      ~predecessor_fitness
      ~predecessor
      ~timestamp
  in
  Tezos_protocol_environment.Context.load_cache
    predecessor
    predecessor_context
    `Lazy
    value_of_key

let unexpected_protocol protocol_hash =
  if
    protocol_hash
    = Protocol_hash.of_b58check_exn
        "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
  then
    Error_monad.failwith
      "Context was probably ill loaded, found Genesis protocol.@;\
       Known protocols are: %a"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
          Protocol_hash.pp)
      (List.map (fun (module P : Sigs.PROTOCOL) -> P.hash)
      @@ Known_protocols.get_all ())
  else
    Error_monad.failwith
      "Unknown protocol hash: %a.@;Known protocols are: %a"
      Protocol_hash.pp
      protocol_hash
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
          Protocol_hash.pp)
      (List.map (fun (module P : Sigs.PROTOCOL) -> P.hash)
      @@ Known_protocols.get_all ())

let get_context_with_loaded_cache ?level ~network_opt base_dir =
  let open Lwt_result_syntax in
  let* protocol_hash, context, header, store, block =
    get_context ?level ~network_opt base_dir
  in
  match protocol_of_hash protocol_hash with
  | None -> unexpected_protocol protocol_hash
  | Some protocol ->
      Format.printf "@[<h>Detected protocol:@;<10 0>%a@]@." pp_protocol protocol ;
      let* context = load_cache protocol context store block in
      return (protocol, context, header, store)

exception
  Done of (Signature.public_key_hash * Signature.public_key * int64) list

let get_rich_accounts (module P : Sigs.PROTOCOL) context
    (header : Block_header.shell_header) ~count ~min_threshold =
  let open Lwt_result_syntax in
  let level = header.Block_header.level in
  let predecessor_timestamp = header.timestamp in
  let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
  let* context =
    P.prepare_context context ~level ~predecessor_timestamp ~timestamp
  in
  Format.printf "Searching %d accounts over %Ld tez@." count min_threshold ;
  (* Convert to mutez *)
  let min_threshold_tz =
    P.Tez.of_mutez_exn (Int64.mul min_threshold 1_000_000L)
  in
  Tezos_stdlib_unix.Animation.(
    three_dots ~progress_display_mode:Auto ~msg:"Loading rich accounts")
    (fun () ->
      let* accounts =
        Lwt.catch
          (fun () ->
            P.Contract.fold context ~init:(Ok []) ~f:(fun contract acc ->
                let*? acc in
                let* balance = P.Contract.balance context contract in
                if balance >= min_threshold_tz then
                  let pkh = P.Contract.contract_address contract in
                  match
                    Tezos_crypto.Signature.V_latest.Public_key_hash
                    .of_b58check_opt
                      pkh
                  with
                  | None -> return acc
                  | Some k -> (
                      let pkh =
                        match P.Signature.Of_latest.public_key_hash k with
                        | None ->
                            (* Not expected at all. *)
                            assert false
                        | Some k -> k
                      in
                      let*! pk = P.Contract.get_manager_key context pkh in
                      match pk with
                      | Error _ ->
                          (* Can fail for missing_manager_contract or
                             Unrevealed_manager_key errors. Ignoring these
                             accounts. *)
                          return acc
                      | Ok pk ->
                          let res =
                            ( P.Signature.To_latest.public_key_hash pkh,
                              P.Signature.To_latest.public_key pk,
                              P.Tez.to_mutez balance )
                            :: acc
                          in
                          if List.length res >= count then raise (Done res)
                          else return res)
                else return acc))
          (function
            | Done res -> return res
            | e -> failwith "Unexpected error: %s@." (Printexc.to_string e))
      in
      let sorted_accounts =
        List.sort (fun (_, _, x) (_, _, y) -> Int64.compare y x) accounts
      in
      let selected_accounts =
        Tezos_stdlib.TzList.take_n count sorted_accounts
      in
      let res =
        List.mapi
          (fun i (pkh, pk, tez) ->
            let alias = Format.sprintf "rich_%d" i in
            (alias, pkh, pk, tez))
          selected_accounts
      in
      Format.printf
        "Extracted the %d accounts with a spendable balance over %Ldꜩ:@."
        count
        min_threshold ;
      List.iter
        (fun (alias, pkh, _, tez) ->
          Format.printf
            "%s (%s): %Ldꜩ@."
            alias
            (Signature.Public_key_hash.to_b58check pkh)
            (Int64.div tez 1_000_000L))
        res ;
      return res)

(** [load_bakers_public_keys ?staking_share_opt ?network_opt ?level
    ~active_backers_only base_dir alias_phk_pk_list] checkouts the head context
    at the given [base_dir] and computes a list of [(alias, pkh, pk, stake,
    frozen_deposits, unstake_frozen_deposits)] corresponding to all delegates in
    that context. The [alias] for the delegates are gathered from
    [alias_pkh_pk_list]).

    if [active_bakers_only] then the deactivated delegates are
    filtered out of the list. if an optional [level] is given, use the
    context from this level instead of head, if it exists.
*)
let load_bakers_public_keys ?staking_share_opt ?(network_opt = "mainnet") ?level
    ?rich_accounts_over ~active_bakers_only base_dir alias_pkh_pk_list
    other_accounts_pkh =
  let open Lwt_result_syntax in
  let* protocol, context, header, store =
    get_context_with_loaded_cache ?level ~network_opt base_dir
  in
  let* ( (delegates :
           (Signature.public_key_hash
           * Signature.public_key
           * ((Signature.public_key_hash * Signature.public_key) option
             * (Signature.public_key_hash * Signature.public_key) list)
           * ((Signature.Bls.Public_key_hash.t * Bls12_381_signature.MinPk.pk)
              option
             * (Signature.Bls.Public_key_hash.t * Bls12_381_signature.MinPk.pk)
               list)
           * int64
           * int64
           * int64)
           list),
         other_accounts ) =
    Format.printf "@[<h>Detected protocol:@;<10 0>%a@]@." pp_protocol protocol ;
    get_delegates_and_accounts
      protocol
      context
      header
      active_bakers_only
      staking_share_opt
      other_accounts_pkh
  in
  let with_alias =
    List.fold_left_i
      (fun i
           acc
           ( pkh,
             pk,
             (active_consensus_key, pending_consensus_keys),
             (active_companion_key, pending_companion_keys),
             stake,
             frozen_deposits,
             unstake_frozen_deposits )
         ->
        let to_b58check (pkh, pk) =
          ( Tezos_crypto.Signature.Public_key_hash.to_b58check pkh,
            Tezos_crypto.Signature.Public_key.to_b58check pk )
        in
        let pkh, pk = to_b58check (pkh, pk) in
        let active_consensus_key, consensus_keys =
          let pendings = List.map to_b58check pending_consensus_keys in
          match active_consensus_key with
          | Some keys ->
              let keys = to_b58check keys in
              (Some keys, keys :: pendings)
          | None -> (None, pendings)
        in
        let alias =
          List.find_map
            (fun (alias, pkh', _) ->
              if String.equal pkh' pkh then Some alias else None)
            alias_pkh_pk_list
        in
        let alias =
          Option.value_f alias ~default:(fun () -> Format.asprintf "baker_%d" i)
        in
        let delegate_alias =
          ( alias,
            pkh,
            pk,
            active_consensus_key,
            stake,
            frozen_deposits,
            unstake_frozen_deposits )
        in
        let consensus_keys_alias =
          List.mapi
            (fun i (cpkh, cpk) ->
              let alias = Format.asprintf "%s_consensus_key_%d" alias i in
              (alias, cpkh, cpk, None, 0L, 0L, 0L))
            consensus_keys
        in
        let companion_keys_alias =
          let to_b58check (pkh, pk) =
            ( Tezos_crypto.Signature.Bls.Public_key_hash.to_b58check pkh,
              Tezos_crypto.Signature.Bls.Public_key.to_b58check pk )
          in
          let companion_keys =
            let pendings = List.map to_b58check pending_companion_keys in
            match active_companion_key with
            | Some keys -> to_b58check keys :: pendings
            | None -> pendings
          in
          List.mapi
            (fun i (cpkh, cpk) ->
              let alias = Format.asprintf "%s_companion_key_%d" alias i in
              (alias, cpkh, cpk, None, 0L, 0L, 0L))
            companion_keys
        in
        companion_keys_alias @ consensus_keys_alias @ (delegate_alias :: acc))
      []
      delegates
    |> List.rev
  in
  let* rich_accounts =
    match rich_accounts_over with
    | Some (count, min) ->
        let* r =
          get_rich_accounts protocol context header ~count ~min_threshold:min
        in
        List.map
          (fun (alias, pkh, pk, tez) ->
            ( alias,
              Tezos_crypto.Signature.Public_key_hash.to_b58check pkh,
              Tezos_crypto.Signature.Public_key.to_b58check pk,
              tez ))
          r
        |> return
    | None -> return []
  in
  let other_accounts =
    List.map
      (fun (pkh, pk) ->
        let pkh = Tezos_crypto.Signature.Public_key_hash.to_b58check pkh in
        let pk = Tezos_crypto.Signature.Public_key.to_b58check pk in
        let alias =
          List.find_map
            (fun (alias, pkh', _) ->
              if String.equal pkh' pkh then Some alias else None)
            alias_pkh_pk_list
        in
        let alias = Option.value alias ~default:pkh in
        (alias, pkh, pk))
      other_accounts
  in
  let*! () = Tezos_store.Store.close_store store in
  return (with_alias, rich_accounts, other_accounts)

(** [load_contracts ?dump_contracts ?network ?level base_dir] checkouts the block
    context at the given [base_dir] (at level [?level] or defaulting
    to head) and computes the total supply of tez at this level
    (reading all contracts and commitments).
*)
let load_contracts ?dump_contracts ?(network_opt = "mainnet") ?level base_dir =
  let open Lwt_result_syntax in
  let* protocol, context, header, store =
    get_context_with_loaded_cache ?level ~network_opt base_dir
  in
  let* (contracts : contract_info list) =
    Format.printf "@[<h>Detected protocol:@;<10 0>%a@]@." pp_protocol protocol ;
    get_contracts ?dump_contracts protocol context header
  in
  let*! () = Tezos_store.Store.close_store store in
  return contracts

let build_yes_wallet ?staking_share_opt ?network_opt ?rich_accounts_over
    base_dir ~active_bakers_only ~aliases ~other_accounts_pkh =
  let open Lwt_result_syntax in
  let+ bakers, rich_accounts, other_accounts =
    load_bakers_public_keys
      ?staking_share_opt
      ?network_opt
      ?rich_accounts_over
      base_dir
      ~active_bakers_only
      aliases
      other_accounts_pkh
    (* get rid of stake *)
  in
  List.map
    (fun (alias, pkh, pk, ck, _stake, _, _) -> (alias, pkh, pk, ck))
    bakers
  @ List.map
      (fun (alias, pkh, pk, _) ->
        (* Consensus keys are not supported in [rich_accounts]. Setting it to
           None. *)
        (alias, pkh, pk, None))
      rich_accounts
  @ List.map
      (fun (alias, pkh, pk) ->
        (* Consensus keys are not supported in [other_accounts]. Setting it to
           None. *)
        (alias, pkh, pk, None))
      other_accounts
