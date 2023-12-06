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
   - creates a 'yes-wallet' directory to be passed to tezos-client -d option
 *)

let pp_protocol ppf (module P : Sigs.PROTOCOL) = Protocol_hash.pp ppf P.hash

let pkh_json (alias, pkh, _pk) =
  Ezjsonm.(dict [("name", string alias); ("value", string pkh)])

let pk_json (alias, _pkh, pk) =
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

let sk_of_pk (pk_s : string) : string =
  let open Tezos_crypto.Signature.V_latest in
  let pk = Public_key.of_b58check_exn pk_s in
  let pk_b = Data_encoding.Binary.to_bytes_exn Public_key.encoding pk in
  let sk_b = Bytes.sub pk_b 0 33 in
  let sk = Data_encoding.Binary.of_bytes_exn Secret_key.encoding sk_b in
  let sk_s = Secret_key.to_b58check sk in
  sk_s

let sk_json (alias, _pkh, pk) =
  Ezjsonm.(
    dict
      [
        ("name", string alias); ("value", string @@ "unencrypted:" ^ sk_of_pk pk);
      ])

let map_bind_to_json f list = Ezjsonm.list f list

let pkh_list_json list = map_bind_to_json pkh_json list

let pk_list_json list = map_bind_to_json pk_json list

let sk_list_json list = map_bind_to_json sk_json list

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
  map_bind_to_json (fun (alias, pk_s) -> sk_json (alias, alias, pk_s)) list

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

(* Creaye a yes-wallet in [dest] using the alias list [alias_pkh_pk_list]. *)
let write_yes_wallet dest alias_pkh_pk_list =
  let pkh_filename = Filename.concat dest "public_key_hashs" in
  let pk_filename = Filename.concat dest "public_keys" in
  let sk_filename = Filename.concat dest "secret_keys" in
  if not (Sys.file_exists dest) then Unix.mkdir dest 0o750 ;
  json_to_file (pkh_list_json alias_pkh_pk_list) pkh_filename ;
  json_to_file (pk_list_json alias_pkh_pk_list) pk_filename ;
  json_to_file (sk_list_json alias_pkh_pk_list) sk_filename

(** Assuming that the [keys_list] is sorted in descending order, the
    function extracts the first keys until reaching the limit of
    [total_stake] by a percentage of [share]. *)
let filter_up_to_staking_share share total_stake to_mutez keys_list =
  let total_stake = to_mutez total_stake in
  match share with
  | None -> List.map (fun (pkh, pk, stb) -> (pkh, pk, to_mutez stb)) keys_list
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
        | (pkh, pk, stb) :: l ->
            if Compare.Int64.(stb_acc > staking_amount_limit) then acc
              (* Stop whenever the limit is exceeded. *)
            else
              loop
                ( (pkh, pk, to_mutez stb) :: keys_acc,
                  Int64.add (to_mutez stb) stb_acc )
                l
      in
      loop ([], 0L) keys_list |> fst |> List.rev

let get_delegates (module P : Sigs.PROTOCOL) context
    (header : Block_header.shell_header) active_bakers_only staking_share_opt =
  let open Lwt_result_syntax in
  let level = header.Block_header.level in
  let predecessor_timestamp = header.timestamp in
  let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
  let* ctxt =
    P.prepare_context context ~level ~predecessor_timestamp ~timestamp
  in
  (* Loop on delegates to extract keys and compute the total stake. *)
  let* delegates, total_stake =
    P.Delegate.fold
      ctxt
      ~order:`Sorted
      ~init:(Ok ([], P.Tez.zero))
      ~f:(fun pkh acc ->
        let* pk = P.Delegate.pubkey ctxt pkh in
        let*? key_list_acc, staking_balance_acc = acc in
        let* staking_balance = P.Delegate.staking_balance ctxt pkh in
        let*? updated_staking_balance_acc =
          P.Tez.(staking_balance_acc +? staking_balance)
        in
        let staking_balance_info =
          ( P.Signature.To_latest.public_key_hash pkh,
            P.Signature.To_latest.public_key pk,
            staking_balance )
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
  @@ filter_up_to_staking_share staking_share_opt total_stake P.Tez.to_mutez
  @@ (* By swapping x and y we do a descending sort *)
  List.sort (fun (_, _, x) (_, _, y) -> P.Tez.compare y x) delegates

let protocol_of_hash protocol_hash =
  List.find
    (fun (module P : Sigs.PROTOCOL) -> Protocol_hash.equal P.hash protocol_hash)
    (Known_protocols.get_all ())

(** load mainnet store from [base_dir]
*)
let genesis ~network =
  match network with
  | `Mainnet ->
      {
        Genesis.time = Time.Protocol.of_notation_exn "2018-06-30T16:07:32Z";
        block =
          Block_hash.of_b58check_exn
            "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2";
        protocol =
          Protocol_hash.of_b58check_exn
            "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
      }
  | `Ghostnet ->
      {
        Genesis.time = Time.Protocol.of_notation_exn "2022-01-25T15:00:00Z";
        block =
          Block_hash.of_b58check_exn
            "BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9";
        protocol =
          Protocol_hash.of_b58check_exn
            "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
      }

(** [load_mainnet_bakers_public_keys base_dir active_backers_only
    alias_phk_pk_list] checkouts the head context at the given
    [base_dir] and computes a list of triples [(alias, pkh, pk)]
    corresponding to all delegates in that context. The [alias] for
    the delegates are gathered from [alias_pkh_pk_list]).

    if [active_bakers_only] then the deactivated delegates are filtered out of
    the list.
*)
let load_bakers_public_keys ?(staking_share_opt = None)
    ?(network_opt = `Mainnet) base_dir ~active_bakers_only alias_pkh_pk_list =
  let open Lwt_result_syntax in
  let open Tezos_store in
  let genesis = genesis ~network:network_opt in
  let* store =
    Tezos_store.Store.init
      ~store_dir:(Filename.concat base_dir "store")
      ~context_dir:(Filename.concat base_dir "context")
      ~allow_testchains:true
      ~readonly:true
      genesis
  in
  let main_chain_store = Store.main_chain_store store in
  let*! block = Tezos_store.Store.Chain.current_head main_chain_store in
  Format.printf
    "@[<h>Head block:@;<17 0>%a@]@."
    Block_hash.pp
    (Tezos_store.Store.Block.hash block) ;
  let header = Store.Block.header block in
  let*! context =
    let*! r = Store.Block.context_exn main_chain_store block in
    Lwt.return r
  in
  let*! protocol_hash = Store.Block.protocol_hash_exn main_chain_store block in
  let header = header.shell in
  let* delegates =
    match protocol_of_hash protocol_hash with
    | None ->
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
    | Some protocol ->
        Format.printf
          "@[<h>Detected protocol:@;<10 0>%a@]@."
          pp_protocol
          protocol ;
        get_delegates
          protocol
          context
          header
          active_bakers_only
          staking_share_opt
  in
  let*! () = Tezos_store.Store.close_store store in
  return
  @@ List.mapi
       (fun i (pkh, pk, stake) ->
         let pkh = Tezos_crypto.Signature.Public_key_hash.to_b58check pkh in
         let pk = Tezos_crypto.Signature.Public_key.to_b58check pk in
         let alias =
           List.find_map
             (fun (alias, pkh', _) ->
               if String.equal pkh' pkh then Some alias else None)
             alias_pkh_pk_list
         in
         let alias =
           Option.value_f alias ~default:(fun () ->
               Format.asprintf "baker_%d" i)
         in
         (alias, pkh, pk, stake))
       delegates

let build_yes_wallet ?staking_share_opt ?network_opt base_dir
    ~active_bakers_only ~aliases =
  let open Lwt_result_syntax in
  let+ mainnet_bakers =
    load_bakers_public_keys
      ?staking_share_opt
      ?network_opt
      base_dir
      ~active_bakers_only
      aliases
    (* get rid of stake *)
  in
  List.map (fun (alias, pkh, pk, _stake) -> (alias, pkh, pk)) mainnet_bakers
