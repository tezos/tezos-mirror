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

open Tezos_base.TzPervasives

type protocol = Florence | Granada | Hangzhou | Ithaca | Alpha

(*
   dune exec scripts/yes-wallet/yes-wallet.exe

   Given a list of aliases and public key hashes:
   - encodes each public key as a fake secret key that can be used
     with the yes-node.patch
   - creates a 'yes-wallet' directory to be passed to tezos-client -d option
 *)

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
  let open Tezos_crypto.Signature in
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

let alias_pkh_pk_list =
  [
    ( "foundation1",
      "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9",
      "p2pk67wVncLFS1DQDm2gVR45sYCzQSXTtqn3bviNYXVCq6WRoqtxHXL" );
    ( "foundation2",
      "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5",
      "p2pk66n1NmhPDEkcf9sXEKe9kBoTwBoTYxke1hx16aTRVq8MoXuwNqo" );
    ( "foundation3",
      "tz3RB4aoyjov4KEVRbuhvQ1CKJgBJMWhaeB8",
      "p2pk67NECc8vGK4eLbXGEgBZGhk53x1pCMbgnFEgLxZEMGDtzVcFQok" );
    ( "foundation4",
      "tz3bTdwZinP8U1JmSweNzVKhmwafqWmFWRfk",
      "p2pk6796esaR3dNr8jUx8S7xxZdRvpYSrhHMg6NagjwMRJHsERMiUKM" );
    ( "foundation5",
      "tz3NExpXn9aPNZPorRE4SdjJ2RGrfbJgMAaV",
      "p2pk66iTZwLmRPshQgUr2HE3RUzSFwAN5MNaBQ5rfduT1dGKXd25pNN" );
    ( "foundation6",
      "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r",
      "p2pk65ffAqpYT6Et73DXdNqudthwmSNzNyzL3Wdn2EYuiiMwoPu6vFJ" );
    ( "foundation7",
      "tz3WMqdzXqRWXwyvj5Hp2H7QEepaUuS7vd9K",
      "p2pk67Cwb5Ke6oSmqeUbJxURXMe3coVnH9tqPiB2xD84CYhHbBKs4oM" );
    ( "foundation8",
      "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN",
      "p2pk67uapBxwkM1JNasGJ6J3rozzYELgrtcqxKZwZLjvsr4XcAr4FqC" );
  ]

let get_delegates (proto : protocol) context
    (header : Block_header.shell_header) active_bakers_only =
  let level = header.Block_header.level in
  let predecessor_timestamp = header.timestamp in
  let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
  let fitness = header.fitness in
  match proto with
  | Florence ->
      let open Tezos_protocol_009_PsFLoren.Protocol in
      Alpha_context.prepare
        context
        ~level
        ~predecessor_timestamp
        ~timestamp
        ~fitness
      >|= Environment.wrap_tzresult
      >>=? fun (ctxt, _) ->
      Alpha_context.Delegate.fold ctxt ~init:(ok []) ~f:(fun pkh acc ->
          Alpha_context.Roll.delegate_pubkey ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun pk ->
          acc >>?= fun acc ->
          Alpha_context.Delegate.staking_balance ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun staking_balance ->
          (* Filter deactivated bakers if required *)
          if active_bakers_only then
            Alpha_context.Delegate.deactivated ctxt pkh
            >|= Environment.wrap_tzresult
            >>=? function
            | true -> return acc
            | false -> return ((pkh, pk, staking_balance) :: acc)
          else return ((pkh, pk, staking_balance) :: acc))
      >>=? fun delegates ->
      return
      @@ List.map (fun (pkh, pk, _) -> (pkh, pk))
      @@ (* By swapping x and y we do a descending sort *)
      List.sort
        (fun (_, _, x) (_, _, y) -> Alpha_context.Tez.compare y x)
        delegates
  | Granada ->
      let open Tezos_protocol_010_PtGRANAD.Protocol in
      Alpha_context.prepare
        context
        ~level
        ~predecessor_timestamp
        ~timestamp
        ~fitness
      >|= Environment.wrap_tzresult
      >>=? fun (ctxt, _, _) ->
      Alpha_context.Delegate.fold ctxt ~init:(ok []) ~f:(fun pkh acc ->
          Alpha_context.Roll.delegate_pubkey ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun pk ->
          acc >>?= fun acc ->
          Alpha_context.Delegate.staking_balance ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun staking_balance ->
          (* Filter deactivated bakers if required *)
          if active_bakers_only then
            Alpha_context.Delegate.deactivated ctxt pkh
            >|= Environment.wrap_tzresult
            >>=? function
            | true -> return acc
            | false -> return ((pkh, pk, staking_balance) :: acc)
          else return ((pkh, pk, staking_balance) :: acc))
      >>=? fun delegates ->
      return
      @@ List.map (fun (pkh, pk, _) -> (pkh, pk))
      @@ (* By swapping x and y we do a descending sort *)
      List.sort
        (fun (_, _, x) (_, _, y) -> Alpha_context.Tez.compare y x)
        delegates
  | Hangzhou ->
      let open Tezos_protocol_011_PtHangz2.Protocol in
      Alpha_context.prepare
        context
        ~level
        ~predecessor_timestamp
        ~timestamp
        ~fitness
      >|= Environment.wrap_tzresult
      >>=? fun (ctxt, _, _) ->
      Alpha_context.Delegate.fold ctxt ~init:(ok []) ~f:(fun pkh acc ->
          Alpha_context.Roll.delegate_pubkey ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun pk ->
          acc >>?= fun acc ->
          Alpha_context.Delegate.staking_balance ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun staking_balance ->
          (* Filter deactivated bakers if required *)
          if active_bakers_only then
            Alpha_context.Delegate.deactivated ctxt pkh
            >|= Environment.wrap_tzresult
            >>=? function
            | true -> return acc
            | false -> return ((pkh, pk, staking_balance) :: acc)
          else return ((pkh, pk, staking_balance) :: acc))
      >>=? fun delegates ->
      return
      @@ List.map (fun (pkh, pk, _) -> (pkh, pk))
      @@ (* By swapping x and y we do a descending sort *)
      List.sort
        (fun (_, _, x) (_, _, y) -> Alpha_context.Tez.compare y x)
        delegates
  | Ithaca ->
      let open Tezos_protocol_012_PsiThaCa.Protocol in
      Alpha_context.prepare context ~level ~predecessor_timestamp ~timestamp
      >|= Environment.wrap_tzresult
      >>=? fun (ctxt, _, _) ->
      Alpha_context.Delegate.fold
        ctxt
        ~order:`Sorted
        ~init:(ok [])
        ~f:(fun pkh acc ->
          Alpha_context.Delegate.pubkey ctxt pkh >|= Environment.wrap_tzresult
          >>=? fun pk ->
          acc >>?= fun acc ->
          Alpha_context.Delegate.staking_balance ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun staking_balance ->
          (* Filter deactivated bakers if required *)
          if active_bakers_only then
            Alpha_context.Delegate.deactivated ctxt pkh
            >|= Environment.wrap_tzresult
            >>=? function
            | true -> return acc
            | false -> return ((pkh, pk, staking_balance) :: acc)
          else return ((pkh, pk, staking_balance) :: acc))
      >>=? fun delegates ->
      return
      @@ List.map (fun (pkh, pk, _) -> (pkh, pk))
      @@ (* By swapping x and y we do a descending sort *)
      List.sort
        (fun (_, _, x) (_, _, y) -> Alpha_context.Tez.compare y x)
        delegates
  | Alpha ->
      let open Tezos_protocol_alpha.Protocol in
      Alpha_context.prepare context ~level ~predecessor_timestamp ~timestamp
      >|= Environment.wrap_tzresult
      >>=? fun (ctxt, _, _) ->
      Alpha_context.Delegate.fold
        ctxt
        ~order:`Sorted
        ~init:(ok [])
        ~f:(fun pkh acc ->
          Alpha_context.Delegate.pubkey ctxt pkh >|= Environment.wrap_tzresult
          >>=? fun pk ->
          acc >>?= fun acc ->
          Alpha_context.Delegate.staking_balance ctxt pkh
          >|= Environment.wrap_tzresult
          >>=? fun staking_balance ->
          (* Filter deactivated bakers if required *)
          if active_bakers_only then
            Alpha_context.Delegate.deactivated ctxt pkh
            >|= Environment.wrap_tzresult
            >>=? function
            | true -> return acc
            | false -> return ((pkh, pk, staking_balance) :: acc)
          else return ((pkh, pk, staking_balance) :: acc))
      >>=? fun delegates ->
      return
      @@ List.map (fun (pkh, pk, _) -> (pkh, pk))
      @@ (* By swapping x and y we do a descending sort *)
      List.sort
        (fun (_, _, x) (_, _, y) -> Alpha_context.Tez.compare y x)
        delegates

let protocol_of_hash protocol_hash =
  if Protocol_hash.equal protocol_hash Tezos_protocol_009_PsFLoren.Protocol.hash
  then Some Florence
  else if
    Protocol_hash.equal protocol_hash Tezos_protocol_010_PtGRANAD.Protocol.hash
  then Some Granada
  else if
    Protocol_hash.equal protocol_hash Tezos_protocol_011_PtHangz2.Protocol.hash
  then Some Hangzhou
  else if
    Protocol_hash.equal protocol_hash Tezos_protocol_012_PsiThaCa.Protocol.hash
  then Some Ithaca
  else if Protocol_hash.equal protocol_hash Tezos_protocol_alpha.Protocol.hash
  then Some Alpha
  else None

(** [load_mainnet_bakers_public_keys base_dir active_backers_only] checkouts
    the head context at the given [base_dir] and computes a list of triples
    [(alias, pkh, pk)] corresponding to all delegates in that context. The
    [alias] is either procedurally generated for non-foundation bakers, or of
    the form ["foundationN"] for foundation bakers (see [alias_pkh_pk_list]).

    if [active_bakers_only] then the deactivated delegates are filtered out of
    the list.
*)
let load_mainnet_bakers_public_keys base_dir active_bakers_only =
  let open Tezos_store in
  let mainnet_genesis =
    {
      Genesis.time = Time.Protocol.of_notation_exn "2018-06-30T16:07:32Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }
  in
  Tezos_store.Store.init
    ~store_dir:(Filename.concat base_dir "store")
    ~context_dir:(Filename.concat base_dir "context")
    ~allow_testchains:true
    ~readonly:true
    mainnet_genesis
  >>=? fun store ->
  let main_chain_store = Store.main_chain_store store in
  Tezos_store.Store.Chain.current_head main_chain_store >>= fun block ->
  Format.printf
    "Head block found and loaded (%a)@."
    Block_hash.pp
    (Tezos_store.Store.Block.hash block) ;
  let header = Store.Block.header block in
  Store.Block.context_exn main_chain_store block
  >|= Tezos_shell_context.Shell_context.wrap_disk_context
  >>= fun context ->
  Store.Block.protocol_hash_exn main_chain_store block >>= fun protocol_hash ->
  let header = header.shell in
  (match protocol_of_hash protocol_hash with
  | None -> Error_monad.failwith "unknown protocol hash"
  | Some protocol -> get_delegates protocol context header active_bakers_only)
  >>=? fun delegates ->
  Tezos_store.Store.close_store store >>= fun () ->
  return
  @@ List.mapi
       (fun i (pkh, pk) ->
         let pkh = Signature.Public_key_hash.to_b58check pkh in
         let pk = Signature.Public_key.to_b58check pk in
         let alias =
           List.find_map
             (fun (alias, pkh', _) ->
               if String.equal pkh' pkh then Some alias else None)
             alias_pkh_pk_list
         in
         let alias =
           Option.value ~default:(Format.asprintf "baker_%d" i) alias
         in
         (alias, pkh, pk))
       delegates

let load_mainnet_bakers_public_keys base_dir active_bakers_only =
  match
    Lwt_main.run (load_mainnet_bakers_public_keys base_dir active_bakers_only)
  with
  | Ok alias_pkh_pk_list -> alias_pkh_pk_list
  | Error trace ->
      Format.eprintf "error:@.%a@." Error_monad.pp_print_trace trace ;
      exit 1
