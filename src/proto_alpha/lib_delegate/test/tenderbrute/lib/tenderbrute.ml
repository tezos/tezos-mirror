(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type delegate_selection =
  (Raw_level_repr.t * (Round_repr.t * Signature.public_key_hash) list) list

module LevelRoundMap = Map.Make (struct
  type t = Level_repr.t * Round_repr.t

  let compare (l1, r1) (l2, r2) =
    Stdlib.compare
      (Raw_level_repr.to_int32 l1.Level_repr.level, Round_repr.to_int32 r1)
      (Raw_level_repr.to_int32 l2.Level_repr.level, Round_repr.to_int32 r2)
end)

let _ = Client_keys.register_signer (module Tezos_signer_backends.Unencrypted)

(* Initialize a context in memory with the Mockup *)
let init_context ?constants_overrides_json ?bootstrap_accounts_json parameters =
  let open Lwt_result_syntax in
  let parameters =
    Data_encoding.Binary.of_bytes_exn Mockup.M.parameters_encoding
    @@ Data_encoding.Binary.to_bytes_exn
         Mockup.Protocol_parameters.encoding
         parameters
  in
  let* mockup_init =
    Mockup.M.init
      ~cctxt:Client_context.null_printer
      ~parameters
      ~constants_overrides_json
      ~bootstrap_accounts_json
  in
  let ctxt = mockup_init.rpc_context.context in
  let timestamp = Time.Protocol.of_seconds 0L in
  (* The timestamp is irrelevant for the rights *)
  let*! result =
    Raw_context.prepare
      ctxt
      ~level:1l
      ~predecessor_timestamp:timestamp
      ~timestamp
      ~adaptive_issuance_enable:false
  in
  Lwt.return @@ Environment.wrap_tzresult result

(* Change the initial seed for the first preserved cycles. This suppose that the
   seeds for these cycles are already set, which is the case because this
   function is always called after {!init_context}. *)
let change_seed ?initial_seed ctxt =
  let open Lwt_result_syntax in
  let preserved = Constants_storage.preserved_cycles ctxt in
  let+ (_ : int), ctxt =
    List.fold_left_es
      (fun (c, ctxt) seed ->
        let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
        let* ctxt = Storage.Seed.For_cycle.remove_existing ctxt cycle in
        let+ ctxt = Storage.Seed.For_cycle.init ctxt cycle seed in
        (c + 1, ctxt))
      (0, ctxt)
      (Seed_repr.initial_seeds ?initial_seed (preserved + 2))
  in
  ctxt

let init ?constants_overrides_json ?bootstrap_accounts_json parameters =
  let open Lwt_result_syntax in
  let* ctxt =
    init_context ?constants_overrides_json ?bootstrap_accounts_json parameters
  in
  let blocks_per_cycle = Constants_storage.blocks_per_cycle ctxt in
  let blocks_per_commitment = Constants_storage.blocks_per_commitment ctxt in
  let cycle_eras =
    [
      Level_repr.
        {
          first_level = Raw_level_repr.of_int32_exn 0l;
          first_cycle = Cycle_repr.root;
          blocks_per_cycle;
          blocks_per_commitment;
        };
    ]
  in
  let*? cycle_eras =
    Level_repr.create_cycle_eras cycle_eras |> Environment.wrap_tzresult
  in
  return (ctxt, cycle_eras)

(* Check if the given selection correponds to the baking rights assigned by the
   protocol (with the initial seeds registered in the context). Returns [true]
   when the selection is the one of the protocol or [false] otherwise. *)
let check ctxt ~selection =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let* _ctxt =
        LevelRoundMap.fold_es
          (fun (level, round) delegate ctxt ->
            let* ctxt, _, pk =
              let*! result =
                Delegate_sampler.baking_rights_owner ctxt level ~round
              in
              Lwt.return @@ Environment.wrap_tzresult result
            in
            if not (Signature.Public_key_hash.equal delegate pk.delegate) then
              raise Exit
            else return ctxt)
          selection
          ctxt
      in
      return_true)
    (function Exit -> return_false | e -> Lwt.reraise e)

(* Create random 32 bytes *)
let rnd_bytes32 () =
  let b1 = Random.int64 Int64.max_int in
  let b2 = Random.int64 Int64.max_int in
  let b3 = Random.int64 Int64.max_int in
  let b4 = Random.int64 Int64.max_int in
  let b = Bytes.make 32 '\000' in
  TzEndian.set_int64 b 0 b1 ;
  TzEndian.set_int64 b 8 b2 ;
  TzEndian.set_int64 b 16 b3 ;
  TzEndian.set_int64 b 24 b4 ;
  b

let mk_selection_map cycle_eras selection =
  List.fold_left
    (fun acc (level, round_delegates) ->
      let level = Level_repr.level_from_raw ~cycle_eras level in
      List.fold_left
        (fun acc (round, delegate) ->
          if LevelRoundMap.mem (level, round) acc then
            Stdlib.failwith "Duplicate level/round" ;
          LevelRoundMap.add (level, round) delegate acc)
        acc
        round_delegates)
    LevelRoundMap.empty
    selection

(* Bruteforce an initial seed nonce for the desired delegate selection *)
let bruteforce ?(show_progress = false) ?(random_seed = 0) ?max
    ?(parameters = Mockup.Protocol_parameters.default_value)
    ?constants_overrides_json ?bootstrap_accounts_json selection =
  let open Lwt_result_syntax in
  Random.init random_seed ;
  let* ctxt, cycle_eras =
    init ?constants_overrides_json ?bootstrap_accounts_json parameters
  in
  let selection = mk_selection_map cycle_eras selection in
  let last_nb_chars = ref 0 in
  let frames =
    [|
      "( ●    )";
      "(  ●   )";
      "(   ●  )";
      "(    ● )";
      "(     ●)";
      "(    ● )";
      "(   ●  )";
      "(  ●   )";
      "( ●    )";
      "(●     )";
    |]
  in
  let nframes = Array.length frames in
  let frame n = frames.(n mod nframes) in
  let rec loop n =
    if show_progress && n <> 0 && n mod 10_000 = 0 then (
      Format.eprintf "%s" (String.make !last_nb_chars '\b') ;
      let s = frame (n / 10_000) ^ " " ^ string_of_int n in
      last_nb_chars := String.length s ;
      Format.eprintf "%s%!" s) ;
    match max with
    | Some max when n > max -> failwith "Did not find seed nonce"
    | _ -> (
        let initial_seed =
          if n = 0 then None
          else Some (State_hash.of_bytes_exn (rnd_bytes32 ()))
        in
        let* ctxt =
          let*! result = change_seed ?initial_seed ctxt in
          Lwt.return @@ Environment.wrap_tzresult result
        in
        let* result = check ctxt ~selection in
        match result with
        | true ->
            Format.eprintf "%s%!" (String.make !last_nb_chars '\b') ;
            return initial_seed
        | false -> loop (n + 1))
  in
  loop 0

(* Check that an initial seed corresonds to the desired delegate selection *)
let check_seed ?(parameters = Mockup.Protocol_parameters.default_value)
    ?constants_overrides_json ?bootstrap_accounts_json ~seed selection =
  let open Lwt_result_syntax in
  let* ctxt, cycle_eras =
    init ?constants_overrides_json ?bootstrap_accounts_json parameters
  in
  let selection = mk_selection_map cycle_eras selection in
  let* ctxt =
    let*! result = change_seed ?initial_seed:seed ctxt in
    Lwt.return @@ Environment.wrap_tzresult result
  in
  check ctxt ~selection
