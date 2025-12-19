(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module LevelMap = Map.Make (struct
  type t = Int32.t

  (* We enforce the set to use ascending order. *)
  let compare a b = compare a b
end)

type cryptobox_with_precomputation = {
  cryptobox : Cryptobox.t;
  shards_proofs_precomputation : Cryptobox.shards_proofs_precomputation;
}

type t =
  | No_precomputation of Cryptobox.t LevelMap.t
  | With_precomputation of cryptobox_with_precomputation LevelMap.t

let unexpected_precomputed_shards_proofs ~given ~expected =
  Lwt_result_syntax.fail
    [
      Errors.Cryptobox_initialisation_failed
        (Printf.sprintf
           "Cryptobox.precompute_shards_proofs: SRS size (= %d) smaller than \
            expected (= %d)"
           given
           expected);
    ]

let init config proto_parameters profile ~level =
  let open Lwt_result_syntax in
  let prover_srs = Profile_manager.is_prover_profile profile in
  let* () =
    if prover_srs then
      let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.init_prover_dal
        ~find_srs_files
        ~fetch_trusted_setup:config.Configuration_file.fetch_trusted_setup
        ()
    else return_unit
  in
  match Cryptobox.make proto_parameters.Types.cryptobox_parameters with
  | Ok cryptobox ->
      if prover_srs then
        match Cryptobox.precompute_shards_proofs cryptobox with
        | Ok precomputation ->
            return
            @@ With_precomputation
                 (LevelMap.singleton
                    level
                    {cryptobox; shards_proofs_precomputation = precomputation})
        | Error (`Invalid_degree_strictly_less_than_expected {given; expected})
          ->
            unexpected_precomputed_shards_proofs ~given ~expected
      else return @@ No_precomputation (LevelMap.singleton level cryptobox)
  | Error (`Fail msg) -> fail [Errors.Cryptobox_initialisation_failed msg]

let add proto_parameters ~level t =
  let open Lwt_result_syntax in
  match Cryptobox.make proto_parameters.Types.cryptobox_parameters with
  | Ok cryptobox -> (
      match t with
      | With_precomputation prev -> (
          match Cryptobox.precompute_shards_proofs cryptobox with
          | Ok precomputation ->
              return
              @@ With_precomputation
                   (LevelMap.add
                      level
                      {cryptobox; shards_proofs_precomputation = precomputation}
                      prev)
          | Error
              (`Invalid_degree_strictly_less_than_expected {given; expected}) ->
              unexpected_precomputed_shards_proofs ~given ~expected)
      | No_precomputation prev ->
          return @@ No_precomputation (LevelMap.add level cryptobox prev))
  | Error (`Fail msg) -> fail [Errors.Cryptobox_initialisation_failed msg]

type error += No_cryptobox

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.no_cryptobox"
    ~title:"DAL node: no cryptobox"
    ~description:"DAL node: no cryptobox registered at all"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "No cryptobox has been registered. Unexpected DAL initialization.")
    Data_encoding.unit
    (function No_cryptobox -> Some () | _ -> None)
    (fun () -> No_cryptobox)

let get_as_pair f t =
  let open Result_syntax in
  match t with
  | No_precomputation prev -> (
      match LevelMap.find_last f prev with
      | None -> tzfail @@ No_cryptobox
      | Some (_, cryptobox) -> return (cryptobox, None))
  | With_precomputation prev -> (
      match LevelMap.find_last f prev with
      | None -> tzfail @@ No_cryptobox
      | Some (_, {cryptobox; shards_proofs_precomputation}) ->
          return (cryptobox, Some shards_proofs_precomputation))

let get_for_level ~level =
  get_as_pair (fun first_level ->
      (* Assumes that the LevelMap uses ascending order. *)
      level >= first_level)

let get_latest =
  get_as_pair (fun _ ->
      (* Assumes that the LevelMap uses ascending order. *)
      true)
