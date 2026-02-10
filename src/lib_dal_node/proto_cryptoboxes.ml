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

let init_state config proto_parameters profile ~level =
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
            let*! () = Event.emit_cryptobox_registered ~level in
            return
            @@ With_precomputation
                 (LevelMap.singleton
                    level
                    {cryptobox; shards_proofs_precomputation = precomputation})
        | Error (`Invalid_degree_strictly_less_than_expected {given; expected})
          ->
            unexpected_precomputed_shards_proofs ~given ~expected
      else
        let*! () = Event.emit_cryptobox_registered ~level in
        return @@ No_precomputation (LevelMap.singleton level cryptobox)
  | Error (`Fail msg) -> fail [Errors.Cryptobox_initialisation_failed msg]

let add proto_parameters ~level t =
  let open Lwt_result_syntax in
  match Cryptobox.make proto_parameters.Types.cryptobox_parameters with
  | Ok cryptobox -> (
      match t with
      | With_precomputation prev -> (
          match Cryptobox.precompute_shards_proofs cryptobox with
          | Ok precomputation ->
              let*! () = Event.emit_cryptobox_registered ~level in
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
          let*! () = Event.emit_cryptobox_registered ~level in
          return @@ No_precomputation (LevelMap.add level cryptobox prev))
  | Error (`Fail msg) -> fail [Errors.Cryptobox_initialisation_failed msg]

type error += No_cryptobox | Cannot_register_shard_layout of {msg : string}

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
    (fun () -> No_cryptobox) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.Cannot_register_shard_layout"
    ~title:"DAL node: Cannot register shard layout"
    ~description:"DAL node: Cannot register shard layout"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Cannot register shard layout. Failed to make cryptobox: %s."
        msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Cannot_register_shard_layout {msg} -> Some msg | _ -> None)
    (fun msg -> Cannot_register_shard_layout {msg})

let add_slots_and_shards_layouts ~layout_level ~proto_parameters =
  let open Lwt_result_syntax in
  Store.Slots.add_file_layout layout_level proto_parameters ;
  match Store.Shards_disk.add_file_layout layout_level proto_parameters with
  | Ok () -> return_unit
  | Error (`Fail msg) -> tzfail (Cannot_register_shard_layout {msg})

(* This function aims to associate a cryptobox to every block, and thus, for
   each relevant protocols associated; the relevant ones being all covering the
   level from [first_seen_level] to the current head level. *)
let init ~cctxt ~header ~config ~first_seen_level profile_ctxt proto_plugins =
  let open Lwt_result_syntax in
  let* l1_known_protocols =
    let* res = L1_helpers.fetch_l1_known_protocols cctxt in
    (* Invariant: must be ordered with the oldest protocols first. *)
    List.sort
      (fun x y -> Int.compare x.Chain_services.proto_level y.proto_level)
      res
    |> return
  in
  let needed_protocols =
    let biggest_below_first_seen_opt =
      List.fold_left
        (fun acc ({Chain_services.activation_block; _} as p) ->
          let activation_level = snd activation_block in
          if Int32.succ activation_level <= first_seen_level then Some p
          else acc)
        None
        l1_known_protocols
    in
    (* all activation levels between first_seen_level and head *)
    let in_between =
      List.filter
        (fun {Chain_services.activation_block; _} ->
          let activation_level = snd activation_block in
          activation_level > 0l
          && Int32.succ activation_level > first_seen_level
          && activation_level <= header.Block_header.shell.level)
        l1_known_protocols
    in
    match biggest_below_first_seen_opt with
    | None -> in_between
    | Some proto ->
        let level = snd proto.Chain_services.activation_block in
        if level > 0l then proto :: in_between else in_between
  in
  let* proto_cryptoboxes_opt =
    List.fold_left_es
      (fun box pi ->
        let activation_level = snd pi.Chain_services.activation_block in
        if activation_level = 0l then
          (* Do not register any cryptobox for level 0. *)
          return box
        else
          let first_proto_level = activation_level in
          (* For a given activation block, the layout associated to it always
             start at the next level. Indeed, the shards and slots of the
             activation block have been created with the protocol running before
             the activation. *)
          let current_layout_level =
            if activation_level <= first_seen_level then first_seen_level
            else Int32.add first_proto_level 1l
          in
          let*? _, proto_params =
            Proto_plugins.get_plugin_and_parameters_for_level
              proto_plugins
              ~level:first_proto_level
          in
          let* () =
            add_slots_and_shards_layouts
              ~layout_level:current_layout_level
              ~proto_parameters:proto_params.cryptobox_parameters
          in
          match box with
          | Some box ->
              let* new_state = add proto_params ~level:first_proto_level box in
              return_some new_state
          | None ->
              let* new_state =
                init_state
                  config
                  proto_params
                  profile_ctxt
                  ~level:first_proto_level
              in
              return_some new_state)
      None
      needed_protocols
  in
  match proto_cryptoboxes_opt with
  | Some v -> return v
  | None -> tzfail No_cryptobox

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
