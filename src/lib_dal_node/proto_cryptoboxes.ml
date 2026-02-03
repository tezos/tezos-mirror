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
let init ~cctxt ~header ~config ~current_head_proto_parameters ~first_seen_level
    profile_ctxt proto_plugins =
  let open Lwt_result_syntax in
  let* l1_known_protocols = L1_helpers.fetch_l1_known_protocols cctxt in
  let head_protocol_info =
    List.find
      (fun e ->
        e.Chain_services.proto_level = header.Block_header.shell.proto_level)
      l1_known_protocols
  in
  let* head_proto_first_block =
    match head_protocol_info with
    | Some p -> return (snd p.Chain_services.activation_block)
    | None -> tzfail No_cryptobox
  in
  (* Init Proto_cryptoboxes state with the current head's cryptobox. *)
  let* head_cryptobox =
    init_state
      config
      current_head_proto_parameters
      profile_ctxt
      ~level:head_proto_first_block
  in
  let* () =
    (* If the activation block is <= to the first_seen_level, we associate the
       first_seen_level to that protocol's layout. Thus, the first_seen_level is
       associated to a layout.

       For a given activation block, the layout associated to it always start at
       the next level. Indeed, the shards and slots of the activation block have
       been created with the protocol running before the activation. *)
    let head_layout_level =
      if head_proto_first_block <= first_seen_level then first_seen_level
      else Int32.add head_proto_first_block 1l
    in
    add_slots_and_shards_layouts
      ~layout_level:head_layout_level
      ~proto_parameters:current_head_proto_parameters.cryptobox_parameters
  in
  (* Retrieve the protocol_info needed to handle all blocks above
     [first_seen_level]. *)
  let additional_protocols_info =
    let sorted_protocol_info =
      List.sort
        (fun p1 p2 -> Int.compare p1.Chain_services.proto_level p2.proto_level)
        l1_known_protocols
    in
    (* Loop over protocols activation block and stop as soon as the
       [first_seen_block] fits into a protocol interval. *)
    let rec aux acc = function
      | [] -> []
      | hd :: tl ->
          if snd hd.Chain_services.activation_block < first_seen_level then
            aux [hd] tl
          else acc @ (hd :: tl)
    in
    let tmp = aux [] sorted_protocol_info in
    (* Remove the current head protocol_info as it was already added above. *)
    List.filter
      (fun e ->
        e.Chain_services.proto_level != header.Block_header.shell.proto_level)
      tmp
  in
  List.fold_left_es
    (fun box pi ->
      let activation_level = snd pi.Chain_services.activation_block in
      if activation_level = 0l then
        (* Do not register any cryptobox for level 0. *)
        return box
      else
        let first_proto_level = activation_level in
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
        add proto_params ~level:first_proto_level box)
    head_cryptobox
    additional_protocols_info

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
