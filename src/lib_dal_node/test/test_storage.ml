(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Dal node storage
   Invocation:   dune exec src/lib_dal_node/test/main.exe \
                  -- --file test_storage.ml
   Subject:      Testsuite related with the DAL node storage backend
*)

(* [number_of_slots] indicates the number of available slots per
   block.
   TODO: Update this value to match the current protocol constant when
   needed. *)
let number_of_slots = 32

module Helpers = struct
  open Ppx_hash_lib.Std.Hash.Builtin
  open Bam.Std
  open Bam.Std.Syntax
  open Tezos_dal_node_lib.Dal_proto_types
  open Tezos_dal_node_services.Types

  (* Generate levels. *)
  module Level = struct
    type t = level

    let hash_fold_t = hash_fold_int32

    let min = 0l

    (* Arbitrary value. *)
    let max = 100_000l

    let gen = int32 ~min ~max ()
  end

  (* In the following we approximate the size of the data used by the store. *)

  (* Helper to hash bytes on which relies ppx_hash. *)
  let hash_fold_bytes state b = hash_fold_string state (Bytes.to_string b)

  module Skip_list_hash = struct
    include Skip_list_hash

    let hash_fold_t state t =
      let bytes = Data_encoding.Binary.to_bytes_exn Skip_list_hash.encoding t in
      hash_fold_bytes state bytes

    (* A pointer hash is 32 bytes length. *)
    let gen =
      let size = return 32 in
      let* data = bytes ~size ~char:(char ~printable:true ()) () in
      (* To write the data size we perform {to/of}_bytes_exn. *)
      let bytes = Data_encoding.(Binary.to_bytes_exn bytes) data in
      let t = Data_encoding.Binary.of_bytes_exn Skip_list_hash.encoding bytes in
      return t

    let pp fmt t =
      Format.fprintf
        fmt
        "\"%s\""
        (Data_encoding.Binary.to_string_exn Skip_list_hash.encoding t)
  end

  module Skip_list_cell = struct
    include Skip_list_cell

    let hash_fold_t state t =
      let bytes = Data_encoding.Binary.to_bytes_exn Skip_list_cell.encoding t in
      hash_fold_bytes state bytes

    (* We support at most 64 back-pointers, each of which takes 32
       bytes. *)
    let gen =
      let sizes = Stdlib.List.init 65 (fun i -> (1, return @@ (32 * i))) in
      let size = oneof sizes in
      let* data = bytes ~size ~char:(char ~printable:true ()) () in
      (* To write the data size we perform {to/of}_bytes_exn. *)
      let bytes = Data_encoding.(Binary.to_bytes_exn bytes) data in
      let t = Data_encoding.Binary.of_bytes_exn Skip_list_cell.encoding bytes in
      return t

    (* This can be pretty large, the data is then truncated. *)
    let pp fmt t =
      let s = Data_encoding.Binary.to_string_exn Skip_list_cell.encoding t in
      let l = String.length s in
      let s' = String.sub s 0 (min 10 l) ^ if l < 10 then "" else ".." in
      Format.fprintf fmt "\"%s\"" s'
  end

  module HashSet = Set.Make (struct
    type t = Skip_list_hash.t

    let compare = compare
  end)

  module LevelMap = Map.Make (Int32)

  module State = struct
    (* The state keeps traces of:
       - each registered cell hash in [registered_hashes], together
       with their attested level.
       - each removed cell hash in [removed_hashes]. *)
    type t = {
      mutable removed_hashes : HashSet.t;
      mutable registered_hashes : HashSet.t LevelMap.t;
    }

    let zero =
      {removed_hashes = HashSet.empty; registered_hashes = LevelMap.empty}

    let pp_removed_hashes fmt removed_hashes =
      let elements = HashSet.elements removed_hashes in
      match elements with
      | [] -> Format.fprintf fmt "[]"
      | _ ->
          Format.pp_print_list
            Skip_list_hash.pp
            fmt
            (HashSet.elements removed_hashes)

    let pp_registered_hashes_item fmt (level, removed_hashes) =
      Format.fprintf
        fmt
        "registered_hashes.level %ld = %a"
        level
        pp_removed_hashes
        removed_hashes

    let pp_registered_hashes fmt registered_hashes =
      let bindings = LevelMap.bindings registered_hashes in
      match bindings with
      | [] -> Format.fprintf fmt "[]"
      | _ -> Format.pp_print_list pp_registered_hashes_item fmt bindings

    let pp fmt state =
      Format.fprintf
        fmt
        "removed_hashes = %a@. registered_hashes = %a"
        pp_removed_hashes
        state.removed_hashes
        pp_registered_hashes
        state.registered_hashes
  end

  module Action = struct
    type t =
      | Insert of {
          attested_level : Level.t;
          payload : (Skip_list_hash.t * Skip_list_cell.t * int) list;
        }
      | Find of {skip_list_hash : Skip_list_hash.t}
      | Remove of {attested_level : Level.t}
    [@@deriving hash]

    let gen_Insert ?level () =
      return @@ fun state ->
      let* attested_level = Option.fold ~none:Level.gen ~some:return level in
      let* payload =
        list
          ~size:(return number_of_slots)
          (pair Skip_list_hash.gen Skip_list_cell.gen)
      in
      let payload = List.mapi (fun i (h, c) -> (h, c, i)) payload in
      let hashes =
        List.fold_left
          (fun acc (hash, _, _) -> HashSet.add hash acc)
          HashSet.empty
          payload
      in
      let new_registered_hashes =
        LevelMap.add attested_level hashes state.State.registered_hashes
      in
      let new_removed_hashes = HashSet.diff state.State.removed_hashes hashes in
      let new_state =
        {
          State.registered_hashes = new_registered_hashes;
          removed_hashes = new_removed_hashes;
        }
      in
      return (Some (new_state, Insert {attested_level; payload}))

    let gen_Find =
      return @@ fun state ->
      match LevelMap.choose_opt state.State.registered_hashes with
      | None -> return None
      | Some (_attested_level, removed_hashes) -> (
          match HashSet.choose_opt removed_hashes with
          | None -> return None
          | Some skip_list_hash -> return (Some (state, Find {skip_list_hash})))

    let gen_Remove =
      return @@ fun state ->
      match LevelMap.choose_opt state.State.registered_hashes with
      | None -> return None
      | Some (attested_level, removed_hashes) ->
          let new_registered_hashes =
            LevelMap.remove attested_level state.registered_hashes
          in
          let new_removed_hashes =
            HashSet.union removed_hashes state.State.removed_hashes
          in
          let new_state =
            {
              State.registered_hashes = new_registered_hashes;
              removed_hashes = new_removed_hashes;
            }
          in
          return (Some (new_state, Remove {attested_level}))

    let pp_action_insert_payload_item fmt (hash, cell, slot_index) =
      Format.fprintf
        fmt
        "(%a, %a, %d)"
        Skip_list_hash.pp
        hash
        Skip_list_cell.pp
        cell
        slot_index

    let pp_action_insert_payload fmt payload =
      Format.pp_print_list pp_action_insert_payload_item fmt payload

    let pp fmt action =
      match action with
      | Insert {attested_level; payload} ->
          Format.fprintf
            fmt
            "insert at attested level %ld the following payload:@, %a"
            attested_level
            pp_action_insert_payload
            payload
      | Find {skip_list_hash} ->
          Format.fprintf
            fmt
            "find skip list hash %a"
            Skip_list_hash.pp
            skip_list_hash
      | Remove {attested_level} ->
          Format.fprintf fmt "remove attested level %ld" attested_level
  end

  type scenario = {state : State.t; [@hash.ignore] actions : Action.t list}
  [@@deriving hash]

  let pp fmt {state; actions} =
    Format.fprintf
      fmt
      "scenario.state: %a@. scenario.actions:%a"
      State.pp
      state
      (Format.pp_print_list Action.pp)
      actions

  (** [gen_with_state ?insert_dist ?find_dist ?remove_dist state n]
      generates a [scenario] containing a list of [n] actions and a
      [state] resulting from their interpretations. Actions are
      selected according to the probability distribution defined by
      [insert_dist], [find_dist], and [remove_dist] (each defaulting
      to 1).

      The sum of distribution weights must be non-zero. Additionally,
      [insert_dist] must be non-zero since [find] and [remove] actions
      operate on payloads introduced by prior [insert] actions. *)
  let gen_with_state ?(insert_dist = 1) ?(find_dist = 1) ?(remove_dist = 1)
      state n =
    let open Bam.Std.Syntax in
    assert (n >= 0) ;
    let rec loop l acc state n =
      if n = 0 then return {state; actions = List.rev acc}
      else
        let choices =
          (insert_dist, Action.gen_Insert ~level:l ())
          :: (find_dist, Action.gen_Find)
          :: [(remove_dist, Action.gen_Remove)]
        in
        let* choice = Bam.Std.oneof choices in
        let* res = choice state in
        match res with
        | None -> loop l acc state n
        | Some (new_state, (Action.Insert _ as action)) ->
            loop (Int32.succ l) (action :: acc) new_state (pred n)
        | Some (new_state, action) -> loop l (action :: acc) new_state (pred n)
    in
    loop 0l [] state n

  let gen ?insert_dist ?find_dist ?remove_dist n =
    gen_with_state ?insert_dist ?find_dist ?remove_dist State.zero n
end
