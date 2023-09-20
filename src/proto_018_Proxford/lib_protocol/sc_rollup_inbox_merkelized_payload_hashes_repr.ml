(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error += (* `Permanent *) Merkelized_payload_hashes_proof_error of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"internal.smart_rollup_merklized_payload_hashes_proof"
    ~title:
      "Internal error: error occurred during proof production or validation"
    ~description:"A merkelized payload hashes proof error."
    ~pp:(fun ppf e -> Format.fprintf ppf "Proof error: %s" e)
    (obj1 (req "error" (string Plain)))
    (function Merkelized_payload_hashes_proof_error e -> Some e | _ -> None)
    (fun e -> Merkelized_payload_hashes_proof_error e)

module Skip_list_parameters = struct
  let basis = 4
end

module Skip_list = Skip_list.Make (Skip_list_parameters)
module Hash = Smart_rollup.Merkelized_payload_hashes_hash

type t = (Sc_rollup_inbox_message_repr.Hash.t, Hash.t) Skip_list.cell

let equal = Skip_list.equal Hash.equal Sc_rollup_inbox_message_repr.Hash.equal

let hash merkelized =
  let payload_hash = Skip_list.content merkelized in
  let back_pointers_hashes = Skip_list.back_pointers merkelized in
  Sc_rollup_inbox_message_repr.Hash.to_bytes payload_hash
  :: List.map Hash.to_bytes back_pointers_hashes
  |> Hash.hash_bytes

let pp fmt merkelized =
  Format.fprintf
    fmt
    "@[<v>cell ptr: %a@,@[<v 2>cell content:@,%a@]@]"
    Hash.pp_short
    (hash merkelized)
    (Skip_list.pp
       ~pp_content:Sc_rollup_inbox_message_repr.Hash.pp_short
       ~pp_ptr:Hash.pp_short)
    merkelized

let encoding =
  Skip_list.encoding Hash.encoding Sc_rollup_inbox_message_repr.Hash.encoding

type merkelized_and_payload = {
  merkelized : t;
  payload : Sc_rollup_inbox_message_repr.serialized;
}

let equal_merkelized_and_payload {merkelized; payload} mp2 =
  equal merkelized mp2.merkelized
  && String.equal (payload :> string) (mp2.payload :> string)

let pp_merkelized_and_payload fmt {merkelized; payload} =
  Format.fprintf
    fmt
    "@[<hv 2>merkelized:@,%a@,payload: %a@]"
    pp
    merkelized
    Format.pp_print_string
    (payload :> string)

let merkelized_and_payload_encoding =
  let open Data_encoding in
  conv
    (fun {merkelized; payload} -> (merkelized, (payload :> string)))
    (fun (merkelized, payload) ->
      {
        merkelized;
        payload = Sc_rollup_inbox_message_repr.unsafe_of_string payload;
      })
    (merge_objs encoding (obj1 (req "payload" (string Hex))))

module History = struct
  include
    Bounded_history_repr.Make
      (struct
        let name = "Smart_rollup_level_inbox_history"
      end)
      (Hash)
      (struct
        type nonrec t = merkelized_and_payload

        let pp = pp_merkelized_and_payload

        let equal = equal_merkelized_and_payload

        let encoding = merkelized_and_payload_encoding
      end)

  let no_history = empty ~capacity:0L
end

let remember history merkelized payload =
  let prev_cell_ptr = hash merkelized in
  History.remember prev_cell_ptr {merkelized; payload} history

let genesis_no_history payload =
  let payload_hash =
    Sc_rollup_inbox_message_repr.hash_serialized_message payload
  in
  Skip_list.genesis payload_hash

let genesis history payload =
  let open Result_syntax in
  let merkelized = genesis_no_history payload in
  let+ history = remember history merkelized payload in
  (history, merkelized)

let add_payload_no_history prev_merkelized payload =
  let prev_merkelized_ptr = hash prev_merkelized in
  Skip_list.next
    ~prev_cell:prev_merkelized
    ~prev_cell_ptr:prev_merkelized_ptr
    (Sc_rollup_inbox_message_repr.hash_serialized_message payload)

let add_payload history prev_merkelized payload =
  let open Result_syntax in
  let merkelized = add_payload_no_history prev_merkelized payload in
  let* history = remember history merkelized payload in
  return (history, merkelized)

let get_payload_hash = Skip_list.content

let get_index = Skip_list.index

type proof = t list

let pp_proof = Format.pp_print_list pp

let proof_encoding = Data_encoding.list encoding

let produce_proof history ~index merkelized =
  let open Option_syntax in
  let deref ptr =
    let* {merkelized; payload = _} = History.find ptr history in
    return merkelized
  in
  let current_ptr = hash merkelized in
  let lift_ptr =
    let rec aux acc = function
      | [] -> None
      | [last_ptr] ->
          let+ ({merkelized; _} as merkelized_and_payload) =
            History.find last_ptr history
          in
          (merkelized_and_payload, List.rev (merkelized :: acc))
      | ptr :: rest ->
          let* merkelized = deref ptr in
          aux (merkelized :: acc) rest
    in
    aux []
  in
  let* ptr_path =
    Skip_list.back_path ~deref ~cell_ptr:current_ptr ~target_index:index
  in
  lift_ptr ptr_path

let verify_proof inclusion_proof =
  let open Result_syntax in
  let* cell =
    match inclusion_proof with
    | cell :: _ -> ok cell
    | [] -> error (Merkelized_payload_hashes_proof_error "proof is empty")
  in
  let rec aux (hash_map, ptr_list) = function
    | [] -> error (Merkelized_payload_hashes_proof_error "proof is empty")
    | [target] ->
        let target_ptr = hash target in
        let hash_map = Hash.Map.add target_ptr target hash_map in
        let ptr_list = List.rev (target_ptr :: ptr_list) in
        ok (hash_map, ptr_list, target, target_ptr)
    | merkelized :: tail ->
        let ptr = hash merkelized in
        aux (Hash.Map.add ptr merkelized hash_map, ptr :: ptr_list) tail
  in
  let* hash_map, ptr_list, target, target_ptr =
    aux (Hash.Map.empty, []) inclusion_proof
  in
  let deref ptr = Hash.Map.find ptr hash_map in
  let cell_ptr = hash cell in
  let* () =
    error_unless
      (Skip_list.valid_back_path
         ~equal_ptr:Hash.equal
         ~deref
         ~cell_ptr
         ~target_ptr
         ptr_list)
      (Merkelized_payload_hashes_proof_error "invalid proof")
  in
  return (target, cell)

module Internal_for_tests = struct
  let find_predecessor_payload payloads_history ~index payloads =
    let open Option_syntax in
    let deref ptr =
      let* {merkelized; _} = History.find ptr payloads_history in
      return merkelized
    in
    let cell_ptr = hash payloads in
    Skip_list.find ~deref ~cell_ptr ~target_index:index

  let make_proof proof = proof
end
