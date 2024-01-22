(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Store_sigs

let decode encoding encoded_value =
  let open Result_syntax in
  let res = Data_encoding.Binary.of_bytes encoding encoded_value in
  Result.bind_error res (fun e -> tzfail @@ Store_errors.Decoding_error e)

let encode encoding value =
  let open Result_syntax in
  let res = Data_encoding.Binary.to_bytes encoding value in
  Result.bind_error res (fun e -> tzfail @@ Store_errors.Encoding_error e)

module Make (B : BACKEND) = struct
  type 'a t = 'a B.t

  module Make_map (S : STORAGE_INFO) (K : KEY) (V : VALUE) = struct
    type key = K.key

    type 'a store = 'a B.t

    type value = V.value

    let path = S.path

    let make_key key = B.make_key_path path @@ K.to_path_representation key

    let mem store key = B.mem store (make_key key)

    let decode_value = decode V.encoding

    let encode_value = encode V.encoding

    let get store key =
      let open Lwt_result_syntax in
      let* e = B.get store (make_key key) in
      Lwt.return @@ decode_value e

    let find store key =
      let open Lwt_result_syntax in
      let* value = B.find store (make_key key) in
      Lwt.return @@ Option.map_e decode_value value

    let find_with_default store key ~on_default =
      let open Lwt_result_syntax in
      let* value = find store key in
      return @@ Option.value_f value ~default:on_default
  end

  module Make_updatable_map (S : STORAGE_INFO) (K : KEY) (V : VALUE) = struct
    include Make_map (S) (K) (V)

    let add store key value =
      let open Lwt_result_syntax in
      let*? encoded_value = encode_value value in
      B.set store (make_key key) encoded_value
  end

  module Make_append_only_map (S : STORAGE_INFO) (K : KEY) (V : VALUE) = struct
    include Make_map (S) (K) (V)

    let add store key value =
      let open Lwt_result_syntax in
      let* existing_value = find store key in
      let*? encoded_value = encode_value value in
      match existing_value with
      | None -> B.set store (make_key key) encoded_value
      | Some existing_value ->
          let*? encoded_existing_value = encode_value existing_value in
          (* To be robust to interruption in the middle of processes,
             we accept to redo some work when we restart the node.
             Hence, it is fine to insert twice the same value for a
             given value. *)
          if not (Bytes.equal encoded_existing_value encoded_value) then
            tzfail
            @@ Store_errors.Cannot_overwrite_key_in_store
                 {
                   name = B.name;
                   key = B.path_to_string @@ make_key key;
                   old_value = String.of_bytes encoded_existing_value;
                   new_value = String.of_bytes encoded_value;
                 }
          else return_unit
  end

  module Make_mutable_value (S : STORAGE_INFO) (V : VALUE) = struct
    type 'a store = 'a B.t

    type value = V.value

    let path_key = S.path

    let decode_value = decode V.encoding

    let encode_value = encode V.encoding

    let set store value =
      let open Lwt_result_syntax in
      let*? encoded_value = encode_value value in
      B.set store path_key encoded_value

    let get store =
      let open Lwt_result_syntax in
      let* value = B.get store path_key in
      Lwt.return @@ decode_value value

    let find store =
      let open Lwt_result_syntax in
      let* value = B.find store path_key in
      Lwt.return @@ Option.map_e decode_value value
  end

  module Make_nested_map
      (S : STORAGE_INFO)
      (K1 : KEY)
      (K2 : COMPARABLE_KEY)
      (V : VALUE) =
  struct
    type 'a store = 'a B.t

    type primary_key = K1.key

    type secondary_key = K2.key

    type value = V.value

    let path = S.path

    module Secondary_key_map = Map.Make (struct
      type t = K2.key

      let compare = K2.compare
    end)

    module Map_as_value = struct
      type value = V.value Secondary_key_map.t

      let encoding =
        Data_encoding.conv
          (fun map -> Secondary_key_map.bindings map)
          (fun bindings -> Secondary_key_map.of_seq @@ List.to_seq bindings)
          Data_encoding.(
            list @@ obj2 (req K2.name K2.encoding) (req V.name V.encoding))

      let encode_value = encode V.encoding

      let name = ""
    end

    module M = Make_updatable_map (S) (K1) (Map_as_value)

    (* Return the bindings associated with a primary key. *)
    let list_secondary_keys_with_values store ~primary_key =
      let open Lwt_result_syntax in
      let+ slots_map = M.find store primary_key in
      Option.fold ~none:[] ~some:Secondary_key_map.bindings slots_map

    (* Check whether the updatable store contains an entry
       for the primary_key, which itself contains a
       binding whose key is ~secondary_key. *)
    let mem store ~primary_key ~secondary_key =
      let open Lwt_result_syntax in
      let* primary_key_binding_exists = M.mem store primary_key in
      if not primary_key_binding_exists then return_false
      else
        let+ secondary_key_map = M.get store primary_key in
        Secondary_key_map.mem secondary_key secondary_key_map

    (* Unsafe call. The existence of a value for
       primary and secondary key in the store must be
       checked before invoking this function. *)
    let get store ~primary_key ~secondary_key =
      let open Lwt_result_syntax in
      let* secondary_key_map = M.get store primary_key in
      match Secondary_key_map.find secondary_key secondary_key_map with
      | None ->
          let key = K1.to_path_representation primary_key in
          tzfail @@ Store_errors.Cannot_read_key_from_store {name = B.name; key}
      | Some value -> return value

    let find store ~primary_key ~secondary_key =
      let open Lwt_result_syntax in
      let* binding_exists = mem store ~primary_key ~secondary_key in
      if binding_exists then
        let+ value = get store ~primary_key ~secondary_key in
        Some value
      else return_none

    (* Returns the set of keys from the bindings of
       ~primary_key in the store. *)
    let list_secondary_keys store ~primary_key =
      let open Lwt_result_syntax in
      let+ secondary_keys_with_values =
        list_secondary_keys_with_values store ~primary_key
      in
      List.map (fun (key, _value) -> key) secondary_keys_with_values

    (* Returns the set of values from the bindings of
       ~primary_key in the store. *)
    let list_values store ~primary_key =
      let open Lwt_result_syntax in
      let+ secondary_keys_with_values =
        list_secondary_keys_with_values store ~primary_key
      in
      List.map (fun (_key, value) -> value) secondary_keys_with_values

    (* Updates the entry of the updatable map with key ~primary_key
       by adding to it a binding with key ~secondary_key and
       value `value`.*)
    let add store ~primary_key ~secondary_key value =
      let open Lwt_result_syntax in
      let* value_map = M.find store primary_key in
      let value_map = Option.value ~default:Secondary_key_map.empty value_map in
      let*? value_can_be_added, encoded_old_value, encoded_value =
        let open Result_syntax in
        let* encoded_value = Map_as_value.encode_value value in
        match Secondary_key_map.find secondary_key value_map with
        | None -> return (true, None, encoded_value)
        | Some old_value ->
            let* encoded_old_value = Map_as_value.encode_value old_value in
            return
            @@ ( Bytes.equal encoded_old_value encoded_value,
                 Some encoded_old_value,
                 encoded_value )
      in
      if value_can_be_added then
        let updated_map = Secondary_key_map.add secondary_key value value_map in
        M.add store primary_key updated_map
      else
        let key =
          B.path_to_string @@ B.make_key_path S.path
          @@ K1.to_path_representation primary_key
        in
        match encoded_old_value with
        | None ->
            tzfail
            @@ Store_errors.Cannot_write_key_value_pair_to_store
                 {name = B.name; key; value = String.of_bytes encoded_value}
        | Some encoded_old_value ->
            tzfail
            @@ Store_errors.Cannot_overwrite_key_in_store
                 {
                   name = B.name;
                   key;
                   old_value = String.of_bytes encoded_old_value;
                   new_value = String.of_bytes encoded_value;
                 }
  end
end
