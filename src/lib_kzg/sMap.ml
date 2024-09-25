(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module M = Map.Make (String)
include M

let of_list l = of_seq (List.to_seq l)

let t (inner : 'a Repr.t) : 'a t Repr.t =
  let module M = Repr.Of_map (struct
    include M

    let key_t = Repr.string
  end) in
  M.t inner

let keys t = bindings t |> List.map fst

let values t = bindings t |> List.map snd

let to_pair m = (map fst m, map snd m)

let add_unique k v m =
  if mem k m then
    raise (Invalid_argument (Printf.sprintf "key %s already present in map." k))
  else add k v m

let union_disjoint ?(common_keys_equal_elt = ([], ( = ))) x y =
  let common_keys, equal_elt = common_keys_equal_elt in
  union
    (fun key elt_1 elt_2 ->
      if not (List.mem key common_keys) then
        raise
          (Invalid_argument
             (Printf.sprintf
                "the key %s appears in both union arguments and does not belong\n\
                \                                 to common_keys."
                key))
      else if not (equal_elt elt_1 elt_2) then
        raise
          (Invalid_argument
             (Printf.sprintf
                "the key %s appears in both union argument with different \
                 values"
                key))
      else Some elt_1)
    x
    y

let union_disjoint_list ?(common_keys_equal_elt = ([], ( = ))) map_list =
  List.fold_left (union_disjoint ~common_keys_equal_elt) empty map_list

let map_list_to_list_map map_list =
  let join _key x y = Some (x @ y) in
  List.fold_left
    (fun list_map m -> union join list_map (map (fun x -> [x]) m))
    empty
    map_list

let sub_map sub_map map =
  let res = filter (fun name _ -> mem name sub_map) map in
  if cardinal res <> cardinal sub_map then
    failwith "sub_map : first argument is not contained in the second." ;
  res

let update_keys f map = fold (fun k v acc -> add (f k) v acc) map empty

module Aggregation = struct
  (* separator between prefixes & name ; must be only one character *)
  let sep = "~"

  let update_key_name f str =
    match String.rindex_from_opt str (String.length str - 1) sep.[0] with
    | None -> f str
    | Some i ->
        String.sub str 0 (i + 1)
        ^ f (String.sub str (i + 1) (String.length str - i - 1))

  let padded ~n i =
    let str = string_of_int i in
    let len = String.length (string_of_int (n - 1)) in
    String.(make (len - length str) '0') ^ str

  let add_prefix ?(no_sep = false) ?(n = 0) ?(i = 0) ?(shift = 0) prefix str =
    let prefix = if prefix = "" || no_sep then prefix else prefix ^ sep in
    if n = 0 then prefix ^ str else prefix ^ padded ~n (i + shift) ^ sep ^ str

  let build_all_names prefix n name =
    List.init n (fun i -> add_prefix ~n ~i prefix name)

  let prefix_map ?n ?i ?shift prefix str_map =
    fold (fun k -> add (add_prefix ?n ?i ?shift prefix k)) str_map empty

  let of_list ?n ?shift prefix name l =
    of_list @@ List.mapi (fun i x -> (add_prefix ?n ~i ?shift prefix name, x)) l

  (* This function will merge the maps of the list, by prefixing each key with it’s index in the list, optionnally with a shift, with the index prefix prefixed with zero to we able to handle n elements with the same prefix size (with n either given by [shift] or by the length of [list_map]) ; if a [prefix] is given, it will be put before the index.
       *)
  let map_of_list_map ?(prefix = "") ?shift list_map =
    let shift, n = Option.value ~default:(0, List.length list_map) shift in
    List.mapi (fun i m -> prefix_map ~n ~i ~shift prefix m) list_map
    |> union_disjoint_list

  let smap_of_smap_smap mapmap =
    fold (fun prefix map res -> prefix_map prefix map :: res) mapmap []
    |> union_disjoint_list

  let gather_maps ?(shifts_map = empty) map_list_map =
    mapi
      (fun name list_map ->
        map_of_list_map ?shift:(find_opt name shifts_map) list_map)
      map_list_map
    |> smap_of_smap_smap

  let filter_by_circuit_name circuit_name =
    let sep_char =
      assert (String.length sep = 1) ;
      String.get sep 0
    in
    filter (fun key _ ->
        let name_parts = String.split_on_char sep_char key in
        circuit_name = ""
        || List.exists
             (String.equal circuit_name)
             (* we exclude the last element in [name_parts] *)
             (List.rev name_parts |> List.tl))

  let select_answers_by_circuit circuit_name =
    map (filter_by_circuit_name circuit_name)

  let add_map_list_map m1 m2 =
    mapi
      (fun k l1 ->
        match find_opt k m2 with
        | Some l2 -> List.map2 union_disjoint l1 l2
        | None -> l1)
      m1
end
