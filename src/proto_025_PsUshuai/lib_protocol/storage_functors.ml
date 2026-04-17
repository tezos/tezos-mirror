(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Storage_sigs

module Registered = struct
  let ghost = false
end

module Ghost = struct
  let ghost = true
end

module type ENCODER = sig
  type t

  val of_bytes : key:(unit -> string list) -> bytes -> t tzresult

  val to_bytes : t -> bytes
end

module Make_encoder (V : VALUE) : ENCODER with type t := V.t = struct
  let of_bytes ~key b =
    let open Result_syntax in
    match Data_encoding.Binary.of_bytes_opt V.encoding b with
    | None -> tzfail (Raw_context.Storage_error (Corrupted_data (key ())))
    | Some v -> return v

  let to_bytes v =
    match Data_encoding.Binary.to_bytes_opt V.encoding v with
    | Some b -> b
    | None -> Bytes.empty
end

let len_name = "len"

let data_name = "data"

let encode_len_value bytes =
  let length = Bytes.length bytes in
  Data_encoding.(Binary.to_bytes_exn int31) length

let decode_len_value key len =
  let open Result_syntax in
  match Data_encoding.(Binary.of_bytes_opt int31) len with
  | None -> tzfail (Raw_context.Storage_error (Corrupted_data key))
  | Some len -> return len

module Make_subcontext (R : REGISTER) (C : Raw_context.T) (N : NAME) :
  Raw_context.T with type t = C.t and type local_context = C.local_context =
struct
  type t = C.t

  type local_context = C.local_context

  let to_key k = N.name @ k

  let mem t k = C.mem t (to_key k)

  let mem_tree t k = C.mem_tree t (to_key k)

  let get t k = C.get t (to_key k)

  let get_tree t k = C.get_tree t (to_key k)

  let find t k = C.find t (to_key k)

  let find_tree t k = C.find_tree t (to_key k)

  let add t k v = C.add t (to_key k) v

  let add_tree t k v = C.add_tree t (to_key k) v

  let init t k v = C.init t (to_key k) v

  let init_tree t k v = C.init_tree t (to_key k) v

  let update t k v = C.update t (to_key k) v

  let update_tree t k v = C.update_tree t (to_key k) v

  let add_or_remove t k v = C.add_or_remove t (to_key k) v

  let add_or_remove_tree t k v = C.add_or_remove_tree t (to_key k) v

  let remove_existing t k = C.remove_existing t (to_key k)

  let remove_existing_tree t k = C.remove_existing_tree t (to_key k)

  let remove t k = C.remove t (to_key k)

  let list t ?offset ?length k = C.list t ?offset ?length (to_key k)

  let fold ?depth t k ~order ~init ~f =
    C.fold ?depth t (to_key k) ~order ~init ~f

  let config t = C.config t

  module Tree = C.Tree
  module Proof = C.Proof

  let verify_tree_proof = C.verify_tree_proof

  let verify_stream_proof = C.verify_stream_proof

  let equal_config = C.equal_config

  let project = C.project

  let absolute_key c k = C.absolute_key c (to_key k)

  type error += Block_quota_exceeded = C.Block_quota_exceeded

  type error += Operation_quota_exceeded = C.Operation_quota_exceeded

  let consume_gas = C.consume_gas

  let check_enough_gas = C.check_enough_gas

  let description =
    let description =
      if R.ghost then Storage_description.create () else C.description
    in
    Storage_description.register_named_subcontext description N.name

  let length = C.length

  let with_local_context ctxt k f = C.with_local_context ctxt (to_key k) f

  module Local_context = C.Local_context
end

module Make_single_data_storage
    (R : REGISTER)
    (C : Raw_context.T)
    (N : NAME)
    (V : VALUE) : Single_data_storage with type t = C.t and type value = V.t =
struct
  type t = C.t

  type context = t

  type value = V.t

  let mem t = C.mem t N.name

  include Make_encoder (V)

  let get t =
    let open Lwt_result_syntax in
    let* b = C.get t N.name in
    let key () = C.absolute_key t N.name in
    let*? v = of_bytes ~key b in
    return v

  let find t =
    let open Lwt_result_syntax in
    let*! bytes_opt = C.find t N.name in
    match bytes_opt with
    | None -> return_none
    | Some b ->
        let key () = C.absolute_key t N.name in
        let*? v = of_bytes ~key b in
        return_some v

  let init t v =
    let open Lwt_result_syntax in
    let+ t = C.init t N.name (to_bytes v) in
    C.project t

  let update t v =
    let open Lwt_result_syntax in
    let+ t = C.update t N.name (to_bytes v) in
    C.project t

  let add t v =
    let open Lwt_syntax in
    let+ t = C.add t N.name (to_bytes v) in
    C.project t

  let add_or_remove t v =
    let open Lwt_syntax in
    let+ t = C.add_or_remove t N.name (Option.map to_bytes v) in
    C.project t

  let remove t =
    let open Lwt_syntax in
    let+ t = C.remove t N.name in
    C.project t

  let remove_existing t =
    let open Lwt_result_syntax in
    let+ t = C.remove_existing t N.name in
    C.project t

  let () =
    let open Storage_description in
    let description =
      if R.ghost then Storage_description.create () else C.description
    in
    register_value
      ~get:find
      (register_named_subcontext description N.name)
      V.encoding
end

module type INDEX = sig
  type t

  include Path_encoding.S with type t := t

  type 'a ipath

  val args : ('a, t, 'a ipath) Storage_description.args
end

module Pair (I1 : INDEX) (I2 : INDEX) : INDEX with type t = I1.t * I2.t = struct
  type t = I1.t * I2.t

  let path_length = I1.path_length + I2.path_length

  let to_path (x, y) l = I1.to_path x (I2.to_path y l)

  let of_path l =
    match Misc.take I1.path_length l with
    | None -> None
    | Some (l1, l2) -> (
        match (I1.of_path l1, I2.of_path l2) with
        | Some x, Some y -> Some (x, y)
        | _ -> None)

  type 'a ipath = 'a I1.ipath I2.ipath

  let args = Storage_description.Pair (I1.args, I2.args)
end

module Make_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Data_set_storage with type t = C.t and type elt = I.t = struct
  type t = C.t

  type context = t

  type elt = I.t

  let inited = Bytes.of_string "inited"

  let mem s i = C.mem s (I.to_path i [])

  let add s i =
    let open Lwt_syntax in
    let+ t = C.add s (I.to_path i []) inited in
    C.project t

  let remove s i =
    let open Lwt_syntax in
    let+ t = C.remove s (I.to_path i []) in
    C.project t

  let clear s =
    let open Lwt_syntax in
    let+ t = C.remove s [] in
    C.project t

  let fold s ~order ~init ~f =
    C.fold ~depth:(`Eq I.path_length) s [] ~order ~init ~f:(fun file tree acc ->
        match C.Tree.kind tree with
        | `Value -> (
            match I.of_path file with None -> assert false | Some p -> f p acc)
        | `Tree -> Lwt.return acc)

  let elements s =
    fold s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

  let () =
    let open Lwt_result_syntax in
    let open Storage_description in
    let unpack = unpack I.args in
    register_value (* TODO fixme 'elements...' *)
      ~get:(fun c ->
        let c, k = unpack c in
        let*! result = mem c k in
        match result with true -> return_some true | false -> return_none)
      (register_indexed_subcontext
         ~list:(fun c ->
           let*! result = elements c in
           return result)
         C.description
         I.args)
      Data_encoding.bool
end

module Make_indexed_data_storage (C : Raw_context.T) (I : INDEX) (V : VALUE) :
  Indexed_data_storage with type t = C.t and type key = I.t and type value = V.t =
struct
  type t = C.t

  type context = t

  type key = I.t

  type value = V.t

  include Make_encoder (V)

  let mem s i = C.mem s (I.to_path i [])

  let is_empty i =
    let open Lwt_syntax in
    let* root = C.find_tree i [] in
    match root with
    | None -> return_true
    | Some root -> return @@ C.Tree.is_empty root

  let get s i =
    let open Lwt_result_syntax in
    let* b = C.get s (I.to_path i []) in
    let key () = C.absolute_key s (I.to_path i []) in
    let*? v = of_bytes ~key b in
    return v

  let find s i =
    let open Lwt_result_syntax in
    let*! bytes_opt = C.find s (I.to_path i []) in
    match bytes_opt with
    | None -> return_none
    | Some b ->
        let key () = C.absolute_key s (I.to_path i []) in
        let*? v = of_bytes ~key b in
        return_some v

  let update s i v =
    let open Lwt_result_syntax in
    let+ t = C.update s (I.to_path i []) (to_bytes v) in
    C.project t

  let init s i v =
    let open Lwt_result_syntax in
    let+ t = C.init s (I.to_path i []) (to_bytes v) in
    C.project t

  let add s i v =
    let open Lwt_syntax in
    let+ t = C.add s (I.to_path i []) (to_bytes v) in
    C.project t

  let add_or_remove s i v =
    let open Lwt_syntax in
    let+ t = C.add_or_remove s (I.to_path i []) (Option.map to_bytes v) in
    C.project t

  let remove s i =
    let open Lwt_syntax in
    let+ t = C.remove s (I.to_path i []) in
    C.project t

  let remove_existing s i =
    let open Lwt_result_syntax in
    let+ t = C.remove_existing s (I.to_path i []) in
    C.project t

  let clear s =
    let open Lwt_syntax in
    let+ t = C.remove s [] in
    C.project t

  let fold s ~order ~init ~f =
    let open Lwt_syntax in
    C.fold ~depth:(`Eq I.path_length) s [] ~order ~init ~f:(fun file tree acc ->
        let* bytes_opt = C.Tree.to_value tree in
        match bytes_opt with
        | Some v -> (
            match I.of_path file with
            | None -> assert false
            | Some path -> (
                let key () = C.absolute_key s file in
                match of_bytes ~key v with
                | Ok v -> f path v acc
                | Error _ -> return acc))
        | None -> return acc)

  let fold_keys s ~order ~init ~f =
    fold s ~order ~init ~f:(fun k _ acc -> f k acc)

  let bindings s =
    fold s ~order:`Sorted ~init:[] ~f:(fun p v acc ->
        Lwt.return ((p, v) :: acc))

  let keys s =
    fold_keys s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

  let () =
    let open Lwt_result_syntax in
    let open Storage_description in
    let unpack = unpack I.args in
    register_value
      ~get:(fun c ->
        let c, k = unpack c in
        find c k)
      (register_indexed_subcontext
         ~list:(fun c ->
           let*! result = keys c in
           return result)
         C.description
         I.args)
      V.encoding
end

(* Internal-use-only version of {!Make_indexed_carbonated_data_storage} to
   expose fold_keys_unaccounted *)
module Make_indexed_carbonated_data_storage_INTERNAL
    (C : Raw_context.T)
    (I : INDEX)
    (V : VALUE) :
  Indexed_carbonated_data_storage_INTERNAL
    with type t = C.t
     and type key = I.t
     and type value = V.t = struct
  type t = C.t

  type context = t

  type key = I.t

  type value = V.t

  include Make_encoder (V)

  let data_key i = I.to_path i [data_name]

  let len_key i = I.to_path i [len_name]

  let consume_mem_gas c key =
    let path_length = List.length @@ C.absolute_key c key in
    C.consume_gas c (Storage_costs.read_access ~path_length ~read_bytes:0)

  let existing_size c i =
    let open Lwt_result_syntax in
    let*! bytes_opt = C.find c (len_key i) in
    match bytes_opt with
    | None -> return (0, false)
    | Some len ->
        let*? len = decode_len_value (len_key i) len in
        return (len, true)

  let consume_read_gas get c i =
    let open Lwt_result_syntax in
    let len_key = len_key i in
    let* len = get c len_key in
    let path_length = List.length @@ C.absolute_key c len_key in
    let*? read_bytes = decode_len_value len_key len in
    let cost = Storage_costs.read_access ~path_length ~read_bytes in
    let*? t = C.consume_gas c cost in
    return t

  (* For the future: here, we bill a generic cost for encoding the value
     to bytes. It would be cleaner for users of this functor to provide
     gas costs for the encoding. *)
  let consume_serialize_write_gas set c i v =
    let open Lwt_result_syntax in
    let bytes = to_bytes v in
    let len = Bytes.length bytes in
    let*? c = C.consume_gas c (Gas_limit_repr.alloc_mbytes_cost len) in
    let cost = Storage_costs.write_access ~written_bytes:len in
    let*? c = C.consume_gas c cost in
    let+ c = set c (len_key i) (encode_len_value bytes) in
    (c, bytes)

  let consume_remove_gas del c i =
    let open Lwt_result_syntax in
    let*? c = C.consume_gas c (Storage_costs.write_access ~written_bytes:0) in
    del c (len_key i)

  let mem s i =
    let open Lwt_result_syntax in
    let key = data_key i in
    let*? s = consume_mem_gas s key in
    let*! exists = C.mem s key in
    return (C.project s, exists)

  let is_empty s =
    let open Lwt_result_syntax in
    let root_key = [] in
    let*? s = consume_mem_gas s root_key in
    let*! root_opt = C.find_tree s root_key in
    match root_opt with
    | None -> return @@ (C.project s, true)
    | Some root ->
        let is_empty = C.Tree.is_empty root in
        return @@ (C.project s, is_empty)

  let get_unprojected s i =
    let open Lwt_result_syntax in
    let* s = consume_read_gas C.get s i in
    let* b = C.get s (data_key i) in
    let key () = C.absolute_key s (data_key i) in
    let*? v = of_bytes ~key b in
    return (s, v)

  let get s i =
    let open Lwt_result_syntax in
    let+ s, v = get_unprojected s i in
    (C.project s, v)

  let find s i =
    let open Lwt_result_syntax in
    let key = data_key i in
    let*? s = consume_mem_gas s key in
    let*! exists = C.mem s key in
    if exists then
      let+ s, v = get s i in
      (s, Some v)
    else return (C.project s, None)

  let update s i v =
    let open Lwt_result_syntax in
    let* prev_size, _ = existing_size s i in
    let* s, bytes = consume_serialize_write_gas C.update s i v in
    let+ t = C.update s (data_key i) bytes in
    let size_diff = Bytes.length bytes - prev_size in
    (C.project t, size_diff)

  let init s i v =
    let open Lwt_result_syntax in
    let* s, bytes = consume_serialize_write_gas C.init s i v in
    let+ t = C.init s (data_key i) bytes in
    let size = Bytes.length bytes in
    (C.project t, size)

  let add s i v =
    let open Lwt_result_syntax in
    let add s i v =
      let*! ctxt = C.add s i v in
      return ctxt
    in
    let* prev_size, existed = existing_size s i in
    let* s, bytes = consume_serialize_write_gas add s i v in
    let+ t = add s (data_key i) bytes in
    let size_diff = Bytes.length bytes - prev_size in
    (C.project t, size_diff, existed)

  let remove s i =
    let open Lwt_result_syntax in
    let remove s i =
      let*! ctxt = C.remove s i in
      return ctxt
    in
    let* prev_size, existed = existing_size s i in
    let* s = consume_remove_gas remove s i in
    let+ t = remove s (data_key i) in
    (C.project t, prev_size, existed)

  let clear s =
    let open Lwt_result_syntax in
    let*? s = C.consume_gas s (Storage_costs.write_access ~written_bytes:0) in
    let*! t = C.remove s [] in
    return (C.project t)

  let remove_existing s i =
    let open Lwt_result_syntax in
    let* prev_size, _ = existing_size s i in
    let* s = consume_remove_gas C.remove_existing s i in
    let+ t = C.remove_existing s (data_key i) in
    (C.project t, prev_size)

  let add_or_remove s i v =
    match v with None -> remove s i | Some v -> add s i v

  (* TODO https://gitlab.com/tezos/tezos/-/issues/3318
     Switch implementation to use [C.list].
     Given that MR !2771 which flattens paths is done, we should use
     [C.list] to avoid having to iterate over all keys when [length] and/or
     [offset] is passed.
  *)
  let list_key_values ?(offset = 0) ?(length = max_int) s =
    let open Lwt_result_syntax in
    let root = [] in
    let depth = `Eq I.path_length in
    let*! size = C.length s root in
    (* Regardless of the [length] argument, all elements stored in the context
       are traversed. We therefore pay a gas cost proportional to the number of
       elements, given by [size], upfront. We also pay gas for decoding elements
       whenever they are loaded in the body of the fold. *)
    let*? s = C.consume_gas s (Storage_costs.list_key_values_traverse ~size) in
    let+ s, rev_values, _offset, _length =
      C.fold
        s
        root
        ~depth
        ~order:`Sorted
        ~init:(Ok (s, [], offset, length))
        ~f:(fun file tree acc ->
          match (C.Tree.kind tree, acc) with
          | `Tree, Ok (s, rev_values, offset, length) -> (
              if Compare.Int.(length <= 0) then
                (* Keep going until the end, we have no means of short-circuiting *)
                Lwt.return acc
              else if Compare.Int.(offset > 0) then
                (* Offset (first element) not reached yet *)
                let offset = pred offset in
                Lwt.return (Ok (s, rev_values, offset, length))
              else
                (* Nominal case *)
                match I.of_path file with
                | None -> assert false
                | Some key ->
                    (* This also accounts for gas for loading the element. *)
                    let+ s, value = get_unprojected s key in
                    (s, (key, value) :: rev_values, 0, pred length))
          | _ ->
              (* Even if we run out of gas or fail in some other way, we still
                 traverse the whole tree. In this case there is no context to
                 update. *)
              Lwt.return acc)
    in
    (C.project s, List.rev rev_values)

  let fold_keys_unaccounted s ~order ~init ~f =
    C.fold
      ~depth:(`Eq (1 + I.path_length))
      s
      []
      ~order
      ~init
      ~f:(fun file tree acc ->
        match C.Tree.kind tree with
        | `Value -> (
            match List.rev file with
            | last :: _ when Compare.String.(last = len_name) -> Lwt.return acc
            | last :: rest when Compare.String.(last = data_name) -> (
                let file = List.rev rest in
                match I.of_path file with
                | None -> assert false
                | Some path -> f path acc)
            | _ -> assert false)
        | `Tree -> Lwt.return acc)

  let keys_unaccounted s =
    fold_keys_unaccounted s ~order:`Sorted ~init:[] ~f:(fun p acc ->
        Lwt.return (p :: acc))

  let () =
    let open Storage_description in
    let open Lwt_result_syntax in
    let unpack = unpack I.args in
    register_value (* TODO export consumed gas ?? *)
      ~get:(fun c ->
        let c, k = unpack c in
        let+ _, v = find c k in
        v)
      (register_indexed_subcontext
         ~list:(fun c ->
           let*! result = keys_unaccounted c in
           return result)
         C.description
         I.args)
      V.encoding
end

module Make_indexed_carbonated_data_storage : functor
  (C : Raw_context.T)
  (I : INDEX)
  (V : VALUE)
  ->
  Indexed_carbonated_data_storage
    with type t = C.t
     and type key = I.t
     and type value = V.t =
  Make_indexed_carbonated_data_storage_INTERNAL

module Make_carbonated_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Carbonated_data_set_storage with type t = C.t and type elt = I.t = struct
  module V = struct
    type t = unit

    let encoding = Data_encoding.unit
  end

  module M = Make_indexed_carbonated_data_storage_INTERNAL (C) (I) (V)

  type t = M.t

  type context = t

  type elt = I.t

  let mem = M.mem

  let is_empty = M.is_empty

  let clear = M.clear

  let init s i = M.init s i ()

  let add s i = M.add s i ()

  let remove s i = M.remove s i

  let fold_keys_unaccounted = M.fold_keys_unaccounted
end

module Make_indexed_data_snapshotable_storage
    (C : Raw_context.T)
    (Snapshot_index : INDEX)
    (I : INDEX)
    (V : VALUE) :
  Indexed_data_snapshotable_storage
    with type t = C.t
     and type snapshot = Snapshot_index.t
     and type key = I.t
     and type value = V.t = struct
  type snapshot = Snapshot_index.t

  let data_name = ["current"]

  let snapshot_name = ["snapshot"]

  module C_data =
    Make_subcontext (Registered) (C)
      (struct
        let name = data_name
      end)

  module C_snapshot =
    Make_subcontext (Registered) (C)
      (struct
        let name = snapshot_name
      end)

  module V_encoder = Make_encoder (V)
  include Make_indexed_data_storage (C_data) (I) (V)
  module Snapshot =
    Make_indexed_data_storage (C_snapshot) (Pair (Snapshot_index) (I)) (V)

  let snapshot_path id = snapshot_name @ Snapshot_index.to_path id []

  let snapshot_exists s id = C.mem_tree s (snapshot_path id)

  let err_missing_key key = Raw_context.storage_error (Missing_key (key, Copy))

  let snapshot s id =
    let open Lwt_result_syntax in
    let*! tree_opt = C.find_tree s data_name in
    match tree_opt with
    | None -> Lwt.return (err_missing_key data_name)
    | Some tree ->
        let*! t = C.add_tree s (snapshot_path id) tree in
        return (C.project t)

  let fold_snapshot s id ~order ~init ~f =
    let open Lwt_result_syntax in
    let*! tree_opt = C.find_tree s (snapshot_path id) in
    match tree_opt with
    | None -> Lwt.return (err_missing_key data_name)
    | Some tree ->
        C_data.Tree.fold
          tree
          ~depth:(`Eq I.path_length)
          []
          ~order
          ~init:(Ok init)
          ~f:(fun file tree acc ->
            let*? acc in
            let*! bytes_opt = C.Tree.to_value tree in
            match bytes_opt with
            | Some v -> (
                match I.of_path file with
                | None -> assert false
                | Some path -> (
                    let key () = C.absolute_key s file in
                    match V_encoder.of_bytes ~key v with
                    | Ok v -> f path v acc
                    | Error _ -> return acc))
            | None -> return acc)

  let delete_snapshot s id =
    let open Lwt_syntax in
    let+ t = C.remove s (snapshot_path id) in
    C.project t
end

module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX) :
  Indexed_raw_context
    with type t = C.t
     and type key = I.t
     and type 'a ipath = 'a I.ipath
     and type local_context = C.local_context = struct
  type t = C.t

  type context = t

  type key = I.t

  type 'a ipath = 'a I.ipath

  type local_context = C.local_context

  let clear t =
    let open Lwt_syntax in
    let+ t = C.remove t [] in
    C.project t

  let is_empty i =
    let open Lwt_syntax in
    let* root = C.find_tree i [] in
    match root with
    | None -> return_true
    | Some root -> return @@ C.Tree.is_empty root

  let fold_keys t ~order ~init ~f =
    C.fold ~depth:(`Eq I.path_length) t [] ~order ~init ~f:(fun path tree acc ->
        match C.Tree.kind tree with
        | `Tree -> (
            match I.of_path path with
            | None -> assert false
            | Some path -> f path acc)
        | `Value -> Lwt.return acc)

  let keys t =
    fold_keys t ~order:`Sorted ~init:[] ~f:(fun i acc -> Lwt.return (i :: acc))

  let err_missing_key key = Raw_context.storage_error (Missing_key (key, Copy))

  let copy t ~from ~to_ =
    let open Lwt_result_syntax in
    let from = I.to_path from [] in
    let to_ = I.to_path to_ [] in
    let*! tree_opt = C.find_tree t from in
    match tree_opt with
    | None -> Lwt.return (err_missing_key from)
    | Some tree ->
        let*! ctxt = C.add_tree t to_ tree in
        return ctxt

  let remove t k = C.remove t (I.to_path k [])

  let description =
    let open Lwt_result_syntax in
    Storage_description.register_indexed_subcontext
      ~list:(fun c ->
        let*! result = keys c in
        return result)
      C.description
      I.args

  let unpack = Storage_description.unpack I.args

  let pack = Storage_description.pack I.args

  module Raw_context :
    Raw_context.T
      with type t = C.t I.ipath
       and type local_context = C.local_context = struct
    type t = C.t I.ipath

    type local_context = C.local_context

    let to_key i k = I.to_path i k

    let mem c k =
      let t, i = unpack c in
      C.mem t (to_key i k)

    let mem_tree c k =
      let t, i = unpack c in
      C.mem_tree t (to_key i k)

    let get c k =
      let t, i = unpack c in
      C.get t (to_key i k)

    let get_tree c k =
      let t, i = unpack c in
      C.get_tree t (to_key i k)

    let find c k =
      let t, i = unpack c in
      C.find t (to_key i k)

    let find_tree c k =
      let t, i = unpack c in
      C.find_tree t (to_key i k)

    let list c ?offset ?length k =
      let t, i = unpack c in
      C.list t ?offset ?length (to_key i k)

    let init c k v =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t = C.init t (to_key i k) v in
      pack t i

    let init_tree c k v =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t = C.init_tree t (to_key i k) v in
      pack t i

    let update c k v =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t = C.update t (to_key i k) v in
      pack t i

    let update_tree c k v =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t = C.update_tree t (to_key i k) v in
      pack t i

    let add c k v =
      let open Lwt_syntax in
      let t, i = unpack c in
      let+ t = C.add t (to_key i k) v in
      pack t i

    let add_tree c k v =
      let open Lwt_syntax in
      let t, i = unpack c in
      let+ t = C.add_tree t (to_key i k) v in
      pack t i

    let add_or_remove c k v =
      let open Lwt_syntax in
      let t, i = unpack c in
      let+ t = C.add_or_remove t (to_key i k) v in
      pack t i

    let add_or_remove_tree c k v =
      let open Lwt_syntax in
      let t, i = unpack c in
      let+ t = C.add_or_remove_tree t (to_key i k) v in
      pack t i

    let remove_existing c k =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t = C.remove_existing t (to_key i k) in
      pack t i

    let remove_existing_tree c k =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t = C.remove_existing_tree t (to_key i k) in
      pack t i

    let remove c k =
      let open Lwt_syntax in
      let t, i = unpack c in
      let+ t = C.remove t (to_key i k) in
      pack t i

    let fold ?depth c k ~order ~init ~f =
      let t, i = unpack c in
      C.fold ?depth t (to_key i k) ~order ~init ~f

    let config c =
      let t, _ = unpack c in
      C.config t

    module Tree = struct
      include C.Tree

      let empty c =
        let t, _ = unpack c in
        C.Tree.empty t
    end

    module Proof = C.Proof

    let verify_tree_proof = C.verify_tree_proof

    let verify_stream_proof = C.verify_stream_proof

    let equal_config = C.equal_config

    let project c =
      let t, _ = unpack c in
      C.project t

    let absolute_key c k =
      let t, i = unpack c in
      C.absolute_key t (to_key i k)

    type error += Block_quota_exceeded = C.Block_quota_exceeded

    type error += Operation_quota_exceeded = C.Operation_quota_exceeded

    let consume_gas c g =
      let open Result_syntax in
      let t, i = unpack c in
      let* t = C.consume_gas t g in
      return (pack t i)

    let check_enough_gas c g =
      let t, _i = unpack c in
      C.check_enough_gas t g

    let description = description

    let length c =
      let t, _i = unpack c in
      C.length t

    let with_local_context c k f =
      let open Lwt_result_syntax in
      let t, i = unpack c in
      let+ t, res = C.with_local_context t (to_key i k) f in
      (pack t i, res)

    module Local_context = C.Local_context
  end

  let with_local_context s i f =
    let open Lwt_result_syntax in
    let+ c, x = Raw_context.with_local_context (pack s i) [] f in
    let s, _ = unpack c in
    (s, x)

  module Make_set (R : REGISTER) (N : NAME) :
    Data_set_storage with type t = t and type elt = key = struct
    type t = C.t

    type context = t

    type elt = I.t

    let inited = Bytes.of_string "inited"

    let mem s i = Raw_context.mem (pack s i) N.name

    let add s i =
      let open Lwt_syntax in
      let+ c = Raw_context.add (pack s i) N.name inited in
      let s, _ = unpack c in
      C.project s

    let remove s i =
      let open Lwt_syntax in
      let+ c = Raw_context.remove (pack s i) N.name in
      let s, _ = unpack c in
      C.project s

    let clear s =
      let open Lwt_syntax in
      let+ t =
        fold_keys s ~init:s ~order:`Sorted ~f:(fun i s ->
            let+ c = Raw_context.remove (pack s i) N.name in
            let s, _ = unpack c in
            s)
      in
      C.project t

    let fold s ~order ~init ~f =
      let open Lwt_syntax in
      fold_keys s ~order ~init ~f:(fun i acc ->
          let* result = mem s i in
          match result with true -> f i acc | false -> Lwt.return acc)

    let elements s =
      fold s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

    let () =
      let open Lwt_result_syntax in
      let open Storage_description in
      let unpack = unpack I.args in
      let description =
        if R.ghost then Storage_description.create ()
        else Raw_context.description
      in
      register_value
        ~get:(fun c ->
          let c, k = unpack c in
          let*! result = mem c k in
          match result with true -> return_some true | false -> return_none)
        (register_named_subcontext description N.name)
        Data_encoding.bool
  end

  module Make_map (R : REGISTER) (N : NAME) (V : VALUE) :
    Indexed_data_storage_with_local_context
      with type t = t
       and type key = key
       and type value = V.t
       and type local_context = local_context = struct
    type t = C.t

    type context = t

    type key = I.t

    type value = V.t

    type nonrec local_context = local_context

    include Make_encoder (V)

    let is_empty i =
      let open Lwt_syntax in
      let* root = C.find_tree i [] in
      match root with
      | None -> return_true
      | Some root -> return @@ C.Tree.is_empty root

    let mem s i = Raw_context.mem (pack s i) N.name

    let get s i =
      let open Lwt_result_syntax in
      let* b = Raw_context.get (pack s i) N.name in
      let key () = Raw_context.absolute_key (pack s i) N.name in
      let*? v = of_bytes ~key b in
      return v

    let find s i =
      let open Lwt_result_syntax in
      let*! bytes_opt = Raw_context.find (pack s i) N.name in
      match bytes_opt with
      | None -> return_none
      | Some b ->
          let key () = Raw_context.absolute_key (pack s i) N.name in
          let*? v = of_bytes ~key b in
          return_some v

    let update s i v =
      let open Lwt_result_syntax in
      let+ c = Raw_context.update (pack s i) N.name (to_bytes v) in
      let s, _ = unpack c in
      C.project s

    let init s i v =
      let open Lwt_result_syntax in
      let+ c = Raw_context.init (pack s i) N.name (to_bytes v) in
      let s, _ = unpack c in
      C.project s

    let add s i v =
      let open Lwt_syntax in
      let+ c = Raw_context.add (pack s i) N.name (to_bytes v) in
      let s, _ = unpack c in
      C.project s

    let add_or_remove s i v =
      let open Lwt_syntax in
      let+ c =
        Raw_context.add_or_remove (pack s i) N.name (Option.map to_bytes v)
      in
      let s, _ = unpack c in
      C.project s

    let remove s i =
      let open Lwt_syntax in
      let+ c = Raw_context.remove (pack s i) N.name in
      let s, _ = unpack c in
      C.project s

    let remove_existing s i =
      let open Lwt_result_syntax in
      let+ c = Raw_context.remove_existing (pack s i) N.name in
      let s, _ = unpack c in
      C.project s

    let clear s =
      let open Lwt_syntax in
      let+ t =
        fold_keys s ~order:`Sorted ~init:s ~f:(fun i s ->
            let+ c = Raw_context.remove (pack s i) N.name in
            let s, _ = unpack c in
            s)
      in
      C.project t

    let fold s ~order ~init ~f =
      let open Lwt_syntax in
      fold_keys s ~order ~init ~f:(fun i acc ->
          let* value_opt = get s i in
          match value_opt with Error _ -> return acc | Ok v -> f i v acc)

    let bindings s =
      fold s ~order:`Sorted ~init:[] ~f:(fun p v acc ->
          Lwt.return ((p, v) :: acc))

    let fold_keys s ~order ~init ~f =
      let open Lwt_syntax in
      fold_keys s ~order ~init ~f:(fun i acc ->
          let* result = mem s i in
          match result with false -> return acc | true -> f i acc)

    let keys s =
      fold_keys s ~order:`Sorted ~init:[] ~f:(fun p acc ->
          Lwt.return (p :: acc))

    let () =
      let open Storage_description in
      let unpack = unpack I.args in
      let description =
        if R.ghost then Storage_description.create ()
        else Raw_context.description
      in
      register_value
        ~get:(fun c ->
          let c, k = unpack c in
          find c k)
        (register_named_subcontext description N.name)
        V.encoding

    module Local = struct
      type context = Raw_context.Local_context.t

      let mem local = Raw_context.Local_context.mem local N.name

      let get local =
        let open Lwt_result_syntax in
        let* r = Raw_context.Local_context.get local N.name in
        let key () = Raw_context.Local_context.absolute_key local N.name in
        let*? v = of_bytes ~key r in
        return v

      let find local =
        let open Lwt_result_syntax in
        let*! bytes_opt = Raw_context.Local_context.find local N.name in
        match bytes_opt with
        | None -> return_none
        | Some b ->
            let key () = Raw_context.Local_context.absolute_key local N.name in
            let*? v = of_bytes ~key b in
            return_some v

      let init local v =
        Raw_context.Local_context.init local N.name (to_bytes v)

      let update local v =
        Raw_context.Local_context.update local N.name (to_bytes v)

      let add local v = Raw_context.Local_context.add local N.name (to_bytes v)

      let add_or_remove local vo =
        Raw_context.Local_context.add_or_remove
          local
          N.name
          (Option.map to_bytes vo)

      let remove_existing local =
        Raw_context.Local_context.remove_existing local N.name

      let remove local = Raw_context.Local_context.remove local N.name
    end
  end

  module Make_carbonated_map (R : REGISTER) (N : NAME) (V : VALUE) :
    Non_iterable_indexed_carbonated_data_storage
      with type t = t
       and type key = key
       and type value = V.t = struct
    type t = C.t

    type context = t

    type key = I.t

    type value = V.t

    include Make_encoder (V)

    let len_name = len_name :: N.name

    let data_name = data_name :: N.name

    let consume_mem_gas c =
      let path_length = List.length (Raw_context.absolute_key c N.name) + 1 in
      Raw_context.consume_gas
        c
        (Storage_costs.read_access ~path_length ~read_bytes:0)

    let existing_size c =
      let open Lwt_result_syntax in
      let*! bytes_opt = Raw_context.find c len_name in
      match bytes_opt with
      | None -> return (0, false)
      | Some len ->
          let*? len = decode_len_value len_name len in
          return (len, true)

    let consume_read_gas get c =
      let open Lwt_result_syntax in
      let path_length = List.length (Raw_context.absolute_key c N.name) + 1 in
      let* len = get c len_name in
      let*? read_bytes = decode_len_value len_name len in
      let*? c =
        Raw_context.consume_gas
          c
          (Storage_costs.read_access ~path_length ~read_bytes)
      in
      return c

    let consume_write_gas set c v =
      let open Lwt_result_syntax in
      let bytes = to_bytes v in
      let len = Bytes.length bytes in
      let*? c =
        Raw_context.consume_gas
          c
          (Storage_costs.write_access ~written_bytes:len)
      in
      let+ c = set c len_name (encode_len_value bytes) in
      (c, bytes)

    let consume_remove_gas del c =
      let open Lwt_result_syntax in
      let*? c =
        Raw_context.consume_gas c (Storage_costs.write_access ~written_bytes:0)
      in
      del c len_name

    let mem s i =
      let open Lwt_result_syntax in
      let*? c = consume_mem_gas (pack s i) in
      let*! res = Raw_context.mem c data_name in
      return (Raw_context.project c, res)

    let get s i =
      let open Lwt_result_syntax in
      let* c = consume_read_gas Raw_context.get (pack s i) in
      let* b = Raw_context.get c data_name in
      let key () = Raw_context.absolute_key c data_name in
      let*? v = of_bytes ~key b in
      return (Raw_context.project c, v)

    let find s i =
      let open Lwt_result_syntax in
      let*? c = consume_mem_gas (pack s i) in
      let s, _ = unpack c in
      let*! exists = Raw_context.mem (pack s i) data_name in
      if exists then
        let+ s, v = get s i in
        (s, Some v)
      else return (C.project s, None)

    let update s i v =
      let open Lwt_result_syntax in
      let* prev_size, _ = existing_size (pack s i) in
      let* c, bytes = consume_write_gas Raw_context.update (pack s i) v in
      let+ c = Raw_context.update c data_name bytes in
      let size_diff = Bytes.length bytes - prev_size in
      (Raw_context.project c, size_diff)

    let init s i v =
      let open Lwt_result_syntax in
      let* c, bytes = consume_write_gas Raw_context.init (pack s i) v in
      let+ c = Raw_context.init c data_name bytes in
      let size = Bytes.length bytes in
      (Raw_context.project c, size)

    let add s i v =
      let open Lwt_result_syntax in
      let add c k v =
        let*! ctxt = Raw_context.add c k v in
        return ctxt
      in
      let* prev_size, existed = existing_size (pack s i) in
      let* c, bytes = consume_write_gas add (pack s i) v in
      let+ c = add c data_name bytes in
      let size_diff = Bytes.length bytes - prev_size in
      (Raw_context.project c, size_diff, existed)

    let remove s i =
      let open Lwt_result_syntax in
      let remove c k =
        let*! ctxt = Raw_context.remove c k in
        return ctxt
      in
      let* prev_size, existed = existing_size (pack s i) in
      let* c = consume_remove_gas remove (pack s i) in
      let+ c = remove c data_name in
      (Raw_context.project c, prev_size, existed)

    let remove_existing s i =
      let open Lwt_result_syntax in
      let* prev_size, _ = existing_size (pack s i) in
      let* c = consume_remove_gas Raw_context.remove_existing (pack s i) in
      let+ c = Raw_context.remove_existing c data_name in
      (Raw_context.project c, prev_size)

    let add_or_remove s i v =
      match v with None -> remove s i | Some v -> add s i v

    let mem_unaccounted s i = Raw_context.mem (pack s i) data_name

    let fold_keys_unaccounted s ~order ~init ~f =
      let open Lwt_syntax in
      fold_keys s ~order ~init ~f:(fun i acc ->
          let* result = mem_unaccounted s i in
          match result with false -> return acc | true -> f i acc)

    let keys_unaccounted s =
      fold_keys_unaccounted s ~order:`Sorted ~init:[] ~f:(fun p acc ->
          Lwt.return (p :: acc))

    let () =
      let open Storage_description in
      let open Lwt_result_syntax in
      let unpack = unpack I.args in
      let description =
        if R.ghost then Storage_description.create ()
        else Raw_context.description
      in
      register_value
        ~get:(fun c ->
          let c, k = unpack c in
          let+ _, v = find c k in
          v)
        (register_named_subcontext description N.name)
        V.encoding
  end
end

module type WRAPPER = sig
  type t

  type key

  val wrap : t -> key

  val unwrap : key -> t option
end

module Wrap_indexed_data_storage
    (C : Indexed_data_storage)
    (K : WRAPPER with type key := C.key) :
  Indexed_data_storage
    with type t = C.t
     and type key = K.t
     and type value = C.value = struct
  type t = C.t

  type context = C.t

  type key = K.t

  type value = C.value

  let is_empty ctxt = C.is_empty ctxt

  let mem ctxt k = C.mem ctxt (K.wrap k)

  let get ctxt k = C.get ctxt (K.wrap k)

  let find ctxt k = C.find ctxt (K.wrap k)

  let update ctxt k v = C.update ctxt (K.wrap k) v

  let init ctxt k v = C.init ctxt (K.wrap k) v

  let add ctxt k v = C.add ctxt (K.wrap k) v

  let add_or_remove ctxt k v = C.add_or_remove ctxt (K.wrap k) v

  let remove_existing ctxt k = C.remove_existing ctxt (K.wrap k)

  let remove ctxt k = C.remove ctxt (K.wrap k)

  let clear ctxt = C.clear ctxt

  let fold ctxt ~order ~init ~f =
    C.fold ctxt ~order ~init ~f:(fun k v acc ->
        match K.unwrap k with None -> Lwt.return acc | Some k -> f k v acc)

  let bindings s =
    fold s ~order:`Sorted ~init:[] ~f:(fun p v acc ->
        Lwt.return ((p, v) :: acc))

  let fold_keys s ~order ~init ~f =
    C.fold_keys s ~order ~init ~f:(fun k acc ->
        match K.unwrap k with None -> Lwt.return acc | Some k -> f k acc)

  let keys s =
    fold_keys s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))
end
