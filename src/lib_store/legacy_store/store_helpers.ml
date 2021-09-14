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

module Events = Store_events
open Store_sigs

module Make_value (V : ENCODED_VALUE) = struct
  type t = V.t

  let of_bytes b =
    match Data_encoding.Binary.of_bytes V.encoding b with
    | Error re ->
        error_with "Cannot parse data: %a" Data_encoding.Binary.pp_read_error re
    | Ok v -> ok v

  let to_bytes v =
    match Data_encoding.Binary.to_bytes V.encoding v with
    | Ok b -> b
    | Error we ->
        (* this debug event is asyncronous, and we don't have lwt here *)
        Events.(emit__dont_wait__use_with_care serializing_error we) ;
        Bytes.create 0
end

module Raw_value = struct
  type t = Bytes.t

  let of_bytes b = ok b

  let to_bytes b = b
end

module Make_single_store (S : STORE) (N : NAME) (V : VALUE) = struct
  type t = S.t

  type value = V.t

  let known t = S.known t N.name

  let read t = S.read t N.name >>=? fun b -> Lwt.return (V.of_bytes b)

  let read_opt t = read t >|= function Error _ -> None | Ok v -> Some v

  let store t v = S.store t N.name (V.to_bytes v)

  let remove t = S.remove t N.name
end

let map_key f = function `Key k -> `Key (f k) | `Dir k -> `Dir (f k)

module Make_substore (S : STORE) (N : NAME) : STORE with type t = S.t = struct
  type t = S.t

  type key = string list

  type value = Bytes.t

  let name_length = List.length N.name

  let to_key k = N.name @ k

  let of_key k = List.remove name_length k

  let known t k = S.known t (to_key k)

  let known_dir t k = S.known_dir t (to_key k)

  let read t k = S.read t (to_key k)

  let read_opt t k = S.read_opt t (to_key k)

  let store t k v = S.store t (to_key k) v

  let remove t k = S.remove t (to_key k)

  type key_or_dir = [`Key of key | `Dir of key]

  let fold t k ~init ~f =
    S.fold t (to_key k) ~init ~f:(fun k acc -> f (map_key of_key k) acc)

  let keys t k = S.keys t (to_key k) >|= fun keys -> List.map of_key keys

  let fold_keys t k ~init ~f =
    S.fold_keys t (to_key k) ~init ~f:(fun k acc -> f (of_key k) acc)

  let remove_dir t k = S.remove_dir t (to_key k)
end

module Make_indexed_substore (S : STORE) (I : INDEX) = struct
  type t = S.t

  type key = I.t

  module Store = struct
    type t = S.t * I.t

    type key = string list

    type value = Bytes.t

    let to_key i k =
      assert (List.length (I.to_path i []) = I.path_length) ;
      I.to_path i k

    let of_key k = List.remove I.path_length k

    let known (t, i) k = S.known t (to_key i k)

    let known_dir (t, i) k = S.known_dir t (to_key i k)

    let read (t, i) k = S.read t (to_key i k)

    let read_opt (t, i) k = S.read_opt t (to_key i k)

    let store (t, i) k v = S.store t (to_key i k) v

    let remove (t, i) k = S.remove t (to_key i k)

    type key_or_dir = [`Key of key | `Dir of key]

    let fold (t, i) k ~init ~f =
      S.fold t (to_key i k) ~init ~f:(fun k acc -> f (map_key of_key k) acc)

    let keys (t, i) k =
      S.keys t (to_key i k) >|= fun keys -> List.map of_key keys

    let fold_keys (t, i) k ~init ~f =
      S.fold_keys t (to_key i k) ~init ~f:(fun k acc -> f (of_key k) acc)

    let remove_dir (t, i) k = S.remove_dir t (to_key i k)
  end

  let remove_all t i = Store.remove_dir (t, i) []

  let fold_indexes t ~init ~f =
    let rec dig i path acc =
      if i <= 0 then
        match I.of_path path with
        | None -> assert false
        | Some path -> f path acc
      else
        S.fold t path ~init:acc ~f:(fun k acc ->
            match k with
            | `Dir k -> dig (i - 1) k acc
            | `Key _ -> Lwt.return acc)
    in
    dig I.path_length [] init

  let indexes t =
    fold_indexes t ~init:[] ~f:(fun i acc -> Lwt.return (i :: acc))

  let list t k = S.fold t k ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

  let resolve_index t prefix =
    let rec loop i prefix = function
      | [] when i = I.path_length -> (
          match I.of_path prefix with
          | None -> assert false
          | Some path -> Lwt.return [path])
      | [] ->
          list t prefix >>= fun prefixes ->
          List.map_p
            (function `Key prefix | `Dir prefix -> loop (i + 1) prefix [])
            prefixes
          >|= List.flatten
      | [d] when i = I.path_length - 1 ->
          if i >= I.path_length then invalid_arg "IO.resolve" ;
          list t prefix >>= fun prefixes ->
          List.map_p
            (function
              | `Key prefix | `Dir prefix ->
                  let open Option in
                  bind (List.last_opt prefix) (fun last_prefix ->
                      String.remove_prefix ~prefix:d last_prefix)
                  |> fold ~none:Lwt.return_nil ~some:(fun _ ->
                         loop (i + 1) prefix []))
            prefixes
          >|= List.flatten
      | "" :: ds ->
          list t prefix >>= fun prefixes ->
          List.map_p
            (function `Key prefix | `Dir prefix -> loop (i + 1) prefix ds)
            prefixes
          >|= List.flatten
      | d :: ds -> (
          if i >= I.path_length then invalid_arg "IO.resolve" ;
          S.known_dir t (prefix @ [d]) >>= function
          | true -> loop (i + 1) (prefix @ [d]) ds
          | false -> Lwt.return_nil)
    in
    loop 0 [] prefix

  module Make_set (N : NAME) = struct
    type t = S.t

    type elt = I.t

    let inited = Bytes.of_string "inited"

    let known s i = Store.known (s, i) N.name

    let store s i = Store.store (s, i) N.name inited

    let remove s i = Store.remove (s, i) N.name

    let remove_all s = fold_indexes s ~init:() ~f:(fun i () -> remove s i)

    let fold s ~init ~f =
      fold_indexes s ~init ~f:(fun i acc ->
          known s i >>= function true -> f i acc | false -> Lwt.return acc)

    let elements s = fold s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

    let iter s ~f = fold s ~init:() ~f:(fun p () -> f p)
  end

  module Make_buffered_set (N : NAME) (Set : Set.S with type elt = I.t) = struct
    include Make_set (N)
    module Set = Set

    let read_all s =
      fold s ~init:Set.empty ~f:(fun i set -> Lwt.return (Set.add i set))

    let store_all s new_set =
      read_all s >>= fun old_set ->
      Set.iter_p (remove s) (Set.diff old_set new_set) >>= fun () ->
      Set.iter_p (store s) (Set.diff new_set old_set)
  end

  module Make_map (N : NAME) (V : VALUE) = struct
    type t = S.t

    type key = I.t

    type value = V.t

    let known s i = Store.known (s, i) N.name

    let read s i =
      Store.read (s, i) N.name >>=? fun b -> Lwt.return (V.of_bytes b)

    let read_opt s i =
      read s i >>= function
      | Error _ -> Lwt.return_none
      | Ok v -> Lwt.return_some v

    let store s i v = Store.store (s, i) N.name (V.to_bytes v)

    let remove s i = Store.remove (s, i) N.name

    let remove_all s = fold_indexes s ~init:() ~f:(fun i () -> remove s i)

    let fold s ~init ~f =
      fold_indexes s ~init ~f:(fun i acc ->
          read_opt s i >>= function
          | None -> Lwt.return acc
          | Some v -> f i v acc)

    let bindings s =
      fold s ~init:[] ~f:(fun p v acc -> Lwt.return ((p, v) :: acc))

    let iter s ~f = fold s ~init:() ~f:(fun p v () -> f p v)

    let fold_keys s ~init ~f =
      fold_indexes s ~init ~f:(fun i acc ->
          known s i >>= function false -> Lwt.return acc | true -> f i acc)

    let keys s = fold_keys s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

    let iter_keys s ~f = fold_keys s ~init:() ~f:(fun p () -> f p)
  end

  module Make_buffered_map
      (N : NAME)
      (V : VALUE)
      (Map : Map.S with type key = I.t) =
  struct
    include Make_map (N) (V)
    module Map = Map

    let read_all s =
      fold s ~init:Map.empty ~f:(fun i v set -> Lwt.return (Map.add i v set))

    let store_all s map = remove_all s >>= fun () -> Map.iter_p (store s) map
  end
end

module Make_set (S : STORE) (I : INDEX) = struct
  type t = S.t

  type elt = I.t

  let inited = Bytes.of_string "inited"

  let known s i = S.known s (I.to_path i [])

  let store s i = S.store s (I.to_path i []) inited

  let remove s i = S.remove s (I.to_path i [])

  let remove_all s = S.remove_dir s []

  let fold s ~init ~f =
    let rec dig i path acc =
      if i <= 1 then
        S.fold s path ~init:acc ~f:(fun k acc ->
            match k with
            | `Dir _ -> Lwt.return acc
            | `Key file -> (
                match I.of_path file with
                | None -> assert false
                | Some p -> f p acc))
      else
        S.fold s path ~init:acc ~f:(fun k acc ->
            match k with
            | `Dir k -> dig (i - 1) k acc
            | `Key _ -> Lwt.return acc)
    in
    dig I.path_length [] init

  let elements s = fold s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

  let iter s ~f = fold s ~init:() ~f:(fun p () -> f p)
end

module Make_buffered_set
    (S : STORE)
    (I : INDEX)
    (Set : Set.S with type elt = I.t) =
struct
  include Make_set (S) (I)
  module Set = Set

  let read_all s =
    fold s ~init:Set.empty ~f:(fun i set -> Lwt.return (Set.add i set))

  let store_all s new_set =
    read_all s >>= fun old_set ->
    Set.iter_p (remove s) (Set.diff old_set new_set) >>= fun () ->
    Set.iter_p (store s) (Set.diff new_set old_set)
end

module Make_map (S : STORE) (I : INDEX) (V : VALUE) = struct
  type t = S.t

  type key = I.t

  type value = V.t

  let known s i = S.known s (I.to_path i [])

  let read s i =
    S.read s (I.to_path i []) >>=? fun b -> Lwt.return (V.of_bytes b)

  let read_opt s i =
    read s i >>= function
    | Error _ -> Lwt.return_none
    | Ok v -> Lwt.return_some v

  let store s i v = S.store s (I.to_path i []) (V.to_bytes v)

  let remove s i = S.remove s (I.to_path i [])

  let remove_all s = S.remove_dir s []

  let fold s ~init ~f =
    let rec dig i path acc =
      if i <= 1 then
        S.fold s path ~init:acc ~f:(fun k acc ->
            match k with
            | `Dir _ -> Lwt.return acc
            | `Key file -> (
                S.read_opt s file >>= function
                | None -> Lwt.return acc
                | Some b -> (
                    match V.of_bytes b with
                    | Error _ ->
                        (* Silently ignore unparsable data *)
                        Lwt.return acc
                    | Ok v -> (
                        match I.of_path file with
                        | None -> assert false
                        | Some path -> f path v acc))))
      else
        S.fold s path ~init:acc ~f:(fun k acc ->
            match k with
            | `Dir k -> dig (i - 1) k acc
            | `Key _ -> Lwt.return acc)
    in
    dig I.path_length [] init

  let bindings s =
    fold s ~init:[] ~f:(fun p v acc -> Lwt.return ((p, v) :: acc))

  let iter s ~f = fold s ~init:() ~f:(fun p v () -> f p v)

  let fold_keys s ~init ~f =
    S.fold s [] ~init ~f:(fun p acc ->
        match p with
        | `Dir _ -> Lwt.return acc
        | `Key p -> (
            match I.of_path p with
            | None -> assert false
            | Some path -> f path acc))

  let keys s = fold_keys s ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

  let iter_keys s ~f = fold_keys s ~init:() ~f:(fun p () -> f p)
end

module Make_buffered_map
    (S : STORE)
    (I : INDEX)
    (V : VALUE)
    (Map : Map.S with type key = I.t) =
struct
  include Make_map (S) (I) (V)
  module Map = Map

  let read_all s =
    fold s ~init:Map.empty ~f:(fun i v set -> Lwt.return (Map.add i v set))

  let store_all s map = remove_all s >>= fun () -> Map.iter_p (store s) map
end

module Integer_index = struct
  type t = int

  let path_length = 1

  let to_path x l = string_of_int x :: l

  let of_path = function [x] -> int_of_string_opt x | _ -> None
end
