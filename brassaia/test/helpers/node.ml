(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let check _pos typ ~expected actual =
  let typ =
    Alcotest.testable
      Brassaia.Type.(pp_dump typ)
      Brassaia.Type.(unstage (equal typ))
  in
  Alcotest.check typ "" expected actual

module type Map = sig
  type t [@@deriving brassaia]
  type data [@@deriving brassaia]
  type key := string

  val empty : unit -> t
  val is_empty : t -> bool
  val length : t -> int
  val list : ?offset:int -> ?length:int -> ?cache:bool -> t -> (key * data) list
  val find : ?cache:bool -> t -> key -> data option
  val add : t -> key -> data -> t
  val remove : t -> key -> t

  (* Generators for use by the tests: *)
  val random_data : unit -> data
end

module Suite (Map : Map) = struct
  type key = string [@@deriving brassaia]

  let random_bindings n =
    List.init n (fun i -> (string_of_int i, Map.random_data ()))

  let map_of_bindings kvs =
    List.fold_left (fun t (k, v) -> Map.add t k v) (Map.empty ()) kvs

  let test_empty () =
    check __POS__ [%typ: bool] ~expected:true Map.(is_empty (empty ()));
    check __POS__ [%typ: int] ~expected:0 Map.(length (empty ()));
    check __POS__ [%typ: (key * Map.data) list] ~expected:[]
      Map.(list (empty ()))

  let test_add () =
    let with_binding k v t = Map.add t k v in
    let d1 = Map.random_data () and d2 = Map.random_data () in
    let a = Map.empty () |> with_binding "1" d1 |> with_binding "2" d2 in
    check __POS__ [%typ: int] ~expected:2 (Map.length a)

  let test_remove () =
    (* Remove is a no-op on an empty node *)
    check __POS__ [%typ: Map.t] ~expected:(Map.empty ())
      Map.(remove (empty ()) "foo")

  let test_find () =
    let bindings = random_bindings 256 in
    let node = map_of_bindings bindings in
    bindings
    |> List.iter (fun (k, v) ->
           check __POS__ [%typ: Map.data option] ~expected:(Some v)
             (Map.find node k))

  let test_equal () =
    let module Map = struct
      include Map

      type nonrec t = t
      [@@deriving brassaia ~equal ~to_bin_string ~of_bin_string]
    end in
    let bindings = random_bindings 256 in
    let m = map_of_bindings bindings in

    let m_rev = map_of_bindings (List.rev bindings) in
    check __POS__ [%typ: bool] ~expected:true (Map.equal m m_rev);

    let m_subset = map_of_bindings (List.tl bindings) in
    check __POS__ [%typ: bool] ~expected:false (Map.equal m m_subset);

    let m_serialised =
      m |> Map.to_bin_string |> Map.of_bin_string |> Result.get_ok
    in
    check __POS__ [%typ: bool] ~expected:true (Map.equal m m_serialised)

  let suite =
    [
      ("empty", test_empty);
      ("add", test_add);
      ("remove", test_remove);
      ("find", test_find);
      ("equal", test_equal);
    ]
end

module Make (Make_node : Brassaia.Node.Generic_key.Maker) : sig
  val suite : unit Alcotest.test_case list
end = struct
  (* For each [Node] maker, we can instantiate the test suite above twice: once
     for regular nodes, and once for portable nodes. *)

  module Schema = Brassaia.Schema.KV (Brassaia.Contents.String)
  module Hash = Schema.Hash
  module Key = Brassaia.Key.Of_hash (Hash)
  module Node = Make_node (Hash) (Key) (Key)

  type key = Key.t [@@deriving brassaia]

  module Extras = struct
    type data = [ `Node of Key.t | `Contents of Key.t ]

    let data_t =
      let open Repr in
      variant "weak_value" (fun node contents -> function
        | `Node k -> node k | `Contents k -> contents (k, ()))
      |~ case1 "node" key_t (fun k -> `Node k)
      |~ case1 "contents" (pair key_t unit) (fun (k, ()) -> `Contents k)
      |> sealv

    let random_data =
      let hash_of_string = Brassaia.Type.(unstage (of_bin_string Hash.t)) in
      let random_string =
        Brassaia.Type.(unstage (random (string_of (`Fixed Hash.hash_size))))
      in
      fun () ->
        match hash_of_string (random_string ()) with
        | Error _ -> assert false
        | Ok x -> (
            match Random.int 2 with
            | 0 -> `Node x
            | 1 -> `Contents x
            | _ -> assert false)
  end

  let suite =
    let tc (name, f) = Alcotest.test_case name `Quick f in
    let module Suite_node = Suite (struct
      include Node
      include Extras
    end) in
    let module Suite_node_portable = Suite (struct
      include Node.Portable
      include Extras
    end) in
    List.map tc Suite_node.suite
    @ List.map
        (fun (name, f) -> tc ("Portable." ^ name, f))
        Suite_node_portable.suite
end
