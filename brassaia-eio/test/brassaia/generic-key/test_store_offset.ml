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

module Brassaia = Brassaia_eio.Brassaia
module Brassaia_mem = Brassaia_eio_mem.Brassaia_mem
open Brassaia

(** A store abstraction over an append-only sequence of values. The key of a
    value is the slot at which it's stored in this sequence. There is no index
    for finding stored values by their hash, so any sharing must be achieved at
    the level of keys. *)
module Slot_keyed_vector : Indexable.Maker_concrete_key1 = struct
  type 'h key = {
    slot : int;
    hash : 'h;
    (* Sanity check that keys are used only w/ the stores that created them: *)
    store_id : < >; 
  }

  let key_t hash_t =
    let open Type in
    let hash_equal = unstage (equal hash_t) in
    let hash_pre_hash = unstage (pre_hash hash_t) in
    record "key" (fun _ _ ->
        Alcotest.fail ~pos:__POS__ "Key implementation is non-serialisable")
    |+ field "slot" int (fun t -> t.slot)
    |+ field "hash" hash_t (fun t -> t.hash)
    |> sealr
    |> like (* TODO: write tests that expose the need for these directly *)
         ~equal:(fun a b -> hash_equal a.hash b.hash)
         ~pre_hash:(fun t f -> hash_pre_hash t.hash f)

  module Key (Hash : Hash.S) = struct
    type t = Hash.t key [@@deriving brassaia]

    type hash = Hash.t

    let to_hash t = t.hash

    let pp ppf kt = Type.(pp t) ppf kt

    let encoding =
      Data_encoding.conv
        (Type.to_string t)
        (Type.of_string_exn
           ~path:
             "test/brassaia/generic-key/test_store_offset.ml/Slot_keyed_vector/Key/of_string"
           t)
        Data_encoding.string
  end

  module Make (Hash : Hash.S) (Value : Type.S) = struct
    type instance = {data : (Hash.t * Value.t) option Vector.t; id : < > }

    type _ t = {instance : instance option ref}

    let init =
      (* NOTE: at time of writing, [brassaia-test] relies on the fact that the
         store constructor is memoised (modulo [close] semantics, which must be
         non-memoised), so we must use a singleton here. *)
      let singleton = {data = Vector.create ~dummy:None; id = object end} in
      fun _ -> {instance = ref (Some singleton)}

    type nonrec key = Hash.t key [@@deriving brassaia]

    type value = Value.t

    type hash = Hash.t [@@deriving brassaia ~equal]

    let index _ _ = None

    module Key = struct
      type t = key [@@deriving brassaia]

      type hash = Hash.t

      let to_hash t = t.hash

      let pp ppf kt = Type.(pp t) ppf kt

      let encoding =
        Data_encoding.conv
          (Type.to_string t)
          (Type.of_string_exn
             ~path:
               "test/brassaia/generic-key/test_store_offset.ml/Make/Key/of_string"
             t)
          Data_encoding.string
    end

    module Hash = Brassaia.Hash.Typed (Hash) (Value)

    let check_not_closed t =
      match !(t.instance) with None -> raise Closed | Some t -> t

    let check_key_belongs_to_store pos (k : key) (t : instance) =
      let key_store_id = k.store_id and expected_id = t.id in
      let r = key_store_id == expected_id in
      if not r then
        Alcotest.(check ~pos int)
          "Key ID matches the given store ID"
          (Oo.id expected_id)
          (Oo.id key_store_id)

    let check_hash_is_consistent pos k recovered_hash =
      let r = equal_hash k.hash recovered_hash in
      if not r then
        Alcotest.(
          check ~pos (Brassaia_eio_test_helpers.Brassaia_test.testable Hash.t))
          "Recovered hash is consistent with the key"
          k.hash
          recovered_hash

    let unsafe_add t hash v =
      let t = check_not_closed t in
      Vector.push t.data (Some (hash, v)) ;
      let key = {slot = Vector.length t.data - 1; hash; store_id = t.id} in
      key

    let add t v = unsafe_add t (Hash.hash v) v

    let find t k =
      let t = check_not_closed t in
      check_key_belongs_to_store __POS__ k t ;
      match Vector.get t.data k.slot with
      | exception Not_found -> None
      | None ->
          Alcotest.failf "Invalid key slot %d. No data contained here." k.slot
      | Some (recovered_hash, data) ->
          check_hash_is_consistent __POS__ k recovered_hash ;
          Some data

    let mem t k =
      let t = check_not_closed t in
      check_key_belongs_to_store __POS__ k t ;
      assert (k.slot < Vector.length t.data) ;
      true

    let batch t f =
      let _ = check_not_closed t in
      f (t :> Perms.read_write t)

    let close t = t.instance := None
  end
end

module Store_maker = Generic_key.Maker (struct
  module Contents_store = Indexable.Maker_concrete_key2_of_1 (Slot_keyed_vector)
  module Node_store = Slot_keyed_vector
  module Commit_store = Slot_keyed_vector
  module Branch_store = Atomic_write.Check_closed (Brassaia_mem.Atomic_write)
end)

module Store = Store_maker.Make (Schema.KV (Contents.String))

let suite =
  let store =
    (module Store : Brassaia_eio_test_helpers.Brassaia_test.Generic_key)
  in
  let config = Brassaia_eio_mem.Brassaia_mem.config () in
  Brassaia_eio_test_helpers.Brassaia_test.Suite.create_generic_key
    ~name:"store_offset"
    ~store
    ~config
    ~import_supported:false
    ()
