(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type 'a tag = ..

type t =
  | T : {
      tag : 'a tag;
      impl : (module Intf.NORMAL with type Registry.t = 'a);
      value : 'a;
    }
      -> t

let wrap (type a) (tag : a tag)
    (impl : (module Intf.NORMAL with type Registry.t = a)) (value : a) =
  T {tag; impl; value}

type packed =
  | Packed : {
      tag : 'a tag;
      impl : (module Intf.NORMAL with type Registry.t = 'a);
      value : 'a;
    }
      -> packed

let unpack (T {tag; impl; value}) = Packed {tag; impl; value}

let size (T {impl = (module M); value; _}) = M.Registry.size value

let resize (T {impl = (module M); value; _}) n = M.Registry.resize value n

let copy_database (T {impl = (module M); value; _}) ~src ~dst =
  M.Registry.copy_database value ~src ~dst

let move_database (T {impl = (module M); value; _}) ~src ~dst =
  M.Registry.move_database value ~src ~dst

let clear (T {impl = (module M); value; _}) db_index =
  M.Registry.clear value db_index

let registry_hash (T {impl = (module M); value; _}) = M.Registry.hash value

let exists (T {impl = (module M); value; _}) ~db_index ~key =
  M.Database.exists value ~db_index ~key

let read (T {impl = (module M); value; _}) ~db_index ~key ~offset ~len =
  M.Database.read value ~db_index ~key ~offset ~len

let write (T {impl = (module M); value; _}) ~db_index ~key ~offset ~value:v =
  M.Database.write value ~db_index ~key ~offset ~value:v

let set (T {impl = (module M); value; _}) ~db_index ~key ~value:v =
  M.Database.set value ~db_index ~key ~value:v

let delete (T {impl = (module M); value; _}) ~db_index ~key =
  M.Database.delete value ~db_index ~key

let value_length (T {impl = (module M); value; _}) ~db_index ~key =
  M.Database.value_length value ~db_index ~key

let hash (T {impl = (module M); value; _}) ~db_index =
  M.Database.hash value ~db_index
