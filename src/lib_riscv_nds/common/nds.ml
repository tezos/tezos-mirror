(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | T : {
      impl :
        (module Intf.NORMAL
           with type Registry.t = 'a
            and type Registry.invalid_argument_error = 'e);
      value : 'a;
      to_error : 'e -> Nds_errors.invalid_argument_error;
    }
      -> t

let wrap (type a e)
    (impl :
      (module Intf.NORMAL
         with type Registry.t = a
          and type Registry.invalid_argument_error = e)) (value : a)
    (to_error : e -> Nds_errors.invalid_argument_error) =
  T {impl; value; to_error}

let conv (type e) (to_error : e -> Nds_errors.invalid_argument_error) r =
  Result.map_error to_error r

let size (T {impl = (module M); value; to_error}) =
  M.Registry.size value |> conv to_error

let resize (T {impl = (module M); value; to_error}) n =
  M.Registry.resize value n |> conv to_error

let copy_database (T {impl = (module M); value; to_error}) ~src ~dst =
  M.Registry.copy_database value ~src ~dst |> conv to_error

let move_database (T {impl = (module M); value; to_error}) ~src ~dst =
  M.Registry.move_database value ~src ~dst |> conv to_error

let clear (T {impl = (module M); value; to_error}) db_index =
  M.Registry.clear value db_index |> conv to_error

let registry_hash (T {impl = (module M); value; to_error}) =
  M.Registry.hash value |> conv to_error

let exists (T {impl = (module M); value; to_error}) ~db_index ~key =
  M.Database.exists value ~db_index ~key |> conv to_error

let read (T {impl = (module M); value; to_error}) ~db_index ~key ~offset ~len =
  M.Database.read value ~db_index ~key ~offset ~len |> conv to_error

let write (T {impl = (module M); value; to_error}) ~db_index ~key ~offset
    ~value:v =
  M.Database.write value ~db_index ~key ~offset ~value:v |> conv to_error

let set (T {impl = (module M); value; to_error}) ~db_index ~key ~value:v =
  M.Database.set value ~db_index ~key ~value:v |> conv to_error

let delete (T {impl = (module M); value; to_error}) ~db_index ~key =
  M.Database.delete value ~db_index ~key |> conv to_error

let value_length (T {impl = (module M); value; to_error}) ~db_index ~key =
  M.Database.value_length value ~db_index ~key |> conv to_error

let hash (T {impl = (module M); value; to_error}) ~db_index =
  M.Database.hash value ~db_index |> conv to_error
