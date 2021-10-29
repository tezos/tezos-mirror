(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Invariants on the string: 1 <= length <= 31 *)
include Compare.String

type error += Name_too_long of string

let () =
  (* Entrypoint name too long *)
  register_error_kind
    `Permanent
    ~id:"michelson_v1.entrypoint_name_too_long"
    ~title:"Entrypoint name too long (type error)"
    ~description:
      "An entrypoint name exceeds the maximum length of 31 characters."
    Data_encoding.(obj1 (req "name" string))
    (function Name_too_long entrypoint -> Some entrypoint | _ -> None)
    (fun entrypoint -> Name_too_long entrypoint)

type error += Unexpected_default of Script_repr.location

let () =
  register_error_kind
    `Permanent
    ~id:"michelson_v1.unexpected_default_entrypoint"
    ~title:
      "The annotation 'default' was encountered where an entrypoint is expected"
    ~description:
      "A node in the syntax tree was improperly annotated. An annotation used \
       to designate an entrypoint cannot be exactly 'default'."
    Data_encoding.(obj1 (req "location" Script_repr.location_encoding))
    (function Unexpected_default loc -> Some loc | _ -> None)
    (fun loc -> Unexpected_default loc)

let default = "default"

let is_default name = name = default

type of_string_result =
  | Ok of t
  | Too_long  (** length > 31 *)
  | Got_default
      (** Got exactly "default", which can be an error in some cases or OK in others *)

let of_string str =
  if str = "" then
    (* The empty string always means the default entrypoint *)
    Ok default
  else if Compare.Int.(String.length str > 31) then Too_long
  else if is_default str then Got_default
  else Ok str

let of_string_strict ~loc str =
  match of_string str with
  | Too_long -> error (Name_too_long str)
  | Got_default -> error (Unexpected_default loc)
  | Ok name -> Ok name

let of_string_strict' str =
  match of_string str with
  | Too_long -> Error "Entrypoint name too long"
  | Got_default -> Error "Unexpected annotation: default"
  | Ok name -> Ok name

let of_string_strict_exn str =
  match of_string_strict' str with Ok v -> v | Error err -> invalid_arg err

let of_annot_strict ~loc (a : Non_empty_string.t) =
  of_string_strict ~loc (a :> string)

let of_string_lax_opt str =
  match of_string str with
  | Too_long -> None
  | Got_default -> Some default
  | Ok name -> Some name

let of_annot_lax_opt (a : Non_empty_string.t) = of_string_lax_opt (a :> string)

let of_string_lax str =
  match of_string_lax_opt str with
  | None -> error (Name_too_long str)
  | Some name -> Ok name

let of_annot_lax (a : Non_empty_string.t) = of_string_lax (a :> string)

let root = "root"

let do_ = "do"

let set_delegate = "set_delegate"

let remove_delegate = "remove_delegate"

let to_address_suffix name = if is_default name then "" else "%" ^ name

let pp = Format.pp_print_string

let in_memory_size name =
  Cache_memory_helpers.string_size_gen (String.length name)

module Set = Set.Make (String)
module Map = Map.Make (String)
