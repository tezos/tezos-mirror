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
type t = Non_empty_string.t

let compare = Non_empty_string.compare

let ( = ) = Non_empty_string.( = )

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

let default = Non_empty_string.of_string_exn "default"

let is_default name = name = default

type of_string_result =
  | Ok of t
  | Too_long  (** length > 31 *)
  | Got_default
      (** Got exactly "default", which can be an error in some cases or OK in others *)

let of_non_empty_string (str : Non_empty_string.t) =
  if Compare.Int.(String.length (str :> string) > 31) then Too_long
  else if is_default str then Got_default
  else Ok str

let of_string str =
  match Non_empty_string.of_string str with
  | None (* empty string *) ->
      (* The empty string always means the default entrypoint *)
      Ok default
  | Some str -> of_non_empty_string str

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

let of_annot_strict ~loc a =
  match of_non_empty_string a with
  | Too_long -> error (Name_too_long (a :> string))
  | Got_default -> error (Unexpected_default loc)
  | Ok name -> Ok name

let of_annot_lax_opt a =
  match of_non_empty_string a with
  | Too_long -> None
  | Got_default -> Some default
  | Ok name -> Some name

let of_string_lax_opt str =
  match of_string str with
  | Too_long -> None
  | Got_default -> Some default
  | Ok name -> Some name

let of_string_lax str =
  match of_string_lax_opt str with
  | None -> error (Name_too_long str)
  | Some name -> Ok name

let of_annot_lax a =
  match of_non_empty_string a with
  | Too_long -> error (Name_too_long (a :> string))
  | Got_default -> Ok default
  | Ok name -> Ok name

let of_string_lax' str =
  match of_string_lax_opt str with
  | None -> Error ("Entrypoint name too long \"" ^ str ^ "\"")
  | Some name -> Ok name

let root = Non_empty_string.of_string_exn "root"

let do_ = Non_empty_string.of_string_exn "do"

let set_delegate = Non_empty_string.of_string_exn "set_delegate"

let remove_delegate = Non_empty_string.of_string_exn "remove_delegate"

let to_address_suffix (name : t) =
  if is_default name then "" else "%" ^ (name :> string)

let of_string_lax_exn str =
  match of_string_lax' str with Ok name -> name | Error err -> invalid_arg err

let pp fmt (name : t) = Format.pp_print_string fmt (name :> string)

let simple_encoding =
  Data_encoding.conv_with_guard
    (fun (name : t) -> (name :> string))
    of_string_lax'
    Data_encoding.string

let value_encoding =
  Data_encoding.conv_with_guard
    (fun name -> if is_default name then "" else (name :> string))
    of_string_strict'
    Data_encoding.Variable.string

let smart_encoding =
  let open Data_encoding in
  def
    ~title:"entrypoint"
    ~description:"Named entrypoint to a Michelson smart contract"
    "entrypoint"
  @@
  let builtin_case tag (name : Non_empty_string.t) =
    case
      (Tag tag)
      ~title:(name :> string)
      (constant (name :> string))
      (fun n -> if n = name then Some () else None)
      (fun () -> name)
  in
  union
    [
      builtin_case 0 default;
      builtin_case 1 root;
      builtin_case 2 do_;
      builtin_case 3 set_delegate;
      builtin_case 4 remove_delegate;
      case
        (Tag 255)
        ~title:"named"
        (Bounded.string 31)
        (fun (name : Non_empty_string.t) -> Some (name :> string))
        of_string_lax_exn;
    ]

let rpc_arg =
  RPC_arg.make
    ~descr:"A Michelson entrypoint (string of length < 32)"
    ~name:"entrypoint"
    ~construct:(fun (name : t) -> (name :> string))
    ~destruct:of_string_lax'
    ()

let in_memory_size (name : t) =
  Cache_memory_helpers.string_size_gen (String.length (name :> string))

module Set = Set.Make (Non_empty_string)
module Map = Map.Make (Non_empty_string)
