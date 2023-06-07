(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

type error +=
  | Cannot_load_store of {name : string; path : string}
  | Cannot_write_to_store of string
  | Cannot_write_key_value_pair_to_store of {
      key : string;
      value : string;
      name : string;
    }
  | Cannot_write_singleton_value_to_store of {value : string; name : string}
  | Cannot_remove_key_from_store of {key : string; name : string}
  | Cannot_read_from_store of string
  | Cannot_read_key_from_store of {key : string; name : string}
  | Decoding_error of Data_encoding.Binary.read_error
  | Encoding_error of Data_encoding.Binary.write_error
  | Cannot_close_store of string
  | Cannot_overwrite_key_in_store of {
      key : string;
      old_value : string;
      new_value : string;
      name : string;
    }

let pp_hex ppf s = Hex.pp ppf (Hex.of_string s)

let () =
  register_error_kind
    ~id:"layer2_store.cannot_load_store"
    ~title:"Store cannot be loaded"
    ~description:"Store cannot be loaded."
    ~pp:(fun ppf (name, path) ->
      Format.fprintf ppf "Store %s cannot be loaded from %s." name path)
    `Permanent
    Data_encoding.(obj2 (req "name" string) (req "path" string))
    (function Cannot_load_store {name; path} -> Some (name, path) | _ -> None)
    (fun (name, path) -> Cannot_load_store {name; path})

let () =
  register_error_kind
    ~id:"layer2_store.cannot_write_to_store"
    ~title:"Cannot write to store"
    ~description:"Cannot write to store %s."
    ~pp:(fun ppf name -> Format.fprintf ppf "Cannot write to store %s." name)
    `Permanent
    Data_encoding.(obj1 (req "name" string))
    (function Cannot_write_to_store name -> Some name | _ -> None)
    (fun n -> Cannot_write_to_store n)

let () =
  register_error_kind
    ~id:"layer2_store.cannot_write_key_value_pair_to_store"
    ~title:"Cannot write key-value pair to store"
    ~description:"Cannot write key-value pair to store."
    ~pp:(fun ppf (key, value, name) ->
      Format.fprintf
        ppf
        "The key-value pair with key %a and value %a cannot be written to \
         store %s."
        pp_hex
        key
        pp_hex
        value
        name)
    `Permanent
    Data_encoding.(
      obj3
        (req "key" (string' Hex))
        (req "value" (string' Hex))
        (req "name" string))
    (function
      | Cannot_write_key_value_pair_to_store {key; value; name} ->
          Some (key, value, name)
      | _ -> None)
    (fun (key, value, name) ->
      Cannot_write_key_value_pair_to_store {key; value; name})

let () =
  register_error_kind
    ~id:"layer2_store.cannot_write_singleton_value_to_store"
    ~title:"Cannot write singleton value to store"
    ~description:"Cannot write singleton value to store."
    ~pp:(fun ppf (value, name) ->
      Format.fprintf
        ppf
        "The singleton value %a cannot be written to store %s."
        pp_hex
        value
        name)
    `Permanent
    Data_encoding.(obj2 (req "value" (string' Hex)) (req "name" string))
    (function
      | Cannot_write_singleton_value_to_store {value; name} -> Some (value, name)
      | _ -> None)
    (fun (value, name) -> Cannot_write_singleton_value_to_store {value; name})

let () =
  register_error_kind
    ~id:"layer2_store.cannot_remove_key_from_store"
    ~title:"Cannot remove key from store"
    ~description:"Cannot remove key from store."
    ~pp:(fun ppf (key, name) ->
      Format.fprintf ppf "Cannot remove key %a from store %s" pp_hex key name)
    `Permanent
    Data_encoding.(obj2 (req "key" (string' Hex)) (req "name" string))
    (function
      | Cannot_remove_key_from_store {key; name} -> Some (key, name) | _ -> None)
    (fun (key, name) -> Cannot_remove_key_from_store {key; name})

let () =
  register_error_kind
    ~id:"layer2_store.cannot_close_store"
    ~title:"Store cannot be closed"
    ~description:"Store cannot be closed."
    ~pp:(fun ppf name -> Format.fprintf ppf "Store %s cannot be closed." name)
    `Permanent
    Data_encoding.(obj1 (req "name" string))
    (function Cannot_close_store n -> Some n | _ -> None)
    (fun n -> Cannot_close_store n)

let () =
  register_error_kind
    ~id:"layer2_store.cannot_read_from_store"
    ~title:"Value cannot be read from store"
    ~description:"Value cannot be read from store."
    ~pp:(fun ppf name ->
      Format.fprintf ppf "Value cannot be read from store %s." name)
    `Permanent
    Data_encoding.(obj1 (req "name" string))
    (function Cannot_read_from_store n -> Some n | _ -> None)
    (fun n -> Cannot_read_from_store n)

let () =
  register_error_kind
    ~id:"layer2_store.cannot_read_key_from_store"
    ~title:"Cannot read key from store"
    ~description:"Cannot read key from store."
    ~pp:(fun ppf (key, name) ->
      Format.fprintf ppf "Cannot read key %a from store %s" pp_hex key name)
    `Permanent
    Data_encoding.(obj2 (req "key" (string' Hex)) (req "name" string))
    (function
      | Cannot_read_key_from_store {key; name} -> Some (key, name) | _ -> None)
    (fun (key, name) -> Cannot_read_key_from_store {key; name})

let () =
  register_error_kind
    ~id:"layer2_store.decoding_error"
    ~title:"Cannot decode persisted element"
    ~description:"The raw value for a persisted element could not be decoded"
    ~pp:(fun ppf error ->
      Format.fprintf
        ppf
        "Decoding error: %a"
        Data_encoding.Json.pp
        (Data_encoding.Json.construct
           Data_encoding.Binary.read_error_encoding
           error))
    `Permanent
    Data_encoding.(obj1 (req "error" Data_encoding.Binary.read_error_encoding))
    (function Decoding_error e -> Some e | _ -> None)
    (fun e -> Decoding_error e)

let () =
  register_error_kind
    ~id:"layer2_store.encoding_error"
    ~title:"Cannot encode element"
    ~description:"An element could not be encoded to a raw value."
    ~pp:(fun ppf error ->
      Format.fprintf
        ppf
        "Encoding error: %a"
        Data_encoding.Json.pp
        (Data_encoding.Json.construct
           Data_encoding.Binary.write_error_encoding
           error))
    `Permanent
    Data_encoding.(obj1 (req "error" Data_encoding.Binary.write_error_encoding))
    (function Encoding_error e -> Some e | _ -> None)
    (fun e -> Encoding_error e)

let () =
  register_error_kind
    ~id:"layer2_store.cannot_overwrite_key_in_store"
    ~title:"Key is already present with a different value in store"
    ~description:"Key is already present with a different value in store."
    ~pp:(fun ppf (key, old_value, new_value, name) ->
      Format.fprintf
        ppf
        "Cannot update key %a to value %a in store %s, as a binding for the \
         key with a different value %a already exists."
        pp_hex
        key
        pp_hex
        new_value
        name
        pp_hex
        old_value)
    `Permanent
    Data_encoding.(
      obj4
        (req "key" (string' Hex))
        (req "old_value" (string' Hex))
        (req "new_value" (string' Hex))
        (req "name" string))
    (function
      | Cannot_overwrite_key_in_store {key; old_value; new_value; name} ->
          Some (key, old_value, new_value, name)
      | _ -> None)
    (fun (key, old_value, new_value, name) ->
      Cannot_overwrite_key_in_store {key; old_value; new_value; name})
