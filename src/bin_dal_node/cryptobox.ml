(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Tezos_crypto_dal

module Constants = struct
  let redundancy_factor = 2

  let segment_size = 4096

  let slot_size = 1048576 (* 1Mb *)

  let shards_amount = 2048

  let trusted_setup_logarithm_size = 21
end

include Dal_cryptobox.Builder (Constants)

exception Trusted_setup_not_found of string list

type error +=
  | Failed_to_load_trusted_setup of string
  | No_trusted_setup of string list

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_loading_failed"
    ~title:"Trusted setup loading failed"
    ~description:"Trusted setup failed to load"
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Trusted setup failed to load: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Failed_to_load_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> Failed_to_load_trusted_setup parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_not_found"
    ~title:"No trusted setup found"
    ~description:"No trusted setup found in the explored paths"
    ~pp:(fun ppf locations ->
      Format.fprintf
        ppf
        "@[<v>cannot find Trusted setup in any of:@,%a@]@."
        (Format.pp_print_list (fun fmt -> Format.fprintf fmt "- %s"))
        locations)
    Data_encoding.(obj1 (req "paths" (list string)))
    (function No_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> No_trusted_setup parameter)

let srs =
  let open Constants in
  srs ~redundancy_factor ~segment_size ~shards_amount ~slot_size
