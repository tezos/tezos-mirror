(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = Signature.Public_key_hash.t list

let encoding =
  let open Data_encoding in
  (* The whitelist size is checked to forbid the origination of a
     whitelist that is bigger than any subsequent whitelist
     update. This check is valid because the binary outbox message
     encoding size is only two bytes longer than the maximum whitelist
     update size (encoded in binary), where the encoding of a key is
     20 bytes long. *)
  check_size (Constants_repr.sc_rollup_message_size_limit - 2)
  @@ list Signature.Public_key_hash.encoding

let pp ppf =
  let open Format in
  fprintf
    ppf
    "@[<hv>@[<hv 2>[@,%a@]@,]@]"
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
       Signature.Public_key_hash.pp_short)

type last_whitelist_update = {
  message_index : Z.t;
  outbox_level : Raw_level_repr.t;
}

let last_whitelist_update_encoding =
  Data_encoding.(
    conv
      (fun {message_index; outbox_level} -> (message_index, outbox_level))
      (fun (message_index, outbox_level) -> {message_index; outbox_level})
      (obj2
         (req "message_index" n)
         (req "outbox_level" Raw_level_repr.encoding)))

type update = Public | Private of t

let update_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Public"
        (obj1 (req "kind" (constant "public")))
        (function Public -> Some () | _ -> None)
        (fun () -> Public);
      case
        (Tag 1)
        ~title:"Private"
        (obj2 (req "kind" (constant "update")) (req "whitelist" encoding))
        (function Private whitelist -> Some ((), whitelist) | _ -> None)
        (fun ((), whitelist) -> Private whitelist);
    ]
