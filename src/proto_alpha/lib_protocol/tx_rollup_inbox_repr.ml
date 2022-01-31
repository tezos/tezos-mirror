(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type t = {contents : Tx_rollup_message_repr.hash list; cumulated_size : int}

let pp fmt {contents; cumulated_size} =
  Format.fprintf
    fmt
    "tx rollup inbox: %d messages using %d bytes"
    (List.length contents)
    cumulated_size

let encoding =
  let open Data_encoding in
  conv
    (fun {contents; cumulated_size} -> (contents, cumulated_size))
    (fun (contents, cumulated_size) -> {contents; cumulated_size})
    (obj2
       (req "contents" @@ list Tx_rollup_message_repr.hash_encoding)
       (req "cumulated_size" int31))

type metadata = {
  cumulated_size : int;
  predecessor : Raw_level_repr.t option;
  successor : Raw_level_repr.t option;
}

let metadata_encoding =
  let open Data_encoding in
  conv
    (fun {cumulated_size; predecessor; successor} ->
      (cumulated_size, predecessor, successor))
    (fun (cumulated_size, predecessor, successor) ->
      {cumulated_size; predecessor; successor})
    (obj3
       (req "cumulated_size" int31)
       (req "predecessor" (option Raw_level_repr.encoding))
       (req "successor" (option Raw_level_repr.encoding)))
