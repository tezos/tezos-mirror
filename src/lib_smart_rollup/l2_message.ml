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

type t = {
  counter : Z.t;
      (** Each message is given a unique counter to allow for the batcher to
          receive multiple identical messages. *)
  content : string;  (** The actual content of the message. *)
}

let content_encoding =
  let open Data_encoding in
  def "sc_l2_message" ~description:"A hex encoded smart rollup message"
  @@ string' Hex

let encoding =
  let open Data_encoding in
  conv
    (fun {counter; content} -> (counter, content))
    (fun (counter, content) -> {counter; content})
  @@ obj2 (req "counter" z) (req "content" content_encoding)

let make =
  let counter = ref Z.zero in
  fun content ->
    let m = {content; counter = !counter} in
    counter := Z.succ !counter ;
    m

let content m = m.content

module Id =
  Tezos_crypto.Blake2B.Make
    (Tezos_crypto.Base58)
    (struct
      let name = "sc_rollup_l2_message_id"

      let title = "A smart rollup layer 2 message identifier"

      let b58check_prefix = "\003\250\179\247\196" (* scmsg(55) *)

      let size = None
    end)

let () =
  Tezos_crypto.Base58.check_encoded_prefix Id.b58check_encoding "scmsg" 55

type id = Id.t

let id t = Id.hash_bytes [Data_encoding.Binary.to_bytes_exn encoding t]
