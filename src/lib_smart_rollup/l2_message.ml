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

let content_encoding =
  let open Data_encoding in
  def "sc_l2_message" ~description:"A hex encoded smart rollup message"
  @@ string' Hex

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

let id_encoding =
  let open Data_encoding in
  obj2 (opt "counter" z) (req "content" content_encoding)

let id ?counter content =
  Id.hash_bytes
    [Data_encoding.Binary.to_bytes_exn id_encoding (counter, content)]

type t = {order : Z.t option; counter : Z.t; id : Id.t; content : string}

let make =
  let counter = ref Z.zero in
  fun ?order ~unique content ->
    let counter_for_id = if unique then Some !counter else None in
    let id = id ?counter:counter_for_id content in
    let m = {order; counter = !counter; id; content} in
    counter := Z.succ !counter ;
    m

let id t = t.id

let content t = t.content

let counter t = t.counter
(*
       We need to transform a the encoding to be retro compatible with the previous encoding

       {
         "id: <string>,
         "message": {
           "content": <hex_string>,
           "counter: <n>
          }
       }

    *)

let encoding =
  let open Data_encoding in
  conv
    (fun {order; counter; id; content} -> (order, id, (content, counter)))
    (fun (order, id, (content, counter)) -> {order; counter; id; content})
  @@ obj3
       (opt "order" n)
       (req "id" Id.encoding)
       (req "message" (obj2 (req "content" content_encoding) (req "counter" n)))

(* compare order messages in the following order: First are messages
   with an `order`, meaning the messages priority was set by the
   caller, then the messages are ordered by the timestamp of
   registration. *)
let compare msg1 msg2 =
  let counter_cmp () = Z.compare msg1.counter msg2.counter in
  match (msg1.order, msg2.order) with
  | Some o1, Some o2 ->
      let cmp = Z.compare o1 o2 in
      if cmp = 0 then counter_cmp () else cmp
  | None, None -> counter_cmp ()
  | Some _p, _ -> -1
  | _, Some _p -> 1

let order {order; _} = order
