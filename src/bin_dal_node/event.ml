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

include Event_legacy

let stored_slot_content =
  declare_1
    ~section
    ~name:"stored_slot_content"
    ~msg:"Slot stored: commitment {commitment}"
    ~level:Notice
    ("commitment", Cryptobox.Commitment.encoding)

let stored_slot_shards =
  declare_2
    ~section
    ~name:"stored_slot_shards"
    ~msg:"Slot stored: commitment {commitment}, shards {shards}"
    ~level:Notice
    ("commitment", Cryptobox.Commitment.encoding)
    ("shards", Data_encoding.int31)

let decoding_data_failed =
  declare_1
    ~section
    ~name:"decoding_failed"
    ~msg:"Error while decoding a {data_kind} value"
    ~level:Warning
    ("data_kind", Types.kind_encoding)

let loading_shard_data_failed =
  declare_1
    ~section
    ~name:"loading_shard_data_failed"
    ~msg:"Error while reading shard data {message}"
    ~level:Warning
    ("message", Data_encoding.string)

let message_validation_error =
  declare_2
    ~section
    ~name:"message_validation_failed"
    ~msg:
      "Validating message with id {message_id} failed with error \
       {validation_error}"
    ~level:Warning
    ~pp1:Gossipsub.Worker.GS.Message_id.pp
    ("message_id", Gossipsub.message_id_encoding)
    ("validation_error", Data_encoding.string)
