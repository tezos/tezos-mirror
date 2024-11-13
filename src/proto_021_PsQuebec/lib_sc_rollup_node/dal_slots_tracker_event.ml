(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context

module Simple = struct
  include Internal_event.Simple

  let section = [Protocol.name; "smart_rollup_node"; "dal_slots_tracker"]

  let slot_has_been_confirmed =
    declare_3
      ~section
      ~name:"dal_confirmed_slot"
      ~msg:
        "Slot header for index {slot_index} was published at block \
         {published_hash}. The slot header has been confirmed at \
         {confirmed_hash}. The slot contents will be downloaded"
      ~level:Notice
      ("slot_index", Dal.Slot_index.encoding)
      ("published_hash", Block_hash.encoding)
      ("confirmed_hash", Block_hash.encoding)
end

let slot_has_been_confirmed slot published_hash confirmed_hash =
  Simple.(emit slot_has_been_confirmed (slot, published_hash, confirmed_hash))
