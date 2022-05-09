(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs. <nomadic@tezcore.com>               *)
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
open Store_types

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "reconstruction"]

  (* Notice *)
  let reconstruct_start_default =
    declare_1
      ~section
      ~level:Notice
      ~name:"reconstruct_start_default"
      ~msg:"starting reconstruct from genesis toward block {block}"
      ~pp1:pp_block_descriptor
      ("block", block_descriptor_encoding)

  let reconstruct_resuming =
    declare_2
      ~section
      ~level:Notice
      ~name:"reconstruct_resuming"
      ~msg:
        "resuming reconstruction from block {start_block} toward block \
         {end_block}"
      ~pp1:pp_block_descriptor
      ("start_block", block_descriptor_encoding)
      ~pp2:pp_block_descriptor
      ("end_block", block_descriptor_encoding)

  let reconstruct_enum =
    declare_0
      ~section
      ~level:Notice
      ~name:"reconstruct_enum"
      ~msg:"enumerating all blocks to reconstruct"
      ()

  let reconstruct_success =
    declare_0
      ~section
      ~level:Notice
      ~name:"reconstruct_success"
      ~msg:"the storage was successfully reconstructed"
      ()

  let reconstruct_block_success =
    declare_1
      ~section
      ~level:Debug
      ~name:"reconstruct_block_success"
      ~msg:"the block {block_descr} was successfully reconstructed"
      ~pp1:pp_block_descriptor
      ("block_descr", block_descriptor_encoding)
end
