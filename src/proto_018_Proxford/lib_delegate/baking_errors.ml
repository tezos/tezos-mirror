(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error += Cannot_open_context_index of {context_path : string}

type error += Node_connection_lost

type error += Cannot_load_local_file of string

let make_id id = String.concat "." [Protocol.name; id]

let () =
  Error_monad.register_error_kind
    `Temporary
    ~id:(make_id "cannot_open_context_index")
    ~title:"Cannot open context index"
    ~description:"Failed to open the context index at the given location"
    ~pp:(fun fmt path ->
      Format.fprintf fmt "Cannot open context index at %s" path)
    Data_encoding.(obj1 (req "cannot_open_context_index" Data_encoding.string))
    (function
      | Cannot_open_context_index {context_path} -> Some context_path
      | _ -> None)
    (fun context_path -> Cannot_open_context_index {context_path}) ;
  register_error_kind
    `Temporary
    ~id:(make_id "baking_scheduling.node_connection_lost")
    ~title:"Node connection lost"
    ~description:"The connection with the node was lost."
    ~pp:(fun fmt () -> Format.fprintf fmt "Lost connection with the node")
    Data_encoding.empty
    (function Node_connection_lost -> Some () | _ -> None)
    (fun () -> Node_connection_lost) ;
  register_error_kind
    `Temporary
    ~id:(make_id "baking_scheduling.cannot_load_local_file")
    ~title:"Cannot load local file"
    ~description:"Cannot load local file."
    ~pp:(fun fmt filename ->
      Format.fprintf fmt "Cannot load the local file %s" filename)
    Data_encoding.(obj1 (req "file" string))
    (function Cannot_load_local_file s -> Some s | _ -> None)
    (fun s -> Cannot_load_local_file s)
