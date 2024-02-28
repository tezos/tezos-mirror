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

type error += Node_connection_lost

type error += Cannot_load_local_file of string

type error += Broken_locked_values_invariant

let register_error_kind category ~id ~title ~description ~pp encoding from_error
    to_error =
  Error_monad.register_error_kind
    category
    ~id:(String.concat "." ["baker"; Protocol.name; id])
    ~title
    ~description
    ~pp
    encoding
    from_error
    to_error

let () =
  register_error_kind
    `Temporary
    ~id:"Baking_scheduling.node_connection_lost"
    ~title:"Node connection lost"
    ~description:"The connection with the node was lost."
    ~pp:(fun fmt () -> Format.fprintf fmt "Lost connection with the node")
    Data_encoding.empty
    (function Node_connection_lost -> Some () | _ -> None)
    (fun () -> Node_connection_lost) ;
  register_error_kind
    `Temporary
    ~id:"Baking_scheduling.cannot_load_local_file"
    ~title:"Cannot load local file"
    ~description:"Cannot load local file."
    ~pp:(fun fmt filename ->
      Format.fprintf fmt "Cannot load the local file %s" filename)
    Data_encoding.(obj1 (req "file" string))
    (function Cannot_load_local_file s -> Some s | _ -> None)
    (fun s -> Cannot_load_local_file s) ;
  register_error_kind
    `Permanent
    ~id:"Baking_state.broken_locked_values_invariant"
    ~title:"Broken locked values invariant"
    ~description:
      "The expected consistency invariant on locked values does not hold"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The expected consistency invariant on locked values does not hold")
    Data_encoding.unit
    (function Broken_locked_values_invariant -> Some () | _ -> None)
    (fun () -> Broken_locked_values_invariant)

type error += Failed_to_checkout_context

type error += Invalid_context

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.failed_to_checkout_context"
    ~title:"Failed to checkout context"
    ~description:"The given context hash does not exist in the context."
    ~pp:(fun ppf () -> Format.fprintf ppf "Failed to checkout the context")
    Data_encoding.unit
    (function Failed_to_checkout_context -> Some () | _ -> None)
    (fun () -> Failed_to_checkout_context) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.invalid_context"
    ~title:"Invalid context"
    ~description:"Occurs when the context is inconsistent."
    ~pp:(fun ppf () -> Format.fprintf ppf "The given context is invalid.")
    Data_encoding.unit
    (function Invalid_context -> Some () | _ -> None)
    (fun () -> Invalid_context)
