(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

let () =
  (* The default allocation policy of Octez is "best-fit" which gives
     the best compromise in terms of performances and memory
     consumption. This default policy can be changed if the user set
     an environment variable. *)
  (* Any change to this constant should be replicated into the
     external validator in [src/bin_node/main.ml]. *)
  let default_allocation_policy = 2 in
  let current = Gc.get () in
  (match Sys.getenv_opt "OCAMLRUNPARAM" with
  | None -> Gc.set {current with allocation_policy = default_allocation_policy}
  | Some _ -> ()) ;
  if (Gc.get ()).allocation_policy <> default_allocation_policy then
    Format.eprintf
      "WARNING: Default allocation policy changed: %d (default %d)@."
      current.allocation_policy
      default_allocation_policy

let () = Command_line.run ()
