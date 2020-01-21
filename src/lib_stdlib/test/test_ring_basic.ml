(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let r = Ring.create 5

let () = assert (Ring.elements r = [])

let () = assert (Ring.last r = None)

let () = Ring.add r "00"

let () = assert (Ring.elements r = ["00"])

let () = assert (Ring.last r = Some "00")

let () = Ring.add r "01"

let () = Ring.add r "02"

let () = Ring.add r "03"

let () = assert (Ring.add_and_return_erased r "04" = None)

let () = assert (Ring.elements r = ["00"; "01"; "02"; "03"; "04"])

let () = assert (Ring.last r = Some "04")

let () = Ring.add r "10"

let () = assert (Ring.elements r = ["01"; "02"; "03"; "04"; "10"])

let () = assert (Ring.last r = Some "10")

let () = assert (Ring.add_and_return_erased r "11" = Some "01")

let () = assert (Ring.elements r = ["02"; "03"; "04"; "10"; "11"])

let () = assert (Ring.last r = Some "11")

let () =
  Ring.add_list r ["garbage"; "trash"; "junk"; "20"; "21"; "22"; "23"; "24"]

let () = assert (Ring.elements r = ["20"; "21"; "22"; "23"; "24"])

let () = assert (Ring.last r = Some "24")
