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

module V : Hashtbl.HashedType with type t = string = struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end

module Ring = Ring.MakeTable (V)

let ( =?= ) l1 l2 =
  (* unordered comparison for lists, sort-of *)
  List.length l1 = List.length l2
  && List.for_all (fun x -> List.mem x l2) l1
  && List.for_all (fun x -> List.mem x l1) l2

let r = Ring.create 5

let () = assert (Ring.elements r =?= [])

let () = Ring.add r "00"

let () = assert (Ring.elements r =?= ["00"])

let () = Ring.add r "01"

let () = Ring.add r "02"

let () = Ring.add r "03"

let () = Ring.add r "04"

let () = assert (Ring.elements r =?= ["00"; "01"; "02"; "03"; "04"])

let () = assert (Ring.mem r "00")

let () = assert (Ring.mem r "01")

let () = assert (Ring.mem r "02")

let () = assert (Ring.mem r "03")

let () = assert (Ring.mem r "04")

let () = Ring.add r "10"

let () = assert (Ring.elements r =?= ["01"; "02"; "03"; "04"; "10"])

let () = Ring.add r "11"

let () = assert (Ring.elements r =?= ["02"; "03"; "04"; "10"; "11"])

let () = Ring.remove r "03"

let () = Ring.remove r "04"

let () = assert (Ring.elements r =?= ["02"; "10"; "11"])

let () = assert (not @@ Ring.mem r "03")

let () = assert (not @@ Ring.mem r "04")

(* NOTE: there are holes in the ring because of removed elements *)

let () = Ring.add r "12"

let () = assert (Ring.elements r =?= ["10"; "11"; "12"])

let () = assert (not @@ Ring.mem r "03")

let () = assert (not @@ Ring.mem r "04")

let () = assert (Ring.mem r "10")

let () = assert (Ring.mem r "11")

let () = assert (Ring.mem r "12")

let () = Ring.add r "13"

let () = assert (Ring.elements r =?= ["10"; "11"; "12"; "13"])
