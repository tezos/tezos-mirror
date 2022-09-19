(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

external force_linking : unit -> unit = "%opaque"

let () =
  (* Zarith-related encodings *)
  let open Data_encoding in
  Registration.register
    ~pp:Z.pp_print
    (def "ground.Z" ~description:"Arbitrary precision integers" z) ;
  Registration.register
    ~pp:Z.pp_print
    (def "ground.N" ~description:"Arbitrary precision natural numbers" n)

let () =
  (* zero-length encodings *)
  let open Data_encoding in
  Registration.register (def "ground.unit" unit) ;
  Registration.register
    (def "ground.empty" ~description:"An empty (0-field) object or tuple" empty) ;
  Registration.register (def "ground.null" ~description:"A null value" null)

let () =
  (* integers of various sizes encodings *)
  let open Data_encoding in
  Registration.register
    (def "ground.uint8" ~description:"Unsigned 8 bit integers" uint8) ;
  Registration.register
    (def "ground.int8" ~description:"Signed 8 bit integers" int8) ;
  Registration.register
    (def "ground.uint16" ~description:"Unsigned 16 bit integers" uint16) ;
  Registration.register
    (def "ground.int16" ~description:"Signed 16 bit integers" int16) ;
  Registration.register
    (def "ground.int31" ~description:"Signed 31 bit integers" int31) ;
  Registration.register
    (def "ground.int32" ~description:"Signed 32 bit integers" int32) ;
  Registration.register
    (def "ground.int64" ~description:"Signed 64 bit integers" int64)

let () =
  (* string encodings *)
  let open Data_encoding in
  Registration.register (def "ground.string" string) ;
  Registration.register (def "ground.variable.string" Variable.string) ;
  Registration.register (def "ground.bytes" bytes) ;
  Registration.register (def "ground.variable.bytes" Variable.bytes)

let () =
  (* misc other ground encodings *)
  let open Data_encoding in
  Registration.register (def "ground.bool" ~description:"Boolean values" bool) ;
  Registration.register
    (def "ground.float" ~description:"Floating point numbers" float)

let () =
  Registration.register
    (def "ground.json" ~description:"JSON values" Data_encoding.json)
