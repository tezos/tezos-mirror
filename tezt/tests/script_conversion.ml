(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Testing
   -------
   Component:    Client/Scripts
   Invocation:   dune exec tezt/tests/main.exe -- --file script_conversion.ml
   Subject:      Tests related to Micheline conversions
*)

let convert_source_formats = [`Michelson; `Json; `Binary]

let convert_destination_formats = [`Michelson; `Json; `Binary; `OCaml]

let format_to_string = function
  | `Michelson -> "Michelson"
  | `Json -> "JSON"
  | `Binary -> "Binary"
  | `OCaml -> "OCaml"

let convert_script = function
  | `Michelson ->
      {|{ parameter unit ;
  storage unit ;
  code { CDR ;
         NIL operation ;
         SELF ;
         PUSH mutez 0 ;
         UNIT ;
         TRANSFER_TOKENS ;
         DUP ;
         DIP { CONS } ;
         CONS ;
         PAIR } }|}
  | `Json ->
      {|[ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
  { "prim": "storage", "args": [ { "prim": "unit" } ] },
  { "prim": "code",
    "args":
      [ [ { "prim": "CDR" },
          { "prim": "NIL", "args": [ { "prim": "operation" } ] },
          { "prim": "SELF" },
          { "prim": "PUSH", "args": [ { "prim": "mutez" }, { "int": "0" } ] },
          { "prim": "UNIT" }, { "prim": "TRANSFER_TOKENS" },
          { "prim": "DUP" },
          { "prim": "DIP", "args": [ [ { "prim": "CONS" } ] ] },
          { "prim": "CONS" }, { "prim": "PAIR" } ] ] } ]|}
  | `Binary ->
      "0x02000000300500036c0501036c050202000000210317053d036d03490743036a0000034f034d0321051f0200000002031b031b0342"
  | `OCaml ->
      "Seq (0, [Prim (1, K_parameter, [Prim (2, T_unit, [], [])], []); Prim \
       (3, K_storage, [Prim (4, T_unit, [], [])], []); Prim (5, K_code, [Seq \
       (6, [Prim (7, I_CDR, [], []); Prim (8, I_NIL, [Prim (9, T_operation, \
       [], [])], []); Prim (10, I_SELF, [], []); Prim (11, I_PUSH, [Prim (12, \
       T_mutez, [], []); Int (13, Z.zero)], []); Prim (14, I_UNIT, [], []); \
       Prim (15, I_TRANSFER_TOKENS, [], []); Prim (16, I_DUP, [], []); Prim \
       (17, I_DIP, [Seq (18, [Prim (19, I_CONS, [], [])])], []); Prim (20, \
       I_CONS, [], []); Prim (21, I_PAIR, [], [])])], [])])"

let convert_data = function
  | `Michelson ->
      {|{ DROP ;
  PUSH address "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" ;
  CONTRACT unit ;
  { IF_NONE { { UNIT ; FAILWITH } } {} } ;
  PUSH mutez 1 ;
  UNIT ;
  TRANSFER_TOKENS ;
  DIP { NIL operation } ;
  CONS }|}
  | `Json ->
      {|[ { "prim": "DROP" },
  { "prim": "PUSH",
    "args":
      [ { "prim": "address" },
        { "string": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" } ] },
  { "prim": "CONTRACT", "args": [ { "prim": "unit" } ] },
  [ { "prim": "IF_NONE",
      "args": [ [ [ { "prim": "UNIT" }, { "prim": "FAILWITH" } ] ], [] ] } ],
  { "prim": "PUSH", "args": [ { "prim": "mutez" }, { "int": "1" } ] },
  { "prim": "UNIT" }, { "prim": "TRANSFER_TOKENS" },
  { "prim": "DIP",
    "args": [ [ { "prim": "NIL", "args": [ { "prim": "operation" } ] } ] ] },
  { "prim": "CONS" } ]|}
  | `Binary ->
      "0x020000006403200743036e0100000024747a31666173774354446369527a45346f4a396a6e32566d3264766a6579413966557a550555036c0200000015072f02000000090200000004034f032702000000000743036a0001034f034d051f0200000004053d036d031b"
  | `OCaml ->
      {|Seq (0, [Prim (1, I_DROP, [], []); Prim (2, I_PUSH, [Prim (3, T_address, [], []); String (4, "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU")], []); Prim (5, I_CONTRACT, [Prim (6, T_unit, [], [])], []); Seq (7, [Prim (8, I_IF_NONE, [Seq (9, [Seq (10, [Prim (11, I_UNIT, [], []); Prim (12, I_FAILWITH, [], [])])]); Seq (13, [])], [])]); Prim (14, I_PUSH, [Prim (15, T_mutez, [], []); Int (16, Z.one)], []); Prim (17, I_UNIT, [], []); Prim (18, I_TRANSFER_TOKENS, [], []); Prim (19, I_DIP, [Seq (20, [Prim (21, I_NIL, [Prim (22, T_operation, [], [])], [])])], []); Prim (23, I_CONS, [], [])])|}

let convert_data_type = "lambda unit (list operation)"

let iter xs f = Lwt_list.iter_s f xs

let test_convert_script =
  Protocol.register_test
    ~__FILE__
    ~title:"convert script"
    ~tags:["michelson"; "conversion"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  iter convert_source_formats @@ fun src_format ->
  iter convert_destination_formats @@ fun dst_format ->
  let input_script = convert_script src_format in
  let expected_output_script = convert_script dst_format in
  let* results =
    Client.convert_script ~src_format ~dst_format ~script:input_script client
  in
  Check.(
    (String.trim results = expected_output_script)
      string
      ~__LOC__
      ~error_msg:
        ("Unexpected results when converting from "
        ^ Client.conversion_format_to_string src_format
        ^ " to "
        ^ Client.conversion_format_to_string src_format
        ^ ". Expected %R, got %L.")) ;
  unit

let test_convert_data =
  Protocol.register_test
    ~__FILE__
    ~title:"convert data"
    ~tags:["michelson"; "conversion"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  iter convert_source_formats @@ fun src_format ->
  iter convert_destination_formats @@ fun dst_format ->
  iter [None; Some convert_data_type] @@ fun typecheck ->
  let input_data = convert_data src_format in
  let expected_output_data = convert_data dst_format in
  let* results =
    Client.convert_data
      ~src_format
      ~dst_format
      ~data:input_data
      ?typecheck
      client
  in
  Check.(
    (String.trim results = expected_output_data)
      string
      ~__LOC__
      ~error_msg:
        ("Unexpected results when converting from "
        ^ format_to_string src_format
        ^ " to "
        ^ format_to_string src_format
        ^ ". Expected %R, got %L.")) ;
  unit

let register ~protocols =
  test_convert_script protocols ;
  test_convert_data protocols
