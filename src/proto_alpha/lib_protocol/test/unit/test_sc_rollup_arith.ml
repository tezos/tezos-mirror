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

(** Testing
    -------
    Component:  Protocol (saturated arithmetic)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] sc rollup arith$"
    Subject:    Basic testing of the arithmetic rollup example
*)

open Protocol
open Sc_rollup_arith.ProtocolImplementation
open Alpha_context

let create_context () =
  Context.init1 () >>=? fun (block, _contract) -> return block.context

let setup boot_sector f =
  create_context () >>=? fun ctxt ->
  initial_state ctxt boot_sector >>= fun state -> f state

let pre_boot boot_sector f =
  parse_boot_sector boot_sector |> function
  | None -> failwith "Invalid boot sector"
  | Some boot_sector -> setup boot_sector @@ f

let test_preboot () =
  [""; "1"; "1 2 +"]
  |> List.iter_es (fun boot_sector ->
         pre_boot boot_sector @@ fun _state -> return ())

let boot boot_sector f = pre_boot boot_sector @@ fun state -> eval state >>= f

let test_boot () =
  boot "" @@ fun state ->
  is_input_state state >>= function
  | Some _ -> return ()
  | _ -> failwith "After booting, the machine must be waiting for input."

let test_input_message () =
  let open Sc_rollup_PVM_sem in
  boot "" @@ fun state ->
  let input =
    {
      inbox_level = Raw_level.root;
      message_counter = Z.zero;
      payload = "MESSAGE";
    }
  in
  set_input input state >>= fun state ->
  eval state >>= fun state ->
  is_input_state state >>= function
  | Some _ ->
      failwith
        "After receiving a message, the rollup must not be waiting for input."
  | None -> return ()

let go ~max_steps target_status state =
  let rec aux i state =
    pp state >>= fun pp ->
    Format.eprintf "%a" pp () ;
    if i > max_steps then
      failwith "Maximum number of steps reached before target status."
    else
      get_status state >>= fun current_status ->
      if target_status = current_status then return state
      else eval state >>= aux (i + 1)
  in
  aux 0 state

let test_parsing_message ~valid (source, expected_code) =
  let open Sc_rollup_PVM_sem in
  boot "" @@ fun state ->
  let input =
    {inbox_level = Raw_level.root; message_counter = Z.zero; payload = source}
  in
  set_input input state >>= fun state ->
  eval state >>= fun state ->
  go ~max_steps:10000 Evaluating state >>=? fun state ->
  get_parsing_result state >>= fun result ->
  Assert.equal
    ~loc:__LOC__
    (Option.equal Bool.equal)
    "Unexpected parsing resutlt"
    (fun fmt r ->
      Format.fprintf
        fmt
        (match r with
        | None -> "No parsing running"
        | Some true -> "Syntax correct"
        | Some false -> "Syntax error"))
    (Some valid)
    result
  >>=? fun () ->
  if valid then
    get_code state >>= fun code ->
    Assert.equal
      ~loc:__LOC__
      (List.equal equal_instruction)
      "The parsed code is not what we expected: "
      (Format.pp_print_list pp_instruction)
      expected_code
      code
  else return ()

let syntactically_valid_messages =
  List.map
    (fun nums ->
      ( String.concat " " (List.map string_of_int nums),
        List.map (fun x -> IPush x) nums ))
    [[0]; [42]; [373]; [0; 1]; [0; 123; 42; 73; 34; 13; 31]]
  @ [
      ("1 2 +", [IPush 1; IPush 2; IAdd]);
      ( "1 2 3 +    + 3 +",
        [IPush 1; IPush 2; IPush 3; IAdd; IAdd; IPush 3; IAdd] );
      ("1 2+", [IPush 1; IPush 2; IAdd]);
      ("1 2 3++3+", [IPush 1; IPush 2; IPush 3; IAdd; IAdd; IPush 3; IAdd]);
      ("", []);
      ("1 a", [IPush 1; IStore "a"]);
    ]

let syntactically_invalid_messages =
  List.map
    (fun s -> (s, []))
    ["@"; "  @"; "  @  "; "---"; "12 +++ --"; "1a"; "a1"]

let test_parsing_messages () =
  List.iter_es (test_parsing_message ~valid:true) syntactically_valid_messages
  >>=? fun () ->
  List.iter_es
    (test_parsing_message ~valid:false)
    syntactically_invalid_messages

let test_evaluation_message ~valid
    (boot_sector, source, expected_stack, expected_vars) =
  let open Sc_rollup_PVM_sem in
  boot boot_sector @@ fun state ->
  let input =
    {inbox_level = Raw_level.root; message_counter = Z.zero; payload = source}
  in
  set_input input state >>= fun state ->
  eval state >>= fun state ->
  go ~max_steps:10000 WaitingForInputMessage state >>=? fun state ->
  if valid then
    get_stack state >>= fun stack ->
    Assert.equal
      ~loc:__LOC__
      (List.equal Compare.Int.equal)
      "The stack is not what we expected: "
      Format.(pp_print_list (fun fmt -> fprintf fmt "%d;@;"))
      expected_stack
      stack
    >>=? fun () ->
    List.iter_es
      (fun (x, v) ->
        get_var state x >>= function
        | None -> failwith "The variable %s cannot be found." x
        | Some v' ->
            Assert.equal
              ~loc:__LOC__
              Compare.Int.equal
              (Printf.sprintf "The variable %s has not the right value: " x)
              (fun fmt x -> Format.fprintf fmt "%d" x)
              v
              v')
      expected_vars
  else
    get_evaluation_result state >>= function
    | Some true -> failwith "This code should lead to an evaluation error."
    | None -> failwith "We should have reached the evaluation end."
    | Some false -> return ()

let valid_messages =
  [
    ("", "0", [0], []);
    ("", "1 2", [2; 1], []);
    ("", "1 2 +", [3], []);
    ("", "1 2 + 3 +", [6], []);
    ("", "1 2 + 3 + 1 1 + +", [8], []);
    ("0 ", "", [0], []);
    ("1 ", "2", [2; 1], []);
    ("1 2 ", "+", [3], []);
    ("1 2 + ", "3 +", [6], []);
    ("1 2 + ", "3 + 1 1 + +", [8], []);
    ("", "1 a", [1], [("a", 1)]);
    ("", "1 a 2 + b 3 +", [6], [("a", 1); ("b", 3)]);
    ("", "1 a 2 + b 3 + result", [6], [("a", 1); ("b", 3); ("result", 6)]);
    ("1 a ", "2 b", [2; 1], [("a", 1); ("b", 2)]);
    ("1 a ", "2 a", [2; 1], [("a", 2)]);
    ("", "1 a 2 a + a", [3], [("a", 3)]);
    ("", "1 a b", [1], [("a", 1); ("b", 1)]);
    ("1 a", "", [1], [("a", 1)]);
  ]

let invalid_messages =
  List.map
    (fun s -> ("", s, [], []))
    ["+"; "1 +"; "1 1 + +"; "1 1 + 1 1 + + +"; "a"]

let test_evaluation_messages () =
  List.iter_es (test_evaluation_message ~valid:true) valid_messages
  >>=? fun () ->
  List.iter_es (test_evaluation_message ~valid:false) invalid_messages

let tests =
  [
    Tztest.tztest "PreBoot" `Quick test_preboot;
    Tztest.tztest "Boot" `Quick test_boot;
    Tztest.tztest "Input message" `Quick test_input_message;
    Tztest.tztest "Parsing message" `Quick test_parsing_messages;
    Tztest.tztest "Evaluating message" `Quick test_evaluation_messages;
  ]
