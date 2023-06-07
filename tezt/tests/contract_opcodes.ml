(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_opcodes.ml
   Subject:      Runs Michelson opcode tests using [octez-client -mode mockup run script ...].
*)

(* For these tests, the default hooks would scrub much output of
   interest. We use a lighter hook that only scrubs the clients
   [--base-dir], since it varies between executions. *)
let hooks =
  Tezos_regression.hooks_custom
    ~scrubbed_global_options:["--base-dir"; "-d"]
    ~replace_variables:Fun.id
    ()

let public_key = Account.Bootstrap.keys.(0).public_key

let hex s = s |> Bytes.of_string |> Hex.of_bytes |> Hex.show

let register_opcode_tests ?supports parameterization protocols =
  Fun.flip List.iter parameterization
  @@ fun (script, storage, input, expected) ->
  ( Protocol.register_regression_test
      ?supports
      ~__FILE__
      ~title:
        (sf
           "opcodes [%s--storage%d--input%d]"
           script
           (Hashtbl.hash storage)
           (Hashtbl.hash input))
      ~tags:["michelson"]
  @@ fun protocol ->
    let client = Client.create_with_mode Mockup in
    let script_path =
      Michelson_script.(find ["opcodes"; script] protocol |> path)
    in
    let* {storage; _} =
      Client.run_script
        ~protocol_hash:(Protocol.hash protocol)
        ~no_base_dir_warnings:true
        ~trace_stack:true
        ~hooks
        ~prg:script_path
        ~storage
        ~input
          (* We force the level to 1 to get consistent results from the
             [LEVEL] instructions *)
        ~level:1
        client
    in
    let error_msg =
      "Expected storage %R, got %L when executing"
      ^ sf " %s with input %s and storage %s" script_path input storage
    in
    Check.((storage = expected) string ~__LOC__ ~error_msg) ;
    unit )
    protocols

let test_protocol_independent =
  register_opcode_tests
    [
      ("cons", "{}", "10", "{ 10 }");
      ("cons", "{ 10 }", "-5", "{ -5 ; 10 }");
      ("cons", "{ -5 ; 10 }", "99", "{ 99 ; -5 ; 10 }");
      (* Tests on Options *)
      ("none", "Some 10", "Unit", "None");
      ("ret_int", "None", "Unit", "(Some 300)");
      (* Map block on lists *)
      ("list_map_block", "{0}", "{}", "{}");
      ("list_map_block", "{0}", "{ 1 ; 1 ; 1 ; 1 }", "{ 1 ; 2 ; 3 ; 4 }");
      ("list_map_block", "{0}", "{ 1 ; 2 ; 3 ; 0 }", "{ 1 ; 3 ; 5 ; 3 }");
      ("emit", "Unit", "Unit", "Unit");
      (* Reverse a list *)
      ("reverse", {|{""}|}, "{}", "{}");
      ("reverse", {|{""}|}, {|{ "c" ; "b" ; "a" }|}, {|{ "a" ; "b" ; "c" }|});
      (* Reverse using LOOP_LEFT *)
      ("loop_left", {|{""}|}, "{}", "{}");
      ("loop_left", {|{""}|}, {|{ "c" ; "b" ; "a" }|}, {|{ "a" ; "b" ; "c" }|});
      (* Identity on strings *)
      ("str_id", "None", {|"Hello"|}, {|(Some "Hello")|});
      ("str_id", "None", {|"abcd"|}, {|(Some "abcd")|});
      (* Slice strings *)
      ("slice", "None", "Pair 0 0", "None");
      ("slice", {|Some "Foo"|}, "Pair 10 5", "None");
      ("slice", {|Some "Foo"|}, "Pair 0 0", {|(Some "")|});
      ("slice", {|Some "Foo"|}, "Pair 0 10", "None");
      ("slice", {|Some "Foo"|}, "Pair 0 2", {|(Some "Fo")|});
      ("slice", {|Some "Foo"|}, "Pair 1 3", "None");
      ("slice", {|Some "Foo"|}, "Pair 1 1", {|(Some "o")|});
      (* Stress-test the failure case of slice for a *)
      (* non-trivial gas consumption *)
      ( "slice",
        "Some" ^ "\""
        ^ (String.concat "" @@ List.init 2000 (Fun.const "Foo"))
        ^ "\"",
        "Pair 1 10000",
        "None" );
      (* Slice bytes *)
      ("slice_bytes", "None", "Pair 0 1", "None");
      ("slice_bytes", "Some 0xaabbcc", "Pair 0 0", "(Some 0x)");
      ("slice_bytes", "Some 0xaabbcc", "Pair 0 1", "(Some 0xaa)");
      ("slice_bytes", "Some 0xaabbcc", "Pair 1 1", "(Some 0xbb)");
      ("slice_bytes", "Some 0xaabbcc", "Pair 1 2", "(Some 0xbbcc)");
      ("slice_bytes", "Some 0xaabbcc", "Pair 1 3", "None");
      ("slice_bytes", "Some 0xaabbcc", "Pair 2 1", "(Some 0xcc)");
      ("slice_bytes", "Some 0xaabbcc", "Pair 2 2", "None");
      (* Stress-test the failure case of slice for a *)
      (* non-trivial gas  consumption *)
      ( "slice_bytes",
        "Some 0x" ^ String.concat "" @@ List.init 2000 (Fun.const "aabbcc"),
        "Pair 1 10000",
        "None" );
      (* Identity on pairs *)
      ("pair_id", "None", "(Pair True False)", "(Some (Pair True False))");
      ("pair_id", "None", "(Pair False True)", "(Some (Pair False True))");
      ("pair_id", "None", "(Pair True True)", "(Some (Pair True True))");
      ("pair_id", "None", "(Pair False False)", "(Some (Pair False False))");
      (* Tests CAR and CDR instructions *)
      ("car", "0", "(Pair 34 17)", "34");
      ("cdr", "0", "(Pair 34 17)", "17");
      (* Logical not *)
      ("not", "None", "True", "(Some False)");
      ("not", "None", "False", "(Some True)");
      (* Logical and *)
      ("and", "None", "(Pair False False)", "(Some False)");
      ("and", "None", "(Pair False True)", "(Some False)");
      ("and", "None", "(Pair True False)", "(Some False)");
      ("and", "None", "(Pair True True)", "(Some True)");
      (* Logical or *)
      ("or", "None", "(Pair False False)", "(Some False)");
      ("or", "None", "(Pair False True)", "(Some True)");
      ("or", "None", "(Pair True False)", "(Some True)");
      ("or", "None", "(Pair True True)", "(Some True)");
      (* Logical and *)
      ("and_logical_1", "False", "(Pair False False)", "False");
      ("and_logical_1", "False", "(Pair False True)", "False");
      ("and_logical_1", "False", "(Pair True False)", "False");
      ("and_logical_1", "False", "(Pair True True)", "True");
      (* Binary and *)
      ("and_binary", "Unit", "Unit", "Unit");
      (* Binary or *)
      ("or_binary", "None", "(Pair 4 8)", "(Some 12)");
      ("or_binary", "None", "(Pair 0 8)", "(Some 8)");
      ("or_binary", "None", "(Pair 8 0)", "(Some 8)");
      ("or_binary", "None", "(Pair 15 4)", "(Some 15)");
      ("or_binary", "None", "(Pair 14 1)", "(Some 15)");
      ("or_binary", "None", "(Pair 7 7)", "(Some 7)");
      (* Binary not *)
      ("not_binary", "None", "(Left 0)", "(Some -1)");
      ("not_binary", "None", "(Left 8)", "(Some -9)");
      ("not_binary", "None", "(Left 7)", "(Some -8)");
      ("not_binary", "None", "(Left -9)", "(Some 8)");
      ("not_binary", "None", "(Left -8)", "(Some 7)");
      ("not_binary", "None", "(Right 0)", "(Some -1)");
      ("not_binary", "None", "(Right 8)", "(Some -9)");
      ("not_binary", "None", "(Right 7)", "(Some -8)");
      (* XOR *)
      ("xor", "None", "Left (Pair False False)", "(Some (Left False))");
      ("xor", "None", "Left (Pair False True)", "(Some (Left True))");
      ("xor", "None", "Left (Pair True False)", "(Some (Left True))");
      ("xor", "None", "Left (Pair True True)", "(Some (Left False))");
      ("xor", "None", "Right (Pair 0 0)", "(Some (Right 0))");
      ("xor", "None", "Right (Pair 0 1)", "(Some (Right 1))");
      ("xor", "None", "Right (Pair 1 0)", "(Some (Right 1))");
      ("xor", "None", "Right (Pair 1 1)", "(Some (Right 0))");
      ("xor", "None", "Right (Pair 42 21)", "(Some (Right 63))");
      ("xor", "None", "Right (Pair 42 63)", "(Some (Right 21))");
      (* test shifts: LSL & LSR *)
      ("shifts", "None", "(Left (Pair 8 1))", "(Some 16)");
      ("shifts", "None", "(Left (Pair 0 0))", "(Some 0)");
      ("shifts", "None", "(Left (Pair 0 1))", "(Some 0)");
      ("shifts", "None", "(Left (Pair 1 2))", "(Some 4)");
      ("shifts", "None", "(Left (Pair 15 2))", "(Some 60)");
      ("shifts", "None", "(Right (Pair 8 1))", "(Some 4)");
      ("shifts", "None", "(Right (Pair 0 0))", "(Some 0)");
      ("shifts", "None", "(Right (Pair 0 1))", "(Some 0)");
      ("shifts", "None", "(Right (Pair 1 2))", "(Some 0)");
      ("shifts", "None", "(Right (Pair 15 2))", "(Some 3)");
      (* Concatenate all strings of a list into one string *)
      ("concat_list", {|""|}, {|{ "a" ; "b" ; "c" }|}, {|"abc"|});
      ("concat_list", {|""|}, "{}", {|""|});
      ( "concat_list",
        {|""|},
        {|{ "Hello" ; " " ; "World" ; "!" }|},
        {|"Hello World!"|} );
      (* Concatenate the bytes in storage with all bytes in the given list *)
      ("concat_hello_bytes", "{}", "{ 0xcd }", "{ 0xffcd }");
      ("concat_hello_bytes", "{}", "{}", "{}");
      ("concat_hello_bytes", "{}", "{ 0xab ; 0xcd }", "{ 0xffab ; 0xffcd }");
      (* Identity on lists *)
      ("list_id", {|{""}|}, {|{ "1" ; "2" ; "3" }|}, {|{ "1" ; "2" ; "3" }|});
      ("list_id", {|{""}|}, "{}", "{}");
      ("list_id", {|{""}|}, {|{ "a" ; "b" ; "c" }|}, {|{ "a" ; "b" ; "c" }|});
      ("list_id_map", {|{""}|}, {|{ "1" ; "2" ; "3" }|}, {|{ "1" ; "2" ; "3" }|});
      ("list_id_map", {|{""}|}, "{}", "{}");
      ("list_id_map", {|{""}|}, {|{ "a" ; "b" ; "c" }|}, {|{ "a" ; "b" ; "c" }|});
      (* Identity on maps *)
      ("map_id", "{}", "{ Elt 0 1 }", "{ Elt 0 1 }");
      ("map_id", "{}", "{ Elt 0 0 }", "{ Elt 0 0 }");
      ("map_id", "{}", "{ Elt 0 0 ; Elt 3 4 }", "{ Elt 0 0 ; Elt 3 4 }");
      (* Memberships in maps *)
      ( "map_mem_nat",
        "(Pair { Elt 0 1 } None)",
        "1",
        "(Pair { Elt 0 1 } (Some False))" );
      ("map_mem_nat", "(Pair {} None)", "1", "(Pair {} (Some False))");
      ( "map_mem_nat",
        "(Pair { Elt 1 0 } None)",
        "1",
        "(Pair { Elt 1 0 } (Some True))" );
      ( "map_mem_nat",
        "(Pair { Elt 1 4 ; Elt 2 11 } None)",
        "1",
        "(Pair { Elt 1 4 ; Elt 2 11 } (Some True))" );
      ( "map_mem_nat",
        "(Pair { Elt 1 4 ; Elt 2 11 } None)",
        "2",
        "(Pair { Elt 1 4 ; Elt 2 11 } (Some True))" );
      ( "map_mem_nat",
        "(Pair { Elt 1 4 ; Elt 2 11 } None)",
        "3",
        {|(Pair { Elt 1 4 ; Elt 2 11 } (Some False))|} );
      ( "map_mem_string",
        {|(Pair { Elt "foo" 1 } None)|},
        {|"bar"|},
        {|(Pair { Elt "foo" 1 } (Some False))|} );
      ( "map_mem_string",
        {|(Pair {} None)|},
        {|"bar"|},
        {|(Pair {} (Some False))|} );
      ( "map_mem_string",
        {|(Pair { Elt "foo" 0 } None)|},
        {|"foo"|},
        {|(Pair { Elt "foo" 0 } (Some True))|} );
      ( "map_mem_string",
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } None)|},
        {|"foo"|},
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } (Some True))|} );
      ( "map_mem_string",
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } None)|},
        {|"bar"|},
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } (Some True))|} );
      ( "map_mem_string",
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } None)|},
        {|"baz"|},
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } (Some False))|} );
      (* Mapping over maps *)
      ("map_map", "{}", "10", "{}");
      ("map_map", {|{ Elt "foo" 1 }|}, "10", {|{ Elt "foo" 11 }|});
      ( "map_map",
        {|{ Elt "bar" 5 ; Elt "foo" 1 }|},
        "15",
        {|{ Elt "bar" 20 ; Elt "foo" 16 }|} );
      (* Memberships in big maps *)
      ( "big_map_mem_nat",
        "(Pair { Elt 0 1 } None)",
        "1",
        "(Pair 4 (Some False))" );
      ("big_map_mem_nat", "(Pair {} None)", "1", "(Pair 4 (Some False))");
      ("big_map_mem_nat", "(Pair { Elt 1 0 } None)", "1", "(Pair 4 (Some True))");
      ( "big_map_mem_nat",
        "(Pair { Elt 1 4 ; Elt 2 11 } None)",
        "1",
        "(Pair 4 (Some True))" );
      ( "big_map_mem_nat",
        "(Pair { Elt 1 4 ; Elt 2 11 } None)",
        "2",
        "(Pair 4 (Some True))" );
      ( "big_map_mem_nat",
        "(Pair { Elt 1 4 ; Elt 2 11 } None)",
        "3",
        "(Pair 4 (Some False))" );
      ( "big_map_mem_string",
        {|(Pair { Elt "foo" 1 } None)|},
        {|"bar"|},
        "(Pair 4 (Some False))" );
      ( "big_map_mem_string",
        "(Pair {} None)",
        {|"bar"|},
        "(Pair 4 (Some False))" );
      ( "big_map_mem_string",
        {|(Pair { Elt "foo" 0 } None)|},
        {|"foo"|},
        "(Pair 4 (Some True))" );
      ( "big_map_mem_string",
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } None)|},
        {|"foo"|},
        "(Pair 4 (Some True))" );
      ( "big_map_mem_string",
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } None)|},
        {|"bar"|},
        "(Pair 4 (Some True))" );
      ( "big_map_mem_string",
        {|(Pair { Elt "bar" 4 ; Elt "foo" 11 } None)|},
        {|"baz"|},
        "(Pair 4 (Some False))" );
      (* Identity on sets *)
      ("set_id", "{}", {|{ "a" ; "b" ; "c" }|}, {|{ "a" ; "b" ; "c" }|});
      ("set_id", "{}", "{}", "{}");
      ("set_id", "{}", {|{ "asdf" ; "bcde" }|}, {|{ "asdf" ; "bcde" }|});
      (* List concat *)
      ("list_concat", {|"abc"|}, {|{ "d" ; "e" ; "f" }|}, {|"abcdef"|});
      ("list_concat", {|"abc"|}, "{}", {|"abc"|});
      ("list_concat_bytes", "0x00ab", "{ 0xcd ; 0xef ; 0x00 }", "0x00abcdef00");
      ("list_concat_bytes", "0x", "{ 0x00 ; 0x11 ; 0x00 }", "0x001100");
      ("list_concat_bytes", "0xabcd", "{}", "0xabcd");
      ("list_concat_bytes", "0x", "{}", "0x");
      (* List iter *)
      ("list_iter", "0", "{ 10 ; 2 ; 1 }", "20");
      ("list_iter", "0", "{ 3 ; 6 ; 9 }", "162");
      (* List size *)
      ("list_size", "111", "{}", "0");
      ("list_size", "111", "{ 1 }", "1");
      ("list_size", "111", "{ 1 ; 2 ; 3 }", "3");
      ("list_size", "111", "{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }", "6");
      (* Set member -- set is in storage *)
      ("set_member", "(Pair {} None)", {|"Hi"|}, "(Pair {} (Some False))");
      ( "set_member",
        {|(Pair { "Hi" } None)|},
        {|"Hi"|},
        {|(Pair { "Hi" } (Some True))|} );
      ( "set_member",
        {|(Pair { "Hello" ; "World" } None)|},
        {|""|},
        {|(Pair { "Hello" ; "World" } (Some False))|} );
      (* Set size *)
      ("set_size", "111", "{}", "0");
      ("set_size", "111", "{ 1 }", "1");
      ("set_size", "111", "{ 1 ; 2 ; 3 }", "3");
      ("set_size", "111", "{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }", "6");
      (* Set iter *)
      ("set_iter", "111", "{}", "0");
      ("set_iter", "111", "{ 1 }", "1");
      ("set_iter", "111", "{ -100 ; 1 ; 2 ; 3 }", "-94");
      (* Map size *)
      ("map_size", "111", "{}", "0");
      ("map_size", "111", {|{ Elt "a" 1 }|}, "1");
      ("map_size", "111", {|{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 }|}, "3");
      ( "map_size",
        "111",
        {|{ Elt "a" 1 ; Elt "b" 2 ; Elt "c" 3 ; Elt "d" 4 ; Elt "e" 5 ; Elt "f" 6 }|},
        "6" );
      (* Contains all elements -- does the second list contain all of the same elements as the first one? I'm ignoring  element multiplicity *)
      ("contains_all", "None", "(Pair {} {})", "(Some True)");
      ("contains_all", "None", {|(Pair { "c" } { "B" })|}, "(Some False)");
      ("contains_all", "None", {|(Pair { "A" } { "B" })|}, "(Some False)");
      ("contains_all", "None", {|(Pair { "B" } { "B" })|}, "(Some True)");
      ( "contains_all",
        "None",
        {|(Pair { "B" ; "C" ; "asdf" } { "B" ; "B" ; "asdf" ; "C" })|},
        "(Some True)" );
      ( "contains_all",
        "None",
        {|(Pair { "B" ; "B" ; "asdf" ; "C" } { "B" ; "C" ; "asdf" })|},
        "(Some True)" );
      (* Concatenate the string in storage with all strings in *)
      (* the given list *)
      ("concat_hello", "{}", {|{ "World!" }|}, {|{ "Hello World!" }|});
      ("concat_hello", "{}", "{}", "{}");
      ( "concat_hello",
        "{}",
        {|{ "test1" ; "test2" }|},
        {|{ "Hello test1" ; "Hello test2" }|} );
      (* Create an empty map and add a string to it *)
      ("empty_map", "{}", "Unit", {|{ Elt "hello" "world" }|});
      (* Get the value stored at the given key in the map *)
      ( "get_map_value",
        {|(Pair None { Elt "hello" "hi" })|},
        {|"hello"|},
        {|(Pair (Some "hi") { Elt "hello" "hi" })|} );
      ( "get_map_value",
        {|(Pair None { Elt "hello" "hi" })|},
        {|""|},
        {|(Pair None { Elt "hello" "hi" })|} );
      ( "get_map_value",
        {|(Pair None { Elt "1" "one" ; Elt "2" "two" })|},
        {|"1"|},
        {|(Pair (Some "one") { Elt "1" "one" ; Elt "2" "two" })|} );
      (* Get and update the value stored at the given key in the map *)
      ("get_and_update_map", "(Pair None {})", {|"hello"|}, "(Pair None {})");
      ( "get_and_update_map",
        "(Pair (Some 4) {})",
        {|"hello"|},
        {|(Pair None { Elt "hello" 4 })|} );
      ( "get_and_update_map",
        {|(Pair None { Elt "hello" 4 })|},
        {|"hello"|},
        "(Pair (Some 4) {})" );
      ( "get_and_update_map",
        {|(Pair (Some 5) { Elt "hello" 4 })|},
        {|"hello"|},
        {|(Pair (Some 4) { Elt "hello" 5 })|} );
      ( "get_and_update_map",
        {|(Pair (Some 5) { Elt "hello" 4 })|},
        {|"hi"|},
        {|(Pair None { Elt "hello" 4 ; Elt "hi" 5 })|} );
      ( "get_and_update_map",
        {|(Pair None { Elt "1" 1 ; Elt "2" 2 })|},
        {|"1"|},
        {|(Pair (Some 1) { Elt "2" 2 })|} );
      ( "get_and_update_map",
        {|(Pair None { Elt "1" 1 ; Elt "2" 2 })|},
        {|"2"|},
        {|(Pair (Some 2) { Elt "1" 1 })|} );
      (* Map iter *)
      ("map_iter", "(Pair 0 0)", "{ Elt 0 100 ; Elt 2 100 }", "(Pair 2 200)");
      ("map_iter", "(Pair 0 0)", "{ Elt 1 1 ; Elt 2 100 }", "(Pair 3 101)");
      (* Return True if True branch of if was taken and False otherwise *)
      ("if", "None", "True", "(Some True)");
      ("if", "None", "False", "(Some False)");
      (* Generate a pair of or types *)
      ("left_right", {|(Left "X")|}, "(Left True)", "(Right True)");
      ("left_right", {|(Left "X")|}, {|(Right "a")|}, {|(Left "a")|});
      (* Reverse a list *)
      ("reverse_loop", {|{""}|}, "{}", "{}");
      ( "reverse_loop",
        {|{""}|},
        {|{ "c" ; "b" ; "a" }|},
        {|{ "a" ; "b" ; "c" }|} );
      (* Exec concat contract *)
      ("exec_concat", {|"?"|}, {|""|}, {|"_abc"|});
      ("exec_concat", {|"?"|}, {|"test"|}, {|"test_abc"|});
      (* Get the current balance of the contract *)
      ("balance", "111", "Unit", "4000000000000");
      (* Get the current level of the block *)
      (* Test the produced variable annotation *)
      ("level", "111", "Unit", "1");
      (* Test addition and subtraction on tez *)
      ( "tez_add_sub",
        "None",
        "(Pair 2000000 1000000)",
        "(Some (Pair 3000000 1000000))" );
      ( "tez_add_sub",
        "None",
        "(Pair 2310000 1010000)",
        "(Some (Pair 3320000 1300000))" );
      (* Test various additions *)
      ("add", "Unit", "Unit", "Unit");
      (* Test ABS *)
      ("abs", "Unit", "12039123919239192312931", "Unit");
      ("abs", "Unit", "0", "Unit");
      ("abs", "Unit", "948", "Unit");
      (* Test INT *)
      ("int", "None", "0", "(Some 0)");
      ("int", "None", "1", "(Some 1)");
      ("int", "None", "9999", "(Some 9999)");
      (* Test DIP *)
      ("dip", "(Pair 0 0)", "(Pair 15 9)", "(Pair 15 24)");
      ("dip", "(Pair 0 0)", "(Pair 1 1)", "(Pair 1 2)");
      (* Test get first element of list *)
      ("first", "111", "{ 1 ; 2 ; 3 ; 4 }", "1");
      ("first", "111", "{ 4 }", "4");
      (* Hash input string *)
      (* Test assumed to be correct -- hash is based on encoding of AST *)
      ( "hash_string",
        "0x00",
        {|"abcdefg"|},
        "0x46fdbcb4ea4eadad5615c"
        ^ "daa17d67f783e01e21149ce2b27de497600b4cd8f4e" );
      ( "hash_string",
        "0x00",
        {|"12345"|},
        "0xb4c26c20de52a4eaf0d8a34"
        ^ "0db47ad8cb1e74049570859c9a9a3952b204c772f" );
      (* IF_SOME *)
      ("if_some", {|"?"|}, {|(Some "hello")|}, {|"hello"|});
      ("if_some", {|"?"|}, "None", {|""|});
      (* Tests the SET_CAR and SET_CDR instructions *)
      ("set_car", {|(Pair "hello" 0)|}, {|"world"|}, {|(Pair "world" 0)|});
      ("set_car", {|(Pair "hello" 0)|}, {|"abc"|}, {|(Pair "abc" 0)|});
      ("set_car", {|(Pair "hello" 0)|}, {|""|}, {|(Pair "" 0)|});
      ("set_cdr", {|(Pair "hello" 0)|}, "1", {|(Pair "hello" 1)|});
      ("set_cdr", {|(Pair "hello" 500)|}, "3", {|(Pair "hello" 3)|});
      ("set_cdr", {|(Pair "hello" 7)|}, "100", {|(Pair "hello" 100)|});
      (* Convert a public key to a public key hash *)
      ( "hash_key",
        "None",
        {|"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"|},
        {|(Some "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")|} );
      ( "hash_key",
        "None",
        {|"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES"|},
        {|(Some "tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k")|} );
      (* Test timestamp operations *)
      ( "add_timestamp_delta",
        "None",
        "(Pair 100 100)",
        {|(Some "1970-01-01T00:03:20Z")|} );
      ( "add_timestamp_delta",
        "None",
        "(Pair 100 -100)",
        {|(Some "1970-01-01T00:00:00Z")|} );
      ( "add_timestamp_delta",
        "None",
        {|(Pair "1970-01-01T00:00:00Z" 0)|},
        {|(Some "1970-01-01T00:00:00Z")|} );
      ( "add_delta_timestamp",
        "None",
        "(Pair 100 100)",
        {|(Some "1970-01-01T00:03:20Z")|} );
      ( "add_delta_timestamp",
        "None",
        "(Pair -100 100)",
        {|(Some "1970-01-01T00:00:00Z")|} );
      ( "add_delta_timestamp",
        "None",
        {|(Pair 0 "1970-01-01T00:00:00Z")|},
        {|(Some "1970-01-01T00:00:00Z")|} );
      ( "sub_timestamp_delta",
        "111",
        "(Pair 100 100)",
        {|"1970-01-01T00:00:00Z"|} );
      ( "sub_timestamp_delta",
        "111",
        "(Pair 100 -100)",
        {|"1970-01-01T00:03:20Z"|} );
      ( "sub_timestamp_delta",
        "111",
        "(Pair 100 2000000000000000000)",
        "-1999999999999999900" );
      ("diff_timestamps", "111", "(Pair 0 0)", "0");
      ("diff_timestamps", "111", "(Pair 0 1)", "-1");
      ("diff_timestamps", "111", "(Pair 1 0)", "1");
      ( "diff_timestamps",
        "111",
        {|(Pair "1970-01-01T00:03:20Z" "1970-01-01T00:00:00Z")|},
        "200" );
      (* Test pack/unpack *)
      ( "packunpack_rev",
        "Unit",
        {|(Pair -1 (Pair 1 (Pair "foobar" (Pair 0x00AABBCC (Pair 1000 (Pair False (Pair "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5" (Pair "2019-09-09T08:35:33Z" "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"))))))))|},
        "Unit" );
      ( "packunpack_rev",
        "Unit",
        {|(Pair -1  (Pair 1 (Pair "foobar" (Pair 0x00AABBCC (Pair 1000 (Pair False (Pair "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5" (Pair "2019-09-09T08:35:33Z" "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"))))))))|},
        "Unit" );
      ( "packunpack_rev_cty",
        "Unit",
        {|(Pair "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" (Pair Unit (Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" (Pair (Some "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7") (Pair { Unit }  (Pair { True }  (Pair (Pair 19 10) (Pair (Left "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5") (Pair { Elt 0 "foo" ; Elt 1 "bar" }  { PACK } )))))))))|},
        "Unit" );
      ( "packunpack_rev_cty",
        "Unit",
        {|(Pair "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" (Pair Unit (Pair "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" (Pair None (Pair {  }  (Pair {  }  (Pair (Pair 40 -10) (Pair (Right "2019-09-09T08:35:33Z") (Pair {  }  { DUP ; DROP ; PACK } )))))))))|},
        "Unit" );
      (* Test EDIV on nat and int *)
      ( "ediv",
        "(Pair None None None None)",
        "(Pair 10 -3)",
        "(Pair (Some (Pair -3 1)) (Some (Pair 3 1)) "
        ^ "(Some (Pair -3 1)) (Some (Pair 3 1)))" );
      ( "ediv",
        "(Pair None None None None)",
        "(Pair 10 0)",
        "(Pair None None None None)" );
      ( "ediv",
        "(Pair None None None None)",
        "(Pair -8 2)",
        "(Pair (Some (Pair -4 0)) (Some (Pair -4 0)) "
        ^ "(Some (Pair 4 0)) (Some (Pair 4 0)))" );
      (* Test EDIV on mutez *)
      ( "ediv_mutez",
        "(Left None)",
        "(Pair 10 (Left 10))",
        "(Left (Some (Pair 1 0)))" );
      ( "ediv_mutez",
        "(Left None)",
        "(Pair 10 (Left 3))",
        "(Left (Some (Pair 3 1)))" );
      ("ediv_mutez", "(Left None)", "(Pair 10 (Left 0))", "(Left None)");
      ( "ediv_mutez",
        "(Left None)",
        "(Pair 10 (Right 10))",
        "(Right (Some (Pair 1 0)))" );
      ( "ediv_mutez",
        "(Left None)",
        "(Pair 10 (Right 3))",
        "(Right (Some (Pair 3 1)))" );
      ("ediv_mutez", "(Left None)", "(Pair 10 (Right 0))", "(Right None)");
      ( "ediv_mutez",
        "(Left None)",
        "(Pair 5 (Right 10))",
        "(Right (Some (Pair 0 5)))" );
      (* Test compare *)
      ("compare", "Unit", "Unit", "Unit");
      (* Test comparison combinators: *)
      (*   GT, GE, LT, LE, NEQ, EQ *)
      ( "comparisons",
        "{}",
        "{ -9999999; -1 ; 0 ; 1 ; 9999999 }",
        "{ "
        ^ "{ False ; False ; False ; True ; True } ;\n\
          \    { False ; False ; True ; True ; True } ;\n\
          \    { True ; True ; False ; False ; False } ;\n\
          \    { True ; True ; True ; False ; False } ;\n\
          \    { True ; True ; False ; True ; True } ;\n\
          \    { False ; False ; True ; False ; False } }" );
      (* Test ADDRESS *)
      ( "address",
        "None",
        {|"tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"|},
        {|(Some "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5")|} );
      (* Test (CONTRACT unit) *)
      ("contract", "Unit", {|"tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"|}, "Unit");
      (* Test create_contract *)
      ( "create_contract",
        "None",
        "Unit",
        {|(Some "KT1Mjjcb6tmSsLm7Cb3DSQszePjfchPM4Uxm")|} );
      (* Test multiplication - success case (no overflow) *)
      (* Failure case is tested in mul_overflow.tz *)
      ("mul", "Unit", "Unit", "Unit");
      (* Test NEG *)
      ("neg", "0", "(Left 2)", "-2");
      ("neg", "0", "(Right 2)", "-2");
      ("neg", "0", "(Left 0)", "0");
      ("neg", "0", "(Right 0)", "0");
      ("neg", "0", "(Left -2)", "2");
      (* Test DIGN, DUGN, DROPN, DIPN *)
      ("dign", "0", "(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)", "5");
      ("dugn", "0", "(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)", "1");
      ("dropn", "0", "(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)", "5");
      ("dipn", "0", "(Pair (Pair (Pair (Pair 1 2) 3) 4) 5)", "6");
      (* Test DIGN 17 times. *)
      ( "dig_eq",
        "Unit",
        "(Pair 17 (Pair 16 (Pair 15 (Pair 14 (Pair 13 (Pair 12"
        ^ " (Pair 11 (Pair 10 (Pair 9 (Pair 8 (Pair 7 (Pair 6 (P"
        ^ "air 5 (Pair 4 (Pair 3 (Pair 2 1))))))))))))))))",
        "Unit" );
      ( "dig_eq",
        "Unit",
        "(Pair 2 (Pair 3 (Pair 12 (Pair 16 (Pair 10 (Pair 14 ("
        ^ "Pair 19 (Pair 9 (Pair 18 (Pair 6 (Pair 8 (Pair 11 (Pa"
        ^ "ir 4 (Pair 13 (Pair 15 (Pair 5 1))))))))))))))))",
        "Unit" );
      (* Test Partial Exec *)
      ("pexec", "14", "38", "52");
      ("pexec_2", "{ 0 ; 1 ; 2 ; 3}", "4", "{ 0 ; 7 ; 14 ; 21 }");
      (* Test CHAIN_ID *)
      ("chain_id_store", "None", "Unit", {|(Some "NetXynUjJNZm7wi")|});
      ( "chain_id_store",
        "(Some 0x7a06a770)",
        "Unit",
        {|(Some "NetXynUjJNZm7wi")|} );
      ( "chain_id_store",
        {|(Some "NetXynUjJNZm7wi")|},
        "Unit",
        {|(Some "NetXynUjJNZm7wi")|} );
      (* Test SELF *)
      ("self_with_entrypoint", "Unit", "Left (Left 0)", "Unit");
      ("self_with_default_entrypoint", "Unit", "Unit", "Unit");
      (* Test SELF_ADDRESS *)
      ("self_address", "Unit", "Unit", "Unit");
      (* Test UNPAIR *)
      ("unpair", "Unit", "Unit", "Unit");
      (* Test VOTING_POWER *)
      ( "voting_power",
        "(Pair 0 0)",
        sf {|"%s"|} public_key,
        "(Pair 4000000000000 20000000000000)" );
      (* Test KECCAK *)
      ( "keccak",
        "None",
        "0x" ^ hex "Hello, world!",
        "(Some 0xb6e16d27ac5ab427a7f68900ac5559ce2"
        ^ "72dc6c37c82b3e052246c82244c50e4)" );
      (* Test SHA3 *)
      ( "sha3",
        "None",
        "0x" ^ hex "Hello, world!",
        "(Some 0xf345a219da005ebe9c1a1eaad97bbf38"
        ^ "a10c8473e41d0af7fb617caa0c6aa722)" );
      (* Test COMBs *)
      ("comb", "(Pair 0 0 0)", "Unit", "(Pair 1 2 3)");
      ("uncomb", "0", "(Pair 1 4 2)", "142");
      ("comb-get", "Unit", "(Pair 1 4 2 Unit)", "Unit");
      ("comb-set", "(Pair 1 4 2 Unit)", "Unit", "(Pair 2 12 8 Unit)");
      ( "comb-set-2",
        "None",
        "(Pair 1 4 2 Unit)",
        {|(Some (Pair 2 4 "toto" 0x01))|} );
      (* Test DUP n *)
      ("dup-n", "Unit", "Unit", "Unit");
      (* Test Sapling *)
      ("sapling_empty_state", "{}", "Unit", "0");
      (* Test building Fr element from nat. *)
      (* The initial storage is dropped then any value is valid. *)
      (* Random values can be generated using the following OCaml program. *)
      (* let r = Bls12_381.Fr.(random ()) in *)
      (* let x = Bls12_381.Fr.random () in *)
      (* Printf.printf "Param = (Pair %s 0x%s). Result = 0x%s" *)
      (*  (Bls12_381.Fr.to_string r) *)
      (*  (Hex.(show (of_bytes (Bls12_381.Fr.to_bytes x)))) *)
      (*  (Hex.(show (of_bytes (Bls12_381.Fr.(to_bytes (mul r x)))))) *)
      ( "bls12_381_fr_z_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "0",
        "0x00000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "1",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      (* The natural is 1 in Fr. *)
      ( "bls12_381_fr_z_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "52435875175126190479447740508185965837690552500527637822603658699938581184514",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "2",
        "0x02000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_nat",
        "0x5b0ecd0fa853810e356f1eb79721e80b30510fcc3a455f4fc02fdd9a90c"
        ^ "5401f",
        "3364491663033484423912034843462646864953418677080980279259699"
        ^ "6408934105684394",
        "0x2ef123703093cbbbd124e15f2054fa5781ed0b8d092ec3c6e5d76b4ca91"
        ^ "8a221" );
      ( "bls12_381_fr_z_nat",
        "0x4147a5ad0a633e4880d2296f08ec5c12d03e3fa4a6b49ecbd16a30a3cfc"
        ^ "dbe3f",
        "2262028481792278490256467246991799299632821112798447289749169"
        ^ "8543785655336309",
        "0x4e387e0ebfb3d1633153c195036e0c0b672955c4a0e420f93ec20a76fe6"
        ^ "77c62" );
      ( "bls12_381_fr_z_nat",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "1718009307279455880617703583439793220591757728848373965251048"
        ^ "2486858834123369",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      (* Same than previous one, but we added the order to the natural to *)
      (* verify the modulo is computed correctly and the multiplication *)
      (* computation does not fail. *)
      ( "bls12_381_fr_z_nat",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "69615968247920749285624776342583898043608129789011377475114141"
        ^ "186797415307882",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      (* Test with (positive and negative) integers. *)
      ( "bls12_381_fr_z_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "0",
        "0x00000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "1",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "52435875175126190479447740508185965837690552500527637822603658699938581184514",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "2",
        "0x02000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_fr_z_int",
        "0x5b0ecd0fa853810e356f1eb79721e80b30510fcc3a455f4fc02fdd9a90c"
        ^ "5401f",
        "3364491663033484423912034843462646864953418677080980279259699"
        ^ "6408934105684394",
        "0x2ef123703093cbbbd124e15f2054fa5781ed0b8d092ec3c6e5d76b4ca91"
        ^ "8a221" );
      ( "bls12_381_fr_z_int",
        "0x4147a5ad0a633e4880d2296f08ec5c12d03e3fa4a6b49ecbd16a30a3cfc"
        ^ "dbe3f",
        "2262028481792278490256467246991799299632821112798447289749169"
        ^ "8543785655336309",
        "0x4e387e0ebfb3d1633153c195036e0c0b672955c4a0e420f93ec20a76fe6"
        ^ "77c62" );
      ( "bls12_381_fr_z_int",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "1718009307279455880617703583439793220591757728848373965251048"
        ^ "2486858834123369",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      (* Same than previous one, but we added the order to the natural to *)
      (* verify the modulo is computed correctly and the multiplication *)
      (* computation does not fail. *)
      ( "bls12_381_fr_z_int",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "69615968247920749285624776342583898043608129789011377475114141"
        ^ "186797415307882",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      ( "bls12_381_fr_z_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "-1",
        "0x00000000fffffffffe5bfeff02a4bd5305d8a10908d83933487d9d2953"
        ^ "a7ed73" );
      ( "bls12_381_fr_z_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "-42",
        "0xd7fffffffefffffffe5bfeff02a4bd5305d8a10908d83933487d9d2953a"
        ^ "7ed73" );
      (* Test building Fr element from nat. *)
      (* The initial storage is dropped then any value is valid. *)
      (* Random values can be generated using the following OCaml program. *)
      (* let r = Bls12_381.Fr.(random ()) in *)
      (* let x = Bls12_381.Fr.random () in *)
      (* Printf.printf "Param = (Pair %s 0x%s). Result = 0x%s" *)
      (*  (Bls12_381.Fr.to_string r) *)
      (*  (Hex.(show (of_bytes (Bls12_381.Fr.to_bytes x)))) *)
      (*  (Hex.(show (of_bytes (Bls12_381.Fr.(to_bytes (mul r x)))))) *)
      ( "bls12_381_z_fr_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "0",
        "0x00000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "1",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      (* The natural is 1 in Fr. *)
      ( "bls12_381_z_fr_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "52435875175126190479447740508185965837690552500527637822603658699938581184514",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_nat",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "2",
        "0x02000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_nat",
        "0x5b0ecd0fa853810e356f1eb79721e80b30510fcc3a455f4fc02fdd9a90c"
        ^ "5401f",
        "3364491663033484423912034843462646864953418677080980279259699"
        ^ "6408934105684394",
        "0x2ef123703093cbbbd124e15f2054fa5781ed0b8d092ec3c6e5d76b4ca91"
        ^ "8a221" );
      ( "bls12_381_z_fr_nat",
        "0x4147a5ad0a633e4880d2296f08ec5c12d03e3fa4a6b49ecbd16a30a3cfc"
        ^ "dbe3f",
        "2262028481792278490256467246991799299632821112798447289749169"
        ^ "8543785655336309",
        "0x4e387e0ebfb3d1633153c195036e0c0b672955c4a0e420f93ec20a76fe6"
        ^ "77c62" );
      ( "bls12_381_z_fr_nat",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "1718009307279455880617703583439793220591757728848373965251048"
        ^ "2486858834123369",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      (* Same than previous one, but we added the order to the natural to *)
      (* verify the modulo is computed correctly and the multiplication *)
      (* computation does not fail. *)
      ( "bls12_381_z_fr_nat",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "69615968247920749285624776342583898043608129789011377475114141"
        ^ "186797415307882",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      (* Test with (positive and negative) integers. *)
      ( "bls12_381_z_fr_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "0",
        "0x00000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "1",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "52435875175126190479447740508185965837690552500527637822603658699938581184514",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "2",
        "0x02000000000000000000000000000000000000000000000000000000000"
        ^ "00000" );
      ( "bls12_381_z_fr_int",
        "0x5b0ecd0fa853810e356f1eb79721e80b30510fcc3a455f4fc02fdd9a90c"
        ^ "5401f",
        "3364491663033484423912034843462646864953418677080980279259699"
        ^ "6408934105684394",
        "0x2ef123703093cbbbd124e15f2054fa5781ed0b8d092ec3c6e5d76b4ca91"
        ^ "8a221" );
      ( "bls12_381_z_fr_int",
        "0x4147a5ad0a633e4880d2296f08ec5c12d03e3fa4a6b49ecbd16a30a3cfc"
        ^ "dbe3f",
        "2262028481792278490256467246991799299632821112798447289749169"
        ^ "8543785655336309",
        "0x4e387e0ebfb3d1633153c195036e0c0b672955c4a0e420f93ec20a76fe6"
        ^ "77c62" );
      ( "bls12_381_z_fr_int",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "1718009307279455880617703583439793220591757728848373965251048"
        ^ "2486858834123369",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      ( "bls12_381_z_fr_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "-1",
        "0x00000000fffffffffe5bfeff02a4bd5305d8a10908d83933487d9d2953"
        ^ "a7ed73" );
      ( "bls12_381_z_fr_int",
        "0x01000000000000000000000000000000000000000000000000000000000"
        ^ "00000",
        "-42",
        "0xd7fffffffefffffffe5bfeff02a4bd5305d8a10908d83933487d9d2953a"
        ^ "7ed73" );
      (* Same than previous one, but we added the order to the natural to *)
      (* verify the modulo is computed correctly and the multiplication *)
      (* computation does not fail. *)
      ( "bls12_381_z_fr_int",
        "0x8578be1766f92cd82c5e5135c374a03a8562e263ea953a3f9711b0153b7"
        ^ "fcf2d",
        "69615968247920749285624776342583898043608129789011377475114141"
        ^ "186797415307882",
        "0xfaa60dacea8e26112e524d379720fe4f95fbc5a26f1b1a67e229e26ddec"
        ^ "bf221" );
      (* Test Fr bytes can be pushed without being padded *)
      ( "add_bls12_381_fr",
        "None",
        "Pair 0x00 0x00",
        "(Some 0x000000000000000000000000000000000000000000000000000000"
        ^ "0000000000)" );
      ( "add_bls12_381_fr",
        "None",
        "Pair 0x01 0x00",
        "(Some 0x010000000000000000000000000000000000000000000000000000"
        ^ "0000000000)" );
      ( "add_bls12_381_fr",
        "None",
        "Pair 0x010000 0x00",
        "(Some 0x010000000000000000000000000000000000000000000000000000"
        ^ "0000000000)" );
      ( "add_bls12_381_fr",
        "None",
        "Pair 0x010000 0x010000",
        "(Some 0x020000000000000000000000000000000000000000000000000000"
        ^ "0000000000)" );
      ( "bls12_381_fr_push_bytes_not_padded",
        "None",
        "Unit",
        "(Some 0x000000000000000000000000000000000000000000000000000000"
        ^ "0000000000)" );
      ( "bls12_381_fr_push_nat",
        "None",
        "Unit",
        "(Some 0x100000000000000000000000000000000000000000000000000000"
        ^ "0000000000)" );
      ("bls12_381_fr_to_int", "0", "0x00", "0");
      ("bls12_381_fr_to_int", "0", "0x01", "1");
      (* Generated using *)
      (* let r = Bls12_381.Fr.(random ()) in *)
      (* Printf.printf "%s = 0x%s" *)
      (*   (Bls12_381.Fr.to_string r) *)
      (*   (Hex.(show (of_bytes (Bls12_381.Fr.to_bytes r)))) *)
      ( "bls12_381_fr_to_int",
        "0",
        "0x28db8e57af88d9576acd181b89f24e50a89a6423f939026ed91349fc9"
        ^ "af16c27",
        "1783268807701357777652478449446472851821391321341286660405373"
        ^ "5695200962927400" );
      ( "bls12_381_fr_to_int",
        "0",
        "0xb9e8abf8dc324a010007addde986fe0f7c81fab16d26819d0534b7691c"
        ^ "0b0719",
        "1132026582925658583078152196614952946047676740821044523890286"
        ^ "9222031333517497" );
      (* Mutez -> Fr *)
      ( "mutez_to_bls12_381_fr",
        "0x02",
        "16",
        "0x100000000000000000000000000000000000000000000000000000000"
        ^ "0000000" );
      (* # would fail if trying to PACK mutez and UNPACK to Fr *)
      ( "mutez_to_bls12_381_fr",
        "0x00",
        "257",
        "0x010100000000000000000000000000000000000000000000000000000"
        ^ "0000000" );
      (* Fr -> Mutez *)
      ("bls12_381_fr_to_mutez", "0", "0x10", "16");
    ]

let test_bitwise =
  register_opcode_tests
    ~supports:(Protocol.From_protocol 16)
    [
      (* Bitwise operations on bytes *)
      ("and_bytes", "Unit", "Unit", "Unit");
      ("or_bytes", "Unit", "Unit", "Unit");
      ("xor_bytes", "Unit", "Unit", "Unit");
      ("not_bytes", "Unit", "Unit", "Unit");
      ("lsl_bytes", "Unit", "Unit", "Unit");
      ("lsr_bytes", "Unit", "Unit", "Unit");
      (* Bytes <=> int/nat conversions *)
      ("bytes_of_nat", "Unit", "Unit", "Unit");
      ("bytes_of_int", "Unit", "Unit", "Unit");
    ]

let iter l f = Lwt_list.iter_s f l

let run_script_and_check ?(trace_stack = true) ?balance ?now ?level ~storage
    ~input ?expected_big_map_diff client script_name protocol ~expected_storage
    =
  let* {storage; big_map_diff} =
    Client.run_script_at
      ~hooks
      ~trace_stack
      ?balance
      ?now
      ?level
      ~storage
      ~input
      client
      script_name
      protocol
  in
  Check.(
    (storage = expected_storage)
      string
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  Option.iter
    (fun expected_big_map_diff ->
      Check.(
        (big_map_diff = expected_big_map_diff)
          (list string)
          ~__LOC__
          ~error_msg:"Expected %R, got %L"))
    expected_big_map_diff ;
  unit

(* Test that the [--balance] option of [run script] and the Michelson
   [BALANCE] instruction agree. *)
let test_balance protocol client =
  iter [0; 1; 500_000; 1_000_000; 5_000_000; 1000_000_000; 8_000_000_000_000]
  @@ fun balance ->
  run_script_and_check
    ~balance:(Tez.of_mutez_int balance)
    ~storage:"0"
    ~input:"Unit"
    client
    ["opcodes"; "balance"]
    protocol
    ~expected_storage:(string_of_int balance)

let quote s = sf "%S" s

(* Test that the --now flag of 'octez-client run script' affects the value
   returned by the NOW instruction. See also contract_onchain_opcodes.ml
   for a complementary test of the NOW instruction. *)
let test_now protocol client =
  let now = "2021-10-13T10:16:52Z" in
  run_script_and_check
    ~storage:{|"2017-07-13T09:19:01Z"|}
    ~now
    ~input:"Unit"
    client
    ["opcodes"; "store_now"]
    protocol
    ~expected_storage:(quote now)

(* Test that the --level flag of 'octez-client run script' affects the value
   returned by the LEVEL instruction. See also contract_onchain_opcodes.ml
   for a complementary test of the LEVEL instuction. *)
let test_level protocol client =
  let level = 10 in
  run_script_and_check
    ~storage:"9999999"
    ~level
    ~input:"Unit"
    client
    ["opcodes"; "level"]
    protocol
    ~expected_storage:(string_of_int level)

(* Test big map io: adding, removing, and updating values *)
let test_big_map_contract_io protocol client =
  Lwt_list.iter_s
    (fun (script_name, storage, input, expected_storage, expected_big_map_diff) ->
      run_script_and_check
        ~storage
        ~input
        client
        script_name
        protocol
        ~expected_big_map_diff
        ~expected_storage)
    [
      ( ["opcodes"; "get_big_map_value"],
        {|(Pair { Elt "hello" "hi" } None)|},
        {|"hello"|},
        {|(Pair 4 (Some "hi"))|},
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["hello"] to "hi"|};
        ] );
      ( ["opcodes"; "get_big_map_value"],
        {|(Pair { Elt "hello" "hi" } None)|},
        {|""|},
        "(Pair 4 None)",
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["hello"] to "hi"|};
        ] );
      ( ["opcodes"; "get_big_map_value"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } None)|},
        {|"1"|},
        {|(Pair 4 (Some "one"))|},
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
          {|Set map(4)["1"] to "one"|};
        ] );
      (* Test updating big maps *)
      ( ["opcodes"; "update_big_map"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)|},
        "{}",
        "(Pair 4 Unit)",
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
          {|Set map(4)["1"] to "one"|};
        ] );
      ( ["opcodes"; "update_big_map"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)|},
        {|{ Elt "1" (Some "two") }|},
        "(Pair 4 Unit)",
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
          {|Set map(4)["1"] to "two"|};
        ] );
      ( ["opcodes"; "update_big_map"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)|},
        {|{ Elt "3" (Some "three") }|},
        "(Pair 4 Unit)",
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
          {|Set map(4)["3"] to "three"|};
          {|Set map(4)["1"] to "one"|};
        ] );
      ( ["opcodes"; "update_big_map"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)|},
        {|{ Elt "3" None }|},
        "(Pair 4 Unit)",
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
          {|Unset map(4)["3"]|};
          {|Set map(4)["1"] to "one"|};
        ] );
      ( ["opcodes"; "update_big_map"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)|},
        {|{ Elt "2" None }|},
        "(Pair 4 Unit)",
        [
          "New map(4) of type (big_map string string)";
          {|Unset map(4)["2"]|};
          {|Set map(4)["1"] to "one"|};
        ] );
      ( ["opcodes"; "update_big_map"],
        {|(Pair { Elt "1" "one" ; Elt "2" "two" } Unit)|},
        {|{ Elt "1" (Some "two") }|},
        "(Pair 4 Unit)",
        [
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
          {|Set map(4)["1"] to "two"|};
        ] );
      (* test the GET_AND_UPDATE instruction on big maps *)
      (* Get and update the value stored at the given key in the map *)
      ( ["opcodes"; "get_and_update_big_map"],
        "(Pair None {})",
        {|"hello"|},
        "(Pair None 4)",
        ["New map(4) of type (big_map string nat)"; {|Unset map(4)["hello"]|}]
      );
      ( ["opcodes"; "get_and_update_big_map"],
        "(Pair (Some 4) {})",
        {|"hello"|},
        "(Pair None 4)",
        [
          "New map(4) of type (big_map string nat)"; {|Set map(4)["hello"] to 4|};
        ] );
      ( ["opcodes"; "get_and_update_big_map"],
        {|(Pair None { Elt "hello" 4 })|},
        {|"hello"|},
        "(Pair (Some 4) 4)",
        ["New map(4) of type (big_map string nat)"; {|Unset map(4)["hello"]|}]
      );
      ( ["opcodes"; "get_and_update_big_map"],
        {|(Pair (Some 5) { Elt "hello" 4 })|},
        {|"hello"|},
        "(Pair (Some 4) 4)",
        [
          "New map(4) of type (big_map string nat)"; {|Set map(4)["hello"] to 5|};
        ] );
      ( ["opcodes"; "get_and_update_big_map"],
        {|(Pair (Some 5) { Elt "hello" 4 })|},
        {|"hi"|},
        "(Pair None 4)",
        [
          "New map(4) of type (big_map string nat)";
          {|Set map(4)["hello"] to 4|};
          {|Set map(4)["hi"] to 5|};
        ] );
      ( ["opcodes"; "get_and_update_big_map"],
        {|(Pair None { Elt "1" 1 ; Elt "2" 2 })|},
        {|"1"|},
        "(Pair (Some 1) 4)",
        [
          "New map(4) of type (big_map string nat)";
          {|Set map(4)["2"] to 2|};
          {|Unset map(4)["1"]|};
        ] );
      ( ["opcodes"; "get_and_update_big_map"],
        {|(Pair None { Elt "1" 1 ; Elt "2" 2 })|},
        {|"1"|},
        "(Pair (Some 1) 4)",
        [
          "New map(4) of type (big_map string nat)";
          {|Set map(4)["2"] to 2|};
          {|Unset map(4)["1"]|};
        ] );
      (* Test big_map_magic *)
      ( ["mini_scenarios"; "big_map_magic"],
        {|(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))|},
        "(Left Unit)",
        "(Left (Pair 4 5))",
        [
          "New map(5) of type (big_map string string)";
          {|Set map(5)["1"] to "one"|};
          "New map(4) of type (big_map string string)";
          {|Set map(4)["2"] to "two"|};
        ] );
      (* test reset with new map *)
      ( ["mini_scenarios"; "big_map_magic"],
        {|(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))|},
        {|(Right (Left (Left (Pair { Elt "3" "three" } { Elt "4" "four" }))))|},
        "(Left (Pair 4 5))",
        [
          "New map(5) of type (big_map string string)";
          {|Set map(5)["4"] to "four"|};
          "New map(4) of type (big_map string string)";
          {|Set map(4)["3"] to "three"|};
        ] );
      (* test reset to unit *)
      ( ["mini_scenarios"; "big_map_magic"],
        {|(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))|},
        "(Right (Left (Right Unit)))",
        "(Right Unit)",
        [] );
      (* test import to big_map *)
      ( ["mini_scenarios"; "big_map_magic"],
        "(Right Unit)",
        {|(Right (Right (Left (Pair { Pair "foo" "bar" } { Pair "gaz" "baz" }) )))|},
        "(Left (Pair 4 5))",
        [
          "New map(5) of type (big_map string string)";
          {|Set map(5)["gaz"] to "baz"|};
          "New map(4) of type (big_map string string)";
          {|Set map(4)["foo"] to "bar"|};
        ] );
      (* test add to big_map *)
      ( ["mini_scenarios"; "big_map_magic"],
        {|(Left (Pair { Elt "1" "one" } { Elt "2" "two" }) )|},
        {|(Right (Right (Right (Left { Pair "3" "three" }))))|},
        "(Left (Pair 4 5))",
        [
          "New map(5) of type (big_map string string)";
          {|Set map(5)["2"] to "two"|};
          "New map(4) of type (big_map string string)";
          {|Set map(4)["3"] to "three"|};
          {|Set map(4)["1"] to "one"|};
        ] );
      (* test remove from big_map *)
      ( ["mini_scenarios"; "big_map_magic"],
        {|(Left (Pair { Elt "1" "one" } { Elt "2" "two" }))|},
        {|(Right (Right (Right (Right { "1" }))))|},
        "(Left (Pair 4 5))",
        [
          "New map(5) of type (big_map string string)";
          {|Set map(5)["2"] to "two"|};
          "New map(4) of type (big_map string string)";
          {|Unset map(4)["1"]|};
        ] );
    ]

(* Tests the [PACK]/[UNPACK] instructions.

   The [packunpack] script, when called with the parameter [Pair A B]
   will pack [A], and fail unless it is equal [B]. Then, it unpacks
   [B] and fails unless it is equal to [A]. *)
let test_pack_unpack protocol client =
  let build_input to_pack expected_serialization =
    sf "(Pair %s %s)" to_pack expected_serialization
  in
  let* (_ : Client.run_script_result) =
    Client.run_script_at
      ~hooks
      ~trace_stack:true
      ~storage:"Unit"
      ~input:
        (build_input
           {|(Pair (Pair "toto" {3;7;9;1}) {1;2;3})|}
           "0x05070707070100000004746f746f020000000800030007000900010200000006000100020003")
      client
      ["opcodes"; "packunpack"]
      protocol
  in
  let* () =
    Client.spawn_run_script_at
      ~hooks
      ~trace_stack:true
      ~storage:"Unit"
      ~input:
        (build_input
           {|(Pair (Pair "toto" {3;7;9;1}) {1;2;3})|}
           "0x05070707070100000004746f746f0200000008000300070009000102000000060001000200030004")
      client
      ["opcodes"; "packunpack"]
      protocol
    |> Process.check_error ~msg:(rex "script reached FAILWITH instruction")
  in
  unit

(* Test the Michelson [CHECK_SIGNATURE] instruction.

   The [check_signature] scripts, when called with the parameter [KEY]
   and the storage [Pair SIG MSG], fails unless [SIG] is a signature
   of [MSG] serialized produced by [KEY]. *)
let test_check_signature protocol client =
  let build_storage signature message =
    sf {|(Pair "%s" "%s")|} signature message
  in
  let signature =
    "edsigu3QszDjUpeqYqbvhyRxMpVFamEnvm9FYnt7YiiNt9nmjYfh8ZTbsybZ5WnBkhA7zfHsRVyuTnRsGLR6fNHt1Up1FxgyRtF"
  in
  let public_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" in
  let* (_ : Client.run_script_result) =
    Client.run_script_at
      ~hooks
      ~trace_stack:true
      ~storage:(build_storage signature "hello")
      ~input:(quote public_key)
      client
      ["opcodes"; "check_signature"]
      protocol
  in
  let* () =
    Client.spawn_run_script_at
      ~hooks
      ~trace_stack:true
      ~storage:(build_storage signature "abcd")
      ~input:(quote public_key)
      client
      ["opcodes"; "check_signature"]
      protocol
    |> Process.check_error ~msg:(rex "script reached FAILWITH instruction")
  in
  unit

(* Test the consistency of Michelson's [BLAKE2B] instruction
   with the output of the clients [hash data] command. *)
let test_hash_consistency protocol client =
  let data = {|(Pair 22220000000 (Pair "2017-12-13T04:49:00Z" 034))|} in
  let* {raw_script_expr_hash; _} =
    Client.hash_data
      ~hooks
      ~data
      ~typ:"(pair mutez (pair timestamp int))"
      client
  in
  let* () =
    run_script_and_check
      ~storage:"0x00"
      ~input:data
      client
      ["opcodes"; "hash_consistency_checker"]
      protocol
      ~expected_storage:raw_script_expr_hash
  in
  unit

(* Test that the Michelson instructions [LSL], [LSR] throw [unexpected
   arithmetic overflow] if the operand is larger than [256]. Test
   that the Michelson instructions [MUL] over [mutez] throw
   [unexpected arithmetic overflow] when its result does not fit the
   bounds of [mutez]. *)
let test_arithmetic_overflow protocol client =
  Lwt_list.iter_s
    (fun (script_name, storage, input) ->
      Client.spawn_run_script_at
        ~hooks
        ~trace_stack:true
        ~storage
        ~input
        client
        ["opcodes"; script_name]
        protocol
      |> Process.check_error ~msg:(rex "unexpected arithmetic overflow"))
    [
      ("shifts", "None", "(Left (Pair 1 257))");
      ("shifts", "None", "(Left (Pair 123 257))");
      ("shifts", "None", "(Right (Pair 1 257))");
      ("shifts", "None", "(Right (Pair 123 257))");
      ("mul_overflow", "Unit", "Left Unit");
      ("mul_overflow", "Unit", "Right Unit");
    ]

(* Tests that mapping over a Michelson [map] with the [MAP]
   instruction preserves side effects to the stack under the [MAP]'s
   body. *)
let test_map_map_side_effect protocol client =
  Lwt_list.iter_s
    (fun (storage, input, expected_storage) ->
      run_script_and_check
        ~storage
        ~input
        client
        ["opcodes"; "map_map_sideeffect"]
        protocol
        ~expected_storage)
    [
      ("(Pair {} 0)", "10", "(Pair {} 0)");
      ({|(Pair { Elt "foo" 1 } 1)|}, "10", {|(Pair { Elt "foo" 11 } 11)|});
      ( {|(Pair { Elt "bar" 5 ; Elt "foo" 1 } 6)|},
        "15",
        {|(Pair { Elt "bar" 20 ; Elt "foo" 16 } 36)|} );
    ]

let register ~protocols =
  test_protocol_independent protocols ;
  test_bitwise protocols ;
  List.iter
    (fun (test_opcode_name, test_function) ->
      Protocol.register_regression_test
        ~__FILE__
        ~title:("test Michelson opcodes: " ^ test_opcode_name)
        ~tags:["michelson"]
        (fun protocol ->
          let* client = Client.init_mockup ~protocol () in
          test_function protocol client)
        protocols)
    [
      ("BALANCE", test_balance);
      ("NOW", test_now);
      ("LEVEL", test_level);
      ("big_map_contract_io", test_big_map_contract_io);
      ("pack_unpack", test_pack_unpack);
      ("check_signature", test_check_signature);
      ("hash_consistency", test_hash_consistency);
      ("arithmetic_overflow", test_arithmetic_overflow);
      ("map_map_side_effect", test_map_map_side_effect);
    ]
