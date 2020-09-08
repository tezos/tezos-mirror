(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Tezos_error_monad.Error_monad

let string_param ~autocomplete next =
  Clic.(
    param
      ~name:"string"
      ~desc:"string"
      (parameter ~autocomplete (fun _ s -> return s))
      next)

let int_param ~autocomplete next =
  Clic.(
    param
      ~name:"int"
      ~desc:"int"
      (parameter ~autocomplete (fun _ s -> return @@ int_of_string s))
      next)

let test_autocompletion_case ~commands ~args ~expected () =
  let script = "script" in
  let (prev_arg, cur_arg) =
    match List.rev args with
    | [] ->
        (script, "")
    | [cur_arg] ->
        (script, cur_arg)
    | cur_arg :: prev_arg :: _ ->
        (prev_arg, cur_arg)
  in
  let global_options = Clic.no_options in
  let ctxt = () in
  Clic.autocompletion
    ~script
    ~cur_arg
    ~prev_arg
    ~args
    ~global_options
    commands
    ctxt
  >>=? fun next ->
  return
  @@ Alcotest.(
       check
         (list string)
         "auto-complete suggestions match expected values"
         expected
         next)

(* Test the auto-completion suggestions of commands' parameters *)
let test_parameters_autocompletion =
  let param_commands =
    Clic.
      [ command
          ~desc:"command with a param"
          no_options
          ( int_param ~autocomplete:(fun _ctxt -> return ["0"; "1"; "2"; "10"])
          @@ string_param ~autocomplete:(fun _ctxt ->
                 return ["oranges"; "pineapples"])
          @@ stop )
          (fun () _int _string () -> return_unit) ]
  in
  let param_cases =
    [ ( "param: when no arg given, suggests all options",
        let args = [] in
        let expected = ["0"; "1"; "2"; "10"] in
        test_autocompletion_case ~commands:param_commands ~args ~expected );
      ( "param: when given single char arg, suggests options starting with \
         this char",
        let args = ["1"] in
        let expected = ["1"; "10"] in
        test_autocompletion_case ~commands:param_commands ~args ~expected );
      ( "param: when given arg matches exactly one option, suggests that option",
        let args = ["10"] in
        let expected = ["10"] in
        test_autocompletion_case ~commands:param_commands ~args ~expected );
      ( "param: when given prev arg matches first word, suggests the next words",
        let args = ["0"; ""] in
        let expected = ["oranges"; "pineapples"] in
        test_autocompletion_case ~commands:param_commands ~args ~expected );
      ( "param: when given prev arg matches first word and arg matches unique \
         next word, suggests that word",
        let args = ["0"; "o"] in
        let expected = ["oranges"] in
        test_autocompletion_case ~commands:param_commands ~args ~expected ) ]
  in
  let prefix_commands =
    Clic.
      [ command
          ~desc:"command with prefixes"
          no_options
          (prefixes ["show"; "test"] @@ stop)
          (fun () () -> return_unit);
        command
          ~desc:"command with prefixes"
          no_options
          (prefixes ["show"; "result"] @@ stop)
          (fun () () -> return_unit);
        command
          ~desc:"command with prefixes"
          no_options
          (prefixes ["run"; "test"] @@ stop)
          (fun () () -> return_unit);
        command
          ~desc:"command with prefixes"
          no_options
          (prefixes ["stop"; "test"] @@ stop)
          (fun () () -> return_unit) ]
  in
  let prefix_cases =
    [ ( "prefix: when no arg given, suggests all options",
        let args = [] in
        let expected = ["show"; "run"; "stop"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
      ( "prefix: when given single char arg, suggests options starting with \
         this char",
        let args = ["s"] in
        let expected = ["show"; "stop"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
      ( "prefix: when given arg matches exactly one option, suggests that option",
        let args = ["r"] in
        let expected = ["run"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
      ( "prefix: when given prev arg matches first word, suggests the next words",
        let args = ["show"; ""] in
        let expected = ["test"; "result"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
      ( "prefix: when given prev arg matches first word and arg matches \
         unique next word, suggests that word",
        let args = ["show"; "r"] in
        let expected = ["result"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
      ( "prefix: when given prev arg matches a unique first word, suggests \
         the next word",
        let args = ["run"; ""] in
        let expected = ["test"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected ) ]
  in
  let seq_commands =
    Clic.
      [ command
          ~desc:"command with a seq"
          no_options
          ( prefix "number"
          @@ seq_of_param
               (int_param ~autocomplete:(fun _ctxt ->
                    return ["0"; "1"; "2"; "10"])) )
          (fun () _ints () -> return_unit);
        command
          ~desc:"command with a seq"
          no_options
          ( prefix "fruit"
          @@ seq_of_param
               (string_param ~autocomplete:(fun _ctxt ->
                    return ["oranges"; "pineapples"])) )
          (fun () _strings () -> return_unit) ]
  in
  let seq_cases =
    [ ( "seq: when no arg given, suggests all options",
        let args = ["fruit"; ""] in
        let expected = ["oranges"; "pineapples"] in
        test_autocompletion_case ~commands:seq_commands ~args ~expected );
      ( "seq: when arg given, suggests all matching options",
        let args = ["number"; "1"] in
        let expected = ["1"; "10"] in
        test_autocompletion_case ~commands:seq_commands ~args ~expected );
      ( "seq: when prev arg is from a sequence and no arg given, suggests all \
         options in seq",
        let args = ["fruit"; "oranges"; ""] in
        let expected = ["oranges"; "pineapples"] in
        test_autocompletion_case ~commands:seq_commands ~args ~expected );
      ( "seq: when prev args are from a sequence and no arg given, suggests \
         all options in seq",
        let args = ["fruit"; "oranges"; "pineapples"; ""] in
        let expected = ["oranges"; "pineapples"] in
        test_autocompletion_case ~commands:seq_commands ~args ~expected ) ]
  in
  let non_terminal_seq_commands =
    Clic.
      [ command
          ~desc:"command with a non-terminal-seq"
          no_options
          ( prefix "number"
          @@ non_terminal_seq
               ~suffix:["squared"]
               (int_param ~autocomplete:(fun _ctxt ->
                    return ["0"; "1"; "2"; "10"]))
          @@ stop )
          (fun () _ints () -> return_unit);
        command
          ~desc:"command with a non-terminal-seq"
          no_options
          ( prefix "fruit"
          @@ non_terminal_seq
               ~suffix:["juiced"]
               (string_param ~autocomplete:(fun _ctxt ->
                    return ["oranges"; "pineapples"]))
          @@ stop )
          (fun () _strings () -> return_unit);
        command
          ~desc:"command with a non-terminal-seq"
          no_options
          ( prefixes ["two"; "lists"]
          @@ non_terminal_seq
               ~suffix:["and"; "numbers"]
               (string_param ~autocomplete:(fun _ctxt ->
                    return ["oranges"; "pineapples"]))
          @@ non_terminal_seq
               ~suffix:["squared"]
               (int_param ~autocomplete:(fun _ctxt ->
                    return ["0"; "1"; "2"; "10"]))
          @@ stop )
          (fun () _strings _ints () -> return_unit) ]
  in
  let non_terminal_seq_cases =
    [ ( "non-terminal-seq: fails when given an empty suffix",
        fun () ->
          return
          @@ Alcotest.check_raises
               "Expected [Invalid_argument] exception"
               (Invalid_argument "Clic.non_terminal_seq: empty suffix")
               (fun () ->
                 let _failing_param =
                   Clic.(
                     non_terminal_seq
                       ~suffix:[]
                       (int_param ~autocomplete:(fun _ctxt -> return []))
                     @@ stop)
                 in
                 ()) );
      ( "non-terminal-seq: when no arg given, suggests all options in seq and \
         its suffix",
        let args = ["fruit"; ""] in
        let expected = ["oranges"; "pineapples"; "juiced"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected );
      ( "non-terminal-seq: when arg given, suggests all matching options",
        let args = ["number"; "1"] in
        let expected = ["1"; "10"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected );
      ( "non-terminal-seq: when prev arg is from a sequence and no arg given, \
         suggests all options in seq and its suffix",
        let args = ["number"; "1"; ""] in
        let expected = ["0"; "1"; "2"; "10"; "squared"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected );
      ( "non-terminal-seq: when prev args are from a sequence and no arg \
         given, suggests all options in seq and its suffix",
        let args = ["number"; "1"; "10"; "2"; ""] in
        let expected = ["0"; "1"; "2"; "10"; "squared"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected );
      ( "non-terminal-seq: when first non-terminal-seq and its suffix are \
         matched and no arg given, suggests all options in the next seq and \
         its suffix",
        let args =
          ["two"; "lists"; "oranges"; "pineapples"; "and"; "numbers"; ""]
        in
        let expected = ["0"; "1"; "2"; "10"; "squared"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected );
      ( "non-terminal-seq: when first non-terminal-seq and its suffix are \
         matched and args given, suggests all matching options in the next \
         seq and its suffix",
        let args =
          ["two"; "lists"; "oranges"; "pineapples"; "and"; "numbers"; "2"; "1"]
        in
        let expected = ["1"; "10"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected ) ]
  in
  param_cases @ prefix_cases @ seq_cases @ non_terminal_seq_cases

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      f ()
      >>= function
      | Ok () ->
          Lwt.return_unit
      | Error err ->
          Format.kasprintf Lwt.fail_with "%a" pp_print_error err)

let () =
  Alcotest_lwt.run
    "tezos-clic"
    [ ( "auto-completion-parameters",
        List.map wrap test_parameters_autocompletion ) ]
  |> Lwt_main.run
