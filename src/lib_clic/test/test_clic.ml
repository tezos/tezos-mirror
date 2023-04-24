(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Tezos_clic library
    Invocation:   dune exec src/lib_clic/test/main.exe \
                  -- --file test_clic.ml
    Subject:      Test the functionality of the Tezos_clic library, such as CLI
                  command dispatch, parameters and auto-completion.
*)

open Tezos_error_monad.Error_monad

(* definitions *)

let keywords words =
  let open Lwt_result_syntax in
  let words = List.map (fun (w, v) -> (String.lowercase_ascii w, v)) words in
  let matcher _ w =
    let w = String.lowercase_ascii w in
    match List.assoc_opt w words with
    | None -> failwith "wrong argument %s" w
    | Some v -> return v
  in
  let autocomplete _ = return (fst (List.split words)) in
  Tezos_clic.parameter ~autocomplete matcher

type abcd = A | B | C | D | The

type efgh = E | F | G | H | End

let string_of_abcd = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | The -> "the"

let string_of_efgh = function
  | E -> "E"
  | F -> "F"
  | G -> "G"
  | H -> "H"
  | End -> "end"

let abcd_parameter : (abcd, unit) Tezos_clic.parameter =
  keywords [("A", A); ("B", B); ("C", C); ("D", D); ("the", The)]

let efgh_parameter : (efgh, unit) Tezos_clic.parameter =
  keywords [("E", E); ("F", F); ("G", G); ("H", H); ("end", End)]

let abcd_param ~name =
  Tezos_clic.param ~name ~desc:"must be A,B,C,D, or \"the\"" abcd_parameter

let efgh_param ~name =
  Tezos_clic.param ~name ~desc:"must be E,F,G,H, or \"end\"" efgh_parameter

(* instrumentation *)

let dispatch cmds argv () =
  let open Lwt_result_syntax in
  let res = ref "nomatch" in
  let cmd_return v =
    res := v ;
    return ()
  in
  let cmds = List.map (fun cmd -> cmd cmd_return) cmds in
  let* () = Tezos_clic.dispatch cmds () argv in
  return !res

let expect_result line pr exp got =
  let open Lwt_syntax in
  let* got = protect got in
  if
    match (got, exp) with
    | Ok got, Ok exp -> got = exp
    | Error got, Error exp ->
        let got = Format.asprintf "%a" pp_print_trace got in
        Stringext.find_from got ~pattern:exp <> None
    | _ -> false
  then Lwt.return_unit
  else
    let pr_got ppf = function
      | Ok v -> pr ppf v
      | Error e -> pp_print_trace ppf e
    in
    let pr_exp ppf = function
      | Ok v -> pr ppf v
      | Error e -> Format.pp_print_string ppf e
    in
    Lwt.fail_with
      (Format.asprintf
         "at line %d, expected %a, got %a"
         line
         pr_exp
         exp
         pr_got
         got)

(** Test the dispatch of basic commands with no parameters and with [prefix]
    parameters. *)
let test_dispatch_basic () =
  let open Lwt_syntax in
  let empty return =
    Tezos_clic.command
      ~desc:"empty"
      Tezos_clic.no_options
      Tezos_clic.stop
      (fun () () -> return "empty")
  in
  let prefixes words return =
    let name = String.concat "-" words in
    Tezos_clic.command
      ~desc:name
      Tezos_clic.no_options
      (Tezos_clic.prefixes words @@ Tezos_clic.stop)
      (fun () () -> return name)
  in
  let expect line = expect_result line Format.pp_print_string in
  let* () = expect __LINE__ (Ok "empty") (dispatch [empty] []) in
  let* () =
    expect __LINE__ (Error "Extra_arguments") (dispatch [empty] ["one"])
  in
  let cmds = [empty; prefixes ["one"]; prefixes ["one"; "two"]] in
  let* () = expect __LINE__ (Ok "empty") (dispatch cmds []) in
  let* () = expect __LINE__ (Ok "one") (dispatch cmds ["one"]) in
  let* () = expect __LINE__ (Ok "one-two") (dispatch cmds ["one"; "two"]) in
  let* () =
    expect __LINE__ (Error "Command_not_found") (dispatch cmds ["saucisse"])
  in
  let* () =
    expect
      __LINE__
      (Error "Command_not_found")
      (dispatch cmds ["one"; "saucisse"])
  in
  let* () =
    expect
      __LINE__
      (Error "Extra_arguments")
      (dispatch cmds ["one"; "two"; "three"])
  in
  Lwt.return_unit

(** Test the dispatch of commands with non-terminal sequence parameters. *)
let test_dispatch_advanced () =
  let en return =
    Tezos_clic.command
      ~desc:"seq-abcd-en"
      Tezos_clic.no_options
      (Tezos_clic.non_terminal_seq
         ~suffix:["the"; "end"]
         (abcd_param ~name:"item")
      @@ Tezos_clic.stop)
      (fun () l () ->
        return ("E" ^ String.concat "" (List.map string_of_abcd l)))
  in
  let enp return =
    Tezos_clic.command
      ~desc:"seq-abcd-en"
      Tezos_clic.no_options
      (Tezos_clic.non_terminal_seq
         ~suffix:["the"; "end"]
         (abcd_param ~name:"item")
      @@ Tezos_clic.prefix "of" @@ efgh_param ~name:"last" @@ Tezos_clic.stop)
      (fun () l p () ->
        return
          ("E" ^ String.concat "" (List.map string_of_abcd l) ^ string_of_efgh p))
  in
  let fr return =
    Tezos_clic.command
      ~desc:"seq-abcd-fr"
      Tezos_clic.no_options
      (Tezos_clic.non_terminal_seq
         ~suffix:["la"; "fin"]
         (abcd_param ~name:"item")
      @@ Tezos_clic.stop)
      (fun () l () ->
        return ("F" ^ String.concat "" (List.map string_of_abcd l)))
  in
  let prefixed_en return =
    Tezos_clic.command
      ~desc:"en-seq-abcd"
      Tezos_clic.no_options
      (Tezos_clic.prefix "en"
      @@ Tezos_clic.non_terminal_seq
           ~suffix:["the"; "end"]
           (abcd_param ~name:"item")
      @@ Tezos_clic.stop)
      (fun () l () ->
        return ("E" ^ String.concat "" (List.map string_of_abcd l)))
  in
  let prefixed_fr return =
    Tezos_clic.command
      ~desc:"fr-seq-abcd"
      Tezos_clic.no_options
      (Tezos_clic.prefix "fr"
      @@ Tezos_clic.non_terminal_seq
           ~suffix:["la"; "fin"]
           (abcd_param ~name:"item")
      @@ Tezos_clic.stop)
      (fun () l () ->
        return ("F" ^ String.concat "" (List.map string_of_abcd l)))
  in
  let expect line = expect_result line Format.pp_print_string in
  let open Lwt_syntax in
  let* () =
    expect __LINE__ (Error "Unterminated_command") (dispatch [en] ["saucisse"])
  in
  let* () = expect __LINE__ (Ok "E") (dispatch [en] ["the"; "end"]) in
  let* () = expect __LINE__ (Ok "EB") (dispatch [en] ["b"; "the"; "end"]) in
  let* () =
    expect __LINE__ (Ok "EDB") (dispatch [en] ["d"; "b"; "the"; "end"])
  in
  let* () =
    expect __LINE__ (Ok "EB") (dispatch [en; enp] ["b"; "the"; "end"])
  in
  let* () =
    expect __LINE__ (Ok "Ethe") (dispatch [en; enp] ["the"; "the"; "end"])
  in
  let* () =
    expect
      __LINE__
      (Ok "EBtheB")
      (dispatch [en; enp] ["b"; "the"; "b"; "the"; "end"])
  in
  let* () =
    expect
      __LINE__
      (Ok "EBthethethe")
      (dispatch [en; enp] ["b"; "the"; "the"; "the"; "the"; "end"])
  in
  let* () =
    expect
      __LINE__
      (Ok "EBtheBthethe")
      (dispatch [en; enp] ["b"; "the"; "b"; "the"; "the"; "the"; "end"])
  in
  let* () =
    expect
      __LINE__
      (Ok "EBE")
      (dispatch [en; enp] ["b"; "the"; "end"; "of"; "e"])
  in
  let* () =
    expect
      __LINE__
      (Ok "EBend")
      (dispatch [en; enp] ["b"; "the"; "end"; "of"; "end"])
  in
  let* () =
    expect
      __LINE__
      (Ok "Etheend")
      (dispatch [en; enp] ["the"; "the"; "end"; "of"; "end"])
  in
  let* () =
    expect __LINE__ (Ok "EBE") (dispatch [enp] ["b"; "the"; "end"; "of"; "e"])
  in
  let* () =
    expect
      __LINE__
      (Error "wrong argument")
      (dispatch [en] ["d"; "x"; "the"; "end"])
  in
  let* () = expect __LINE__ (Ok "F") (dispatch [fr] ["la"; "fin"]) in
  let* () = expect __LINE__ (Ok "FA") (dispatch [fr] ["a"; "la"; "fin"]) in
  let* () =
    expect __LINE__ (Ok "FCB") (dispatch [fr] ["c"; "b"; "la"; "fin"])
  in
  let* () =
    expect
      __LINE__
      (Ok "EA")
      (dispatch [prefixed_en; prefixed_fr] ["en"; "a"; "the"; "end"])
  in
  let* () =
    expect
      __LINE__
      (Ok "FA")
      (dispatch [prefixed_en; prefixed_fr] ["fr"; "a"; "la"; "fin"])
  in
  let* () =
    expect
      __LINE__
      (Error "Unterminated_command")
      (dispatch [prefixed_en; prefixed_fr] ["fr"; "a"; "the"; "end"])
  in
  (* the following two should  all fail with a command clash error *)
  let expected_error =
    Error
      "Command cannot have different non_terminal_seq_level at the same \
       position"
  in
  let* () =
    expect __LINE__ expected_error (dispatch [en; fr] ["b"; "the"; "end"])
  in
  let* () =
    expect __LINE__ expected_error (dispatch [fr; en] ["b"; "the"; "end"])
  in
  let* () =
    expect __LINE__ expected_error (dispatch [en; fr] ["a"; "la"; "fin"])
  in
  expect __LINE__ expected_error (dispatch [fr; en] ["a"; "la"; "fin"])

let string_param ~autocomplete next =
  Tezos_clic.(
    param
      ~name:"string"
      ~desc:"string"
      (parameter ~autocomplete (fun _ s -> Lwt.return_ok s))
      next)

let int_param ~autocomplete next =
  Tezos_clic.(
    param
      ~name:"int"
      ~desc:"int"
      (parameter ~autocomplete (fun _ s -> Lwt.return_ok @@ int_of_string s))
      next)

let test_autocompletion_case ~commands ~args ~expected () =
  let open Lwt_result_syntax in
  let script = "script" in
  let prev_arg, cur_arg =
    match List.rev args with
    | [] -> (script, "")
    | [cur_arg] -> (script, cur_arg)
    | cur_arg :: prev_arg :: _ -> (prev_arg, cur_arg)
  in
  let global_options = Tezos_clic.no_options in
  let ctxt = () in
  let* next =
    Tezos_clic.autocompletion
      ~script
      ~cur_arg
      ~prev_arg
      ~args
      ~global_options
      commands
      ctxt
  in
  return
  @@ Alcotest.(
       check
         (list string)
         "auto-complete suggestions match expected values"
         expected
         next)

(** Test the suggestions given by the auto-completion of commands' parameters.
    The different types of parameters are being tested: [param], [prefix], [seq]
    and [non_terminal_seq].
*)
let test_parameters_autocompletion =
  let open Lwt_result_syntax in
  let param_commands =
    Tezos_clic.
      [
        command
          ~desc:"command with a param"
          no_options
          (int_param ~autocomplete:(fun _ctxt -> return ["0"; "1"; "2"; "10"])
          @@ string_param ~autocomplete:(fun _ctxt ->
                 return ["oranges"; "pineapples"])
          @@ stop)
          (fun () _int _string () -> return_unit);
      ]
  in
  let param_cases =
    [
      ( "param: when no arg given, suggests all options",
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
        test_autocompletion_case ~commands:param_commands ~args ~expected );
    ]
  in
  let prefix_commands =
    Tezos_clic.
      [
        command
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
          (fun () () -> return_unit);
      ]
  in
  let prefix_cases =
    [
      ( "prefix: when no arg given, suggests all options",
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
      ( "prefix: when given prev arg matches first word and arg matches unique \
         next word, suggests that word",
        let args = ["show"; "r"] in
        let expected = ["result"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
      ( "prefix: when given prev arg matches a unique first word, suggests the \
         next word",
        let args = ["run"; ""] in
        let expected = ["test"] in
        test_autocompletion_case ~commands:prefix_commands ~args ~expected );
    ]
  in
  let seq_commands =
    Tezos_clic.
      [
        command
          ~desc:"command with a seq"
          no_options
          (prefix "number"
          @@ seq_of_param
               (int_param ~autocomplete:(fun _ctxt ->
                    return ["0"; "1"; "2"; "10"])))
          (fun () _ints () -> return_unit);
        command
          ~desc:"command with a seq"
          no_options
          (prefix "fruit"
          @@ seq_of_param
               (string_param ~autocomplete:(fun _ctxt ->
                    return ["oranges"; "pineapples"])))
          (fun () _strings () -> return_unit);
      ]
  in
  let seq_cases =
    [
      ( "seq: when no arg given, suggests all options",
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
        test_autocompletion_case ~commands:seq_commands ~args ~expected );
    ]
  in
  let non_terminal_seq_commands =
    Tezos_clic.
      [
        command
          ~desc:"command with a non-terminal-seq"
          no_options
          (prefix "number"
          @@ non_terminal_seq
               ~suffix:["squared"]
               (int_param ~autocomplete:(fun _ctxt ->
                    return ["0"; "1"; "2"; "10"]))
          @@ stop)
          (fun () _ints () -> return_unit);
        command
          ~desc:"command with a non-terminal-seq"
          no_options
          (prefix "fruit"
          @@ non_terminal_seq
               ~suffix:["juiced"]
               (string_param ~autocomplete:(fun _ctxt ->
                    return ["oranges"; "pineapples"]))
          @@ stop)
          (fun () _strings () -> return_unit);
        command
          ~desc:"command with a non-terminal-seq"
          no_options
          (prefixes ["two"; "lists"]
          @@ non_terminal_seq
               ~suffix:["and"; "numbers"]
               (string_param ~autocomplete:(fun _ctxt ->
                    return ["oranges"; "pineapples"]))
          @@ non_terminal_seq
               ~suffix:["squared"]
               (int_param ~autocomplete:(fun _ctxt ->
                    return ["0"; "1"; "2"; "10"]))
          @@ stop)
          (fun () _strings _ints () -> return_unit);
      ]
  in
  let non_terminal_seq_cases =
    [
      ( "non-terminal-seq: fails when given an empty suffix",
        fun () ->
          return
          @@ Alcotest.check_raises
               "Expected [Invalid_argument] exception"
               (Invalid_argument "Tezos_clic.non_terminal_seq: empty suffix")
               (fun () ->
                 let _failing_param =
                   Tezos_clic.(
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
         matched and args given, suggests all matching options in the next seq \
         and its suffix",
        let args =
          ["two"; "lists"; "oranges"; "pineapples"; "and"; "numbers"; "2"; "1"]
        in
        let expected = ["1"; "10"] in
        test_autocompletion_case
          ~commands:non_terminal_seq_commands
          ~args
          ~expected );
    ]
  in
  param_cases @ prefix_cases @ seq_cases @ non_terminal_seq_cases

let wrap (n, f) =
  let open Lwt_syntax in
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      let* r = f () in
      match r with
      | Ok () -> Lwt.return_unit
      | Error err -> Format.kasprintf Lwt.fail_with "%a" pp_print_trace err)

(* main *)

let () =
  Alcotest_lwt.run
    ~__FILE__
    "Tezos_clic"
    [
      ( "dispatch",
        [
          ("basic", `Quick, test_dispatch_basic);
          ("advanced", `Quick, test_dispatch_advanced);
        ] );
      ( "auto-completion-parameters",
        List.map wrap test_parameters_autocompletion );
    ]
  |> Lwt_main.run
