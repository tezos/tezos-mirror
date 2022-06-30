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

module Directory = Resto_directory.Make (Resto_json.Encoding)
module Service = Resto.MakeService (Resto_json.Encoding)

let ( let* ) = Lwt.bind

let ( let*? ) = Lwt_result.bind

let traverse = function
  | Ok p ->
      let* r = p in
      Lwt.return @@ Ok r
  | Error _ as err -> Lwt.return err

module Resolve_uri_desc = struct
  let uri_desc_res_testable =
    let meth_pp fmt m = Fmt.pf fmt "%s" @@ Resto.string_of_meth m in
    let arg_descr_pp fmt Resto.Arg.{name; descr} =
      Fmt.pf fmt "{name: %s; descr: %a}" name (Fmt.option Fmt.string) descr
    in
    let lookup_error_pp fmt = function
      | `Not_found -> Fmt.string fmt "Not found"
      | `Method_not_allowed ms ->
          Fmt.pf fmt "Method not allowed: %a" (Fmt.list meth_pp) ms
      | `Cannot_parse_path (path, arg, name) ->
          Fmt.pf
            fmt
            "Cannot parse path: %a with arg %a and name %s"
            (Fmt.list Fmt.string)
            path
            arg_descr_pp
            arg
            name
    in
    (* Just checking we are in the right case of error to implement
       tests easily. *)
    let lookup_error_eq a b =
      match (a, b) with
      | `Not_found, `Not_found
      | `Method_not_allowed _, `Method_not_allowed _
      | `Cannot_parse_path (_, _, _), `Cannot_parse_path (_, _, _) ->
          true
      | _ -> false
    in
    let lookup_error_testable =
      Alcotest.testable lookup_error_pp lookup_error_eq
    in
    Alcotest.(result string lookup_error_testable)

  let not_found_case = `Not_found

  let cannot_parse_case = `Cannot_parse_path ([], Resto.Arg.(descr int), "")

  let method_not_allowed_case = `Method_not_allowed [`GET]

  let test ?(title = "") ~exptd res =
    let* r = res in
    Lwt.return @@ Alcotest.check uri_desc_res_testable title exptd r

  let tests =
    [
      ( "succeed to resolve static paths",
        fun () ->
          test
            ~exptd:(Ok "/bar/<int>/<float>/patch")
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["bar"; "2"; "1.7"; "patch"]) );
      ( "succeed to resolve dynamic paths",
        fun () ->
          test
            ~exptd:(Ok "/tartine/<float>/chaussure/<int>/minus")
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["tartine"; "1."; "chaussure"; "7"; "minus"]) );
      ( "succeed to resolve dynamic tails",
        fun () ->
          test
            ~exptd:(Ok "/foobar/<int>/<int>/<int>/<int>/<int>")
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["foobar"; "1"; "2"; "3"; "4"; "5"]) );
      ( "fail to retrieve non existant paths",
        fun () ->
          test
            ~exptd:(Error not_found_case)
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["toto"; "1"; "tata"; "2"; "tutu"; "3"]) );
      ( "fail to retrieve incomplete paths",
        fun () ->
          test
            ~exptd:(Error not_found_case)
            (Directory.lookup_uri_desc Fixtures.Directory.dir () `POST ["foo"])
      );
      ( "fail if an argument can't be serialized",
        fun () ->
          test
            ~exptd:(Error cannot_parse_case)
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["foo"; "surely_not_an_int"; "add"]) );
      ( "fail if a service exist but the method is not good",
        fun () ->
          test
            ~exptd:(Error method_not_allowed_case)
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["foo"; "1"; "add"]) );
    ]
end

module Merge = struct
  let dir1 = Resto.Path.(root / "dir1")

  let dir2 = Resto.Path.(root / "dir2")

  let dir3 = Resto.Path.(dir1 / "dir3")

  let dir4 = Resto.Path.(dir1 / "dir4")

  let dir5 = Resto.Path.(dir3 / "dir5")

  let s1 =
    Service.get_service
      ~query:Resto.Query.empty
      ~output:Json_encoding.int
      ~error:Json_encoding.empty
      dir2

  let s2 =
    Service.get_service
      ~query:Resto.Query.empty
      ~output:Json_encoding.int
      ~error:Json_encoding.empty
      dir1

  let s3 =
    Service.get_service
      ~query:Resto.Query.empty
      ~output:Json_encoding.int
      ~error:Json_encoding.empty
      dir5

  let s4 =
    Service.get_service
      ~query:Resto.Query.empty
      ~output:Json_encoding.int
      ~error:Json_encoding.empty
      dir4

  let register v services =
    let do_register directory service =
      Directory.register0 directory service (fun () () -> Lwt.return @@ `Ok v)
    in
    List.fold_left do_register Directory.empty services

  (* root
     ├── dir1
     │   ├── dir3
     │   │   └── dir5
     │   │       └── s3 ----> 0
     │   └── s2 ------------> 0
     └── dir2
         └── s1 ------------> 0 *)
  let left_dir = register 0 [s1; s2; s3]

  (* root
     └── dir1
         ├── dir3
         │   └── dir5
         │       └── s3 ----> 1
         ├── dir4
         │   └── s4 --------> 1
         └── s2 ------------> 1 *)
  let right_dir = register 1 [s2; s3; s4]

  let check_call_result dir service expd msg =
    let* answer = Directory.transparent_lookup dir service () () () in
    let res =
      match answer with
      | `Ok i -> i
      | _ ->
          Alcotest.fail
            (Format.sprintf
               "The query for service %s must be successful by precondition."
               msg)
    in
    Lwt.return @@ Alcotest.(check int) msg expd res

  let check_raises f =
    match f () with
    | exception _ -> Lwt.return_unit
    | _ -> Alcotest.fail "An exception was expected to raise"

  let tests =
    [
      ( "succeed to pick left",
        fun () ->
          (* root
             ├── dir1
             │   ├── dir3
             │   │   └── dir5
             │   │       └── s3 ----> 0 (Already existing in left)
             │   ├── dir4
             │   │   └── s4 --------> 1 (Added from right)
             │   └── s2 ------------> 0 (Already existing in left)
             └── dir2
                 └── s1 ------------> 0 (Already existing in left) *)
          let merged =
            Directory.merge ~strategy:`Pick_left left_dir right_dir
          in
          let* () = check_call_result merged s1 0 "s1" in
          let* () = check_call_result merged s2 0 "s2" in
          let* () = check_call_result merged s3 0 "s3" in
          check_call_result merged s4 1 "s4" );
      ( "succeed to pick right",
        fun () ->
          (* root
             ├── dir1
             │   ├── dir3
             │   │   └── dir5
             │   │       └── s3 ----> 1 (Already existing in right)
             │   ├── dir4
             │   │   └── s4 --------> 1 (Already existing in right)
             │   └── s2 ------------> 1 (Already existing in right)
             └── dir2
                 └── s1 ------------> 0 (Added from left) *)
          let merged =
            Directory.merge ~strategy:`Pick_right left_dir right_dir
          in
          let* () = check_call_result merged s1 0 "s1" in
          let* () = check_call_result merged s2 1 "s2" in
          let* () = check_call_result merged s3 1 "s3" in
          check_call_result merged s4 1 "s4" );
      ( "fail with Conflict exception when requiring `Raise",
        fun () ->
          check_raises @@ fun () ->
          Directory.merge ~strategy:`Raise left_dir right_dir );
      ( "fail with Conflict exception by default",
        fun () -> check_raises @@ fun () -> Directory.merge left_dir right_dir
      );
    ]
end

module Conflicts = struct
  let test_conflict () =
    Lwt.return
    @@ Alcotest.check_raises
         "conflict is detected"
         (Fixtures.Directory.Conflict
            ([Static "foobar"; DynamicTail {name = "int"; descr = None}], CTail))
         Fixtures.Directory.add_tail_conflict

  let test_print_conflict () =
    Lwt.return
    @@
    try Fixtures.Directory.add_tail_conflict ()
    with exn ->
      let exn_str = Stdlib.Printexc.to_string exn in
      let exn_default_str = Stdlib.Printexc.to_string_default exn in
      Alcotest.(check @@ neg string)
        "exception string is not the default"
        exn_str
        exn_default_str

  let test_print_conflict_explanation () =
    Lwt.return
    @@
    try Fixtures.Directory.add_type_conflict ()
    with exn ->
      let exn_str = Stdlib.Printexc.to_string exn in
      let expected_str =
        "Conflict in registration of service: Type conflict between arguments: \
         found type int but type float was expected in /bar/<float>"
      in
      Alcotest.(check @@ string)
        "exception string is correct explanation"
        exn_str
        expected_str

  let tests =
    [
      ("conflict is detected", test_conflict);
      ("conflict is pretty-printed", test_print_conflict);
      ("conflict is correctly pretty-printed", test_print_conflict_explanation);
    ]
end

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "directory"
       [
         ("resolve_uri_desc", Util.do_test_lwt Resolve_uri_desc.tests);
         ("merge", Util.do_test_lwt Merge.tests);
         ("conflicts", Util.do_test_lwt Conflicts.tests);
       ]
