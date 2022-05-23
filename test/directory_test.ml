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

let ( let* ) = Lwt.bind

module Resolve_uri_desc = struct
  include Resto_directory.Make (Resto_json.Encoding)

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
            (lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["bar"; "2"; "1.7"; "patch"]) );
      ( "succeed to resolve dynamic paths",
        fun () ->
          test
            ~exptd:(Ok "/tartine/<float>/chaussure/<int>/minus")
            (lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["tartine"; "1."; "chaussure"; "7"; "minus"]) );
      ( "succeed to resolve dynamic tails",
        fun () ->
          test
            ~exptd:(Ok "/foobar/<int>/<int>/<int>/<int>/<int>")
            (lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["foobar"; "1"; "2"; "3"; "4"; "5"]) );
      ( "fail to retrieve non existant paths",
        fun () ->
          test
            ~exptd:(Error not_found_case)
            (lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["toto"; "1"; "tata"; "2"; "tutu"; "3"]) );
      ( "fail to retrieve incomplete paths",
        fun () ->
          test
            ~exptd:(Error not_found_case)
            (lookup_uri_desc Fixtures.Directory.dir () `POST ["foo"]) );
      ( "fail if an argument can't be serialized",
        fun () ->
          test
            ~exptd:(Error cannot_parse_case)
            (lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["foo"; "surely_not_an_int"; "add"]) );
      ( "fail if a service exist but the method is not good",
        fun () ->
          test
            ~exptd:(Error method_not_allowed_case)
            (lookup_uri_desc Fixtures.Directory.dir () `GET ["foo"; "1"; "add"])
      );
    ]
end

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "directory"
       [("resolve_uri_desc", Util.do_test_lwt Resolve_uri_desc.tests)]
