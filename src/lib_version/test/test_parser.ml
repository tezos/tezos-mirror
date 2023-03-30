(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    _______
    Component: lib_version
    Invocation: dune exec src/lib_version/test/main.exe
    Subject: Test versions parser
 *)

module Assert = struct
  let fail expected given msg =
    Format.kasprintf failwith "@[%s@ expected: %s@ got: %s@]" msg expected given

  let default_printer _ = ""

  let equal ?(eq = ( = )) ?(prn = default_printer) ?(msg = "") x y =
    if not (eq x y) then fail (prn x) (prn y) msg
end

let legal_versions =
  [
    ("10.93", {Version.major = 10; minor = 93; additional_info = Release});
    ("v10.93", {Version.major = 10; minor = 93; additional_info = Release});
    ("10.93+dev", {Version.major = 10; minor = 93; additional_info = Dev});
    ("10.93-rc1", {Version.major = 10; minor = 93; additional_info = RC 1});
    ( "10.93-rc1+dev",
      {Version.major = 10; minor = 93; additional_info = RC_dev 1} );
  ]

let parse_version s = Tezos_version_parser.version_tag (Lexing.from_string s)

let eq v1 v2 =
  let open Version in
  let additional_info_eq a1 a2 =
    match (a1, a2) with
    | Dev, Dev -> true
    | Dev, _ -> false
    | RC n1, RC n2 | RC_dev n1, RC_dev n2 -> n1 = n2
    | RC _, _ | RC_dev _, _ -> false
    | Release, Release -> true
    | Release, _ -> false
  in
  match (v1, v2) with
  | Some v1, Some v2 ->
      v1.major = v2.major && v1.minor = v2.minor
      && additional_info_eq v1.additional_info v2.additional_info
  | _, _ -> false

let prn = function
  | None ->
      Format.asprintf "%a" Tezos_version_parser.pp Tezos_version_parser.default
  | Some v -> Format.asprintf "%a" Tezos_version_parser.pp v

let test_parser _ =
  ListLabels.iter legal_versions ~f:(fun (x, e) ->
      Assert.equal
        ~msg:(Format.asprintf "testing version string: \"%s\"" x)
        ~eq
        ~prn
        (Some e)
        (parse_version x))

let () =
  Alcotest.run "version" [("parser", [("versions", `Quick, test_parser)])]
