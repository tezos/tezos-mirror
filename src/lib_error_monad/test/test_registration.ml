(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
    Component:    Error Monad
    Invocation:   dune build @src/lib_error_monad/runtest
    Subject:      On the registration and serialization of shallow, deep
                  and recursive errors.
*)

module MakeRecReg () = struct
  open TzCore

  (* Shallow parallel errors *)
  type error += ParSha of error list

  let () =
    register_recursive_error_kind
      `Permanent
      ~id:"test.parsha"
      ~title:"test-parallel-shallow"
      ~description:"Test Parallel Shallow"
      ~pp:(fun fmt errs ->
        Format.fprintf
          fmt
          "ParallelShallow(%d): %a"
          (List.length errs)
          (Format.pp_print_list ~pp_sep:Format.pp_print_space pp)
          errs)
      (fun err_enc ->
        let open Data_encoding in
        obj1 @@ req "errs" (list @@ dynamic_size err_enc))
      (function ParSha errs -> Some errs | _ -> None)
      (fun errs -> ParSha errs)

  let parsha = ParSha []

  let b_par_nil = Data_encoding.Binary.to_bytes_exn error_encoding parsha

  let () =
    assert (parsha = Data_encoding.Binary.of_bytes_exn error_encoding b_par_nil)

  (** Checks that the pretty-printer doesn't fail. That pretty-printer
      relies on at least one global reference and has an assert false
      in its body. *)
  let (_ : string) = Format.asprintf "%a" pp parsha

  let parsha_cube = ParSha [ParSha []]

  let b_parsha_cube =
    Data_encoding.Binary.to_bytes_exn error_encoding parsha_cube

  let () =
    assert (
      parsha_cube
      = Data_encoding.Binary.of_bytes_exn error_encoding b_parsha_cube)

  let (_ : string) = Format.asprintf "%a" pp parsha_cube

  let () =
    let open Data_encoding in
    let s = Json.schema error_encoding in
    let ss = Format.asprintf "%a" Json_schema.pp s in
    let j = Json.construct Json.schema_encoding s in
    let d = Json.destruct Json.schema_encoding j in
    let ds = Format.asprintf "%a" Json_schema.pp d in
    ignore ss ;
    ignore ds

  (* "Normal", non-parallel error *)
  type error += A

  let () =
    register_error_kind
      `Permanent
      ~id:"test.a"
      ~title:"test-a"
      ~description:"Test A"
      Data_encoding.unit
      (function A -> Some () | _ -> None)
      (fun () -> A)

  let parsha_aa = ParSha [A; A]

  let b_parsha_aa = Data_encoding.Binary.to_bytes_exn error_encoding parsha_aa

  let () =
    assert (
      parsha_aa = Data_encoding.Binary.of_bytes_exn error_encoding b_parsha_aa)

  let (_ : string) = Format.asprintf "%a" pp parsha_aa

  (* Deep recursive errors *)
  type error += ParD of TzMonad.tztrace list

  let () =
    register_recursive_error_kind
      `Permanent
      ~id:"test.pard"
      ~title:"test-parallel-deep"
      ~description:"Test ParD"
      ~pp:(fun fmt traces ->
        Format.fprintf
          fmt
          "ParD(%d): %a"
          (List.length traces)
          (Format.pp_print_list
             ~pp_sep:Format.pp_print_space
             TzMonad.pp_print_error)
          traces)
      (fun err_enc ->
        let open Data_encoding in
        obj1
        @@ req "traces" (list @@ dynamic_size (list @@ dynamic_size err_enc)))
      (function ParD traces -> Some traces | _ -> None)
      (fun traces -> ParD traces)

  let pard = ParD [[A]; [ParSha [ParD [[A]; [A]]]]; [ParD []]]

  let b_pard = Data_encoding.Binary.to_bytes_exn error_encoding pard

  let () =
    assert (pard = Data_encoding.Binary.of_bytes_exn error_encoding b_pard)

  let () =
    let open Data_encoding in
    let s = Json.schema error_encoding in
    let ss = Format.asprintf "%a" Json_schema.pp s in
    let j = Json.construct Json.schema_encoding s in
    let d = Json.destruct Json.schema_encoding j in
    let ds = Format.asprintf "%a" Json_schema.pp d in
    ignore ss ;
    ignore ds

  let main () = ()
end

(** Asserts that roundtrips [of/to_bytes_exn] preserve the error
    values [ParSha []], [ParSha [ParSha []]], [ParSha [A; A]], as
    well as deep recursion.
*)
let test_register_rec () =
  let module M = MakeRecReg () in
  M.main ()

let tests_recursive_reg =
  [Alcotest.test_case "register-rec" `Quick test_register_rec]

module MakeExtractInfos () = struct
  open TzCore

  type error += A

  let () =
    register_error_kind
      `Permanent
      ~id:"test.extractinfo"
      ~title:"test-extractinfo"
      ~description:"Test Extract Infos"
      Data_encoding.unit
      (function A -> Some () | _ -> None)
      (fun () -> A)

  let () =
    let infos = find_info_of_error A in
    assert (infos.id = "test.extractinfo") ;
    assert (infos.title = "test-extractinfo")

  let main () = ()
end

let test_extract_infos () =
  let module M = MakeExtractInfos () in
  M.main ()

let tests_extract_infos =
  [Alcotest.test_case "extract-infos" `Quick test_extract_infos]

let () =
  Alcotest.run
    "error-registration"
    [
      ("recursive-error", tests_recursive_reg);
      ("extract-info", tests_extract_infos);
    ]
