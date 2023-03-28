(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <contact@metastate.ch>                    *)
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
    Component:  Context Storage
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/integration/main.exe
    Subject:    Test the correctnesss of debug message from storage_functor
 *)

open Protocol
open Storage_functors
open Storage_sigs

module Int32 = struct
  type t = int32

  let encoding = Data_encoding.int32

  module Index = struct
    type t = int

    let path_length = 1

    let to_path c l = string_of_int c :: l

    let of_path = function
      | [] | _ :: _ :: _ -> None
      | [c] -> int_of_string_opt c

    type 'a ipath = 'a * t

    let args =
      Storage_description.One
        {
          rpc_arg = Environment.RPC_arg.int;
          encoding = Data_encoding.int31;
          compare = Compare.Int.compare;
        }
  end
end

module Int64 = struct
  type t = int64

  let encoding = Data_encoding.int64

  module Index = struct
    type t = int64

    let path_length = 1

    let to_path c l = Int64.to_string c :: l

    let of_path = function
      | [] | _ :: _ :: _ -> None
      | [c] -> Int64.of_string_opt c

    type 'a ipath = 'a * t

    let args =
      Storage_description.One
        {
          rpc_arg = Environment.RPC_arg.int64;
          encoding = Data_encoding.int64;
          compare = Compare.Int64.compare;
        }
  end
end

let create_context name : (module Raw_context.T with type t = Raw_context.t) =
  (module Make_subcontext (Registered) (Raw_context)
            (struct
              let name = [name]
            end))

let create_subcontext name
    (module Context : Raw_context.T with type t = Raw_context.t) :
    (module Raw_context.T with type t = Raw_context.t) =
  (module Make_subcontext (Registered) (Context)
            (struct
              let name = [name]
            end))

let create_single_data_storage name
    (module Context : Raw_context.T with type t = Raw_context.t) :
    (module Single_data_storage with type t = Context.t and type value = Int32.t)
    =
  (module Make_single_data_storage (Registered) (Context)
            (struct
              let name = [name]
            end)
            (Int32))

let create_indexed_subcontext_int32
    (module Context : Raw_context.T with type t = Raw_context.t) :
    (module Data_set_storage with type t = Raw_context.t) =
  (module Make_data_set_storage (Context) (Int32.Index))

let create_indexed_subcontext_int64
    (module Context : Raw_context.T with type t = Raw_context.t) :
    (module Data_set_storage with type t = Raw_context.t) =
  (module Make_data_set_storage (Context) (Int64.Index))

let must_failwith f_prog error =
  try
    let _ = f_prog () in
    Alcotest.fail "Unexpected successful result"
  with exc ->
    if exc = error then Lwt.return_unit
    else Alcotest.fail "Unexpected error result"

(** Test:

    This test check that creating value where value already exists
    fails*)
let test_register_single_data () =
  let f_prog () =
    let context = create_context "context1" in
    let _single_data = create_single_data_storage "single_data" context in
    create_single_data_storage "single_data" context
  in
  let error =
    Invalid_argument
      "Could not register a value at [context1 / single_data] because of an \
       existing Value."
  in
  must_failwith f_prog error

(** Test:

    This test check that creating a subcontext where a value already exists
    fails*)
let test_register_named_subcontext () =
  let f_prog () =
    let context = create_context "context2" in
    let subcontext = create_subcontext "sub_context" context in
    let _single_data = create_single_data_storage "error_register" subcontext in
    let subcontext = create_subcontext "error_register" subcontext in
    create_single_data_storage "single_data2" subcontext
  in
  let error =
    Invalid_argument
      "Could not register a named subcontext at [context2 / sub_context / \
       error_register] because of an existing Value."
  in
  must_failwith f_prog error

(** Test:

    This test check that creating a indexed subcontext where a value already
   exists fails*)
let test_register_indexed_subcontext () =
  let f_prog () =
    let context = create_context "context3" in
    let _ = create_single_data_storage "single_value" context in
    create_indexed_subcontext_int32 context
  in
  let error =
    Invalid_argument
      "Could not register an indexed subcontext at [context3] because of an \
       existing \n\
       single_value Value."
  in
  must_failwith f_prog error

(** Test:

    This test check that creating a indexed subcontext where an indexed
    subcontext already exists fails*)
let test_register_indexed_subcontext_2 () =
  let f_prog () =
    let context = create_context "context4" in
    let _ = create_indexed_subcontext_int32 context in
    create_indexed_subcontext_int64 context
  in
  let error =
    Invalid_argument
      "An indexed subcontext at [context4] already exists but has a different \
       argument: `int64` <> `int`."
  in
  must_failwith f_prog error

let tests =
  [
    Alcotest_lwt.test_case
      "register single data in existing path"
      `Quick
      (fun _ -> test_register_single_data);
    Alcotest_lwt.test_case
      "register named subcontext in existing path"
      `Quick
      (fun _ -> test_register_named_subcontext);
    Alcotest_lwt.test_case
      "register indexed subcontext in existing path"
      `Quick
      (fun _ -> test_register_indexed_subcontext);
    Alcotest_lwt.test_case
      "register indexed subcontext with existing indexed subcontext"
      `Quick
      (fun _ -> test_register_indexed_subcontext_2);
  ]
