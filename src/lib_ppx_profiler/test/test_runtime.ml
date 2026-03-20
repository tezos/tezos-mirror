(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    PPX Profiler
    Invocation:   dune exec src/lib_ppx_profiler/test/main.exe \
                  -- --file test_runtime.ml
    Subject:      Runtime behavior of the PPX profiler rewriter. Verifies
                  that annotated expressions invoke the profiler with the
                  correct arguments when [TEZOS_PPX_PROFILER] is set, and
                  that no profiler calls are made when it is not set.
*)

type verbosity = Notice | Info | Debug

let string_of_verbosity = function
  | Notice -> "Notice"
  | Info -> "Info"
  | Debug -> "Debug"

let call : (string * verbosity) option ref = ref None

let reset () = call := None

let ppx_enabled = Sys.getenv_opt "TEZOS_PPX_PROFILER" <> None

let () =
  Log.log
    ~level:Warn
    ~color:Log.Color.FG.yellow
    ~prefix:"warn"
    "PPX profiler tests: TEZOS_PPX_PROFILER %s"
    (if ppx_enabled then "is set" else "is NOT set")

let assert_called name verbosity =
  if ppx_enabled then
    match !call with
    | Some (n, v) when n = name && v = verbosity -> ()
    | Some (n, v) ->
        Alcotest.failf
          "Expected (%s, %s) but got (%s, %s)"
          name
          (string_of_verbosity verbosity)
          n
          (string_of_verbosity v)
    | None ->
        Alcotest.failf
          "Expected (%s, %s) but profiler was not called"
          name
          (string_of_verbosity verbosity)
  else
    match !call with
    | None -> ()
    | Some (n, v) ->
        Alcotest.failf
          "PPX not enabled but profiler was called with (%s, %s)"
          n
          (string_of_verbosity v)

module Profiler = struct
  type nonrec verbosity = verbosity = Notice | Info | Debug

  let record_s ~cpu:_ verbosity (name, _metadata) f =
    call := Some (name, verbosity) ;
    f ()
end

module My_module = struct
  module Profiler = Profiler
end

(* Static module tests *)

let test_static_default () =
  reset () ;
  let _ = (42 [@profiler.record_s {verbosity = Notice} "default_label"]) in
  assert_called "default_label" Notice

let test_static_explicit () =
  reset () ;
  let _ =
    (42
    [@profiler.record_s
      {verbosity = Info; profiler_module = Profiler} "static_simple"])
  in
  assert_called "static_simple" Info

let test_static_qualified () =
  reset () ;
  let _ =
    (42
    [@profiler.record_s
      {verbosity = Debug; profiler_module = My_module.Profiler}
        "static_qualified"])
  in
  assert_called "static_qualified" Debug

(* First-class module support *)

module type PROFILER = sig
  type nonrec verbosity = verbosity = Notice | Info | Debug

  val record_s :
    cpu:bool option -> verbosity -> string * 'a list -> (unit -> 'b) -> 'b
end

let make_profiler call_ref : (module PROFILER) =
  (module struct
    type nonrec verbosity = verbosity = Notice | Info | Debug

    let record_s ~cpu:_ verbosity (name, _metadata) f =
      call_ref := Some (name, verbosity) ;
      f ()
  end)

let assert_fcm_called call_ref name verbosity =
  if ppx_enabled then
    match !call_ref with
    | Some (n, v) when n = name && v = verbosity -> ()
    | Some (n, v) ->
        Alcotest.failf
          "Expected (%s, %s) but got (%s, %s)"
          name
          (string_of_verbosity verbosity)
          n
          (string_of_verbosity v)
    | None ->
        Alcotest.failf
          "Expected (%s, %s) but profiler was not called"
          name
          (string_of_verbosity verbosity)
  else
    match !call_ref with
    | None -> ()
    | Some (n, v) ->
        Alcotest.failf
          "PPX not enabled but profiler was called with (%s, %s)"
          n
          (string_of_verbosity v)

let test_fcm_variable () =
  let call = ref None in
  let profiler = make_profiler call in
  (* Use ignore to prevent unused variable warning when PPX is disabled *)
  ignore profiler ;
  let _ =
    (42
    [@profiler.record_s
      {verbosity = Notice; profiler_module = profiler} "fcm_variable"])
  in
  assert_fcm_called call "fcm_variable" Notice

module Holder = struct
  let profiler call_ref : (module PROFILER) = make_profiler call_ref
end

let test_fcm_module_value () =
  let call = ref None in
  let _ =
    (42
    [@profiler.record_s
      {verbosity = Info; profiler_module = Holder.profiler call}
        "fcm_module_value"])
  in
  assert_fcm_called call "fcm_module_value" Info

type state = {profiler : (module PROFILER)}

let test_fcm_field () =
  let call = ref None in
  let state = {profiler = make_profiler call} in
  (* Use ignore to prevent unused variable warning when PPX is disabled *)
  ignore state ;
  let _ =
    (42
    [@profiler.record_s
      {verbosity = Debug; profiler_module = state.profiler} "fcm_field"])
  in
  assert_fcm_called call "fcm_field" Debug

type inner = {profiler : (module PROFILER)}

type outer = {inner : inner}

let test_fcm_nested () =
  let call = ref None in
  let outer = {inner = {profiler = make_profiler call}} in
  (* Use ignore to prevent unused variable warning when PPX is disabled *)
  ignore outer ;
  let _ =
    (42
    [@profiler.record_s
      {verbosity = Notice; profiler_module = outer.inner.profiler} "fcm_nested"])
  in
  assert_fcm_called call "fcm_nested" Notice

let () =
  Alcotest.run
    ~__FILE__
    "ppx_profiler"
    [
      ( "static module",
        [
          ("default profiler", `Quick, test_static_default);
          ("explicit Profiler", `Quick, test_static_explicit);
          ("qualified path", `Quick, test_static_qualified);
        ] );
      ( "first-class module",
        [
          ("variable", `Quick, test_fcm_variable);
          ("module value access", `Quick, test_fcm_module_value);
          ("field access", `Quick, test_fcm_field);
          ("nested field access", `Quick, test_fcm_nested);
        ] );
    ]
