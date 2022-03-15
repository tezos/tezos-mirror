(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Client
    Invocation:   dune build @src/lib_proxy_server_config/runtest
    Description:  Test various functions and types regarding the configuration
                  of [tezos-proxy-server]
*)

open Lib_test
open Tezos_proxy_server_config

(** Lift an arbitrary of ['a] to ['a option] by always creating [Some] values *)
let to_some gen = QCheck.Gen.(map Option.some gen)

(** A generator that generates valid values for the [rpc_addr] field *)
let rpc_addr_gen =
  let open QCheck.Gen in
  let octet = 0 -- 255 in
  (fun i j k l port ->
    let host =
      Format.sprintf
        "%s"
        (String.concat "." @@ List.map Int.to_string [i; j; k; l])
    in
    let str = Format.sprintf "http://%s:%d" host port in
    Uri.of_string str)
  <$> octet <*> octet <*> octet <*> octet <*> 1 -- 32768

let path_gen =
  let open QCheck.Gen in
  (* Generate something that looks like a Unix path *)
  let+ s =
    list_size (1 -- 8) (string_size ~gen:(char_range 'a' 'z') (1 -- 8))
  in
  String.concat "/" ("" :: s)

(** A generator that generates valid values for the [rpc_tls] field *)
let rpc_tls_gen =
  QCheck.Gen.(
    let+ (cert, key) = pair path_gen path_gen in
    cert ^ "," ^ key)

(** A generator that generates valid values for the
    [sym_block_caching_time] field *)
let sym_block_caching_time_gen = QCheck.Gen.(1 -- 256)

(** A generator that generates random strings for the
    [data_dir_gen] field. This is still useful, since we are only
    checking command-line option handling here. *)
let data_dir_gen = path_gen

(** An arbitrary for [Proxy_server_config.t] that is parameterized
    by the subgenerators *)
let proxy_server_config_arb endpoint_gen rpc_addr_gen rpc_tls_gen
    sym_block_caching_time_gen data_dir_gen =
  let gen =
    QCheck.Gen.(
      let* endpoint = endpoint_gen in
      let* rpc_addr = rpc_addr_gen in
      let* rpc_tls = rpc_tls_gen in
      let* sym_block_caching_time = sym_block_caching_time_gen in
      let+ data_dir = data_dir_gen in
      Proxy_server_config.make
        ~endpoint
        ~rpc_addr
        ~rpc_tls
        ~sym_block_caching_time
        ~data_dir)
  in
  let print config = Format.asprintf "%a" Proxy_server_config.pp config in
  QCheck.make ~print gen

module UnionRightBias = struct
  let arb =
    QCheck.Gen.(
      proxy_server_config_arb
        (opt @@ QCheck.gen Qcheck_helpers.endpoint_arb)
        (opt @@ QCheck.gen Qcheck_helpers.endpoint_arb)
        (opt rpc_tls_gen)
        (opt sym_block_caching_time_gen)
        (opt data_dir_gen))

  let reflexive =
    QCheck.Test.make ~name:"[union_right_bias t t] = t" arb @@ fun expected ->
    Qcheck_helpers.qcheck_eq'
      ~pp:Proxy_server_config.pp
      ~expected
      ~actual:Proxy_server_config.(union_right_bias expected expected)
      ()

  let right_bias =
    QCheck.Test.make
      ~name:"[union_right_bias] is right biased"
      QCheck.(pair arb arb)
    @@ fun (config1, config2) ->
    let union = Proxy_server_config.union_right_bias config1 config2 in
    let right opt1 opt2 =
      match (opt1, opt2) with (_, Some _) -> opt2 | _ -> opt1
    in
    let endpoint ({endpoint = x; _} : Proxy_server_config.t) = x in
    let rpc_addr ({rpc_addr = x; _} : Proxy_server_config.t) = x in
    let rpc_tls ({rpc_tls = x; _} : Proxy_server_config.t) = x in
    let sym_block_caching_time
        ({sym_block_caching_time = x; _} : Proxy_server_config.t) =
      x
    in
    let data_dir ({data_dir = x; _} : Proxy_server_config.t) = x in
    let check_field f =
      Qcheck_helpers.qcheck_eq (right (f config1) (f config2)) (f union)
    in
    check_field endpoint && check_field rpc_addr && check_field rpc_tls
    && check_field sym_block_caching_time
    && check_field data_dir

  let associative =
    QCheck.Test.make
      ~name:
        "union_right_bias t1 (union_right_bias t2 t3) = union_right_bias \
         (union_right_bias t1 t2) t3"
      QCheck.(triple arb arb arb)
    @@ fun (cfg1, cfg2, cfg3) ->
    let open Proxy_server_config in
    let left = union_right_bias cfg1 (union_right_bias cfg2 cfg3) in
    let right = union_right_bias (union_right_bias cfg1 cfg2) cfg3 in
    Qcheck_helpers.qcheck_eq ~pp:Proxy_server_config.pp left right

  (** Helper for doing generic operations over fields of values of
      [Proxy_server_config.t] *)
  type field = Field : 'a option -> field

  let fields (t : Proxy_server_config.t) =
    [
      Field t.endpoint;
      Field t.rpc_addr;
      Field t.rpc_tls;
      Field t.sym_block_caching_time;
      Field t.data_dir;
    ]

  let common_some t1 t2 =
    let combination = Stdlib.List.combine (fields t1) (fields t2) in
    List.exists
      (fun (Field v1, Field v2) -> Option.(is_some v1 && is_some v2))
      combination

  let disjoint_commutative =
    QCheck.Test.make
      ~name:
        "t1 and t2 do not have a common Some ==> [union_right_bias t1 t2] = \
         [union_right_bias t2 t1]"
      QCheck.(pair arb arb)
    @@ fun (cfg1, cfg2) ->
    let open Proxy_server_config in
    QCheck.assume @@ not @@ common_some cfg1 cfg2 ;
    Qcheck_helpers.qcheck_eq
      (union_right_bias cfg1 cfg2)
      (union_right_bias cfg2 cfg1)
end

let test_example () =
  let fail msg_opt =
    let suffix =
      Option.fold
        ~none:", but an unknown error happened"
        ~some:(fun msg -> ", but received error: " ^ msg)
        msg_opt
    in
    QCheck.Test.fail_reportf
      "[example_config] should be parseable and validated%s."
      suffix
  in
  let json =
    Data_encoding.Json.from_string Proxy_server_config.example_config
  in
  match json with
  | Error msg -> fail (Some msg)
  | Ok json -> (
      match Proxy_server_config.destruct_config json with
      | Proxy_server_config.Valid _ -> ()
      | Proxy_server_config.Invalid msg -> fail (Some msg)
      | Proxy_server_config.CannotDeserialize -> fail None)

(** This generator is such that [Proxy_server_config.to_runtime]
    succeeds on generated values (if optional argument are left
    to their defaults), as tested in [test_good_arb_to_runtime_ok] below.
    It acts both as:

    - a test of the subgenerators (in particular [Qcheck_helpers.endpoint_arb])
    - a specification of [to_runtime]. *)
let good_proxy_server_config_arb
    ?(sym_block_caching_time_gen = QCheck.Gen.(opt sym_block_caching_time_gen))
    () =
  proxy_server_config_arb
    (* [endpoint] is required *)
    (to_some @@ QCheck.(gen Qcheck_helpers.endpoint_arb))
    (* [rpc-addr] is required *)
    (to_some rpc_addr_gen)
    QCheck.Gen.(opt rpc_tls_gen)
    sym_block_caching_time_gen
    QCheck.Gen.(opt data_dir_gen)

let test_good_arb_to_runtime_ok =
  QCheck.Test.make
    ~name:"[to_runtime] returns [Ok] on expected values"
    (good_proxy_server_config_arb ())
  @@ fun config ->
  match Proxy_server_config.to_runtime config with
  | Ok _ -> true
  | Error msg ->
      QCheck.Test.fail_reportf
        "Expected [to_runtime] to succeed, but it failed as follows: %s."
        msg

let test_union_preserves_to_runtime_ok =
  let arb = good_proxy_server_config_arb () in
  let to_runtime_is_ok config =
    Proxy_server_config.to_runtime config |> Result.is_ok
  in
  QCheck.Test.make
    ~name:"[union_right_bias] preserves [Result.is_ok to_runtime]"
    QCheck.(pair arb arb)
  @@ fun (config1, config2) ->
  (* These asserts must hold by [test_good_arb_to_runtime_ok] *)
  assert (to_runtime_is_ok config1) ;
  assert (to_runtime_is_ok config2) ;
  let union = Proxy_server_config.union_right_bias config1 config2 in
  Qcheck_helpers.qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(to_runtime_is_ok union)
    ()

let test_to_runtime_err_implies_union_to_runtime_err =
  let url_gen = QCheck.gen Qcheck_helpers.endpoint_arb in
  let arb =
    let open QCheck.Gen in
    (* For this test we wanna have both valid configs and invalid ones *)
    let rpc_addr_gen = oneof [rpc_addr_gen; url_gen] in
    let rpc_tls_gen = oneof [string; rpc_tls_gen] in
    proxy_server_config_arb
      (opt url_gen)
      (opt rpc_addr_gen)
      (opt rpc_tls_gen)
      (opt int)
      (opt data_dir_gen)
  in
  let to_runtime_is_err config =
    Proxy_server_config.to_runtime config |> Result.is_error
  in
  QCheck.Test.make
    ~name:
      "[Result.is_err to_runtime (union_right_bias conf1 conf2)] ==> \
       [Result.is_err to_runtime conf1 || Result.is_err to_runtime conf2]"
    QCheck.(pair arb arb)
  @@ fun (config1, config2) ->
  let union = Proxy_server_config.union_right_bias config1 config2 in
  QCheck.assume @@ to_runtime_is_err union ;
  Qcheck_helpers.qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(to_runtime_is_err config1 || to_runtime_is_err config2)
    ()

let test_wrong_sym_block_caching_time =
  let sym_block_caching_time_gen =
    (* generate negative times: *)
    to_some QCheck.Gen.(neg_int)
  in
  QCheck.Test.make
    ~name:"if [sym_block_caching_time] is <= 0, then [to_runtime] fails"
    (good_proxy_server_config_arb ~sym_block_caching_time_gen ())
  @@ fun config ->
  match Proxy_server_config.to_runtime config with
  | Ok _ ->
      QCheck.Test.fail_report "Expected [to_runtime] to fail, but it succeeded"
  | Error _ -> true

let () =
  Alcotest.run
    "Proxy_server_config"
    [
      ( "union_right_bias",
        Qcheck_helpers.qcheck_wrap
          UnionRightBias.
            [reflexive; right_bias; associative; disjoint_commutative] );
      ( "example",
        [
          Alcotest.test_case
            "[example_config] is successfully parsed and validated"
            `Quick
            test_example;
        ] );
      ( "to_runtime",
        Qcheck_helpers.qcheck_wrap
          [
            test_good_arb_to_runtime_ok;
            test_wrong_sym_block_caching_time;
            test_union_preserves_to_runtime_ok;
            test_to_runtime_err_implies_union_to_runtime_err;
          ] );
    ]
