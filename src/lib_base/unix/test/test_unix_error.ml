(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Base, Unix
   Invocation:   dune exec src/lib_base/unix/test/main.exe -- --file test_unix_error.ml
   Subject:      Check the serialization or deserialization of unix errors
*)

open Unix

let errors =
  [
    E2BIG;
    EACCES;
    EAGAIN;
    EBADF;
    EBUSY;
    ECHILD;
    EDEADLK;
    EDOM;
    EEXIST;
    EFAULT;
    EFBIG;
    EINTR;
    EINVAL;
    EIO;
    EISDIR;
    EMFILE;
    EMLINK;
    ENAMETOOLONG;
    ENFILE;
    ENODEV;
    ENOENT;
    ENOEXEC;
    ENOLCK;
    ENOMEM;
    ENOSPC;
    ENOSYS;
    ENOTDIR;
    ENOTEMPTY;
    ENOTTY;
    ENXIO;
    EPERM;
    EPIPE;
    ERANGE;
    EROFS;
    ESPIPE;
    ESRCH;
    EXDEV;
    EWOULDBLOCK;
    EINPROGRESS;
    EALREADY;
    ENOTSOCK;
    EDESTADDRREQ;
    EMSGSIZE;
    EPROTOTYPE;
    ENOPROTOOPT;
    EPROTONOSUPPORT;
    ESOCKTNOSUPPORT;
    EOPNOTSUPP;
    EPFNOSUPPORT;
    EAFNOSUPPORT;
    EADDRINUSE;
    EADDRNOTAVAIL;
    ENETDOWN;
    ENETUNREACH;
    ENETRESET;
    ECONNABORTED;
    ECONNRESET;
    ENOBUFS;
    EISCONN;
    ENOTCONN;
    ESHUTDOWN;
    ETOOMANYREFS;
    ETIMEDOUT;
    ECONNREFUSED;
    EHOSTDOWN;
    EHOSTUNREACH;
    ELOOP;
    EOVERFLOW;
    EUNKNOWNERR 42;
  ]

let unix_error_testable =
  Alcotest.testable
    (fun ppf error ->
      let error_name =
        Tezos_stdlib_unix.Unix_error.Internal_for_tests.get_constructor_name
          error
      in
      match error with
      | EUNKNOWNERR code -> Format.fprintf ppf "%s %d" error_name code
      | _ -> Format.fprintf ppf "%s" error_name)
    ( = )

let json_encode_and_decode error =
  let open Data_encoding.Json in
  let encoding = Tezos_stdlib_unix.Unix_error.encoding in
  destruct encoding @@ construct encoding error

let binary_encode_and_decode error =
  let open Data_encoding.Binary in
  let encoding = Tezos_stdlib_unix.Unix_error.encoding in
  of_bytes_exn encoding @@ to_bytes_exn encoding error

let test_json () =
  List.iter
    (fun error ->
      let result = json_encode_and_decode error in
      Alcotest.(check unix_error_testable) "unix error equality" error result)
    errors

let test_binary () =
  List.iter
    (fun error ->
      let result = binary_encode_and_decode error in
      Alcotest.(check unix_error_testable) "unix error equality" error result)
    errors

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Base.unix.unix_error"
    [
      ( "encoding",
        [
          test_case "json encoding roundtrip" `Quick test_json;
          test_case "binary encoding roundtrip" `Quick test_binary;
        ] );
    ]
