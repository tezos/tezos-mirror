open Manifest
open Internals
open Product_octez
open Product_etherlink

include Product (struct
  let name = "tezt_cloud_tests"

  let source = ["tezt/tests/cloud"]
end)

let _main =
  private_exe
    "main"
    ~path:"tezt/tests/cloud"
    ~opam:"tezt-tests-cloud"
    ~synopsis:"Tezt tests using Tezt cloud"
    ~bisect_ppx:No
    ~deps:
      [
        bls12_381_archive;
        octez_test_helpers |> open_;
        tezt_wrapper |> open_ |> open_ ~m:"Base";
        tezt_tezos |> open_ |> open_ ~m:"Runnable.Syntax";
        tezt_cloud |> open_;
        tezt_etherlink;
        tezt_benchmark_lib;
        octez_stdlib_unix;
      ]
    ~release_status:Unreleased
    ~with_macos_security_framework:true
