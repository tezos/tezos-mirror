open Rresult
open Ledgerwallet_ssh_agent

let test_open_close () =
  let h = Hidapi.open_id_exn ~vendor_id:0x2C97 ~product_id:0x1005 in
  Hidapi.close h

let test_ping () =
  let h = Hidapi.open_id_exn ~vendor_id:0x2C97 ~product_id:0x1005 in
  match Ledgerwallet.Transport.ping h with
  | Result.Ok () -> Hidapi.close h
  | Result.Error e ->
     let () = Hidapi.close h in
     failwith
       (Format.asprintf "Ledger error: %a" Ledgerwallet.Transport.pp_error e)

let hard x =
  Int32.logor x 0x8000_0000l

let path = [
  hard 44l ; hard 535348l
]

let test_get_public_key () =
  let h = Hidapi.open_id_exn ~vendor_id:0x2C97 ~product_id:0x1005 in
  let out =
    get_public_key h ~curve:Prime256v1 ~path >>= fun pk_prime ->
    get_public_key h ~curve:Curve25519 ~path >>| fun pk_curve ->
    Format.printf "Uncompressed prime256v1 public key %a@." Hex.pp (Hex.of_cstruct pk_prime) ;
    Format.printf "Uncompressed curve25519 public key %a@." Hex.pp (Hex.of_cstruct pk_curve) in
  Hidapi.close h;
  match out with
  | Result.Ok () -> ()
  | Result.Error e ->
     failwith
       (Format.asprintf "Ledger error: %a" Ledgerwallet.Transport.pp_error e)

let basic = [
  "open_close", `Quick, test_open_close ;
  "ping", `Quick, test_ping ;
  "get_public_key", `Quick, test_get_public_key ;
]

let () =
  Alcotest.run "ledgerwallet.ssh-agent" [
    "basic", basic ;
  ]
