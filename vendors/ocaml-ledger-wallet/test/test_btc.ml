open Rresult
open Ledgerwallet_btc
open Bitcoin.Protocol
open Bitcoin.Util

module Secp256k1 = Libsecp256k1.External

let ctx = Secp256k1.Context.create ()
let prevTx = Transaction.of_hex (`Hex "0100000001c3798bf6520ac4e95e24c587b6ee25a1c492f33ddd35615f90f38d89e8e2b47c010000006b483045022100dc48cef9d3e1eb71e84bcf51ceaf7f938328573e482bf8af951e9b53e87a74c802206280177d6ac07455d9984a8dd62f8d9f87c91884820c9fa587c8ada46750a44d4121032af552f85308e3c68c9751c415a5efe01fc165a955e48835a84894ab9986b149ffffffff02005ed0b2000000001976a914c78d002920f40f471846083f4283eae42246035988acb0cecff5150000001976a9147e854f6a0d4b20f61ba91ab0aa8e1f6f428e628e88ac00000000")
let nextTx2 = Transaction.of_hex (`Hex "01000000018f514dcffb6242ff6c2b01aa888700dce343f57e265e72c9fb46607065ffad48000000001976a914b2483f8a51043dca4144748f91e45e83e473730d88acffffffff01e0567b4d000000005d14cf244fd81c2d7cee628190f75b48ca621cbf3fcc75522102e8f164c9c12b039d3e522aa291e3544ddc99240a878080669aee1c359f66d2532103c6ad7f526553affb3979483dac278b1517199a5ac3cd0fbb669644aadbf933ba52ae00000000")
let path = Bitcoin.Wallet.KeyPath.[to_hardened 44l; to_hardened 1l; to_hardened 0l; 0l; 0l]

let nextTx =
  let open Bitcoin.Protocol in
  (* Format.printf "%a@." Transaction.pp prevTx ; *)
  let prev_out_hash = Transaction.hash256 prevTx in
  let my_out = prevTx.outputs.(0) in
  let input =
    TxIn.create' ~prev_out_hash ~prev_out_i:0 ~script:my_out.script () in
  let value = Int64.sub my_out.value 100000L in
  (* Printf.printf "%Ld %Ld\n%!" my_out.value value ; *)
  let output =
    TxOut.create ~value ~script:my_out.script in
  Transaction.create ~inputs:[|input|] ~outputs:[|output|] ()

let with_connection f =
  let h = Hidapi.open_id_exn ~vendor_id:0x2C97 ~product_id:0x0001 in
  try
    match f h with
    | Result.Ok () -> Hidapi.close h
    | Result.Error e ->
       failwith
         (Format.asprintf "Ledger error: %a" Ledgerwallet.Transport.pp_error e)
  with exn ->
    Hidapi.close h ;
    raise exn

let test_open_close () = with_connection (fun _ -> R.ok ())

let test_ping () =
  with_connection Ledgerwallet.Transport.ping

let test_get_info () =
  with_connection begin fun h ->
    get_firmware_version h >>= fun firmware_version ->
    Printf.printf "Firmware: %s\n"
      (Sexplib.Sexp.to_string_hum (Firmware_version.sexp_of_t firmware_version)) ;
    get_operation_mode h >>| fun op_mode ->
    Printf.printf "Operation mode: %s\n"
      (Sexplib.Sexp.to_string_hum (Operation_mode.sexp_of_t op_mode)) ;
    end

let test_get_random () =
  with_connection (fun h -> get_random h 200 >>| ignore)

let test_get_wallet_pk () =
  with_connection begin fun h ->
    get_wallet_public_key h path >>| fun pk ->
    let pk_computed = Secp256k1.Key.read_pk_exn ctx pk.uncompressed.buffer in
    let addr_received = Base58.Bitcoin.of_string_exn c pk.b58addr in
    let addr_computed = Bitcoin.Wallet.Address.of_pubkey ctx pk_computed in
    assert (addr_received = addr_computed) ;
    Format.printf "Uncompressed public key %a@." Hex.pp (Hex.of_cstruct pk.uncompressed) ;
    Printf.printf "Address %s\n%!" pk.b58addr ;
    Format.printf "Address computed %a@." (Base58.Bitcoin.pp c) addr_computed ;
    let addr_computed_testnet =
      Base58.Bitcoin.create ~version:Testnet_P2PKH ~payload:addr_computed.payload in
    Format.printf "Address computed %a@." (Base58.Bitcoin.pp c) addr_computed_testnet ;
    Format.printf "Chaincode %a@." Hex.pp (Hex.of_cstruct pk.bip32_chaincode) ;
    end

let test_get_trusted_input () =
  with_connection begin fun h ->
    get_trusted_input h prevTx 0 >>| fun out ->
    Format.printf "Trusted input %a@." Hex.pp (Hex.of_cstruct out)
    end

let test_sign_segwit () =
  with_connection begin fun h ->
    get_wallet_public_key h path >>= fun pk ->
    let pk_computed = Secp256k1.Key.read_pk_exn ctx pk.uncompressed.buffer in
    let pk_compressed = Secp256k1.Key.to_bytes ctx pk_computed |> Cstruct.of_bigarray in
    sign_segwit ~bch:true ~path ~prev_amounts:[3000000000L] h nextTx >>| fun bch_signatures ->
    Printf.printf "Got %d BCH signatures.\n%!" (List.length bch_signatures) ;
    let bchSig = List.hd bch_signatures in
    let scriptSig =
      Bitcoin.Script.[Element.O (Op_pushdata (Cstruct.len bchSig));
                      D bchSig ;
                      O (Op_pushdata (Cstruct.len pk_compressed)) ;
                      D pk_compressed ;
      ] in
    let nextTx_input = nextTx.inputs.(0) in
    let nextTx_input = { nextTx_input with script = scriptSig @ nextTx_input.script } in
    let txFinal = { nextTx with inputs = [|nextTx_input|] } in
    let cs = Cstruct.create 1024 in
    let cs' = Bitcoin.Protocol.Transaction.to_cstruct cs txFinal in
    let txSerialized = Cstruct.sub cs 0 cs'.off in
    Format.printf "Tx = %a@." Hex.pp (Hex.of_cstruct txSerialized)
    end

let basic = [
    "open_close", `Quick, test_open_close ;
    "ping", `Quick, test_ping ;

    "get_info", `Quick, test_get_info ;
    "get_random", `Quick, test_get_random ;
    "get_wallet_pk", `Quick, test_get_wallet_pk ;
    "get_trusted_input", `Quick, test_get_trusted_input ;
    "sign_segwit", `Quick, test_sign_segwit ;
  ]

let () =
  Alcotest.run "ledgerwallet.btc" [
    "basic", basic ;
  ]
