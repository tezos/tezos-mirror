open Rresult
open Ledgerwallet_zil
open Alcotest

let vendor_id = 0x2C97
let product_id = 0x1015

let with_connection f =
  let h = Hidapi.open_id_exn ~vendor_id ~product_id in
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
let test_ping () = with_connection Ledgerwallet.Transport.ping

let hard x =
  Int32.logor x 0x8000_0000l

let test_getversion () =
  with_connection begin fun h ->
    get_version h >>| fun (ma, mi, pa) ->
    Printf.printf "%d.%d.%d" ma mi pa
  end

let test_getpk ~display_addr () =
  with_connection begin fun h ->
    get_pk ~display_addr h 0l >>| fun (pk, addr) ->
    match Bech32.Segwit.encode addr with
    | Error msg -> fail msg
    | Ok v -> Format.printf "%a %s" Hex.pp (Hex.of_cstruct pk) v
  end

(* let path = [
 *   hard 44l ; hard 1729l
 * ]
 * 
 * let msg = Cstruct.of_string "Voulez-vous coucher avec moi, ce soir ?"
 * let msg_ba = Cstruct.to_bigarray msg
 * 
 * let test_getpk h curve =
 *   let pk = get_public_key h curve path in
 *   Alcotest.(check int "pklen"
 *               (if curve = Ed25519 then 33 else 65) (Cstruct.len pk))
 * 
 * let test_getpk () =
 *   let h = Hidapi.open_id_exn ~vendor_id ~product_id in
 *   List.iter (test_getpk h) curves ;
 *   Hidapi.close h
 * 
 * let test_sign h curve =
 *   let open Alcotest in
 *   let pk = get_public_key h curve path in
 *   let signature = sign h curve path msg in
 *   match curve with
 *   | Ed25519 ->
 *       let pk = Monocypher.Sign.(pk_of_cstruct_exn (Cstruct.sub pk 1 pkbytes)) in
 *       check bool "sign Ed25519" true
 *         (Tweetnacl.Sign.verify_detached ~key:pk ~signature msg)
 *   | Secp256k1 -> begin
 *       let pk = Cstruct.to_bigarray pk in
 *       let signature = Cstruct.to_bigarray signature in
 *       match Uecc.(pk_of_bytes secp256k1 pk) with
 *       | None -> assert false
 *       | Some pk ->
 *           check bool "sign Secp256k1" true (Uecc.verify pk ~msg:msg_ba ~signature)
 *     end
 *   | Secp256r1 -> begin
 *       let pk = Cstruct.to_bigarray pk in
 *       let signature = Cstruct.to_bigarray signature in
 *       match Uecc.(pk_of_bytes secp256r1 pk) with
 *       | None -> assert false
 *       | Some pk ->
 *           check bool "sign Secp256r1" true (Uecc.verify pk ~msg:msg_ba ~signature)
 *     end
 * 
 * let test_sign () =
 *   let h = Hidapi.open_id_exn ~vendor_id ~product_id in
 *   (\* List.iter (test_sign h) curves ; *\)
 *   (\* List.iter (test_sign h) [Secp256k1] ; *\)
 *   Hidapi.close h *)

let basic = [
  (* "open_close", `Quick, test_open_close ;
   * "ping", `Quick, test_ping ; *)
  "version", `Quick, test_getversion ;
  (* "getpk", `Quick, (test_getpk ~display_addr:false) ; *)
  "getaddr", `Quick, (test_getpk ~display_addr:true) ;
  (* "get_public_key", `Quick, test_getpk ;
   * "sign", `Quick, test_sign ; *)
]

let () =
  Alcotest.run "ledgerwallet.zil" [
    "basic", basic ;
  ]
