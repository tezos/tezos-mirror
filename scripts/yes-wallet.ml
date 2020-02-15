
(*
   dune utop src/lib_crypto scripts/yes-wallet.ml

   Given a list of aliases and public key hashes:
   - finds the corresponding public keys using RPCs
     (if you have a running node)
   - encodes each public key as a fake secret key that can be used
     with the yes-node.patch
   - creates a 'yes-wallet' directory to be passed to tezos-client -d option
 *)

let string_to_file s file =
  let oc = open_out file in
  output_string oc s ;
  close_out oc

let json_of_list l =
  Printf.sprintf "[ %s ]\n" (String.concat ",\n" l)

let pkhs = [
  ("foundation1", "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9") ;
  ("foundation2", "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5") ;
  ("foundation3", "tz3RB4aoyjov4KEVRbuhvQ1CKJgBJMWhaeB8") ;
  ("foundation4", "tz3bTdwZinP8U1JmSweNzVKhmwafqWmFWRfk") ;
  ("foundation5", "tz3NExpXn9aPNZPorRE4SdjJ2RGrfbJgMAaV") ;
  ("foundation6", "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r") ;
  ("foundation7", "tz3WMqdzXqRWXwyvj5Hp2H7QEepaUuS7vd9K") ;
  ("foundation8", "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN") ;
]

let pkh_pp (alias, pkh) =
  Printf.sprintf "{ \"name\": \"%s\", \"value\": \"%s\" }" alias pkh


let pk_of_pkh (pkh : string) : string =
  let url = "localhost:8732" in
  let curl = Printf.sprintf "curl -s '%s/chains/main/blocks/head/context/raw/json/contracts/index/%s/manager'" url pkh in
  Lwt_main.run
    (Lwt_process.pread_line (Lwt_process.shell curl))

(* let pks = List.map (fun (_alias, pkh) -> pk_of_pkh pkh) pkhs *)

let pks = [
  "p2pk67wVncLFS1DQDm2gVR45sYCzQSXTtqn3bviNYXVCq6WRoqtxHXL" ;
  "p2pk66n1NmhPDEkcf9sXEKe9kBoTwBoTYxke1hx16aTRVq8MoXuwNqo" ;
  "p2pk67NECc8vGK4eLbXGEgBZGhk53x1pCMbgnFEgLxZEMGDtzVcFQok" ;
  "p2pk6796esaR3dNr8jUx8S7xxZdRvpYSrhHMg6NagjwMRJHsERMiUKM" ;
  "p2pk66iTZwLmRPshQgUr2HE3RUzSFwAN5MNaBQ5rfduT1dGKXd25pNN" ;
  "p2pk65ffAqpYT6Et73DXdNqudthwmSNzNyzL3Wdn2EYuiiMwoPu6vFJ" ;
  "p2pk67Cwb5Ke6oSmqeUbJxURXMe3coVnH9tqPiB2xD84CYhHbBKs4oM" ;
  "p2pk67uapBxwkM1JNasGJ6J3rozzYELgrtcqxKZwZLjvsr4XcAr4FqC" ;
]

let pk_pp (alias,pkh) pk =
  let loc_key =
    Printf.sprintf "{ \"locator\": \"unencrypted:%s\", \"key\": \"%s\" }" pk pk
  in
  Printf.sprintf "{ \"name\": \"%s\", \"value\": %s }" alias loc_key


open Tezos_crypto.Signature ;;
open Data_encoding ;;
open Tezos_stdlib ;;

(* P-256 pk : 33+1 bytes
   ed25519 pk sk : 32+1 bytes
*)

let sk_of_pk (pk_s : string) : string =
  let pk = Public_key.of_b58check_exn pk_s in
  let pk_b = Data_encoding.Binary.to_bytes_exn Public_key.encoding pk in
  let sk_b = MBytes.sub pk_b 0 33 in
  let sk = Data_encoding.Binary.of_bytes_exn Secret_key.encoding sk_b in
  let sk_s = Secret_key.to_b58check sk in
  sk_s

let sks = List.map (sk_of_pk) pks

let sk_pp (alias,pkh) sk =
  Printf.sprintf "{ \"name\": \"%s\", \"value\": \"unencrypted:%s\" }" alias sk


let _ =
  Unix.mkdir "yes-wallet" 0o750 ;
  Unix.chdir "yes-wallet" ;

  let l = List.map pkh_pp pkhs in
  string_to_file (json_of_list l) "public_key_hashs" ;

  let l = List.map2 pk_pp pkhs pks in
  string_to_file (json_of_list l) "public_keys" ;

  let l = List.map2 sk_pp pkhs sks in
  string_to_file (json_of_list l) "secret_keys"
