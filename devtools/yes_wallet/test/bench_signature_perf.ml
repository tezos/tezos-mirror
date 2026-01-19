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
let time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (stop -. start, res)

let keys =
  let keys_p = Tezos_crypto.Signature.generate_key ~algo:P256 () in
  let keys_e = Tezos_crypto.Signature.generate_key ~algo:Ed25519 () in
  let keys_s = Tezos_crypto.Signature.generate_key ~algo:Secp256k1 () in
  let keys_b = Tezos_crypto.Signature.generate_key ~algo:Bls () in
  let keys_m = Tezos_crypto.Signature.generate_key ~algo:Mldsa44 () in
  function
  | Tezos_crypto.Signature.P256 -> keys_p
  | Ed25519 -> keys_e
  | Secp256k1 -> keys_s
  | Bls -> keys_b
  | Mldsa44 -> keys_m

let wrong_keys =
  let keys_p = Tezos_crypto.Signature.generate_key ~algo:P256 () in
  let keys_e = Tezos_crypto.Signature.generate_key ~algo:Ed25519 () in
  let keys_s = Tezos_crypto.Signature.generate_key ~algo:Secp256k1 () in
  let keys_b = Tezos_crypto.Signature.generate_key ~algo:Bls () in
  let keys_m = Tezos_crypto.Signature.generate_key ~algo:Mldsa44 () in

  function
  | Tezos_crypto.Signature.P256 -> keys_p
  | Ed25519 -> keys_e
  | Secp256k1 -> keys_s
  | Bls -> keys_b
  | Mldsa44 -> keys_m

let wrong_pk algo =
  let _, pk, _ = wrong_keys algo in
  pk

let pk algo =
  let _, pk, _ = keys algo in
  pk

let sk algo =
  let _, _, sk = keys algo in
  sk

let fake_sk algo =
  let pk = pk algo in
  let open Tezos_crypto.Signature in
  let pk_b = Data_encoding.Binary.to_bytes_exn Public_key.encoding pk in
  let sk_b = Bytes.sub pk_b 0 33 in
  Data_encoding.Binary.of_bytes_exn Secret_key.encoding sk_b

let combine (t, d) (t0, d0) = (t +. t0, d && d0)

let id a = a

let check signed datas key f =
  List.fold_left2
    ~when_different_lengths:[]
    (fun acc l r ->
      combine
        acc
        (time (fun () ->
             let b = Tezos_crypto.Signature.check key l r in
             assert (f b) ;
             b)))
    (0., true)
    signed
    datas
  |> Result.value_f ~default:(fun () -> failwith "error")

module Ok = struct
  let mark_ok check =
    let res = check () in
    Format.eprintf "" ;
    res

  let check algo signed datas =
    mark_ok @@ fun () -> check signed datas (pk algo) id
end

module Ko = struct
  let check algo signed datas = check signed datas (wrong_pk algo) not
end

let str_of_algo = function
  | Tezos_crypto.Signature.Ed25519 -> "Ed25519"
  | Tezos_crypto.Signature.Secp256k1 -> "Secp256k1"
  | Tezos_crypto.Signature.P256 -> "P256"
  | Tezos_crypto.Signature.Bls -> "Bls"
  | Tezos_crypto.Signature.Mldsa44 -> "Mldsa44"

let time ~yes_crypto ~algo size datas =
  Format.eprintf "generating signatures...@?" ;
  let sign msg =
    Tezos_crypto.Signature.sign
      (if yes_crypto then fake_sk algo else sk algo)
      msg
  in
  let signed = List.rev @@ List.rev_map sign datas in
  Format.eprintf "Compacting memory...@?" ;
  Gc.compact () ;
  Format.eprintf "timing Ko check..." ;
  let time_check_ko, _ = Ko.check algo signed datas in
  Format.eprintf "Compacting memory...@?" ;
  Gc.compact () ;
  Format.eprintf "timing Ok check...@?" ;
  let time_check_ok, _ = Ok.check algo signed datas in
  Format.eprintf "end.@." ;
  Format.printf
    "%s,%d,%f,%f@."
    (str_of_algo algo)
    size
    time_check_ok
    time_check_ko

let () = Random.init 0

let random_bytes bound =
  Bytes.init (Random.int bound) (fun _ -> Char.chr (Random.int 255))

let generate_data len max_msg =
  let rec loop len aux =
    if len <= 0 then aux else loop (len - 1) (random_bytes max_msg :: aux)
  in
  Format.eprintf "Generating data for %d %d...@?" len max_msg ;
  let res = loop len [] in
  Format.eprintf "done.@." ;
  res

let repeat n time gen =
  Format.printf "@." ;
  for _ = 0 to n do
    ignore (time gen)
  done

let time = time ~yes_crypto:false

let () =
  List.iter
    (fun algo ->
      Format.eprintf "@.proceeding algo  %s@." (str_of_algo algo) ;
      let _ = repeat 5 (time ~algo 100) (generate_data 1_000 100) in
      let _ = repeat 5 (time ~algo 1_000) (generate_data 1_000 1_000) in
      let _ = repeat 5 (time ~algo 10_000) (generate_data 1_000 10_000) in
      let _ = repeat 5 (time ~algo 100_000) (generate_data 1_000 100_000) in
      let _ = repeat 5 (time ~algo 1_000_000) (generate_data 1_000 1_000_000) in
      ())
    [Ed25519; Secp256k1; P256; Bls]
(* let _ = time (generate_data 1_000 10_000_000) *)
