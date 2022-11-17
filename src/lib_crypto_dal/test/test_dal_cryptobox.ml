module Test = struct
  module Scalar = Bls12_381.Fr

  let random_indices bound k =
    Random.self_init () ;

    let rand_elt l =
      let rec loop i = function
        | true -> i
        | false ->
            let n = Random.int bound in
            loop n (not @@ List.mem n l)
      in
      loop 0 false
    in

    let rec aux l n =
      match List.length l with
      | x when x = n -> l
      | _ -> aux (rand_elt l :: l) n
    in

    aux [] k

  let bench_DAL_crypto_params () =
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (* We take mainnet parameters we divide by [16] to speed up the test. *)
    let number_of_shards = 2048 / 16 in
    let slot_size = 1048576 / 16 in
    let page_size = 4096 / 16 in
    let msg_size = slot_size / 16 in
    (* [msg] is initialized with random bytes. *)
    let msg = Bytes.create msg_size in
    let slot = Bytes.create slot_size in

    (* We include [msg] in a slot, with additional zero padding *)
    Bytes.blit msg 0 slot 0 msg_size ;
    Bytes.fill slot msg_size (slot_size - msg_size) '0' ;

    let parameters =
      Cryptobox.Internal_for_tests.initialisation_parameters_from_slot_size
        ~slot_size
    in
    let () = Cryptobox.Internal_for_tests.load_parameters parameters in
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_e
      (fun redundancy_factor ->
        let* t =
          Cryptobox.make
            {redundancy_factor; slot_size; page_size; number_of_shards}
        in
        let* p = Cryptobox.polynomial_from_slot t slot in
        let cm = Cryptobox.commit t p in
        let* pi = Cryptobox.prove_page t p 1 in
        let page = Bytes.sub msg page_size page_size in
        let* check = Cryptobox.verify_page t cm ~page_index:1 page pi in
        assert check ;
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let c_indices =
          random_indices
            (number_of_shards - 1)
            (number_of_shards / redundancy_factor)
          |> Array.of_list
        in
        let c =
          Cryptobox.IntMap.filter (fun i _ -> Array.mem i c_indices) enc_shards
        in
        let* decoded_slot = Cryptobox.polynomial_from_shards t c in
        let decoded_msg =
          Bytes.sub (Cryptobox.polynomial_to_bytes t decoded_slot) 0 msg_size
        in
        assert (Bytes.equal msg decoded_msg) ;
        let comm = Cryptobox.commit t p in
        let shard_proofs = Cryptobox.prove_shards t p in
        match Cryptobox.IntMap.find 0 enc_shards with
        | None -> Ok ()
        | Some eval ->
            let check =
              Cryptobox.verify_shard
                t
                comm
                {index = 0; share = eval}
                shard_proofs.(0)
            in
            assert check ;
            let pi = Cryptobox.prove_commitment t p in
            let check = Cryptobox.verify_commitment t comm pi in
            assert check ;
            Ok ()
        (* let point = Scalar.random () in *)
        (* let+ pi_slot = Cryptobox.prove_single trusted_setup p point in
         *
         * assert (
         *   Cryptobox.verify_single
         *     trusted_setup
         *     comm
         *     ~point
         *     ~evaluation:(Cryptobox.polynomial_evaluate p point)
         *     pi_slot) *))
      [2]
    |> fun x -> match x with Ok () -> () | Error _ -> assert false
end

let test =
  [Alcotest.test_case "test_DAL_cryptobox" `Quick Test.bench_DAL_crypto_params]

let () =
  (* Seed for deterministic pseudo-randomness:
      If the environment variable RANDOM_SEED is set, then its value is used as
      as seed. Otherwise, a random seed is used.
     WARNING: using [Random.self_init] elsewhere in the tests breaks thedeterminism.
  *)
  (*Memtrace.trace_if_requested ~context:"Test" () ;*)
  let seed =
    match Sys.getenv_opt "RANDOM_SEED" with
    | None ->
        Random.self_init () ;
        Random.int 1073741823
    | Some v -> int_of_string v
  in

  Random.init seed ;
  Alcotest.run "DAL cryptobox" [("test case", test)]
