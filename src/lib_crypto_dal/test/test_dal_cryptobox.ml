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

  (* Encoding and decoding of Reed-Solomon codes on the erasure channel. *)
  let bench_DAL_crypto_params () =
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (* We take mainnet parameters we divide by [16] to speed up the test. *)
    let number_of_shards = 2048 / 16 in
    let slot_size = 1048576 / 16 in
    let segment_size = 4096 / 16 in
    let msg_size = slot_size in
    let msg = Bytes.create msg_size in
    for i = 0 to (msg_size / 8) - 1 do
      Bytes.set_int64_le msg (i * 8) (Random.int64 Int64.max_int)
    done ;
    let parameters =
      Dal_cryptobox.Internal_for_tests.initialisation_parameters_from_slot_size
        ~slot_size
    in
    let () = Dal_cryptobox.Internal_for_tests.load_parameters parameters in
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_e
      (fun redundancy_factor ->
        let* t =
          Dal_cryptobox.make
            ~redundancy_factor
            ~slot_size
            ~segment_size
            ~number_of_shards
        in
        let* p = Dal_cryptobox.polynomial_from_slot t msg in
        let cm = Dal_cryptobox.commit t p in
        let* pi = Dal_cryptobox.prove_segment t p 1 in
        let segment = Bytes.sub msg segment_size segment_size in
        let* check =
          Dal_cryptobox.verify_segment t cm {index = 1; content = segment} pi
        in

        assert check ;
        let enc_shards = Dal_cryptobox.shards_from_polynomial t p in
        let c_indices =
          random_indices
            (number_of_shards - 1)
            (number_of_shards / redundancy_factor)
          |> Array.of_list
        in
        let c =
          Dal_cryptobox.IntMap.filter
            (fun i _ -> Array.mem i c_indices)
            enc_shards
        in
        let* dec = Dal_cryptobox.polynomial_from_shards t c in
        assert (
          Bytes.compare
            msg
            (Bytes.sub
               (Dal_cryptobox.polynomial_to_bytes t dec)
               0
               (min slot_size msg_size))
          = 0) ;
        let comm = Dal_cryptobox.commit t p in
        let shard_proofs = Dal_cryptobox.prove_shards t p in
        match Dal_cryptobox.IntMap.find 0 enc_shards with
        | None -> Ok ()
        | Some eval ->
            let check =
              Dal_cryptobox.verify_shard
                t
                comm
                {index = 0; share = eval}
                shard_proofs.(0)
            in
            assert check ;
            let pi = Dal_cryptobox.prove_commitment t p in
            let check = Dal_cryptobox.verify_commitment t comm pi in
            assert check ;
            Ok ()
        (* let point = Scalar.random () in *)
        (* let+ pi_slot = Dal_cryptobox.prove_single trusted_setup p point in
         *
         * assert (
         *   Dal_cryptobox.verify_single
         *     trusted_setup
         *     comm
         *     ~point
         *     ~evaluation:(Dal_cryptobox.polynomial_evaluate p point)
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
  Alcotest.run "Kate Amortized" [("DAL cryptobox", test)]
