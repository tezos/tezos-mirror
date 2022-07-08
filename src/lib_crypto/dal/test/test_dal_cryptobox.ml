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
    let shards_amount = 2048 in
    let slot_size = 1048576 in
    let slot_segment_size = 4096 in
    let msg_size = slot_size in
    let msg = Bytes.create msg_size in
    for i = 0 to (msg_size / 8) - 1 do
      Bytes.set_int64_le msg (i * 8) (Random.int64 Int64.max_int)
    done ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    List.iter
      (fun redundancy_factor ->
        let module DAL_crypto = Dal_cryptobox.Make (struct
          let redundancy_factor = redundancy_factor

          let slot_size = slot_size

          let slot_segment_size = slot_segment_size

          let shards_amount = shards_amount
        end) in
        let trusted_setup =
          DAL_crypto.srs
            ~redundancy_factor
            ~slot_size
            ~slot_segment_size
            ~shards_amount
          (*(`Files {srs_g1_file; srs_g2_file; logarithm_size = 21})*)
        in

        match
          let* p = DAL_crypto.polynomial_from_bytes msg in

          let* cm = DAL_crypto.commit trusted_setup p in
          let* pi = DAL_crypto.prove_slot_segment trusted_setup p 1 in

          let slot_segment =
            Bytes.sub msg slot_segment_size (2 * slot_segment_size)
          in
          let* check =
            DAL_crypto.verify_slot_segment trusted_setup cm (1, slot_segment) pi
          in
          assert check ;
          let enc_shards = DAL_crypto.to_shards p in

          (* Only take half of the buckets *)
          let c_indices = random_indices (2048 - 1) 1024 |> Array.of_list in

          let c =
            DAL_crypto.IntMap.filter
              (fun i _ -> Array.mem i c_indices)
              enc_shards
          in

          let* dec = DAL_crypto.from_shards c in
          assert (
            Bytes.compare
              msg
              (Bytes.sub
                 (DAL_crypto.polynomial_to_bytes dec)
                 0
                 (min slot_size msg_size))
            = 0) ;

          let* comm = DAL_crypto.commit trusted_setup p in

          (*let precompute_pi_shards = DAL_crypto.precompute_shards_proofs () in*)
          let filename = "./shard_proofs_precomp" in
          (*let () =
              DAL_crypto.save_precompute_shards_proofs
                precompute_pi_shards
                filename
            in*)
          let precompute_pi_shards =
            DAL_crypto.load_precompute_shards_proofs filename
          in
          let shard_proofs =
            DAL_crypto.prove_shards p ~preprocess:precompute_pi_shards
          in
          match DAL_crypto.IntMap.find 0 enc_shards with
          | None -> Ok ()
          | Some eval ->
              assert (
                DAL_crypto.verify_shard
                  trusted_setup
                  comm
                  (0, eval)
                  shard_proofs.(0)) ;

              let* pi =
                DAL_crypto.prove_degree
                  trusted_setup
                  p
                  (DAL_crypto.polynomial_degree p)
              in

              let* check =
                DAL_crypto.verify_degree
                  trusted_setup
                  comm
                  pi
                  (DAL_crypto.polynomial_degree p)
              in
              assert check ;

              let point = Scalar.random () in
              let+ pi_slot = DAL_crypto.prove_single trusted_setup p point in

              assert (
                DAL_crypto.verify_single
                  trusted_setup
                  comm
                  ~point
                  ~evaluation:(DAL_crypto.polynomial_evaluate p point)
                  pi_slot)
        with
        | Ok () -> ()
        | Error _ -> assert false)
      [2]
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
  Alcotest.run "Kate Amortized" [("DAScryptobox", test)]
