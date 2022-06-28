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

  let ( // ) = Filename.concat

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
    let project_root =
      match Sys.getenv_opt "DUNE_SOURCEROOT" with
      | Some x -> x
      | None -> (
          match Sys.getenv_opt "PWD" with
          | Some x -> x
          | None ->
              (* For some reason, under [dune runtest], [PWD] and
                 [getcwd] have different values. [getcwd] is in
                 [_build/default], and [PWD] is where [dune runtest] was
                 executed, which is closer to what we want. *)
              Sys.getcwd ())
    in
    let open Tezos_error_monad.Error_monad.Result_syntax in
    List.iter
      (fun redundancy_factor ->
        (*let srs_g1 =
            project_root // Filename.dirname __FILE__ // "srs_zcash_g1"
          in
          let srs_g2 =
            project_root // Filename.dirname __FILE__ // "srs_zcash_g2"
          in*)
        let module DAL_crypto = Dal_cryptobox.Make (struct
          let redundancy_factor = redundancy_factor

          let slot_size = slot_size

          let slot_segment_size = slot_segment_size

          let shards_amount = shards_amount

          type trusted_setup_files = {
            srs_g1 : string;
            srs_g2 : string;
            log_size : int;
          }

          let trusted_setup_files = None
          (*Some {srs_g1; srs_g2; log_size = 21}*)
        end) in
        match
          let* p = DAL_crypto.polynomial_from_bytes msg in

          let* cm = DAL_crypto.commit p in
          (*let precompute_pi_segments =
              DAL_crypto.precompute_slot_segments_proofs ()
            in*)
          let filename =
            project_root // Filename.dirname __FILE__
            // "slot_seg_proofs_precomp"
          in
          (*let () =
              DAL_crypto.save_precompute_slot_segments_proofs
                precompute_pi_segments
                filename
            in*)
          let precompute_pi_segments =
            DAL_crypto.load_precompute_slot_segments_proofs filename
          in
          let pis =
            DAL_crypto.prove_slot_segments p ~preprocess:precompute_pi_segments
          in

          let slot_segment = Bytes.sub msg 0 slot_segment_size in
          assert (
            DAL_crypto.verify_slot_segment
              cm
              ~slot_segment
              ~slot_segment_index:0
              pis.(0)) ;
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

          let* comm = DAL_crypto.commit p in

          (*let precompute_pi_shards = DAL_crypto.precompute_shards_proofs () in*)
          let filename =
            project_root // Filename.dirname __FILE__
            // "shard_proofs_precomp"
          in
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
              assert (DAL_crypto.verify_shard comm (0, eval) shard_proofs.(0)) ;

              let* pi =
                DAL_crypto.prove_degree p (DAL_crypto.polynomial_degree p)
              in

              let* check =
                DAL_crypto.verify_degree
                  comm
                  pi
                  (DAL_crypto.polynomial_degree p)
              in
              assert check ;

              let point = Scalar.random () in
              let+ pi_slot = DAL_crypto.prove_single p point in

              assert (
                DAL_crypto.verify_single
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
