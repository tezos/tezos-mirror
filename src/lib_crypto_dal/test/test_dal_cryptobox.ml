module Test = struct
  (* Samples k random integers within the range [0, bound]. *)
  let random_indices bound k =
    let indices = Array.init k (fun _ -> -1) in
    for i = 0 to k - 1 do
      let rec loop () =
        let n = Random.int bound in
        if Array.mem n indices then loop () else n
      in
      indices.(i) <- loop ()
    done ;
    indices

  (* Initializes the DAL parameters *)
  let init slot_size =
    let parameters =
      Cryptobox.Internal_for_tests.initialisation_parameters_from_slot_size
        ~slot_size
    in
    Cryptobox.Internal_for_tests.load_parameters parameters

  (* Returns a tuple (slot, msg) where
     - slot of the given [size] padded with null bytes starting
     from byte index [padding_threshold]
     - msg is the slot without the padding *)
  let make_slot ~size ~padding_threshold =
    (* [msg] is initialized with random bytes. *)
    let msg = Bytes.make padding_threshold '\000' in
    let slot = Bytes.make size '\000' in
    for i = 0 to (padding_threshold / 8) - 1 do
      Bytes.set_int64_le msg (i * 8) (Random.bits64 ())
    done ;
    (* We include [msg] in a slot, with additional padding
       with null bytes *)
    Bytes.blit msg 0 slot 0 padding_threshold ;
    (slot, msg)

  type params = {number_of_shards : int; slot_size : int; page_size : int}

  (* We divide mainnet parameters by [32] to speed up the tests. *)
  let test_params =
    {
      number_of_shards = 2048 / 32;
      slot_size = (1 lsl 20) / 32;
      page_size = 4096 / 32;
    }

  (* Tests that with a fraction 1/redundancy_factor of the shards
     the decoding succeeds. *)
  let test_erasure_code () =
    let {number_of_shards; slot_size; page_size} = test_params in
    let msg_size = slot_size / 16 in
    let slot, msg = make_slot ~size:slot_size ~padding_threshold:msg_size in
    init slot_size ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_e
      (fun redundancy_factor ->
        let* t =
          Cryptobox.make
            {redundancy_factor; slot_size; page_size; number_of_shards}
        in
        let* p = Cryptobox.polynomial_from_slot t slot in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let c_indices =
          random_indices
            (number_of_shards - 1)
            (number_of_shards / redundancy_factor)
        in
        let c =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) -> Array.mem index c_indices)
            enc_shards
        in
        let* decoded_slot = Cryptobox.polynomial_from_shards t c in
        let decoded_msg =
          Bytes.sub (Cryptobox.polynomial_to_bytes t decoded_slot) 0 msg_size
        in
        assert (Bytes.equal msg decoded_msg) ;
        Ok ())
      [2; 4; 16]
    |> function
    | Ok _ -> assert true
    | _ -> assert false

  (* Tests that a page is included in a slot. *)
  let test_page_proofs () =
    let {number_of_shards; slot_size; page_size} = test_params in
    let msg_size = slot_size / 16 in
    let number_of_pages = slot_size / page_size in
    let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
    init slot_size ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* t =
       Cryptobox.make
         {redundancy_factor = 2; slot_size; page_size; number_of_shards}
     in
     let* p = Cryptobox.polynomial_from_slot t slot in
     let cm = Cryptobox.commit t p in
     let page_index = Random.int number_of_pages in
     let* pi = Cryptobox.prove_page t p page_index in
     let page = Bytes.sub slot (page_index * page_size) page_size in
     let* check = Cryptobox.verify_page t cm ~page_index page pi in
     Ok check)
    |> function
    | Ok check -> assert check
    | _ -> assert false

  (* Tests that a shard comes from the erasure-encoded slot. *)
  let test_shard_proofs () =
    let {number_of_shards; slot_size; page_size} = test_params in
    let msg_size = slot_size / 16 in
    let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
    init slot_size ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_e
      (fun redundancy_factor ->
        let* t =
          Cryptobox.make
            {redundancy_factor; slot_size; page_size; number_of_shards}
        in
        let* p = Cryptobox.polynomial_from_slot t slot in
        let cm = Cryptobox.commit t p in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let shard_proofs = Cryptobox.prove_shards t p in
        let shard_index = Random.int number_of_shards in
        match
          Seq.find
            (fun ({index; _} : Cryptobox.shard) -> index = shard_index)
            enc_shards
        with
        | None -> Ok ()
        | Some shard ->
            let check =
              Cryptobox.verify_shard t cm shard shard_proofs.(shard_index)
            in
            assert check ;
            Ok ())
      [2; 16]
    |> function
    | Ok _ -> assert true
    | _ -> assert false

  (* Tests that the slot behind the commitment has its size bounded
     by [t.slot_size]. *)
  let test_commitment_proof () =
    let {number_of_shards; slot_size; page_size} = test_params in
    let msg_size = slot_size / 16 in
    let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
    init slot_size ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* t =
       Cryptobox.make
         {redundancy_factor = 2; slot_size; page_size; number_of_shards}
     in
     let* p = Cryptobox.polynomial_from_slot t slot in
     Ok (t, p))
    |> function
    | Ok (t, p) ->
        let cm = Cryptobox.commit t p in
        let pi = Cryptobox.prove_commitment t p in
        let check = Cryptobox.verify_commitment t cm pi in
        assert check
    | _ -> assert false

  (* We can craft two slots whose commitments are equal for two different
     page sizes.*)
  let test_collision_page_size () =
    let slot_size = 1 lsl 6 in
    init slot_size ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* t1 =
       Cryptobox.make
         {
           redundancy_factor = 2;
           slot_size;
           page_size = 1 lsl 5;
           number_of_shards = 2;
         }
     in
     let* t2 =
       Cryptobox.make
         {
           redundancy_factor = 2;
           slot_size;
           page_size = 1 lsl 6;
           number_of_shards = 2;
         }
     in

     let a = Random.bits () in
     let slot1 = Bytes.make slot_size '\000' in
     Bytes.set_uint8 slot1 0 a ;
     Bytes.set_uint8 slot1 32 a ;
     let slot2 = Bytes.make slot_size '\000' in
     Bytes.set_uint8 slot2 0 a ;
     Bytes.set_uint8 slot2 31 a ;

     let* p1 = Cryptobox.polynomial_from_slot t1 slot1 in
     let* p2 = Cryptobox.polynomial_from_slot t2 slot2 in

     let cm1 = Cryptobox.commit t1 p1 in
     let cm2 = Cryptobox.commit t2 p2 in
     Ok (Cryptobox.Commitment.equal cm1 cm2))
    |> function
    | Ok check -> assert check
    | _ -> assert false
end

let test =
  List.map
    (fun (test_name, test_func) ->
      Alcotest.test_case test_name `Quick test_func)
    [
      ("test_erasure_code", Test.test_erasure_code);
      ("test_page_proofs", Test.test_page_proofs);
      ("test_shard_proofs", Test.test_shard_proofs);
      ("test_commitment_proof", Test.test_commitment_proof);
      ("test_collision_page_size", Test.test_collision_page_size);
    ]

let () =
  (* Seed for deterministic pseudo-randomness:
      If the environment variable RANDOM_SEED is set, then its value is used as
      as seed. Otherwise, a random seed is used.
     WARNING: using [Random.self_init] elsewhere in the tests breaks thedeterminism.
  *)
  let seed =
    match Sys.getenv_opt "RANDOM_SEED" with
    | None ->
        Random.self_init () ;
        Random.int 1073741823
    | Some v -> int_of_string v
  in

  Random.init seed ;
  Alcotest.run "DAL cryptobox" [("DAL cryptobox", test)]
