module Test = struct
  (* Samples k random integers within the range [0, bound[. *)
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
  let init parameters =
    Cryptobox.Internal_for_tests.parameters_initialisation parameters
    |> Cryptobox.Internal_for_tests.load_parameters

  type parameters = {
    slot_size : int;
    page_size : int;
    redundancy_factor : int;
    number_of_shards : int;
    padding_threshold : int;
    slot : bytes;
  }

  let get_cryptobox_parameters parameters : Cryptobox.parameters =
    {
      slot_size = parameters.slot_size;
      page_size = parameters.page_size;
      redundancy_factor = parameters.redundancy_factor;
      number_of_shards = parameters.number_of_shards;
    }

  let generate_parameters =
    let open QCheck2.Gen in
    let* redundancy_factor_log2 = int_range 1 4 in
    let* slot_size_log2 = int_range 0 11 in
    let* page_size_log2 = int_range 0 15 in
    let* number_of_shards_log2 = int_range 0 19 in
    let slot_size = 1 lsl slot_size_log2 in
    let* msg = bytes_size (int_range 0 slot_size) in
    let padding_threshold = Bytes.length msg in
    let slot = Bytes.make slot_size '0' in
    Bytes.blit msg 0 slot 0 padding_threshold ;
    map
      (fun ( slot_size,
             page_size,
             redundancy_factor,
             number_of_shards,
             padding_threshold,
             slot ) : parameters ->
        {
          slot_size;
          page_size;
          redundancy_factor;
          number_of_shards;
          padding_threshold;
          slot;
        })
      (tup6
         (return slot_size)
         (return (1 lsl page_size_log2))
         (return (1 lsl redundancy_factor_log2))
         (return (1 lsl number_of_shards_log2))
         (return padding_threshold)
         (return slot))

  let generate_parameters_tup2 =
    QCheck2.Gen.tup2 generate_parameters generate_parameters

  let print_parameters =
    QCheck2.Print.(
      contramap
        (fun {
               slot_size;
               page_size;
               redundancy_factor;
               number_of_shards;
               padding_threshold;
               slot;
             } ->
          ( slot_size,
            page_size,
            redundancy_factor,
            number_of_shards,
            padding_threshold,
            slot ))
        (tup6 int int int int int bytes))

  let print_parameters_tup2 =
    QCheck2.Print.tup2 print_parameters print_parameters

  let ensure_validity params =
    Cryptobox.Internal_for_tests.ensure_validity
      (get_cryptobox_parameters params)

  let init params = init (get_cryptobox_parameters params)

  (* Tests that with a fraction 1/redundancy_factor of the shards
     the decoding succeeds. Checks equality of polynomials. *)
  let test_erasure_code =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let c_indices =
          random_indices
            (params.number_of_shards - 1)
            (params.number_of_shards / params.redundancy_factor)
        in
        let c =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) -> Array.mem index c_indices)
            enc_shards
        in
        let* decoded_p = Cryptobox.polynomial_from_shards t c in
        return (Cryptobox.Internal_for_tests.polynomials_equal decoded_p p))
        |> function
        | Ok check -> check
        | Error _ -> false)

  (* Tests that with a fraction 1/redundancy_factor of the shards
     the decoding succeeds. Checks equality of slots. *)
  let test_erasure_code_with_slot_conversion =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code with slot conversion"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let c_indices =
          random_indices
            (params.number_of_shards - 1)
            (params.number_of_shards / params.redundancy_factor)
        in
        let c =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) -> Array.mem index c_indices)
            enc_shards
        in
        let* decoded_slot = Cryptobox.polynomial_from_shards t c in
        let decoded_msg =
          Bytes.sub
            (Cryptobox.polynomial_to_slot t decoded_slot)
            0
            params.padding_threshold
        in
        let msg = Bytes.sub params.slot 0 params.padding_threshold in
        return (Bytes.equal msg decoded_msg))
        |> function
        | Ok check -> check
        | _ -> false)

  let test_erasure_code_failure_not_enough_shards =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code not enough shards"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let c_indices =
          random_indices
            (params.number_of_shards - 1)
            ((params.number_of_shards / params.redundancy_factor) - 1)
        in
        let c =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) -> Array.mem index c_indices)
            enc_shards
        in
        Cryptobox.polynomial_from_shards t c)
        |> function
        | Error (`Not_enough_shards _) -> true
        | _ -> false)

  let test_erasure_code_failure_out_of_range =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code shard index out of range"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let c =
          Seq.take
            (params.number_of_shards / params.redundancy_factor)
            enc_shards
          |> Seq.map
               (fun ({index; share} : Cryptobox.shard) : Cryptobox.shard ->
                 {index = index + 1000; share})
        in
        Cryptobox.polynomial_from_shards t c)
        |> function
        | Error (`Shard_index_out_of_range _) -> true
        | _ -> false)

  (* Checking the shards' length to avoid out-of-bounds array accesses.
     The function [polynomial_from_shards] returns an error if the shards
     don't have the expected length. This could happen if the shards were
     produced with a different set of parameters than the ones used by
     [polynomial_from_shards].

     Here, the number of shards for the first set of parameters t1
     second set of parameters t2 are different. *)
  let test_erasure_code_failure_invalid_shard_length =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code shard invalid shard length"
      ~print:print_parameters_tup2
      generate_parameters_tup2
      (fun (params1, params2) ->
        init params1 ;
        assume (ensure_validity params1) ;
        assume (ensure_validity params2) ;
        assume (params1.number_of_shards <> params2.number_of_shards) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t1 = Cryptobox.make (get_cryptobox_parameters params1) in
        let* t2 = Cryptobox.make (get_cryptobox_parameters params2) in
        let enc_shards = Cryptobox.Internal_for_tests.make_dummy_shards t1 in
        Cryptobox.polynomial_from_shards t2 enc_shards)
        |> function
        | Error (`Invalid_shard_length _) -> true
        | _ -> false)

  (* Check that for any slot,
     [polynomial_to_slot (polynomial_from_slot slot) = slot]. *)
  let test_polynomial_slot_conversions =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test polynomial-slot conversions"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let slot_res = Cryptobox.(polynomial_to_slot t p) in
        Ok (Bytes.equal slot_res params.slot))
        |> function
        | Ok check -> check
        | _ -> false)

  (* Tests that a page is included in a slot. *)
  let test_page_proofs =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test page proofs"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let cm = Cryptobox.commit t p in
        let number_of_pages = params.slot_size / params.page_size in
        let page_index = Random.int number_of_pages in
        let* pi = Cryptobox.prove_page t p page_index in
        let page =
          Bytes.sub params.slot (page_index * params.page_size) params.page_size
        in
        Cryptobox.verify_page t cm ~page_index page pi)
        |> function
        | Ok () -> true
        | _ -> false)

  (* Tests that a shard comes from the erasure-encoded slot. *)
  let test_shard_proofs =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test shard proofs"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let cm = Cryptobox.commit t p in
        let enc_shards = Cryptobox.shards_from_polynomial t p in
        let shard_proofs = Cryptobox.prove_shards t p in
        let shard_index = Random.int params.number_of_shards in
        match
          Seq.find
            (fun ({index; _} : Cryptobox.shard) -> index = shard_index)
            enc_shards
        with
        | None ->
            (* The shard index was sampled within the bounds, so this case
               (the queried index is out of bounds) doesn't happen. *)
            assert false
        | Some shard ->
            Cryptobox.verify_shard t cm shard shard_proofs.(shard_index))
        |> function
        | Ok () -> true
        | _ -> false)

  (* Tests that the slot behind the commitment has its size bounded
     by [t.slot_size]. *)
  let test_commitment_proof =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test commitment proof"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init params ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* p = Cryptobox.polynomial_from_slot t params.slot in
        let cm = Cryptobox.commit t p in
        let pi = Cryptobox.prove_commitment t p in
        let check = Cryptobox.verify_commitment t cm pi in
        Ok check)
        |> function
        | Ok true -> true
        | _ -> false)

  (* We can craft two slots whose commitments are equal for two different
     page sizes. *)
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4555
     This test should be adapted with the new constraints on the DAL parameters. *)
  let _test_collision_page_size () =
    let slot_size = 1 lsl 6 in
    init
      {
        slot_size;
        page_size = 2;
        redundancy_factor = 2;
        number_of_shards = 1 lsl 6;
        slot = Bytes.make 1 '\000';
        padding_threshold = 0;
      } ;
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
    [ (*("test_collision_page_size", Test.test_collision_page_size);*) ]

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
  Alcotest.run
    "DAL cryptobox"
    [
      ("Unit tests", test);
      ( "PBT",
        Tezos_test_helpers.Qcheck2_helpers.qcheck_wrap
          [
            Test.test_erasure_code;
            Test.test_erasure_code_with_slot_conversion;
            Test.test_erasure_code_failure_out_of_range;
            Test.test_erasure_code_failure_not_enough_shards;
            Test.test_erasure_code_failure_invalid_shard_length;
            Test.test_page_proofs;
            Test.test_shard_proofs;
            Test.test_commitment_proof;
            Test.test_polynomial_slot_conversions;
          ] );
    ]
