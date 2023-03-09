module Test = struct
  (* [randrange n] returns a random integer in the range [0, n - 1]. *)
  let randrange n = QCheck2.Gen.(generate1 (int_bound (n - 1)))

  (* Samples k random integers within the range [0, bound[. *)
  let random_indices bound k =
    let indices = Array.init k (fun _ -> -1) in
    for i = 0 to k - 1 do
      let rec loop () =
        let n = randrange bound in
        if Array.mem n indices then loop () else n
      in
      indices.(i) <- loop ()
    done ;
    indices

  (* The maximum value for the slot size is chosen to trigger
     cases where some domain sizes for the FFT are not powers
     of two.*)
  let max_slot_size_log2 = 13

  let size_offset_log2 = 3

  let max_page_size_log2 = max_slot_size_log2 - size_offset_log2

  let max_redundancy_factor_log2 = 4

  let max_number_of_shards_log2 = 19

  (* The set of parameters maximizing the SRS length, and which
     is in the codomain of [generate_parameters]. *)
  let max_parameters =
    let max_parameters : Cryptobox.parameters =
      {
        slot_size = 1 lsl max_slot_size_log2;
        page_size = 1 lsl max_page_size_log2;
        redundancy_factor = 1 lsl max_redundancy_factor_log2;
        number_of_shards = 1;
      }
    in
    Cryptobox.Internal_for_tests.parameters_initialisation max_parameters

  (* Initializes the DAL parameters *)
  let init () = Cryptobox.Internal_for_tests.load_parameters max_parameters

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
    let* redundancy_factor_log2 = int_range 1 max_redundancy_factor_log2 in
    let* slot_size_log2 = int_range size_offset_log2 max_slot_size_log2 in
    let* page_size_log2 = int_range 0 (slot_size_log2 - size_offset_log2) in
    let* number_of_shards_log2 = int_range 0 max_number_of_shards_log2 in
    let slot_size = 1 lsl slot_size_log2 in
    let* data = bytes_size (int_range 0 slot_size) in
    let padding_threshold = Bytes.length data in
    let slot = Bytes.make slot_size '\000' in
    Bytes.blit data 0 slot 0 padding_threshold ;
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

  let ensure_validity params =
    Cryptobox.Internal_for_tests.ensure_validity
      (get_cryptobox_parameters params)

  (* Tests that with a fraction 1/redundancy_factor of the shards
     the decoding succeeds. Checks equality of polynomials. *)
  let test_erasure_code =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let shards = Cryptobox.shards_from_polynomial t polynomial in
        let random_shard_indices =
          random_indices
            (params.number_of_shards - 1)
            (params.number_of_shards / params.redundancy_factor)
        in
        let random_shards =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) ->
              Array.mem index random_shard_indices)
            shards
        in
        let* decoded_polynomial =
          Cryptobox.polynomial_from_shards t random_shards
        in
        return
          (Cryptobox.Internal_for_tests.polynomials_equal
             decoded_polynomial
             polynomial))
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
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let shards = Cryptobox.shards_from_polynomial t polynomial in
        let random_shard_indices =
          random_indices
            (params.number_of_shards - 1)
            (params.number_of_shards / params.redundancy_factor)
        in
        let random_shards =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) ->
              Array.mem index random_shard_indices)
            shards
        in
        let* decoded_slot = Cryptobox.polynomial_from_shards t random_shards in
        let decoded_data =
          Bytes.sub
            (Cryptobox.polynomial_to_slot t decoded_slot)
            0
            params.padding_threshold
        in
        let data = Bytes.sub params.slot 0 params.padding_threshold in
        return (Bytes.equal data decoded_data))
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
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let shards = Cryptobox.shards_from_polynomial t polynomial in
        let random_shard_indices =
          random_indices
            (params.number_of_shards - 1)
            ((params.number_of_shards / params.redundancy_factor) - 1)
        in
        let random_shards =
          Seq.filter
            (fun ({index; _} : Cryptobox.shard) ->
              Array.mem index random_shard_indices)
            shards
        in
        Cryptobox.polynomial_from_shards t random_shards)
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
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let shards = Cryptobox.shards_from_polynomial t polynomial in
        let random_shards =
          Seq.take (params.number_of_shards / params.redundancy_factor) shards
          |> Seq.map
               (fun ({index; share} : Cryptobox.shard) : Cryptobox.shard ->
                 {index = index + 1000; share})
        in
        Cryptobox.polynomial_from_shards t random_shards)
        |> function
        | Error (`Shard_index_out_of_range _) -> true
        | _ -> false)

  (* Checking the shards' length to avoid out-of-bounds array accesses.
     The function [polynomial_from_shards] returns an error if the shards
     don't have the expected length. This could happen if the shards were
     produced with a different set of parameters than the ones used by
     [polynomial_from_shards]. *)
  let test_erasure_code_failure_invalid_shard_length =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test erasure code shard invalid shard length"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let state = QCheck_base_runner.random_state () in
        let shards = Cryptobox.Internal_for_tests.make_dummy_shards t ~state in
        Cryptobox.polynomial_from_shards t shards)
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
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let slot = Cryptobox.(polynomial_to_slot t polynomial) in
        Ok (Bytes.equal slot params.slot))
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
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let* commitment = Cryptobox.commit t polynomial in
        let number_of_pages = params.slot_size / params.page_size in
        let page_index = randrange number_of_pages in
        let* page_proof = Cryptobox.prove_page t polynomial page_index in
        let page =
          Bytes.sub params.slot (page_index * params.page_size) params.page_size
        in
        Cryptobox.verify_page t commitment ~page_index page page_proof)
        |> function
        | Ok () -> true
        | _ -> false)

  (* The verification of a page fails on input the wrong page (or the wrong proof). *)
  let test_page_proofs_invalid =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test page proofs invalid page"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let* commitment = Cryptobox.commit t polynomial in
        let number_of_pages = params.slot_size / params.page_size in
        let page_index = randrange number_of_pages in
        let* proof = Cryptobox.prove_page t polynomial page_index in
        let altered_proof =
          Cryptobox.Internal_for_tests.alter_page_proof proof
        in
        let page =
          Bytes.sub params.slot (page_index * params.page_size) params.page_size
        in
        Cryptobox.verify_page t commitment ~page_index page altered_proof)
        |> function
        | Error `Invalid_page -> true
        | _ -> false)

  (* Tests that a shard comes from the erasure-encoded slot. *)
  let test_shard_proofs =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test shard proofs"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let* commitment = Cryptobox.commit t polynomial in
        let shards = Cryptobox.shards_from_polynomial t polynomial in
        let precomputation = Cryptobox.precompute_shards_proofs t in
        let shard_proofs =
          Cryptobox.prove_shards t ~polynomial ~precomputation
        in
        let shard_index = randrange params.number_of_shards in
        match
          Seq.find
            (fun ({index; _} : Cryptobox.shard) -> index = shard_index)
            shards
        with
        | None ->
            (* The shard index was sampled within the bounds, so this case
               (the queried index is out of bounds) doesn't happen. *)
            assert false
        | Some shard ->
            Cryptobox.verify_shard t commitment shard shard_proofs.(shard_index))
        |> function
        | Ok () -> true
        | _ -> false)

  let test_shard_proof_invalid =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test invalid shard proof"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let* commitment = Cryptobox.commit t polynomial in
        let shards = Cryptobox.shards_from_polynomial t polynomial in
        let precomputation = Cryptobox.precompute_shards_proofs t in
        let shard_proofs =
          Cryptobox.prove_shards t ~precomputation ~polynomial
        in
        let shard_index = randrange params.number_of_shards in
        match
          Seq.find
            (fun ({index; _} : Cryptobox.shard) -> index = shard_index)
            shards
        with
        | None ->
            (* The shard index was sampled within the bounds, so this case
               (the queried index is out of bounds) doesn't happen. *)
            assert false
        | Some shard ->
            let altered_proof =
              Cryptobox.Internal_for_tests.alter_shard_proof
                shard_proofs.(shard_index)
            in
            Cryptobox.verify_shard t commitment shard altered_proof)
        |> function
        | Error `Invalid_shard -> true
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
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let* commitment = Cryptobox.commit t polynomial in
        let* commitment_proof = Cryptobox.prove_commitment t polynomial in
        return (Cryptobox.verify_commitment t commitment commitment_proof))
        |> function
        | Ok true -> true
        | _ -> false)

  let test_commitment_proof_invalid =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test invalid commitment proof"
      ~print:print_parameters
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        (let open Tezos_error_monad.Error_monad.Result_syntax in
        let* t = Cryptobox.make (get_cryptobox_parameters params) in
        let* polynomial = Cryptobox.polynomial_from_slot t params.slot in
        let* commitment = Cryptobox.commit t polynomial in
        let* commitment_proof = Cryptobox.prove_commitment t polynomial in
        let altered_proof =
          Cryptobox.Internal_for_tests.alter_commitment_proof commitment_proof
        in
        return (Cryptobox.verify_commitment t commitment altered_proof))
        |> function
        | Ok false -> true
        | _ -> false)

  let test_select_fft_domain =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test FFT domain selection"
      ~print:QCheck2.Print.int
      (QCheck2.Gen.int_range 1 10_000_000)
      (fun n ->
        let domain_size, _, _ =
          Cryptobox.Internal_for_tests.select_fft_domain n
        in
        domain_size >= n
        && List.fold_left
             (fun acc factor -> fst Z.(remove acc (of_int factor)))
             (Z.of_int domain_size)
             [2; 3; 11; 19]
           = Z.one)

  let path filename = project_root // Filename.dirname __FILE__ // filename

  let test_shard_proofs_load_from_file =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test shard proofs loading"
      ~print:print_parameters
      ~count:1
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        let open Error_monad.Result_syntax in
        (let* t =
           Result.map_error
             (function `Fail s -> [Error_monad.error_of_exn (Failure s)])
             (Cryptobox.make (get_cryptobox_parameters params))
         in
         let filename = path "test_precomputation" in
         let precomputation = Cryptobox.precompute_shards_proofs t in
         let* retrieved_precomputation =
           Lwt_main.run
             (let open Error_monad.Lwt_result_syntax in
             let* () =
               Cryptobox.save_precompute_shards_proofs precomputation ~filename
             in
             Cryptobox.load_precompute_shards_proofs ~filename)
         in
         Sys.remove filename ;
         return
           (Cryptobox.Internal_for_tests.precomputation_equal
              precomputation
              retrieved_precomputation))
        |> function
        | Ok true -> true
        | _ -> false)

  let test_dal_initialisation_twice_failure =
    let open QCheck2 in
    Test.make
      ~name:"DAL cryptobox: test DAL initialisation twice failure"
      ~print:print_parameters
      ~count:1
      generate_parameters
      (fun params ->
        init () ;
        assume (ensure_validity params) ;
        let config : Cryptobox.Config.t =
          {
            activated = true;
            use_mock_srs_for_testing = Some (get_cryptobox_parameters params);
          }
        in
        let find_srs_files () : (string * string) Error_monad.tzresult =
          Ok ("", "")
        in
        Lwt_main.run (Cryptobox.Config.init_dal ~find_srs_files config)
        |> function
        | Error [Cryptobox.Dal_initialisation_twice] -> true
        | _ -> false)

  let find_trusted_setup_files () =
    let config : Cryptobox.Config.t =
      {activated = true; use_mock_srs_for_testing = None}
    in
    Cryptobox.Internal_for_tests.reset_initialisation_parameters () ;
    let find_srs_files () : (string * string) Error_monad.tzresult =
      Ok (path "srs_zcash_g1_5", path "srs_zcash_g2_5")
    in
    Lwt_main.run
      (Cryptobox.Config.init_dal ~find_srs_files ~srs_size_log2:5 config)
    |> function
    | Ok () -> ()
    | Error _ -> assert false

  let find_trusted_setup_files_failure () =
    let config : Cryptobox.Config.t =
      {activated = true; use_mock_srs_for_testing = None}
    in
    Cryptobox.Internal_for_tests.reset_initialisation_parameters () ;
    let find_srs_files () : (string * string) Error_monad.tzresult =
      Ok (path "srs_zcash_g1_5", path "srs_zcash_g2_5")
    in
    Lwt_main.run
      (Cryptobox.Config.init_dal ~find_srs_files ~srs_size_log2:6 config)
    |> function
    | Error [Cryptobox.Failed_to_load_trusted_setup s] ->
        if Re.Str.(string_match (regexp "EOF") s 0) then () else assert false
    | _ -> assert false

  (* We can craft two slots whose commitments are equal for two different
     page sizes. *)
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4555
     This test should be adapted with the new constraints on the DAL parameters. *)
  let _test_collision_page_size () =
    let slot_size = 1 lsl 6 in
    init () ;
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

     let* commitment1 = Cryptobox.commit t1 p1 in
     let* commitment2 = Cryptobox.commit t2 p2 in
     Ok (Cryptobox.Commitment.equal commitment1 commitment2))
    |> function
    | Ok check -> assert check
    | _ -> assert false
end

let test =
  List.map
    (fun (test_name, test_func) ->
      Alcotest.test_case test_name `Quick test_func)
    [
      ("find_trusted_setup_files", Test.find_trusted_setup_files);
      ("find_trusted_setup_files_failure", Test.find_trusted_setup_files_failure)
      (*("test_collision_page_size", Test.test_collision_page_size);*);
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
            Test.test_page_proofs_invalid;
            Test.test_shard_proofs;
            Test.test_shard_proof_invalid;
            Test.test_commitment_proof;
            Test.test_commitment_proof_invalid;
            Test.test_polynomial_slot_conversions;
            Test.test_select_fft_domain;
            Test.test_shard_proofs_load_from_file;
            Test.test_dal_initialisation_twice_failure;
          ] );
    ]
