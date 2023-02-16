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
  let init parameters =
    Cryptobox.Internal_for_tests.parameters_initialisation parameters
    |> Cryptobox.Internal_for_tests.load_parameters

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

  let params_gen =
    let open QCheck2.Gen in
    let* redundancy_factor_log2 = int_range 1 4 in
    let* slot_size_log2 = int_range 0 11 in
    let* page_size_log2 = int_range 0 15 in
    let* number_of_shards_log2 = int_range 0 19 in
    map
      (fun (slot_size, page_size, redundancy_factor, number_of_shards) :
           Cryptobox.parameters ->
        {slot_size; page_size; redundancy_factor; number_of_shards})
      (tup4
         (return (1 lsl slot_size_log2))
         (return (1 lsl page_size_log2))
         (return (1 lsl redundancy_factor_log2))
         (return (1 lsl number_of_shards_log2)))

  let print =
    QCheck2.Print.(
      contramap
        (fun ({slot_size; page_size; redundancy_factor; number_of_shards} :
               Cryptobox.parameters) ->
          (slot_size, page_size, redundancy_factor, number_of_shards))
        (tup4 int int int int))

  let is_in_acceptable_errors error_string =
    List.map
      (fun s -> Re.Str.(string_match (regexp s) error_string 0))
      [
        "For the given parameters, the minimum number of shards is";
        "For the given parameters, the maximum number of shards is";
        "Slot size is expected to be a power of 2.";
        "Page size is expected to be a power of 2.";
        "Page size is expected to be greater than '32' and strictly less than";
        "The number of shards must divide";
      ]
    |> List.mem true

  (* Tests that with a fraction 1/redundancy_factor of the shards
     the decoding succeeds. *)
  let test_erasure_code =
    QCheck2.Test.make
      ~name:"DAL cryptobox: test erasure code"
      ~print
      params_gen
      (fun
        ({redundancy_factor; number_of_shards; slot_size; _} as params :
          Cryptobox.parameters)
      ->
        let msg_size = slot_size / 16 in
        let slot, msg = make_slot ~size:slot_size ~padding_threshold:msg_size in
        init params ;
        let open Tezos_error_monad.Error_monad.Result_syntax in
        (let* t = Cryptobox.make params in
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
           Bytes.sub (Cryptobox.polynomial_to_slot t decoded_slot) 0 msg_size
         in
         return decoded_msg)
        |> function
        | Ok decoded_msg -> Bytes.equal msg decoded_msg
        | Error (`Fail s) when is_in_acceptable_errors s -> true
        | Error
            ( `Fail _ | `Not_enough_shards _ | `Slot_wrong_size _
            | `Shard_index_out_of_range _ | `Invalid_shard_length _ ) ->
            false)

  let test_erasure_code_failure_not_enough_shards () =
    let redundancy_factor = 2 in
    let number_of_shards = 1 lsl 4 in
    let slot_size = 1 lsl 10 in
    let msg_size = slot_size / 16 in
    let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
    let params =
      ({redundancy_factor; slot_size; page_size = 1 lsl 5; number_of_shards}
        : Cryptobox.parameters)
    in
    init params ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* t = Cryptobox.make params in
     let* p = Cryptobox.polynomial_from_slot t slot in
     let enc_shards = Cryptobox.shards_from_polynomial t p in
     let c_indices =
       random_indices
         (number_of_shards - 1)
         ((number_of_shards / redundancy_factor) - 1)
     in
     let c =
       Seq.filter
         (fun ({index; _} : Cryptobox.shard) -> Array.mem index c_indices)
         enc_shards
     in
     Cryptobox.polynomial_from_shards t c)
    |> function
    | Error (`Not_enough_shards _) -> assert true
    | Ok _
    | Error
        ( `Fail _ | `Slot_wrong_size _ | `Shard_index_out_of_range _
        | `Invalid_shard_length _ ) ->
        assert false

  let test_erasure_code_failure_out_of_range () =
    let redundancy_factor = 2 in
    let number_of_shards = 1 lsl 4 in
    let slot_size = 1 lsl 10 in
    let msg_size = slot_size / 16 in
    let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
    let params =
      ({redundancy_factor; slot_size; page_size = 1 lsl 5; number_of_shards}
        : Cryptobox.parameters)
    in
    init params ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* t = Cryptobox.make params in
     let* p = Cryptobox.polynomial_from_slot t slot in
     let enc_shards = Cryptobox.shards_from_polynomial t p in
     let c =
       Seq.take (number_of_shards / redundancy_factor) enc_shards
       |> Seq.map (fun ({index; share} : Cryptobox.shard) : Cryptobox.shard ->
              {index = index + 1000; share})
     in
     Cryptobox.polynomial_from_shards t c)
    |> function
    | Error (`Shard_index_out_of_range _) -> assert true
    | Ok _ -> assert false
    | Error
        ( `Fail _ | `Slot_wrong_size _ | `Not_enough_shards _
        | `Invalid_shard_length _ ) ->
        assert false

  (* Checking the shards' length to avoid out-of-bounds array accesses.
     The function [polynomial_from_shards] returns an error if the shards
     don't have the expected length. This could happen if the shards were
     produced with a different set of parameters than the ones used by
     [polynomial_from_shards].

     Here, the number of shards is 16 with the first set of parameters t1
     versus 32 for the second set of paramaters t2. *)
  let test_erasure_code_failure_invalid_shard_length () =
    let redundancy_factor = 2 in
    let slot_size = 1 lsl 10 in
    let params =
      ({
         redundancy_factor;
         slot_size;
         page_size = 1 lsl 5;
         number_of_shards = 1 lsl 4;
       }
        : Cryptobox.parameters)
    in
    init params ;
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* t1 = Cryptobox.make params in
     let* t2 =
       Cryptobox.make
         {
           redundancy_factor;
           slot_size;
           page_size = 1 lsl 5;
           number_of_shards = 1 lsl 5;
         }
     in
     let enc_shards = Cryptobox.Internal_for_tests.make_dummy_shards t1 in
     Cryptobox.polynomial_from_shards t2 enc_shards)
    |> function
    | Error (`Invalid_shard_length _) -> assert true
    | _ -> assert false

  (* Check that for any slot,
     [polynomial_to_slot (polynomial_from_slot slot) = slot]. *)
  let test_polynomial_slot_conversions =
    QCheck2.Test.make
      ~name:"DAL cryptobox: test polynomial-slot conversions"
      ~print
      params_gen
      (fun ({slot_size; _} as params : Cryptobox.parameters) ->
        let slot, _ = make_slot ~size:slot_size ~padding_threshold:slot_size in
        init params ;
        let open Tezos_error_monad.Error_monad.Result_syntax in
        (let* t = Cryptobox.make params in
         let* p = Cryptobox.polynomial_from_slot t slot in
         let slot_res = Cryptobox.(polynomial_to_slot t p) in
         Ok (Bytes.equal slot_res slot))
        |> function
        | Ok check -> check
        | Error (`Fail s) when is_in_acceptable_errors s -> true
        | Error _ -> false)

  (* Tests that a page is included in a slot. *)
  let test_page_proofs =
    QCheck2.Test.make
      ~name:"DAL cryptobox: test page proofs"
      ~print
      params_gen
      (fun ({slot_size; page_size; _} as params : Cryptobox.parameters) ->
        let msg_size = slot_size / 16 in
        let number_of_pages = slot_size / page_size in
        let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
        init params ;
        let open Tezos_error_monad.Error_monad.Result_syntax in
        (let* t = Cryptobox.make params in
         let* p = Cryptobox.polynomial_from_slot t slot in
         let cm = Cryptobox.commit t p in
         let page_index = Random.int number_of_pages in
         let* pi = Cryptobox.prove_page t p page_index in
         let page = Bytes.sub slot (page_index * page_size) page_size in
         Cryptobox.verify_page t cm ~page_index page pi)
        |> function
        | Ok () -> true
        | Error (`Fail s) when is_in_acceptable_errors s -> true
        | Error
            ( `Fail _ | `Slot_wrong_size _ | `Page_length_mismatch
            | `Segment_index_out_of_range | `Invalid_page ) ->
            false)

  (* Tests that a shard comes from the erasure-encoded slot. *)
  let test_shard_proofs =
    QCheck2.Test.make
      ~name:"DAL cryptobox: test shard proofs"
      ~print
      params_gen
      (fun ({number_of_shards; slot_size; _} as params : Cryptobox.parameters)
      ->
        let msg_size = slot_size / 16 in
        let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
        init params ;
        let open Tezos_error_monad.Error_monad.Result_syntax in
        (let* t = Cryptobox.make params in
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
         | None -> (* The shard index is within the bounds *) assert false
         | Some shard ->
             Cryptobox.verify_shard t cm shard shard_proofs.(shard_index))
        |> function
        | Ok () -> true
        | Error (`Fail s) when is_in_acceptable_errors s -> true
        | Error
            ( `Fail _ | `Slot_wrong_size _ | `Shard_index_out_of_range _
            | `Invalid_shard ) ->
            false)

  (* Tests that the slot behind the commitment has its size bounded
     by [t.slot_size]. *)
  let test_commitment_proof =
    QCheck2.Test.make
      ~name:"DAL cryptobox: test commitment proof"
      ~print
      params_gen
      (fun ({slot_size; _} as params : Cryptobox.parameters) ->
        let msg_size = slot_size / 16 in
        let slot, _ = make_slot ~size:slot_size ~padding_threshold:msg_size in
        init params ;
        let open Tezos_error_monad.Error_monad.Result_syntax in
        (let* t = Cryptobox.make params in
         let* p = Cryptobox.polynomial_from_slot t slot in
         Ok (t, p))
        |> function
        | Ok (t, p) ->
            let cm = Cryptobox.commit t p in
            let pi = Cryptobox.prove_commitment t p in
            let check = Cryptobox.verify_commitment t cm pi in
            check
        | Error (`Fail s) when is_in_acceptable_errors s -> true
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
    [
      ( "erasure code failure: not enough shards",
        Test.test_erasure_code_failure_not_enough_shards );
      ( "erasure code failure: shard indices out of range",
        Test.test_erasure_code_failure_out_of_range );
      ( "erasure code failure: invalid shard length",
        Test.test_erasure_code_failure_invalid_shard_length )
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
            Test.test_page_proofs;
            Test.test_shard_proofs;
            Test.test_commitment_proof;
            Test.test_polynomial_slot_conversions;
          ] );
    ]
