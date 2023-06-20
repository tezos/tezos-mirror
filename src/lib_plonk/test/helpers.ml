(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let with_seed (f : unit -> unit) =
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
    | Some v -> (
        try int_of_string v
        with _ ->
          failwith
            (Format.sprintf
               "Invalid random seed '%s'. Maybe you need to run '$ unset \
                RANDOM_SEED' in your terminal?"
               v))
  in
  Printf.printf "Random seed: %d\n" seed ;
  Random.init seed ;
  f ()

let output_buffer = ref stdout

let with_output_to_file (f : unit -> unit) =
  output_buffer := open_out "test.output" ;
  f () ;
  close_out !output_buffer

let set_seed s = Random.init s

let bigstring_of_file filename ~hash =
  let bs =
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0o440 in
    Bigarray.array1_of_genarray
    @@ Unix.map_file
         fd
         Bigarray.char
         Bigarray.c_layout
         false
         [|(* [-1] means read the whole file *) -1|]
  in
  let computed_hash =
    let st =
      Hacl_star.EverCrypt.Hash.init ~alg:Hacl_star.SharedDefs.HashDefs.BLAKE2b
    in
    let len = 48 (* works for both g1 and g2 *) in
    let msg = Bytes.create len in
    for i = 0 to (Bigstringaf.length bs / len) - 1 do
      Bigstringaf.blit_to_bytes bs ~src_off:(i * len) msg ~dst_off:0 ~len ;
      Hacl_star.EverCrypt.Hash.update ~st ~msg
    done ;
    Hacl_star.EverCrypt.Hash.finish ~st
  in
  let hash = Hex.to_bytes (`Hex hash) in
  if computed_hash <> hash then failwith ("Invalid hash: " ^ filename) ;
  bs

let load_real_srs prefix =
  let open Octez_bls12_381_polynomial.Bls12_381_polynomial.Srs in
  let ( // ) s1 s2 = s1 ^ "/" ^ s2 in
  ( ( Srs_g1.of_bigstring
        (bigstring_of_file
           (prefix // "srs_filecoin_g1_21")
           ~hash:
             "25281025229b67eed4bcf4451dca0e5ac3fc6c5bf5934f54449105a7feba8049cf0e9f390f23528d5f860387c07a6b374f2ef6dad6fd73b051e4cc4699974738")
        ~len:(1 lsl 21)
      |> Result.get_ok,
      Srs_g2.of_bigstring
        (bigstring_of_file
           (prefix // "srs_filecoin_g2_1")
           ~hash:
             "ee034f5e6d3d9dc2097861ffb278438573f0a9c84afd6806a5b53b158b0e6e6847dc8a84e1b01c3c161ed4593816d59bf4817c797ffad6fffbea143987e340a4")
        ~len:2
      |> Result.get_ok ),
    ( Srs_g1.of_bigstring
        (bigstring_of_file
           (prefix // "srs_zcash_g1_1")
           ~hash:
             "435fd5b85e1e3271c8e241b25da799df3e312e67d06c9e009fb967a8d597ed37897047aa48659526ce10db857ee02c64e6f577ef80485d34d7506fff40a901b4")
        ~len:2
      |> Result.get_ok,
      Srs_g2.of_bigstring
        (bigstring_of_file
           (prefix // "srs_zcash_g2_10")
           ~hash:
             "39ebc126d18caade1bee9294124292a089746441cee6b80efb37f0a1e6a37e8acabfa3c0a3b0c6ce15ea3a46a5f5373be222bb1a54b332d43e25f489c66dec49")
        ~len:(1 lsl 10)
      |> Result.get_ok ) )

let make_fake_srs () =
  let open Octez_bls12_381_polynomial.Bls12_381_polynomial in
  (Srs.generate_insecure 14 1, Srs.generate_insecure 1 14)

let srs =
  match Sys.getenv_opt "SRS_DIR" with
  | None -> make_fake_srs ()
  | Some prefix -> load_real_srs prefix

let rec repeat n f () =
  if n > 0 then (
    f () ;
    repeat (n - 1) f ())

let must_fail f =
  let exception Local in
  try
    (try f () with _ -> raise Local) ;
    assert false
  with
  | Local -> ()
  | _ -> assert false

let string_of_bytes bytes =
  if bytes <= 1024. then Printf.sprintf "%3.2f B " bytes
  else
    let kilobytes = bytes /. 1024. in
    if kilobytes <= 1024. then Printf.sprintf "%3.2f KB" kilobytes
    else
      let megabytes = kilobytes /. 1024. in
      if megabytes <= 1024. then Printf.sprintf "%3.2f MB" megabytes
      else
        let gigabytes = megabytes /. 1024. in
        Printf.sprintf "%.2f GB" gigabytes

let hash_of_repr t v =
  let serialized_bytes =
    Bytes.of_string @@ Repr.(unstage @@ to_bin_string t) v
  in
  Hacl_star.Hacl.Blake2b_32.hash serialized_bytes 32 |> Hex.of_bytes |> Hex.show

let get_input_com_secrets private_inputs input_com_sizes =
  let secrets, _ =
    List.fold_left
      (fun (secrets, l) size ->
        (Array.sub private_inputs l size :: secrets, l + size))
      ([], 0)
      input_com_sizes
  in
  List.rev secrets

module Time = struct
  type data = {n : int; sum : float; sum_squares : float; last : float}

  let str_time = ref ""

  let zero_data = {n = 0; sum = 0.; sum_squares = 0.; last = 0.}

  let setup = ref zero_data

  let prove = ref zero_data

  let verify = ref zero_data

  let reset () =
    setup := zero_data ;
    prove := zero_data ;
    verify := zero_data

  let update data time =
    let sum = time +. !data.sum in
    let sum_squares = (time *. time) +. !data.sum_squares in
    data := {n = !data.n + 1; sum; sum_squares; last = time}

  let mean data = !data.sum /. float_of_int !data.n

  let var data =
    let m = mean data in
    (!data.sum_squares /. float_of_int !data.n) -. (m *. m)

  let std data = sqrt (var data)

  let string_of_time t =
    if t > 60. then Printf.sprintf "%3.2f m " (t /. 60.)
    else if t > 1. then Printf.sprintf "%3.2f s " t
    else if t > 0.001 then Printf.sprintf "%3.2f ms" (t *. 1_000.)
    else Printf.sprintf "%3.0f µs" (t *. 1_000_000.)

  let time description f =
    Gc.full_major () ;
    let st1 = Gc.stat () in
    let start = Unix.gettimeofday () in
    let res = f () in
    let stop = Unix.gettimeofday () in
    let d = stop -. start in
    let () =
      match description with
      | "setup" -> update setup d
      | "prove" -> update prove d
      | "verify" -> update verify d
      | _ -> ()
    in
    let t_str = string_of_time d in
    let st2 = Gc.stat () in
    let allocations =
      (st2.minor_words +. st2.major_words -. st2.promoted_words
      -. (st1.minor_words +. st1.major_words -. st1.promoted_words))
      *. 8.
      |> string_of_bytes
    in
    let top_heap_words =
      if st2.top_heap_words > st1.top_heap_words then
        st2.top_heap_words |> float_of_int |> Float.mul 8. |> string_of_float
      else "?"
    in
    Printf.printf
      "%-8s: Time: %8s Allocations %6s MaxHeap: %s\n%!"
      description
      t_str
      allocations
      top_heap_words ;
    res

  let reset_str () = str_time := ""

  let update_str ?header () =
    let header =
      match header with None -> "" | Some header -> header ^ "\n"
    in
    str_time :=
      !str_time
      ^ Printf.sprintf
          "%s%f\n%f\n%f\n"
          header
          !setup.last
          !prove.last
          !verify.last

  let print_time_in_file file =
    let oc = open_out file in
    Printf.fprintf oc "%s" !str_time ;
    close_out oc

  let bench_test_circuit ~nb_rep func () =
    reset () ;
    repeat nb_rep func () ;
    assert (nb_rep = !setup.n && nb_rep = !prove.n && nb_rep = !verify.n) ;
    Printf.printf
      "\nTimes over %d repetitions (95%% confidence interval):\n\n"
      nb_rep ;
    let pp = string_of_time in
    let z = 1.96 in
    Printf.printf "  Setup : %s ± %s\n" (pp (mean setup)) (pp (z *. std setup)) ;
    Printf.printf "  Prove : %s ± %s\n" (pp (mean prove)) (pp (z *. std prove)) ;
    Printf.printf
      "  Verify: %s ± %s\n"
      (pp (mean verify))
      (pp (z *. std verify)) ;
    Printf.printf "\n"

  let time_if_verbose verbose description f =
    if verbose then time description f else f ()
end

module Make (Main : Plonk.Main_protocol.S) = struct
  open Plonk.Circuit

  module Singleton = struct
    include Main

    let setup ~zero_knowledge circuit ~srs =
      let circuits_map = Plonk.SMap.singleton "" (circuit, 1) in
      Main.setup ~zero_knowledge circuits_map ~srs

    let prove pp ~(inputs : circuit_prover_input list) =
      let inputs = Plonk.SMap.singleton "" inputs in
      Main.prove pp ~inputs

    let verify pp ~inputs proof =
      let inputs = Plonk.SMap.singleton "" inputs in
      Main.verify pp ~inputs proof
  end

  let multi_input_commit pp input_commitment_secrets =
    List.fold_left
      (fun (cmts, shift) secret ->
        (Main.input_commit ~shift pp secret :: cmts, shift + Array.length secret))
      ([], 0)
      input_commitment_secrets
    |> fst |> List.rev

  let print_info name zero_knowledge proof pp_prover pp_verifier =
    let proof_size =
      Data_encoding.Binary.length Main.proof_encoding proof |> Float.of_int
    in
    let proof_hash = hash_of_repr Main.proof_t proof in
    let prover_pp_hash =
      hash_of_repr Main.prover_public_parameters_t pp_prover
    in
    let verifier_pp_hash =
      hash_of_repr Main.verifier_public_parameters_t pp_verifier
    in
    Printf.fprintf
      !output_buffer
      "%s:\n\
       Proof size: %s\n\
       Proof hash: %s\n\
       Prover_pp hash: %s\n\
       Verifier_pp hash: %s\n\n"
      (if zero_knowledge then "zk-" ^ name else name)
      (string_of_bytes proof_size)
      proof_hash
      prover_pp_hash
      verifier_pp_hash

  let make_secret pp_prover input_com_sizes witness =
    let open Main in
    let input_com_secrets = get_input_com_secrets witness input_com_sizes in
    let input_commitments = multi_input_commit pp_prover input_com_secrets in
    {witness; input_commitments}

  let test_circuits ~name ?(zero_knowledge = false) ?(outcome = Cases.Valid)
      ?(verbose = false) circuit_map private_inputs =
    let time_if_verbose verbose description f =
      if verbose then Time.time description f else f ()
    in
    if verbose then
      Plonk.SMap.iter
        (fun cname (circuit, _n) ->
          Format.printf
            "circuit '%s' has %d constraints\n"
            cname
            circuit.circuit_size)
        circuit_map ;
    let pp_prover, pp_verifier =
      time_if_verbose verbose "setup" (fun () ->
          Main.setup ~zero_knowledge circuit_map ~srs)
    in
    let prover_inputs =
      Plonk.SMap.mapi
        (fun c_name ->
          let c = fst (Plonk.SMap.find c_name circuit_map) in
          List.map (make_secret pp_prover c.input_com_sizes))
        private_inputs
    in
    let verifier_inputs = Main.to_verifier_inputs pp_prover prover_inputs in
    match outcome with
    | Valid -> (
        let proof =
          time_if_verbose verbose "prove" (fun () ->
              Main.prove pp_prover ~inputs:prover_inputs)
        in
        Gc.full_major () ;
        let v =
          time_if_verbose verbose "verify" (fun () ->
              Main.verify pp_verifier ~inputs:verifier_inputs proof)
        in
        assert v ;
        print_info name zero_knowledge proof pp_prover pp_verifier ;
        (* Test that verification fails if we mutate public inputs *)
        match Main.Internal_for_tests.mutate_vi verifier_inputs with
        | None -> () (* No public inputs *)
        | Some verifier_inputs ->
            let v = Main.verify pp_verifier ~inputs:verifier_inputs proof in
            assert (not v))
    | _ -> (
        try
          let proof = Main.prove pp_prover ~inputs:prover_inputs in
          assert (not (Main.verify pp_verifier ~inputs:verifier_inputs proof))
        with
        | Main.Rest_not_null _ ->
            if outcome = Proof_error then ()
            else raise (Invalid_argument "Proving error: incorrect witness")
        | Main.Entry_not_in_table _ ->
            if outcome = Lookup_error then ()
            else raise (Invalid_argument "Proving error: incorrect lookup")
        | e -> raise e)

  (* generator must be n-th root of unity
     n must be in the form 2^i
     for k number of gates
     a_c, b_c, c_c, ql, qr, qo, qm, qc must be lists of length k
     x is an array of length m = 3+2(k-1)
     l between 0 and m-1, l first parameters will be taken as public inputs
     n = k+l
     valid_proof is true if the proof is expected valid, false if it must fail
     if verbose print run times when valid_proof is true
  *)
  let test_circuit ~name ?zero_knowledge ?outcome ?verbose circuit
      private_inputs =
    let circuit_map = Plonk.SMap.singleton name (circuit, 1) in
    let inputs = Plonk.SMap.singleton name [private_inputs] in
    test_circuits ~name ?zero_knowledge ?outcome ?verbose circuit_map inputs

  let run_test_case ~zero_knowledge ?verbose
      Cases.{name; circuit; witness; outcome} () =
    test_circuit ~name ~zero_knowledge ~outcome ?verbose circuit witness

  let test_aggregated_cases ?(prefix = "") cases =
    let name, circuits_map, inputs_map, outcome =
      Cases.aggregate_cases ~prefix cases
    in
    ( name,
      fun ~zero_knowledge () ->
        test_circuits ~name ~zero_knowledge circuits_map inputs_map ~outcome )
end

module Plompiler_Helpers = struct
  open Plompiler
  module CS = Plonk.Circuit

  type test_info = {valid : bool; name : string; flamegraph : bool}

  module type Test = functor (L : LIB) -> sig
    open L

    val tests : ((unit -> unit repr t) * test_info) list
  end

  let to_test ?plonk ?(optimize = true) test () =
    let module Test = (val test : Test) in
    let circuits =
      let module E1 = Test (LibCircuit) in
      E1.tests
    in
    let results =
      let module E2 = Test (LibResult) in
      E2.tests
    in
    let run_one_test (circuit, info) (result, _) =
      let cs = LibCircuit.(get_cs (circuit ())) in
      let initial, _ = LibCircuit.(get_inputs (circuit ())) in
      if info.flamegraph then
        Plompiler.Utils.dump_label_traces (info.name ^ "_flamegraph") cs.cs ;
      let pi =
        try Solver.solve cs.solver initial |> fun x -> Some x with _ -> None
      in
      match pi with
      | None -> assert (not info.valid)
      | Some private_inputs ->
          (* Printf.printf
               "Trace:\n%s\n%s\n"
               (String.concat
                  ","
                  (List.init (Array.length private_inputs) string_of_int))
               (String.concat
                  ","
                  (List.map S.string_of_scalar (Array.to_list private_inputs))) ;
             Printf.printf "CS:\n%s\n" (CS.to_string cs) ; *)
          if not info.valid then assert (not @@ CS.sat cs private_inputs)
          else (
            Printf.fprintf
              !output_buffer
              "%s:\nConstraints: %d\n\n"
              info.name
              Array.(concat cs.cs |> length) ;

            let res = LibResult.get_result (result ()) in
            let serialized_res = LibResult.serialize res in
            let out_size = Array.length serialized_res in
            let trace_out =
              Array.sub
                private_inputs
                (Array.length private_inputs - out_size)
                out_size
            in

            assert (CS.sat cs private_inputs) ;
            let cs, private_inputs =
              if optimize then (
                let cs = LibCircuit.(get_cs ~optimize (circuit ())) in
                Printf.fprintf
                  !output_buffer
                  "%s_optimized:\nConstraints: %d\n\n"
                  info.name
                  Array.(concat cs.cs |> length) ;
                let private_inputs = Solver.solve cs.solver initial in
                assert (CS.sat cs private_inputs) ;

                (cs, private_inputs))
              else (cs, private_inputs)
            in
            if info.flamegraph then
              Plompiler.Utils.dump_label_traces
                (info.name ^ "_opt_flamegraph")
                cs.cs ;
            (* Compare values obtained from Result and Circuit interpreters *)
            assert (Array.for_all2 S.( = ) serialized_res trace_out) ;
            match plonk with
            | None -> ()
            | Some plonk ->
                let module Main = (val plonk : Plonk.Main_protocol.S) in
                let open Make (Main) in
                let circuit = CS.to_plonk cs in
                test_circuit
                  ~name:info.name
                  ~zero_knowledge:false
                  ~outcome:Valid
                  circuit
                  private_inputs)
    in
    List.iter2 run_one_test circuits results

  module Utils (L : LIB) = struct
    open L

    (* We test equality for Num, Bool, lists, tuples *)
    let test_equal x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      assert_equal x z

    let si x = Input.scalar @@ S.of_string @@ string_of_int x

    let test ~valid ?(name = "test") ?(flamegraph = false) x =
      (x, {valid; name; flamegraph})
  end
end

include Plompiler_Helpers
