module PI = Aplonk.Pi_parameters.Rollup_example

module PIs = struct
  let get_pi_module _ = (module PI : Aplonk.Pi_parameters.CircuitPI)
end

module Plonk_Helpers = Plonk_test.Helpers.Make (Plonk.Main_protocol)
module APlonk_Helpers = Plonk_test.Helpers.Make (Aplonk.Main_protocol.Make (PIs))
module Cases = Plonk_test.Cases

let file = "./benchmarks"

let bench_big_circuit nb_constraints_l nb_proofs_l () =
  Gc.full_major () ;
  let nb_proofs = 1 lsl nb_proofs_l in
  Printf.printf
    "\n\n\
     Number of gates : %d (= 2^%d - 1)\n\
     Number of public inputs : %d\n\
     Number of proofs : %d (= 2^%d)\n"
    ((1 lsl nb_constraints_l) - 1)
    nb_constraints_l
    PI.nb_inner
    nb_proofs
    nb_proofs_l ;
  let time = Unix.gettimeofday () in
  let case_list =
    Cases.Big_circuit.make
      ~nb_proofs
      ~public_input_size:PI.nb_inner
      ~k:nb_constraints_l
  in
  let name = "Big Circuit" in
  let _, circuits_map, inputs_map, outcome = Cases.aggregate_cases case_list in
  Printf.printf
    "\ntime build circuit & witness : %f s."
    (Unix.gettimeofday () -. time) ;

  Gc.full_major () ;

  print_endline "\n\n\nPLONK" ;
  Plonk_Helpers.test_circuits
    ~name
    ~outcome
    ~verbose:true
    circuits_map
    inputs_map ;
  Plonk_test.Helpers.Time.update_str ~header:(string_of_int nb_proofs_l) () ;

  Gc.full_major () ;

  print_endline "\naPLONK" ;
  APlonk_Helpers.test_circuits
    ~name
    ~outcome
    ~verbose:true
    circuits_map
    inputs_map ;
  Plonk_test.Helpers.Time.update_str ()

(* Run benches of Plonk(KZG) & aPlonK (nb_proofs_end - nb_proofs_start + 1)
   times, with a nb_proofs from nb_proofs_start until nb_proofs_end (included).
   Prints the execution time in benchmark file, as :
      nb_proofs
      time setup PlonK
      time prove PlonK
      time verify PlonK
      time setup aPlonK
      time prove aPlonK
      time verify aPlonK
*)
let benches_big_circuit nb_constraints nb_proofs_start nb_proofs_end () =
  Plonk_test.Helpers.Time.reset_str () ;
  if nb_proofs_start > nb_proofs_end then
    failwith
      "Zero benchmarks are launched as the second argument is bigger that the \
       third." ;
  try
    for i = nb_proofs_start to nb_proofs_end do
      print_endline "\n\n-------------------------------------" ;
      let () = bench_big_circuit nb_constraints i () in
      ()
    done ;
    Plonk_test.Helpers.Time.print_time_in_file file
  with e ->
    Plonk_test.Helpers.Time.print_time_in_file file ;
    raise e

let () =
  let nb_constraints_l = int_of_string Sys.argv.(1) in
  let nb_proofs_start = int_of_string Sys.argv.(2) in
  let nb_proofs_end = int_of_string Sys.argv.(3) in
  benches_big_circuit nb_constraints_l nb_proofs_start nb_proofs_end ()
