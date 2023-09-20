open Distribution_helpers

module DP = DP_Meta ()

module Runner = Master_runner.Make (DP.D)

open Helpers (DP)

(* Meta-PlonK.(setup, prove & verify) on the circuit with the given witness
   circuit is a function that takes an int (nb of public inputs) & returns a
   CS.t with its solver witness is a function that takes a scalar pi_start &
   a solver and returns a witness that starts with pi_starts with its second
   element. *)

let recompute_setup nb_rep =
  let circuits_map, _ = Circuit_Builder.base nb_rep circuit_size in
  let b =
    DP.MP.setup ~zero_knowledge:false circuits_map ~srs
    |> fst |> DP.get_distributed_pp
  in
  let oc = open_out DP.pp_file in
  output_bytes oc b ;
  close_out oc

let () =
  let args = Sys.argv in
  let ip = args.(1) in
  let port = int_of_string args.(2) in
  let nodes = parse_nodes args.(3) in
  let nb_rep = List.length nodes in
  if Option.value ~default:"0" (Sys.getenv_opt "RECOMPUTE") = "1" then
    recompute_setup nb_rep ;
  run_master ~self_node:Runner.{name = "master"; ip; port} ~nodes ()
