open Flextesa
open Internal_pervasives
open Console

let run state node_exec client_exec () =
  let* nodes, _protocol =
    Test_scenario.network_with_protocol ~size:2 state ~node_exec ~client_exec
  in
  match nodes with
  | [] | [_] | _ :: _ :: _ :: _ -> assert false
  | [n1; n2] ->
      let c1 = Tezos_client.of_node ~exec:client_exec n1 in
      let c2 = Tezos_client.of_node ~exec:client_exec n2 in
      (* TODO: helpers for
         - injecting an op
         - displaying the mempool
         - setting filter plugin config

         TODO: non-interactive test for propagation
         TODO: commands for interactive use *)
      Stdlib.ignore c1 ;
      Stdlib.ignore c2 ;
      let commands = Interactive_test.Commands.all_defaults state ~nodes in
      let* () = Prompt.command state ~commands in
      Running_processes.wait_all state

let cmd () =
  let open Cmdliner in
  let open Term in
  let pp_error = Test_command_line.Common_errors.pp in
  let base_state =
    Test_command_line.Command_making_state.make
      ~application_name:"Flextesa"
      ~command_name:"prevalidation"
      ()
  in
  let term =
    const (fun bnod bcli state ->
        Test_command_line.Run_command.or_hard_fail
          state
          ~pp_error
          (run state bnod bcli))
    $ Tezos_executable.cli_term base_state `Node "tezos"
    $ Tezos_executable.cli_term base_state `Client "tezos"
    $ Test_command_line.cli_state ~name:"prevalidation" ()
  in
  let info = Cmd.info ~doc:"Work-in-progress." "prevalidation" in
  Cmd.v info term
