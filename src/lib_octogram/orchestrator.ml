(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Jingoo.Jg_types

let color = Log.Color.FG.cyan

let rec tvalue_to_string_opt tvalue =
  let open Jingoo.Jg_types in
  match tvalue with
  | Tstr s -> Some s
  | Tint i -> Some (string_of_int i)
  | Tbool b -> Some (string_of_bool b)
  | Tlist l -> (
      try
        Some
          (Format.sprintf
             "[%s]"
             (List.map (fun x -> Option.get (tvalue_to_string_opt x)) l
             |> String.concat ", "))
      with _ -> None)
  | _ -> None

let run_templates_and_update_vars ~vars ~agent ~res ~re ~item
    (updates : Global_variables.update list) =
  List.fold_left
    (fun vars u ->
      Global_variables.update
        vars
        (Template.expand_update_var ~vars ~agent ~res ~re ~item u))
    vars
    updates

let run_job_body ~state ~agent ~re ~item ~vars_updates job_name
    (body : Uri.global_uri Job.body) =
  let agent_name = Remote_agent.name agent in
  let starting_time = Tezos_base.Time.System.now () in
  Log.info
    ~color:(Remote_agent.color agent)
    "[%s] %s%a"
    (agent_name :> string)
    job_name
    Format.(pp_print_option (fun fmt v -> fprintf fmt " (item=%s)" v))
    (tvalue_to_string_opt item) ;

  let* res =
    match body with
    | Copy {source; destination} ->
        assert (Filename.is_relative destination) ;
        let destination = Remote_agent.scope agent destination in
        let* () =
          Helpers.mkdir
            ~p:true
            ~runner:(Remote_agent.runner agent)
            (Filename.dirname destination)
        in
        let* () =
          Helpers.deploy
            ~r:true
            ~for_runner:(Remote_agent.runner agent)
            source
            destination
        in
        return Tnull
    | Remote_procedure {procedure} ->
        let (Packed procedure) =
          Remote_procedure.resolve_global_uris
            ~self:(Remote_agent.name agent)
            (Orchestrator_state.uri_resolver state)
            procedure
        in
        let* handler = Remote_agent.start_request agent procedure in
        let* res = Remote_agent.wait_for_request agent handler in
        let res = Remote_procedure.tvalue_of_response procedure res in
        return res
  in

  let ending_time = Tezos_base.Time.System.now () in
  Log.info
    ~color:(Remote_agent.color agent)
    "[%s] ✓ %s%a (%a)"
    (agent_name :> string)
    job_name
    Format.(pp_print_option (fun fmt v -> fprintf fmt " (item=%s)" v))
    (tvalue_to_string_opt item)
    Ptime.Span.pp
    (Ptime.diff ending_time starting_time) ;

  Orchestrator_state.with_global_variables state (fun vars ->
      run_templates_and_update_vars
        ~vars
        ~agent:(Remote_agent.to_tvalue agent)
        ~res
        ~re
        ~item
        vars_updates) ;

  unit

let enumerate_seq_items ~vars ~agent ~re item_defs =
  Seq.concat
    (List.to_seq (List.map (Job.expand_item ~vars ~agent ~re) item_defs))

let rec seq_prod = function
  | x :: rst ->
      Seq.concat_map
        (fun x ->
          let y = seq_prod rst in
          Seq.map (function Tlist r -> Tlist (x :: r) | _ -> assert false) y)
        x
  | [] -> Seq.return (Tlist [])

let enumerate_items ~vars ~agent ~re with_items =
  match with_items with
  | Job.(Seq item_defs) -> enumerate_seq_items ~vars ~agent ~re item_defs
  | Prod with_items_defs ->
      List.map (enumerate_seq_items ~vars ~agent ~re) with_items_defs
      |> seq_prod

let run_job ~state ~agent ~re (job : string Job.t) =
  let vars = Orchestrator_state.get_global_variables state in
  let agent_tvalue = Remote_agent.to_tvalue agent in
  let items =
    match job.header.with_items with
    | Some with_items ->
        enumerate_items ~vars ~agent:agent_tvalue ~re with_items
    | None -> Seq.return Tnull
  in
  Execution_params.traverse
    job.header.mode
    (fun item ->
      let body =
        Job.expand_body
          ~self:(Remote_agent.name agent)
          ~vars
          ~agent:agent_tvalue
          ~re
          ~item
          job.body
      in
      run_job_body
        ~state
        ~agent
        ~re
        ~item
        ~vars_updates:job.header.vars_updates
        job.header.name
        body)
    items

let run_jobs ~state ~agent ~re mode jobs =
  Execution_params.traverse
    mode
    (fun job -> run_job ~state ~agent ~re job)
    (List.to_seq jobs)

module Re = struct
  let rex r = Re.compile (Re.Perl.re r)

  let ( =~ ) s r = Re.execp r s

  let matches r s =
    let groups = Re.all r s in
    List.to_seq groups
    |> Seq.map (fun group -> Re.Group.all group |> Array.to_seq)
    |> Seq.concat
    |> Seq.map (fun x -> Tstr x)
    |> Array.of_seq
end

let run_stage ~state (stage : Stage.t) =
  let open Re in
  let vars = Orchestrator_state.get_global_variables state in
  Log.info ~color "[orchestrator] %s" stage.name ;
  let agents_regex =
    List.map
      (fun regex ->
        let regex = Template.expand_agent ~vars regex in
        rex regex)
      stage.with_agents
  in

  let* () =
    if stage.ask_confirmation then (
      Log.info ~color "Waiting for user input (enter newline to continue)" ;
      let* _ = Lwt_io.(read_line stdin) in
      unit)
    else unit
  in

  Orchestrator_state.iter_agents stage.run_agents state (fun agent ->
      let agent_name = Remote_agent.name agent in
      let re =
        List.find_map
          (fun regex ->
            if (agent_name :> string) =~ regex then
              let re = matches regex (agent_name :> string) in
              Some (Tarray re)
            else None)
          agents_regex
      in
      match re with
      | Some re -> run_jobs ~state ~agent ~re stage.run_jobs stage.jobs
      | None -> unit)

let run_stages ~state (stages : Stage.t list) =
  Execution_params.traverse Sequential (run_stage ~state) (List.to_seq stages)

let initialize_agent ~octogram_binary ~state agent =
  let open Recipe in
  let runner = Recipe.runner_of_agent agent in

  let agent =
    Remote_agent.create
      ~octogram_binary
      ~name:agent.name
      ~on_new_metrics_source:(fun agent_name node_name node_kind port ->
        match node_kind with
        | Octez_node | Rollup_node ->
            let agent = Orchestrator_state.get_agent state agent_name in
            let runner = Remote_agent.runner agent in
            let addr = sf "http://%s:%d" runner.address port in
            Log.info
              ~color
              "[orchestrator] New metrics source available at %s"
              addr ;
            Orchestrator_state.record_metrics_source
              state
              agent_name
              node_name
              runner.address
              port
        | _ -> ())
      ~runner
      ()
  in
  let* () =
    Remote_agent.run
      ~on_terminate:(fun _status ->
        Log.info "%s has terminated" (Remote_agent.name agent :> string) ;
        Orchestrator_state.forget_agent state (Remote_agent.name agent) ;
        unit)
      agent
  in
  let* () = Remote_agent.wait_for_ready agent in
  Orchestrator_state.record_agent state agent ;
  unit

let initialize_agents ~octogram_binary ~state agents =
  Lwt_list.iter_p (initialize_agent ~octogram_binary ~state) agents

let terminate_agents ~state =
  Orchestrator_state.iter_agents Concurrent state (fun agent ->
      let* handler = Remote_agent.start_request agent Agent_builtins.Quit in
      let* () = Remote_agent.wait_for_request agent handler in
      let* _ = Remote_agent.wait agent in
      unit)

let terminate_prometheus ~state =
  Orchestrator_state.with_prometheus ~default:unit state @@ fun prometheus ->
  let* () = Remote_prometheus.quit prometheus in
  let* _status = Remote_prometheus.wait prometheus in
  unit

let rec wait_for_eof k =
  Lwt.try_bind
    (fun () -> Lwt_io.(read_line stdin))
    (fun _line -> wait_for_eof k)
    (function
      | End_of_file -> k ()
      | exn ->
          Log.debug "Unexpected exception: %s" (Printexc.to_string exn) ;
          wait_for_eof k)

let run_recipe ~keep_alive (recipe : Recipe.t) =
  let* prometheus =
    match recipe.prometheus_agent with
    | Some agent ->
        Log.info ~color "[orchestrator] Starting the prometheus instance" ;
        let prometheus =
          Remote_prometheus.create
            ~name:agent.name
            ~runner:(Recipe.runner_of_agent agent)
            ()
        in
        let* () = Remote_prometheus.run prometheus in
        let* () = Remote_prometheus.wait_for_ready prometheus in
        return (Some prometheus)
    | None ->
        Log.info
          ~color
          "[orchestrator] No prometheus agent specified, skipping \
           initialization" ;
        return None
  in

  let state = Orchestrator_state.initial_state ?prometheus recipe.vars in
  Log.info ~color "[orchestrator] Starting the agents" ;
  let* () =
    initialize_agents
      ~octogram_binary:recipe.octogram_binary
      ~state
      recipe.agents
  in
  let interrupted = ref false in
  let* () =
    Lwt.pick
      [
        (Log.info
           ~color
           "[orchestrator] Starting stages execution. Hit C-d to exit." ;
         run_stages ~state recipe.stages);
        wait_for_eof (fun () ->
            interrupted := true ;
            unit);
      ]
  in

  let* () =
    if keep_alive && not !interrupted then (
      Log.info
        ~color
        "[orchestrator] Keeping agents alive. Enter newline to exit." ;
      let* _ = Lwt_io.(read_line stdin) in
      unit)
    else unit
  in

  Log.info ~color "[orchestrator] Terminating remote agents" ;

  let* () = terminate_agents ~state in
  let* () = terminate_prometheus ~state in
  if !interrupted then Test.fail "Scenario aborted" else unit
