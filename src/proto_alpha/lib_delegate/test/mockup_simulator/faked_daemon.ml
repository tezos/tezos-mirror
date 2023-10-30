module Baker = struct
  let run ~(cctxt : #Protocol_client_context.full) ~stop_on_event ~chain_id
      ~(context_index : Abstract_context_index.t) ~delegates =
    let open Lwt_result_syntax in
    let chain = `Hash chain_id in
    let baking_configuration =
      let open Baking_configuration in
      {
        default_config with
        validation = ContextIndex context_index;
        state_recorder = Memory;
      }
    in
    (* By default errors are simply printed but the baker won't stop
       because of them. This is not what we want for testing. Here we force
       the baker to terminate unsuccessfully if an error occurs. *)
    let canceler = Lwt_canceler.create () in
    let on_error (err : error trace) =
      let*! (_ : (unit, exn trace) result) = Lwt_canceler.cancel canceler in
      Format.printf "%a" Error_monad.pp_print_trace err ;
      Lwt_exit.exit_and_raise 1
    in
    Baking_scheduling.run
      cctxt
      ~canceler
      ~stop_on_event
      ~on_error
      ~chain
      baking_configuration
      delegates
end
