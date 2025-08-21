open Tezos_error_monad.Error_monad

(* [context type] *)
module type CONTEXT = sig
  val list_known_contracts : unit -> unit
end

type context = (module CONTEXT)

(* [context definition] *)
module Dummy_context : CONTEXT = struct
  let list_known_contracts () =
    Format.printf "<Print the list of known contracts>\n"
end

(* [command groups] *)
let wallet_group =
  {Tezos_clic.name = "wallet_group"; title = "Wallet-related commands"}

(* [list known contracts] *)
module List_known_contracts = struct
  let options = Tezos_clic.no_options

  let params = Tezos_clic.(prefixes ["list"; "known"; "contracts"] stop)

  let list_known_contracts_handler :
      unit -> context -> unit Tezos_error_monad.Error_monad.tzresult Lwt.t =
   fun () ctxt ->
    let module C = (val ctxt) in
    C.list_known_contracts () ;
    Lwt_result_syntax.return_unit

  let command =
    Tezos_clic.command
      ~group:wallet_group
      ~desc:"Prints the list of known contracts"
      options
      params
      list_known_contracts_handler
end

let commands = [List_known_contracts.command]

(* [program entrypoint] *)
let () =
  (* 1. Setup formatter with color *)
  ignore
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        ~isatty:(Unix.isatty Unix.stdout)
        Short) ;
  (* 2. Setup context and dispatch commands *)
  let ctxt = (module Dummy_context : CONTEXT) in
  let result =
    Lwt_main.run
      (Tezos_clic.dispatch commands ctxt (Array.to_list Sys.argv |> List.tl))
  in
  (* 3. Handle results *)
  match result with
  | Ok () -> ()
  | Error [Tezos_clic.Help _command] ->
      Format.printf "<display help>\n" ;
      exit 0
  | Error _ ->
      Format.printf "Could not parse command-line arguments.\n" ;
      exit 1
