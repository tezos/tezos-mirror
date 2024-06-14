(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type tools = {
  get_estimated_time_to_next_level : unit -> Ptime.span option Lwt.t;
}

(** [get_estimated_time_to_next_level get_chain_store] gets the estimated time 
    to the next level of the main chain, obtained with the [get_chain_store] *)
let get_estimated_time_to_next_level get_chain_store =
  let open Lwt_option_syntax in
  match get_chain_store () with
  | None -> Lwt.return_none
  | Some chain_store ->
      let*! block = Store.Chain.current_head chain_store in
      let header = Store.Block.shell_header block in
      let*! proto_hash = Store.Block.protocol_hash_exn chain_store block in
      let*? (module Http_cache_headers) =
        Protocol_plugin.find_http_cache_headers proto_hash
      in
      let* round_end =
        Http_cache_headers.get_round_end_time
          ~get_context:(fun () -> Store.Block.context_exn chain_store block)
          header
      in
      let now = Time.System.now () in
      if round_end < now then
        (* If the round has ended, then the next level block is late.
           We cannot be sure when it will arrive. *)
        Lwt.return_none
      else
        let timespan = Ptime.diff round_end now in
        return timespan

let make_tools get_chain_store =
  {
    get_estimated_time_to_next_level =
      (fun () -> get_estimated_time_to_next_level get_chain_store);
  }
