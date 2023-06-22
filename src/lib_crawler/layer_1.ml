(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>                    *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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

(**

    Errors
    ======

*)

type error += Cannot_find_predecessor of Block_hash.t

let () =
  register_error_kind
    ~id:"lib_crawler.cannot_find_predecessor"
    ~title:"Cannot find block predecessor from L1"
    ~description:"A predecessor couldn't be found from the L1 node"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Block with hash %a has no predecessor on the L1 node."
        Block_hash.pp
        hash)
    `Temporary
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Cannot_find_predecessor hash -> Some hash | _ -> None)
    (fun hash -> Cannot_find_predecessor hash)

(**

   State
   =====

*)

type t = {
  name : string;
  protocols : Protocol_hash.t list option;
  reconnection_delay : float;
  heads : (Block_hash.t * Block_header.t) Lwt_stream.t;
  cctxt : Client_context.full;
  stopper : Tezos_rpc.Context.stopper;
  mutable running : bool;
}

let rec connect ~name ?(count = 0) ~delay ~protocols cctxt =
  let open Lwt_syntax in
  let* () =
    if count = 0 then return_unit
    else
      let fcount = float_of_int (count - 1) in
      (* Randomized exponential backoff capped to 1.5h: 1.5^count * delay ± 50% *)
      let delay = delay *. (1.5 ** fcount) in
      let delay = min delay 3600. in
      let randomization_factor = 0.5 (* 50% *) in
      let delay =
        delay
        +. Random.float (delay *. 2. *. randomization_factor)
        -. (delay *. randomization_factor)
      in
      let* () = Layer1_event.wait_reconnect ~name delay in
      Lwt_unix.sleep delay
  in
  let* res =
    Tezos_shell_services.Monitor_services.heads ?protocols cctxt cctxt#chain
  in
  match res with
  | Ok (heads, stopper) ->
      let heads =
        Lwt_stream.map_s
          (fun ( hash,
                 (Tezos_base.Block_header.{shell = {level; _}; _} as header) ) ->
            let+ () = Layer1_event.switched_new_head ~name hash level in
            (hash, header))
          heads
      in
      return (heads, stopper)
  | Error e ->
      let* () = Layer1_event.cannot_connect ~name ~count e in
      connect ~name ~delay ~protocols ~count:(count + 1) cctxt

let start ~name ~reconnection_delay ?protocols (cctxt : #Client_context.full) =
  let open Lwt_syntax in
  let* () = Layer1_event.starting ~name in
  let+ heads, stopper =
    connect ~name ~delay:reconnection_delay ~protocols cctxt
  in
  {
    name;
    cctxt = (cctxt :> Client_context.full);
    heads;
    stopper;
    reconnection_delay;
    protocols;
    running = true;
  }

let reconnect l1_ctxt =
  let open Lwt_syntax in
  let* heads, stopper =
    connect
      ~name:l1_ctxt.name
      ~count:1
      ~delay:l1_ctxt.reconnection_delay
      ~protocols:l1_ctxt.protocols
      l1_ctxt.cctxt
  in
  return {l1_ctxt with heads; stopper}

let shutdown state =
  state.stopper () ;
  state.running <- false ;
  Lwt.return_unit

let is_connection_error trace =
  TzTrace.fold
    (fun yes error ->
      yes
      ||
      match error with
      | RPC_client_errors.(Request_failed {error = Connection_failed _; _}) ->
          true
      | _ -> false)
    false
    trace

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2895
   Use Lwt_stream.iter_es once it is exposed. *)
let iter_heads l1_ctxt f =
  let exception Iter_error of tztrace in
  let rec loop l1_ctxt =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_stream.iter_s
        (fun head ->
          let open Lwt_syntax in
          let* res = f head in
          match res with
          | Ok () -> return_unit
          | Error trace when is_connection_error trace ->
              Format.eprintf
                "@[<v 2>Connection error:@ %a@]@."
                pp_print_trace
                trace ;
              l1_ctxt.stopper () ;
              return_unit
          | Error e -> raise (Iter_error e))
        l1_ctxt.heads
    in
    when_ l1_ctxt.running @@ fun () ->
    let*! () = Layer1_event.connection_lost ~name:l1_ctxt.name in
    let*! l1_ctxt = reconnect l1_ctxt in
    loop l1_ctxt
  in
  Lwt.catch
    (fun () -> Lwt.no_cancel @@ loop l1_ctxt)
    (function Iter_error e -> Lwt.return_error e | exn -> fail (Exn exn))

let wait_first l1_ctxt =
  let rec loop l1_ctxt =
    let open Lwt_syntax in
    let* head = Lwt_stream.peek l1_ctxt.heads in
    match head with
    | Some head -> return head
    | None ->
        let* l1_ctxt = reconnect l1_ctxt in
        loop l1_ctxt
  in
  Lwt.no_cancel @@ loop l1_ctxt

(** [predecessors_of_blocks hashes] given a list of successive block hashes,
    from newest to oldest, returns an associative list that associates a hash to
    its predecessor in this list. *)
let predecessors_of_blocks hashes =
  let rec aux next = function [] -> [] | x :: xs -> (next, x) :: aux x xs in
  match hashes with [] -> [] | x :: xs -> aux x xs

(** [get_predecessor block_hash] returns the predecessor block hash of
    some [block_hash] through an RPC to the Tezos node. To limit the
    number of RPCs, this information is requested for a batch of hashes
    and cached locally. *)
let get_predecessor =
  let max_cached = 65536 in
  let hard_max_read = max_cached in
  (* 2MB *)
  let module HM =
    Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong) (Block_hash)
  in
  let cache = HM.create max_cached in
  fun ~max_read
      cctxt
      (chain : Tezos_shell_services.Chain_services.chain)
      ancestor ->
    let open Lwt_result_syntax in
    (* Don't read more than the hard limit in one RPC call. *)
    let max_read = min max_read hard_max_read in
    (* But read at least two because the RPC also returns the head. *)
    let max_read = max max_read 2 in
    match HM.find_opt cache ancestor with
    | Some pred -> return_some pred
    | None -> (
        let* blocks =
          Tezos_shell_services.Chain_services.Blocks.list
            cctxt
            ~chain
            ~heads:[ancestor]
            ~length:max_read
            ()
        in
        match blocks with
        | [ancestors] -> (
            List.iter
              (fun (h, p) -> HM.replace cache h p)
              (predecessors_of_blocks ancestors) ;
            match HM.find_opt cache ancestor with
            | None ->
                (* This could happen if ancestors was empty, but it shouldn't be. *)
                return_none
            | Some predecessor -> return_some predecessor)
        | _ -> return_none)

let get_predecessor_opt ?(max_read = 8) state (hash, level) =
  let open Lwt_result_syntax in
  if level = 0l then return_none
  else
    let level = Int32.pred level in
    let+ hash = get_predecessor ~max_read state.cctxt state.cctxt#chain hash in
    Option.map (fun hash -> (hash, level)) hash

let get_predecessor ?max_read state ((hash, _) as head) =
  let open Lwt_result_syntax in
  let* pred = get_predecessor_opt ?max_read state head in
  match pred with
  | None -> tzfail (Cannot_find_predecessor hash)
  | Some pred -> return pred

let nth_predecessor ~get_predecessor n block =
  let open Lwt_result_syntax in
  assert (n >= 0) ;
  let rec aux acc n block =
    if n = 0 then return (block, acc)
    else
      let* pred = get_predecessor block in
      (aux [@tailcall]) (block :: acc) (n - 1) pred
  in
  aux [] n block

let get_tezos_reorg_for_new_head l1_state
    ?(get_old_predecessor = get_predecessor l1_state) old_head new_head =
  let open Lwt_result_syntax in
  (* old_head and new_head must have the same level when calling aux *)
  let rec aux reorg old_head new_head =
    let old_head_hash, _ = old_head in
    let new_head_hash, _ = new_head in
    if Block_hash.(old_head_hash = new_head_hash) then return reorg
    else
      let* old_head_pred = get_old_predecessor old_head in
      let* new_head_pred = get_predecessor l1_state new_head in
      let reorg =
        Reorg.
          {
            old_chain = old_head :: reorg.old_chain;
            new_chain = new_head :: reorg.new_chain;
          }
      in
      (aux [@tailcall]) reorg old_head_pred new_head_pred
  in
  (* computing partial reorganization to make old_head and new_head at same
     level *)
  let _, old_head_level = old_head in
  let _, new_head_level = new_head in
  let distance = Int32.(to_int @@ abs @@ sub new_head_level old_head_level) in
  let* old_head, new_head, reorg =
    if old_head_level = new_head_level then
      return (old_head, new_head, Reorg.no_reorg)
    else if old_head_level < new_head_level then
      let max_read = distance + 1 (* reading includes the head *) in
      let+ new_head, new_chain =
        nth_predecessor
          ~get_predecessor:(get_predecessor ~max_read l1_state)
          distance
          new_head
      in
      (old_head, new_head, {Reorg.no_reorg with new_chain})
    else
      let+ old_head, old_chain =
        nth_predecessor ~get_predecessor:get_old_predecessor distance old_head
      in
      (old_head, new_head, {Reorg.no_reorg with old_chain})
  in
  assert (snd old_head = snd new_head) ;
  aux reorg old_head new_head

(** Returns the reorganization of L1 blocks (if any) for [new_head]. *)
let get_tezos_reorg_for_new_head l1_state ?get_old_predecessor old_head new_head
    =
  let open Lwt_result_syntax in
  match old_head with
  | `Level l ->
      let _, new_head_level = new_head in
      (* No known tezos head, we want all blocks from l. *)
      if new_head_level < l then return Reorg.no_reorg
      else
        let distance = Int32.sub new_head_level l |> Int32.to_int in
        let max_read = distance + 1 (* reading includes the head *) in
        let* _block_at_l, new_chain =
          nth_predecessor
            ~get_predecessor:(get_predecessor ~max_read l1_state)
            distance
            new_head
        in
        return Reorg.{old_chain = []; new_chain}
  | `Head old_head ->
      get_tezos_reorg_for_new_head
        l1_state
        ?get_old_predecessor
        old_head
        new_head

module Internal_for_tests = struct
  let dummy cctxt =
    let heads, _push = Lwt_stream.create () in
    {
      name = "dummy_layer_1_for_tests";
      reconnection_delay = 5.0;
      heads;
      cctxt = (cctxt :> Client_context.full);
      stopper = Fun.id;
      protocols = None;
      running = false;
    }
end
