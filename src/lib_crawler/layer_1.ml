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

type error +=
  | Cannot_find_predecessor of Block_hash.t
  | Http_connection_error of (Cohttp.Code.status_code * string)

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

let () =
  let http_status_enc =
    let open Data_encoding in
    let open Cohttp.Code in
    conv code_of_status status_of_code int31
  in
  register_error_kind
    `Permanent
    ~id:"lib_crawler.http_error"
    ~title:"HTTP error when fetching data"
    ~description:"The node encountered an HTTP error when fetching data."
    ~pp:(fun ppf (status, body) ->
      Format.fprintf
        ppf
        "Downloading data resulted in: %s (%s)."
        (Cohttp.Code.string_of_status status)
        body)
    Data_encoding.(
      obj2 (req "status" http_status_enc) (req "body" Data_encoding.string))
    (function
      | Http_connection_error (status, body) -> Some (status, body) | _ -> None)
    (fun (status, body) -> Http_connection_error (status, body))

type error += RPC_timeout of {path : string; timeout : float}

let () =
  register_error_kind
    ~id:"lib_crawler.rpc_timeout"
    ~title:"Timeout in RPC"
    ~description:"An RPC did not respond after the timeout"
    ~pp:(fun ppf (path, timeout) ->
      Format.fprintf ppf "Call %s timeouted after %fs." path timeout)
    `Temporary
    Data_encoding.(obj2 (req "path" string) (req "timeout" float))
    (function RPC_timeout {path; timeout} -> Some (path, timeout) | _ -> None)
    (fun (path, timeout) -> RPC_timeout {path; timeout})

(**

   State
   =====

*)

type connection_info = {
  heads : (Block_hash.t * Block_header.t) Lwt_stream.t;
  stopper : Tezos_rpc.Context.stopper;
  consumer : unit Lwt.t;
      (** This thread is used to consume the stream [heads] in order to prevent
          a leak.  *)
}

type connection_status =
  | Disconnected
  | Connected of connection_info
  | Connecting of connection_info Lwt_condition.t

(** Cache structure for predecessors. *)
module Precessor_cache = struct
  include
    Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong) (Block_hash)

  type nonrec t = Block_hash.t t

  let max_cached = 65536 (* 2MB *)

  let create () = create max_cached
end

type t = {
  name : string;
  chain : Tezos_shell_services.Chain_services.chain;
  protocols : Protocol_hash.t list option;
  reconnection_delay : float;
  cctxt : Tezos_rpc.Context.generic;
  mutable last_seen : (Block_hash.t * Block_header.t) option;
  mutable status : connection_status;
  cache : Precessor_cache.t;
}

let is_running c =
  match c.status with
  | Disconnected -> false
  | Connected _ | Connecting _ -> true

let rec do_connect ~count ~previous_status
    ({name; protocols; reconnection_delay = delay; cctxt; chain; _} as l1_ctxt)
    =
  assert (match l1_ctxt.status with Connecting _ -> true | _ -> false) ;
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
    Tezos_shell_services.Monitor_services.heads ?protocols cctxt chain
  in
  match res with
  | Ok (heads, stopper) ->
      let heads =
        Lwt_stream.map
          (fun ((hash, Tezos_base.Block_header.{shell = {predecessor; _}; _}) as
                head) ->
            Precessor_cache.replace l1_ctxt.cache hash predecessor ;
            head)
          heads
      in
      let consumer =
        Lwt_stream.iter_s
          (fun ((hash, Tezos_base.Block_header.{shell = {level; _}; _}) as head) ->
            l1_ctxt.last_seen <- Some head ;
            Layer1_event.switched_new_head ~name hash level)
          heads
      in
      let conn = {heads; stopper; consumer} in
      l1_ctxt.status <- Connected conn ;
      return conn
  | Error e ->
      let* () = Layer1_event.cannot_connect ~name ~count e in
      do_connect ~count:(count + 1) ~previous_status l1_ctxt

let do_connect ?(count = 0) l1_ctxt =
  let open Lwt_syntax in
  let previous_status = l1_ctxt.status in
  let cond = Lwt_condition.create () in
  l1_ctxt.status <- Connecting cond ;
  let* () =
    match previous_status with
    | Connected {stopper; _} ->
        stopper () ;
        Layer1_event.stopping_old_connection ~name:l1_ctxt.name
    | _ -> return_unit
  in
  let* conn = do_connect ~count ~previous_status l1_ctxt in
  let* () = Layer1_event.connected ~name:l1_ctxt.name in
  Lwt_condition.broadcast cond conn ;
  return conn

let connect l1_ctxt =
  let open Lwt_syntax in
  match l1_ctxt.status with
  | Connected conn ->
      (* Already connected *)
      return conn
  | Connecting c ->
      (* Reconnection already triggered by someone else *)
      Lwt_condition.wait c
  | Disconnected -> do_connect l1_ctxt

let create ~name ~chain ~reconnection_delay ?protocols cctxt =
  {
    name;
    chain;
    cctxt;
    reconnection_delay;
    protocols;
    last_seen = None;
    status = Disconnected;
    cache = Precessor_cache.create ();
  }

let start ~name ~chain ~reconnection_delay ?protocols cctxt =
  let open Lwt_syntax in
  let* () = Layer1_event.starting ~name in
  let l1_ctxt = create ~name ~chain ~reconnection_delay ?protocols cctxt in
  let* (_ : connection_info) = connect l1_ctxt in
  return l1_ctxt

let reconnect ~name l1_ctxt =
  let open Lwt_syntax in
  match l1_ctxt.status with
  | Connecting c ->
      let* () = Layer1_event.reconnect_connecting ~name in
      let* conn = Lwt_condition.wait c in
      let* () = Layer1_event.reconnect_notified ~name in
      return conn
  | Disconnected ->
      (* NOTE: same as calling [connect] but one less indirection *)
      let* () = Layer1_event.reconnect_disconnected ~name in
      do_connect ~count:1 l1_ctxt
  | Connected {stopper; _} ->
      (* force reconnection: call [do_connect] instead of [connect] *)
      let* () = Layer1_event.reconnect_connected ~name in
      stopper () ;
      do_connect ~count:1 l1_ctxt

let disconnect l1_ctxt =
  (match l1_ctxt.status with
  | Disconnected | Connecting _ -> ()
  (* disconnect may leak a reconnection attempt but is used only on
     shutdown. *)
  | Connected {stopper; _} -> stopper ()) ;
  l1_ctxt.status <- Disconnected

let shutdown state =
  disconnect state ;
  Lwt.return_unit

let regexp_ocaml_exception_connection_error = Re.Str.regexp ".*in connect:.*"

let is_connection_error trace =
  TzTrace.fold
    (fun yes error ->
      yes
      ||
      match error with
      | RPC_client_errors.(Request_failed {error = Connection_failed _; _}) ->
          true
      | RPC_client_errors.(Request_failed {error = OCaml_exception s; _}) ->
          (* This error can surface if the external RPC servers of the L1 node are
             shutdown but the request is still in the RPC worker. *)
          Re.Str.string_match regexp_ocaml_exception_connection_error s 0
      | RPC_timeout _ -> true
      | Http_connection_error _ -> true
      | _ -> false)
    false
    trace

type 'a lwt_stream_get_result =
  | Get_none
  | Get_timeout of float
  | Get_elt of 'a

type lwt_stream_iter_with_timeout_ended = Closed | Timeout of float

let timeout_factor = 10.

(** This function is similar to {!Lwt_stream.iter_s} excepted that it resolves
    with {!Get_timeout} if waiting for the next element takes more than some
    time (10x last elapsed time). [init_timeout] is used for the initial timeout
    value (so it should be large enough) and [min_timeout] is the minimal timeout
    considered. *)
let lwt_stream_iter_with_timeout ~min_timeout ~init_timeout f stream =
  let open Lwt_syntax in
  let rec loop timeout =
    let get_promise =
      let+ res = Lwt_stream.get stream in
      match res with None -> Get_none | Some e -> Get_elt e
    in
    let timeout_promise =
      let+ () = Lwt_unix.sleep timeout in
      Get_timeout timeout
    in
    let start_time = Unix.gettimeofday () in
    let* res = Lwt.pick [get_promise; timeout_promise] in
    match res with
    | Get_none -> return_ok Closed
    | Get_timeout t -> return_ok (Timeout t)
    | Get_elt e -> (
        let elapsed = Unix.gettimeofday () -. start_time in
        let new_timeout = elapsed *. timeout_factor in
        let timeout =
          if new_timeout < min_timeout then timeout else new_timeout
        in
        let* res = protect @@ fun () -> f e in
        match res with
        | Ok () -> (loop [@ocaml.tailcall]) timeout
        | Error trace -> return_error trace)
  in
  loop init_timeout

let iter_heads ?name l1_ctxt f =
  let open Lwt_result_syntax in
  let name = Option.value name ~default:l1_ctxt.name in
  let rec loop {heads; stopper; consumer = _} =
    let heads = Lwt_stream.clone heads in
    let heads =
      match l1_ctxt.last_seen with
      | Some l ->
          (* (Re)connection consumes heads, reconstruct stream with latest
             available head in order to start iteration from it. *)
          Lwt_stream.append (Lwt_stream.return l) heads
      | None -> heads
    in
    let* stopping_reason =
      lwt_stream_iter_with_timeout ~min_timeout:10. ~init_timeout:300. f heads
    in
    when_ (is_running l1_ctxt) @@ fun () ->
    stopper () ;
    let*! () =
      match stopping_reason with
      | Closed -> Layer1_event.connection_lost ~name
      | Timeout timeout -> Layer1_event.connection_timeout ~name ~timeout
    in
    let*! conn = reconnect ~name l1_ctxt in
    loop conn
  in
  let*! conn = connect l1_ctxt in
  loop conn

let wait_first l1_ctxt =
  let open Lwt_syntax in
  let rec loop conn =
    match l1_ctxt.last_seen with
    | Some h -> return h
    | None -> (
        let* head = Lwt_stream.peek conn.heads in
        match head with
        | Some head -> return head
        | None ->
            let* conn = reconnect ~name:l1_ctxt.name l1_ctxt in
            loop conn)
  in
  Lwt.no_cancel
  @@ let* conn = connect l1_ctxt in
     loop conn

let get_latest_head l1_ctxt = l1_ctxt.last_seen

let get_status l1_ctxt =
  match l1_ctxt.status with
  | Connected _ -> `Connected
  | Connecting _ -> `Reconnecting
  | Disconnected -> `Disconnected

(** [predecessors_of_blocks hashes] given a list of successive block hashes,
    from newest to oldest, returns an associative list that associates a hash to
    its predecessor in this list. *)
let predecessors_of_blocks hashes =
  let rec aux next = function [] -> [] | x :: xs -> (next, x) :: aux x xs in
  match hashes with [] -> [] | x :: xs -> aux x xs

(** [get_predecessor ~max_read state block_hash] returns the predecessor block
    hash of some [block_hash] through an RPC to the Tezos node. To limit the
    number of RPCs, this information is requested for a batch of hashes (of size
    [max_read]) and cached locally. *)
let get_predecessor ~max_read state ancestor =
  let open Lwt_result_syntax in
  (* Don't read more than the hard limit in one RPC call. *)
  let max_read = min max_read Precessor_cache.max_cached in
  (* But read at least two because the RPC also returns the head. *)
  let max_read = max max_read 2 in
  match Precessor_cache.find_opt state.cache ancestor with
  | Some pred -> return_some pred
  | None -> (
      let* blocks =
        Tezos_shell_services.Chain_services.Blocks.list
          state.cctxt
          ~chain:state.chain
          ~heads:[ancestor]
          ~length:max_read
          ()
      in
      match blocks with
      | [ancestors] -> (
          List.iter
            (fun (h, p) -> Precessor_cache.replace state.cache h p)
            (predecessors_of_blocks ancestors) ;
          match Precessor_cache.find_opt state.cache ancestor with
          | None ->
              (* This could happen if ancestors was empty, but it shouldn't
                 be. *)
              let* pred =
                Tezos_shell_services.Chain_services.Blocks.hash
                  state.cctxt
                  ~chain:state.chain
                  ~block:(`Hash (ancestor, 1))
                  ()
              in
              Precessor_cache.replace state.cache ancestor pred ;
              return_some pred
          | Some predecessor -> return_some predecessor)
      | _ -> return_none)

let get_predecessor_opt ?(max_read = 8) state (hash, level) =
  let open Lwt_result_syntax in
  if level = 0l then return_none
  else
    let level = Int32.pred level in
    let+ hash = get_predecessor ~max_read state hash in
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
    {
      name = "dummy_layer_1_for_tests";
      chain = `Main;
      reconnection_delay = 5.0;
      cctxt;
      protocols = None;
      last_seen = None;
      status = Disconnected;
      cache = Precessor_cache.create ();
    }
end

let timeout_promise service timeout =
  let open Lwt_syntax in
  let* () = Lwt_unix.sleep timeout in
  let path = Tezos_rpc.(Service.Internal.to_service service).path in
  let path =
    Tezos_rpc.Service.Internal.from_path path |> Tezos_rpc.Path.to_string
  in
  Lwt_result_syntax.tzfail (RPC_timeout {path; timeout})

let client_context_with_timeout (obj : #Client_context.full) timeout :
    Client_context.full =
  let open Lwt_syntax in
  object
    inherit Client_context.proxy_context (obj :> Client_context.full)

    method! call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          'p ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      fun service params query body ->
        Lwt.pick
          [
            obj#call_service service params query body;
            timeout_promise service timeout;
          ]

    method! call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          on_chunk:('o -> unit) ->
          on_close:(unit -> unit) ->
          'p ->
          'q ->
          'i ->
          (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        Lwt.pick
          [
            obj#call_streamed_service
              service
              ~on_chunk
              ~on_close
              params
              query
              body;
            timeout_promise service timeout;
          ]

    method! generic_media_type_call meth ?body uri =
      let timeout_promise =
        let* () = Lwt_unix.sleep timeout in
        Lwt_result_syntax.tzfail
          (RPC_timeout {path = Uri.to_string uri; timeout})
      in
      Lwt.pick [obj#generic_media_type_call meth ?body uri; timeout_promise]
  end
