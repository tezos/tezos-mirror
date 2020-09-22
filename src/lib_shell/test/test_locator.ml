(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    -------
    Component:    Shell
    Invocation:   dune exec src/lib_shell/test/test_locator.exe test_locator
    Subject:      Checks operations on locators.
*)

open Filename.Infix

(** Basic blocks *)

let genesis_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L

let state_genesis_block =
  {
    Genesis.time = genesis_time;
    block = genesis_hash;
    protocol = genesis_protocol;
  }

let chain_id = Chain_id.of_block_hash genesis_hash

let proto =
  match Registered_protocol.get genesis_protocol with
  | None ->
      assert false
  | Some proto ->
      proto

module Proto = (val proto)

let incr_timestamp timestamp =
  Time.Protocol.add timestamp (Int64.add 1L (Random.int64 10L))

let incr_fitness fitness =
  let new_fitness =
    match fitness with
    | [fitness] ->
        Data_encoding.Binary.of_bytes_opt Data_encoding.int64 fitness
        |> Option.value ~default:0L |> Int64.succ
        |> Data_encoding.Binary.to_bytes_exn Data_encoding.int64
    | _ ->
        Data_encoding.Binary.to_bytes_exn Data_encoding.int64 1L
  in
  [new_fitness]

(* returns a new state with a single block, genesis *)
let init_chain base_dir : State.Chain.t Lwt.t =
  let store_root = base_dir // "store" in
  let context_root = base_dir // "context" in
  State.init
    ~store_root
    ~context_root
    ~history_mode:Archive
    state_genesis_block
  >>= function
  | Error _ ->
      Stdlib.failwith "read err"
  | Ok (_state, chain, _index, _history_mode) ->
      Lwt.return chain

let block_header ?(context = Context_hash.zero) (pred : State.Block.t) :
    Block_header.t =
  let pred_header = State.Block.shell_header pred in
  let timestamp = incr_timestamp pred_header.timestamp in
  let fitness = incr_fitness pred_header.fitness in
  {
    Block_header.shell =
      {
        level = Int32.add Int32.one (State.Block.level pred);
        proto_level = 0;
        predecessor = State.Block.hash pred;
        timestamp;
        validation_passes = 0;
        operations_hash = Operation_list_list_hash.empty;
        fitness;
        context;
      };
    Block_header.protocol_data = Bytes.of_string "";
  }

let zero = Bytes.create 0

(* adds n blocks on top of an initialized chain *)
let make_empty_chain (chain : State.Chain.t) n : Block_hash.t Lwt.t =
  State.Block.read_opt chain genesis_hash
  >|= WithExceptions.Option.get ~loc:__LOC__
  >>= fun genesis ->
  State.Block.context_exn genesis
  >>= fun empty_context ->
  let header = State.Block.header genesis in
  let timestamp = State.Block.timestamp genesis in
  let empty_context_hash = Context.hash ~time:timestamp empty_context in
  Context.commit ~time:header.shell.timestamp empty_context
  >>= fun context ->
  let header = {header with shell = {header.shell with context}} in
  let empty_result =
    {
      Block_validation.context_hash = empty_context_hash;
      message = None;
      max_operations_ttl = 0;
      last_allowed_fork_level = 0l;
    }
  in
  let rec loop lvl pred =
    if lvl >= n then return pred
    else
      let header =
        {
          header with
          shell =
            {header.shell with predecessor = pred; level = Int32.of_int lvl};
        }
      in
      let block_metadata = zero in
      let block_metadata_hash =
        Option.some @@ Block_metadata_hash.hash_bytes [block_metadata]
      in
      State.Block.store
        chain
        header
        block_metadata
        []
        []
        block_metadata_hash
        None
        empty_result
        ~forking_testchain:false
      >>=? fun _ -> loop (lvl + 1) (Block_header.hash header)
  in
  loop 1 genesis_hash
  >>= function
  | Ok b ->
      Lwt.return b
  | Error err ->
      Error_monad.pp_print_error Format.err_formatter err ;
      assert false

(* adds n blocks on top of an initialized chain and bump block's
   proto_level at given fork points *)
let make_multiple_protocol_chain (chain : State.Chain.t) ~(chain_length : int)
    ~fork_points =
  State.Block.read_opt chain genesis_hash
  >|= WithExceptions.Option.get ~loc:__LOC__
  >>= fun genesis ->
  State.Block.context_exn genesis
  >>= fun empty_context ->
  let header = State.Block.header genesis in
  let timestamp = State.Block.timestamp genesis in
  let empty_context_hash = Context.hash ~time:timestamp empty_context in
  Context.commit ~time:header.shell.timestamp empty_context
  >>= fun context ->
  let genesis_header = {header with shell = {header.shell with context}} in
  let empty_result =
    {
      Block_validation.context_hash = empty_context_hash;
      message = None;
      max_operations_ttl = 0;
      last_allowed_fork_level = 0l;
    }
  in
  let rec loop remaining_fork_points lvl (pred_header : Block_header.t) =
    if lvl > chain_length then return pred_header
    else
      let (proto_level, remaining_fork_points) =
        match remaining_fork_points with
        | h :: t when h = lvl ->
            (pred_header.shell.proto_level + 1, t)
        | remaining_fork_points ->
            (pred_header.shell.proto_level, remaining_fork_points)
      in
      let header =
        {
          pred_header with
          shell =
            {
              pred_header.shell with
              predecessor =
                ( if lvl = 1 then genesis_hash
                else Block_header.hash pred_header );
              level = Int32.of_int lvl;
              proto_level;
            };
        }
      in
      let block_metadata = zero in
      let block_metadata_hash =
        Option.some @@ Block_metadata_hash.hash_bytes [block_metadata]
      in
      State.Block.store
        chain
        header
        zero
        []
        []
        block_metadata_hash
        None
        empty_result
        ~forking_testchain:false
      >>=? function
      | None ->
          assert false
      | Some b ->
          Chain.set_head chain b
          >>=? fun _pred_head ->
          State.Chain.update_level_indexed_protocol_store
            chain
            chain_id
            proto_level
            genesis_protocol
            header
          >>= fun () -> loop remaining_fork_points (lvl + 1) header
  in
  loop fork_points 1 genesis_header
  >>= function
  | Ok b ->
      Lwt.return (Block_header.hash b)
  | Error err ->
      Error_monad.pp_print_error Format.err_formatter err ;
      assert false

(* helper functions ------------------------------------- *)

(* wall clock time of a unit function *)
let time1 (f : unit -> 'a) : 'a * float =
  let t = Unix.gettimeofday () in
  let res = f () in
  let wall_clock = Unix.gettimeofday () -. t in
  (res, wall_clock)

(* returns result from first run and average time of [runs] runs *)
let time ?(runs = 1) f =
  if runs < 1 then invalid_arg "time negative arg"
  else
    let rec loop cnt sum =
      if cnt = runs then sum
      else
        let (_, t) = time1 f in
        loop (cnt + 1) (sum +. t)
    in
    let (res, t) = time1 f in
    let sum = loop 1 t in
    (res, sum /. float runs)

let rec repeat f n =
  if n < 0 then invalid_arg "repeat: negative arg"
  else if n = 0 then ()
  else
    let _ = f () in
    repeat f (n - 1)

(* ----------------------------------------------------- *)

let print_block b =
  Printf.printf
    "%6i %s\n"
    (Int32.to_int (State.Block.level b))
    (Block_hash.to_b58check (State.Block.hash b))

let print_block_h chain bh =
  State.Block.read_opt chain bh
  >|= WithExceptions.Option.get ~loc:__LOC__
  >|= fun b -> print_block b

(* returns the predecessor at distance one, reading the header *)
let linear_predecessor chain (bh : Block_hash.t) : Block_hash.t option Lwt.t =
  State.Block.read_opt chain bh
  >|= WithExceptions.Option.get ~loc:__LOC__
  >>= fun b ->
  State.Block.predecessor b
  >|= function None -> None | Some pred -> Some (State.Block.hash pred)

let print_chain chain bh =
  let rec loop bh cnt =
    let _ = print_block_h chain bh in
    linear_predecessor chain bh
    >>= function Some pred -> loop pred (cnt + 1) | None -> Lwt.return_unit
  in
  loop bh 0

(* returns the predecessors at distance n, traversing all n intermediate blocks *)
let linear_predecessor_n (chain : State.Chain.t) (bh : Block_hash.t)
    (distance : int) : Block_hash.t option Lwt.t =
  (* let _ = Printf.printf "LP: %4i " distance; print_block_h chain bh in *)
  if distance < 1 then invalid_arg "distance<1"
  else
    let rec loop bh distance =
      if distance = 0 then Lwt.return_some bh (* reached distance *)
      else
        linear_predecessor chain bh
        >>= function
        | None -> Lwt.return_none | Some pred -> loop pred (distance - 1)
    in
    loop bh distance

(** Tests that the linear predecessor defined above and the exponential
    predecessor implemented in State.predecessor_n return the same
    block and it is the block at the distance requested
*)
let test_pred (base_dir : string) : unit tzresult Lwt.t =
  let size_chain = 1000 in
  init_chain base_dir
  >>= fun chain ->
  make_empty_chain chain size_chain
  >>= fun head ->
  let test_once distance =
    linear_predecessor_n chain head distance
    >>= fun lin_res ->
    State.Block.read_opt chain head
    >|= WithExceptions.Option.get ~loc:__LOC__
    >>= fun head_block ->
    State.Block.predecessor_n head_block distance
    >>= fun exp_res ->
    match (lin_res, exp_res) with
    | (None, None) ->
        Lwt.return_unit
    | (None, Some _) | (Some _, None) ->
        Assert.fail_msg "mismatch between exponential and linear predecessor_n"
    | (Some lin_res, Some exp_res) ->
        (* check that the two results are the same *)
        assert (lin_res = exp_res) ;
        State.Block.read_opt chain lin_res
        >|= WithExceptions.Option.get ~loc:__LOC__
        >>= fun pred ->
        let level_pred = Int32.to_int (State.Block.level pred) in
        State.Block.read_opt chain head
        >|= WithExceptions.Option.get ~loc:__LOC__
        >>= fun head ->
        let level_start = Int32.to_int (State.Block.level head) in
        (* check distance using the level *)
        assert (level_start - distance = level_pred) ;
        Lwt.return_unit
  in
  let _ = Random.self_init () in
  let range = size_chain + (size_chain / 10) in
  let repeats = 100 in
  return (repeat (fun () -> test_once (1 + Random.int range)) repeats)

let seed =
  let receiver_id =
    P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 'r')
  in
  let sender_id =
    P2p_peer.Id.of_string_exn (String.make P2p_peer.Id.size 's')
  in
  {Block_locator.receiver_id; sender_id}

(* compute locator using the linear predecessor *)
let compute_linear_locator chain_state ~size block =
  let block_hash = State.Block.hash block in
  let header = State.Block.header block in
  Block_locator.compute
    ~get_predecessor:(linear_predecessor_n chain_state)
    block_hash
    header
    ~size
    seed

(* given the size of a chain, returns the size required for a locator
   to reach genesis *)
let compute_size_locator size_chain =
  let repeats = 10. in
  int_of_float ((log (float size_chain /. repeats) /. log 2.) -. 1.) * 10

(* given the size of a locator, returns the size of the chain that it
   can cover back to genesis *)
let compute_size_chain size_locator =
  let repeats = 10. in
  int_of_float (repeats *. (2. ** float (size_locator + 1)))

(*
   chain 1 year   518k   covered by locator 150
   chain 2 months 86k    covered by locator 120
*)

(** Test if the linear and exponential locator are the same and outputs
    their timing.
    Run the test with:
    $ dune build @runbench_locator
    Copy the output to a file timing.dat and plot it with:
    $ generate_locator_plot.sh timing.dat
*)
let test_locator base_dir =
  let size_chain = 80000 in
  (* timing locators with average over [runs] times *)
  let runs = 10 in
  let _ = Printf.printf "#runs %i\n" runs in
  (* limit after which exp should go linear *)
  let exp_limit = compute_size_chain 120 in
  let _ = Printf.printf "#exp_limit %i\n" exp_limit in
  (* size after which locator always reaches genesis *)
  let locator_limit = compute_size_locator size_chain in
  let _ = Printf.printf "#locator_limit %i\n" locator_limit in
  init_chain base_dir
  >>= fun chain ->
  time1 (fun () -> make_empty_chain chain size_chain)
  |> fun (res, t_chain) ->
  let _ =
    Printf.printf
      "#size_chain %i built in %f sec\n#      size      exp       lins\n"
      size_chain
      t_chain
  in
  res
  >>= fun head ->
  let check_locator max_size : unit tzresult Lwt.t =
    State.read_chain_data chain (fun _ data ->
        Lwt.return (data.caboose, data.save_point))
    >>= fun ((_, caboose), _save_point) ->
    State.Block.read chain head
    >>=? fun block ->
    time ~runs (fun () -> State.compute_locator chain ~max_size block seed)
    |> fun (l_exp, t_exp) ->
    time ~runs (fun () ->
        compute_linear_locator chain ~caboose ~size:max_size block)
    |> fun (l_lin, t_lin) ->
    l_exp
    >>= fun l_exp ->
    l_lin
    >>= fun l_lin ->
    let (_, l_exp) = (l_exp : Block_locator.t :> _ * _) in
    let (_, l_lin) = (l_lin : Block_locator.t :> _ * _) in
    let _ = Printf.printf "%10i %f %f\n" max_size t_exp t_lin in
    Lwt.return
    @@ List.iter2
         ~when_different_lengths:(TzTrace.make @@ Exn (Failure __LOC__))
         (fun hn ho ->
           if not (Block_hash.equal hn ho) then
             Assert.fail_msg "Invalid locator %i" max_size)
         l_exp
         l_lin
  in
  let stop = locator_limit + 20 in
  let rec loop size =
    if size < stop then check_locator size >>=? fun _ -> loop (size + 5)
    else return_unit
  in
  loop 1

let test_protocol_locator base_dir =
  init_chain base_dir
  >>= fun chain ->
  let chain_length = 200 in
  let fork_points = [1; 10; 50; 66; 150] in
  (* further_points = List.tl fork_points @ [chain_length] *)
  let further_points = [10; 50; 66; 150; chain_length] in
  let fork_points_assoc =
    List.map2
      ~when_different_lengths:()
      (fun x y -> (x, y))
      fork_points
      further_points
    >|? List.mapi (fun i x -> (i + 1, x))
  in
  let fork_points_assoc =
    match fork_points_assoc with
    | Ok fork_points_assoc ->
        fork_points_assoc
    | Error () ->
        assert false
  in
  make_multiple_protocol_chain chain ~chain_length ~fork_points
  >>= fun head_hash ->
  Lwt_list.iter_s
    (fun (proto_level, (inf, sup)) ->
      State.compute_protocol_locator chain ~proto_level seed
      >>= function
      | None ->
          Assert.fail_msg
            "bad locator for proto level %d (%d, %d)"
            proto_level
            inf
            sup
      | Some locator -> (
          let open Block_locator in
          let steps = to_steps seed locator in
          let has_lower_bound = ref false in
          List.iter_es
            (fun {block; predecessor; _} ->
              State.Block.read chain block
              >>=? fun block ->
              State.Block.read chain predecessor
              >>=? fun predecessor ->
              has_lower_bound :=
                !has_lower_bound
                || Int32.to_int (State.Block.level predecessor) = inf ;
              Assert.is_true
                ~msg:"same proto_level"
                ( State.Block.protocol_level block
                  = State.Block.protocol_level predecessor
                && State.Block.protocol_level block = proto_level ) ;
              Assert.is_true
                ~msg:"increasing levels"
                Compare.Int32.(
                  State.Block.level block >= State.Block.level predecessor
                  && State.Block.level predecessor >= Int32.of_int inf
                  && State.Block.level block <= Int32.of_int sup) ;
              return_unit)
            steps
          >>= function
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_error error
          | Ok () ->
              Assert.is_true
                ~msg:"locator contains the lower bound block"
                !has_lower_bound ;
              Lwt.return_unit ))
    fork_points_assoc
  >>= fun () ->
  State.compute_protocol_locator chain ~proto_level:0 seed
  >>= (function
        | Some locator ->
            let (block_header, hash_list) =
              (locator :> Block_header.t * Block_hash.t list)
            in
            Assert.is_true
              ~msg:"no block in locator"
              (List.length hash_list = 0) ;
            State.Block.read chain genesis_hash
            >>=? fun b ->
            Assert.is_true
              ~msg:"single header is genesis"
              (Block_header.equal (State.Block.header b) block_header) ;
            return_unit
        | None ->
            Alcotest.fail "missing genesis locator")
  >>=? fun () ->
  State.compute_protocol_locator chain ~proto_level:6 seed
  >>= (function
        | Some _ -> Alcotest.fail "unexpected locator" | None -> return_unit)
  >>=? fun () ->
  (* Delete some blocks: only the last protocol remains with only 30+1 blocks *)
  State.Block.read_predecessor chain ~pred:30 head_hash
  >>= (function Some pred -> return pred | None -> assert false)
  >>=? fun pred ->
  State.Chain.set_checkpoint_then_purge_rolling chain (State.Block.header pred)
  >>=? fun () ->
  List.iter_es
    (fun i ->
      State.compute_protocol_locator chain ~proto_level:i seed
      >>= function
      | Some _ ->
          Alcotest.fail "unexpected pruned locator"
      | None ->
          return_unit)
    (1 -- 4)
  >>=? fun () ->
  State.compute_protocol_locator chain ~proto_level:5 seed
  >>= function
  | None ->
      Alcotest.fail "unexpected missing locator after pruning"
  | Some locator ->
      let open Block_locator in
      let steps = to_steps seed locator in
      let has_lower_bound = ref false in
      let inf = 170 in
      let sup = 200 in
      List.iter_es
        (fun {block; predecessor; _} ->
          State.Block.read chain block
          >>=? fun block ->
          State.Block.read chain predecessor
          >>=? fun predecessor ->
          has_lower_bound :=
            !has_lower_bound
            || Int32.to_int (State.Block.level predecessor) = inf ;
          Assert.is_true
            ~msg:"same proto_level after pruning"
            ( State.Block.protocol_level block
              = State.Block.protocol_level predecessor
            && State.Block.protocol_level block = 5 ) ;
          Assert.is_true
            ~msg:"increasing levels after pruning"
            Compare.Int32.(
              State.Block.level block >= State.Block.level predecessor
              && State.Block.level predecessor >= Int32.of_int inf
              && State.Block.level block <= Int32.of_int sup) ;
          return_unit)
        steps
      >>=? fun () ->
      let last_hash =
        (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd steps).predecessor
      in
      Assert.is_true
        ~msg:"last block in locator is the checkpoint"
        (Block_hash.equal last_hash (State.Block.hash pred)) ;
      let first_hash =
        (WithExceptions.Option.get ~loc:__LOC__ @@ List.last_opt steps).block
      in
      Assert.is_true
        ~msg:"first block in locator is the head"
        (Block_hash.equal first_hash head_hash) ;
      return_unit

let wrap n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      Lwt_utils_unix.with_tempdir "tezos_test_" (fun dir ->
          f dir
          >>= function
          | Ok () ->
              Lwt.return_unit
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_error error))

let tests =
  [ wrap "test pred" test_pred;
    wrap "test protocol locator" test_protocol_locator ]

let bench = [wrap "locator" test_locator]

let tests =
  try if Sys.argv.(1) = "--no-bench" then tests else tests @ bench
  with _ -> tests @ bench

let () =
  Alcotest_lwt.run ~argv:[|""|] "tezos-shell" [("locator", tests)]
  |> Lwt_main.run
