(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Usage:
    dune exec devtools/proto_context_du/main.exe -- du path/to/data/dir
*)

module Util = struct
  let progress =
    let c = ref 0 in
    fun () ->
      incr c ;
      if !c mod 10_000 = 0 then Format.printf "progress... % 10d@." !c

  let list_assoc_rev_map_p f l =
    (* Actually doing it sequentially to avoid a stack overflow in Lwt. *)
    List.fold_left_s
      (fun acc (k, x) ->
        let open Lwt_syntax in
        let+ x = f x in
        (k, x) :: acc)
      []
      l
end

module Size_in_bytes = struct
  type t = int

  let pp fp size = Format.fprintf fp "% 12d\t" size
end

module Key = struct
  type t = 'a constraint 'a list = Tezos_protocol_environment.Context.key

  let compare = String.compare
end

module type KeySet = sig
  type t

  val empty : t

  val add : Key.t -> t -> t

  val union : t -> t -> t

  val cardinal : t -> int option

  val size : t -> Size_in_bytes.t option
end

module type AbstractionConfig = sig
  val star_threshold : int
end

module PreciseKeySet : KeySet = struct
  module S = Set.Make (Key)

  type t = S.t

  let empty = S.empty

  let add = S.add

  let union = S.union

  let cardinal s = Some (S.cardinal s)

  let size s = Some (S.fold (fun k acc -> acc + String.length k) s 0)
end

module UnitKeySet : KeySet = struct
  type t = unit

  let empty = ()

  let add _k () = ()

  let union () () = ()

  let cardinal () = None

  let size () = None
end

module TreeAbstraction (KeySet : KeySet) (AConf : AbstractionConfig) = struct
  module KeyMap = struct
    include Map.Make (Key)

    let keys ?(init = KeySet.empty) m =
      fold (fun k _v acc -> KeySet.add k acc) m init
  end

  (* Types *)

  type atree = {value_size_in_bytes : Size_in_bytes.t; node : anode}

  and anode =
    | Node of atree KeyMap.t
    | Star of {keys : KeySet.t; subtree : atree}

  (* Empty *)

  let empty_node = Node KeyMap.empty

  let empty = {value_size_in_bytes = 0; node = empty_node}

  (* Collecting keys *)

  let rec merge_all_keys_in_atree acc {value_size_in_bytes = _; node} =
    merge_all_keys_in_anode acc node

  and merge_all_keys_in_anode acc = function
    | Node km ->
        KeyMap.fold
          (fun k t acc ->
            let acc = merge_all_keys_in_atree acc t in
            KeySet.add k acc)
          km
          acc
    | Star {keys; subtree} ->
        let acc = KeySet.union keys acc in
        merge_all_keys_in_atree acc subtree

  let all_keys_in_atree atree = merge_all_keys_in_atree KeySet.empty atree

  (* Abstracting / Merging *)

  let rec merge_atrees t1 t2 =
    let {value_size_in_bytes = vs1; node = n1} = t1 in
    let {value_size_in_bytes = vs2; node = n2} = t2 in
    let value_size_in_bytes = vs1 + vs2 in
    let node = merge_anodes n1 n2 in
    {value_size_in_bytes; node}

  and merge_anodes n1 n2 =
    match (n1, n2) with
    | Node km1, Node km2 ->
        let km =
          KeyMap.merge
            (fun _k t1 t2 ->
              match (t1, t2) with
              | Some t1, Some t2 -> Some (lazy (merge_atrees t1 t2))
              | None, Some t | Some t, None -> Some (lazy t)
              | None, None -> assert false)
            km1
            km2
        in
        let number_of_keys = KeyMap.cardinal km in
        if number_of_keys > AConf.star_threshold then
          let keys = KeyMap.keys km in
          let subtree = merge_atrees (merge_all km1) (merge_all km2) in
          Star {keys; subtree}
        else
          let km = KeyMap.map Lazy.force km in
          Node km
    | Star {keys = k1; subtree = st1}, Star {keys = k2; subtree = st2} ->
        let keys = KeySet.union k1 k2 in
        let subtree = merge_atrees st1 st2 in
        Star {keys; subtree}
    | Node km, Star {keys; subtree} | Star {keys; subtree}, Node km ->
        let keys = KeyMap.keys ~init:keys km in
        let subtree = merge_atrees subtree (merge_all km) in
        Star {keys; subtree}

  and merge_all km = KeyMap.fold (fun _k t acc -> merge_atrees t acc) km empty

  let node_of_subatrees subatrees =
    let km =
      List.fold_left
        (fun acc (key, atree) -> KeyMap.add key atree acc)
        KeyMap.empty
        subatrees
    in
    (* Just to apply abstraction *)
    merge_anodes empty_node (Node km)

  (* Abstraction entrypoint *)

  let rec abstract_tree tree =
    let open Lwt_syntax in
    let () = Util.progress () in
    let* value_opt = Tezos_protocol_environment.Context.Tree.find tree [] in
    match value_opt with
    | Some v ->
        let value_size_in_bytes = Bytes.length v in
        return {empty with value_size_in_bytes}
    | None ->
        let* subtrees = Tezos_protocol_environment.Context.Tree.list tree [] in
        let+ subatrees = Util.list_assoc_rev_map_p abstract_tree subtrees in
        let node = node_of_subatrees subatrees in
        {empty with node}

  (* Printing *)

  let rec print_atree ~prefix atree =
    let {value_size_in_bytes; node} = atree in
    let size_in_node = print_anode ~prefix node in
    let () =
      if value_size_in_bytes > 0 then
        Format.printf "%a%s\n" Size_in_bytes.pp value_size_in_bytes prefix
    in
    let size_in_tree = value_size_in_bytes + size_in_node in
    let () =
      if size_in_node > 0 then
        Format.printf "%a%s/...\n" Size_in_bytes.pp size_in_tree prefix
    in
    size_in_tree

  and print_anode ~prefix anode =
    let prefix = prefix ^ "/" in
    match anode with
    | Node km ->
        KeyMap.fold
          (fun k t acc ->
            let size_in_tree = print_atree ~prefix:(prefix ^ k) t in
            acc + size_in_tree)
          km
          0
    | Star {keys; subtree} ->
        let k =
          match KeySet.cardinal keys with
          | None -> "*"
          | Some card -> Format.asprintf "*(%d)" card
        in
        print_atree ~prefix:(prefix ^ k) subtree

  let print_tree tree =
    let open Lwt_syntax in
    let+ atree = abstract_tree tree in
    let (_total_size : int) = print_atree ~prefix:"" atree in
    match KeySet.size (all_keys_in_atree atree) with
    | None -> ()
    | Some key_size ->
        Format.printf "%a%s\n" Size_in_bytes.pp key_size "size of all keys\n"
end

module Commands = struct
  let du ~genesis ~precise ~star_threshold ~data_dir =
    let open Lwt_result_syntax in
    Format.printf "Initializing store from data dir '%s'...@." data_dir ;
    let* store =
      Tezos_store.Store.init
        ~store_dir:(Filename.concat data_dir "store")
        ~context_dir:(Filename.concat data_dir "context")
        ~allow_testchains:true
        ~readonly:true
        genesis
    in
    Format.printf "Getting main chain storage and head...@." ;
    let chain_store = Tezos_store.Store.main_chain_store store in
    let chain_id = Tezos_store.Store.Chain.chain_id chain_store in
    Format.printf "Chain id: %a@." Chain_id.pp chain_id ;
    let*! head = Tezos_store.Store.Chain.current_head chain_store in
    Format.printf
      "Head block: %a@."
      Block_hash.pp
      (Tezos_store.Store.Block.hash head) ;
    let* proto_hash = Tezos_store.Store.Block.protocol_hash chain_store head in
    Format.printf "Protocol hash: %a@." Protocol_hash.pp proto_hash ;
    let*! ctxt = Tezos_store.Store.Block.context_exn chain_store head in
    (* Getting tree root *)
    let*! tree = Tezos_protocol_environment.Context.find_tree ctxt [] in
    let tree = WithExceptions.Option.get ~loc:__LOC__ tree in
    let module AConf = struct
      let star_threshold = star_threshold
    end in
    let key_set =
      if precise then (module PreciseKeySet : KeySet)
      else (module UnitKeySet : KeySet)
    in
    let module KeySet = (val key_set) in
    let module TreeAbstraction = TreeAbstraction (KeySet) (AConf) in
    let*! () = TreeAbstraction.print_tree tree in
    return_unit
end

let string_parameter =
  Tezos_clic.parameter (fun _ctx s -> Lwt_result_syntax.return s)

let data_dir_param =
  Tezos_clic.param ~name:"data-dir" ~desc:"Path to context" string_parameter

let network_parameter =
  Tezos_clic.parameter (fun () network_name ->
      match
        List.assoc ~equal:String.equal network_name Config.known_networks
      with
      | None -> failwith "Unknown network name"
      | Some n -> Lwt_result_syntax.return n)

let network_arg =
  Tezos_clic.default_arg
    ~doc:"Network to use"
    ~long:"network"
    ~placeholder:"network name"
    ~default:"mainnet"
    network_parameter

let precision_arg =
  Tezos_clic.switch ~doc:"Use high-precision of keysets" ~long:"precise" ()

let non_negative_parser _ctxt s =
  match int_of_string_opt s with
  | Some i when i >= 0 -> Lwt_result_syntax.return i
  | _ -> failwith "Parameter should be a non-negative integer literal"

let non_negative_parameter = Tezos_clic.parameter non_negative_parser

let star_threshold_arg =
  Tezos_clic.default_arg
    ~doc:"Threshold for merging subtrees into a star"
    ~long:"star-threshold"
    ~placeholder:"star threshold"
    ~default:"30"
    non_negative_parameter

let commands =
  let open Tezos_clic in
  [
    command
      ~desc:"Printout disk usage of the storage"
      (args3 network_arg precision_arg star_threshold_arg)
      (prefix "du" @@ data_dir_param @@ stop)
      (fun (genesis, precise, star_threshold) data_dir () ->
        Commands.du ~genesis ~precise ~star_threshold ~data_dir);
  ]

let run () =
  let argv = Sys.argv |> Array.to_list |> List.tl |> Option.value ~default:[] in
  Tezos_clic.dispatch commands () argv

let () =
  match Lwt_main.run (run ()) with
  | Ok () -> ()
  | Error trace ->
      Format.printf
        "ERROR: %a%!"
        (Tezos_clic.pp_cli_errors
           ~executable_name:Sys.argv.(0)
           ~global_options:Tezos_clic.no_options
           ~default:Error_monad.pp)
        trace
