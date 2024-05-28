(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Helper functions to handle operation results  *)

(** Returns the content of the "operation_result" field from the metadata of the
    operation with hash [operation] in the [check_previous] blocks before the
    current head using  the 'get_chain_block_operations' RPC.
*)
let get_result_for operation ?(check_previous = 10) client =
  let rec aux i =
    if i >= check_previous then failwith "Operation not found"
    else
      let block = Format.sprintf "head~%d" i in
      let* operations =
        Client.RPC.call client
        @@ RPC.get_chain_block_operations_validation_pass
             ~block
             ~force_metadata:true
             ~validation_pass:3
             ()
      in
      let manager_ops = JSON.as_list operations in
      match
        List.find_opt
          (fun op -> JSON.(op |-> "hash" |> as_string = operation))
          manager_ops
      with
      | Some op ->
          let operation_contents = JSON.(op |-> "contents" |> as_list) in
          Log.info "Content of size %d" (List.length operation_contents) ;
          let metadata =
            List.map (fun op -> JSON.(op |-> "metadata")) operation_contents
          in
          return
          @@ List.map (fun json -> JSON.(json |-> "operation_result")) metadata
      | None -> aux (i + 1)
  in
  aux 0

let get_block_metadata client =
  Client.RPC.call client @@ RPC.get_chain_block_metadata_raw ()

module Balance_updates = struct
  type staker =
    | Delegate of {delegate : string; contract : string option}
    | Baker_own_stake of {baker : string}
    | Baker_edge of {baker : string}

  type t = {
    kind : string;
    contract : string option;
    change : int;
    staker : staker option;
    category : string option;
    delayed_operation_hash : string option;
    origin : string;
  }

  let from_result operation_results =
    let bu =
      List.flatten
      @@ List.map
           (fun op ->
             Log.info "result: %s" (JSON.encode op) ;
             JSON.(op |-> "balance_updates" |> as_list))
           operation_results
    in
    return
    @@ List.map
         (fun json ->
           let kind = JSON.(json |-> "kind" |> as_string) in
           let category = JSON.(json |-> "category" |> as_string_opt) in
           let contract = JSON.(json |-> "contract" |> as_string_opt) in
           let change = JSON.(json |-> "change" |> as_int) in
           let staker =
             match JSON.(json |-> "staker" |> as_opt) with
             | Some json -> (
                 match JSON.(json |-> "delegate" |> as_string_opt) with
                 | Some delegate ->
                     Some
                       (Delegate
                          {
                            delegate;
                            contract =
                              JSON.(json |-> "contract" |> as_string_opt);
                          })
                 | None -> (
                     match
                       JSON.(json |-> "baker_own_stake" |> as_string_opt)
                     with
                     | Some baker -> Some (Baker_own_stake {baker})
                     | None -> (
                         match
                           JSON.(json |-> "baker_edge" |> as_string_opt)
                         with
                         | Some baker -> Some (Baker_edge {baker})
                         | None -> None)))
             | None -> None
           in
           let delayed_operation_hash =
             JSON.(json |-> "delayed_operation_hash" |> as_string_opt)
           in
           let origin = JSON.(json |-> "origin" |> as_string) in
           {
             kind;
             contract;
             change;
             staker;
             category;
             delayed_operation_hash;
             origin;
           })
         bu

  let to_string t =
    let staker_to_string = function
      | Delegate {delegate; contract} ->
          Format.sprintf
            "Delegate %s %s"
            delegate
            (Option.value ~default:"" contract)
      | Baker_own_stake {baker} -> Format.sprintf "Baker_own_stake %s" baker
      | Baker_edge {baker} -> Format.sprintf "Baker_edge %s" baker
    in
    let staker_to_string = function
      | Some staker -> staker_to_string staker
      | None -> "None"
    in
    Format.sprintf
      "{kind: %s\n\
       contract: %s\n\
       change: %d\n\
       staker: %s\n\
       category: %s\n\
       delayed_operation_hash: %s\n\
       origin: %s}"
      t.kind
      (Option.value ~default:"None" t.contract)
      t.change
      (staker_to_string t.staker)
      (Option.value ~default:"None" t.category)
      (Option.value ~default:"None" t.delayed_operation_hash)
      t.origin
end
