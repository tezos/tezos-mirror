(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let mkdir dirname =
  try Unix.mkdir dirname 0o775 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

module Make (P : Sigs.PROTOCOL) : Sigs.MAIN = struct
  type serialization_costs = {decode : int; encode : int}

  module Storage_helpers = struct
    include Storage_helpers

    let get_lazy_expr ~what ~getter ~pp ctxt x =
      let open Lwt_result_syntax in
      let+ _, expr = get_value ~what ~getter ~pp ctxt x in
      let expr, decode, encode = P.Script.decode_and_costs expr in
      (expr, {decode; encode})
  end

  module ExprMap = Map.Make (P.Script.Hash)

  type gas = {
    code_costs : serialization_costs;
    storage_costs : serialization_costs;
  }

  type storage = {
    contract : P.Contract.repr;
    storage : P.Script.expr;
    gas : gas option;
  }

  type contract = {
    script : P.Script.expr;
    addresses : P.Contract.repr list;
    storages : storage ExprMap.t;
  }

  module File_helpers = struct
    let print_to_file ?err filename fmt =
      Format.kasprintf
        (fun s ->
          let chan = open_out filename in
          (try output_string chan s
           with exn ->
             flush chan ;
             close_out chan ;
             Option.iter (fun f -> f ()) err ;
             raise exn) ;
          flush chan ;
          close_out chan ;
          match err with
          | None -> ()
          | Some err ->
              let input_chan = open_in filename in
              let file_length = in_channel_length input_chan in
              close_in input_chan ;
              if file_length < 2 then err ())
        fmt

    let print_legacy_file ~dirname ~ext ~hash_string flag =
      let filename = Filename.concat dirname (hash_string ^ ext) in
      let err () =
        Format.eprintf "Could not print legacy flag file %s\n\n" filename
      in
      print_to_file ~err filename "%s@." (if flag then "--legacy" else "")

    let print_expr_file ~dirname ~ext ~hash_string expr =
      let filename = Filename.concat dirname (hash_string ^ ext) in
      let err () = Format.eprintf "Could not print expr file %s\n\n" filename in
      print_to_file ~err filename "%a@." P.Script.print_expr expr

    let print_expr_dir ~dirname ~ext exprs =
      if not (ExprMap.is_empty exprs) then
        let () = mkdir dirname in
        ExprMap.iter
          (fun hash expr ->
            let hash_string = P.Script.Hash.to_b58check hash in
            print_expr_file ~dirname ~ext ~hash_string expr)
          exprs

    let print_legacy_dir ~dirname ~ext exprs =
      if not (ExprMap.is_empty exprs) then
        let () = mkdir dirname in
        ExprMap.iter
          (fun hash flag ->
            let hash_string = P.Script.Hash.to_b58check hash in
            print_legacy_file ~dirname ~ext ~hash_string flag)
          exprs
  end

  module Michelson_helpers = struct
    let hash_expr expr =
      let open P in
      let bytes = Data_encoding.Binary.to_bytes_exn Script.expr_encoding expr in
      Script.Hash.hash_bytes [bytes]

    let parse_ty ctxt type_expr =
      let hashed_ty = hash_expr type_expr in
      match
        P.Translator.parse_ty
          ctxt
          ~allow_lazy_storage:true
          ~allow_operation:true
          ~allow_contract:true
          ~allow_ticket:true
          (Micheline.root type_expr)
      with
      | Ok ex_ty -> (hashed_ty, ex_ty)
      | Error _ -> assert false

    let get_script_storage_type ctxt script_expr =
      let open Lwt_syntax in
      let+ result = P.Translator.parse_toplevel ctxt script_expr in
      match result with
      | Ok code ->
          let storage_type_expr =
            Micheline.strip_locations @@ P.code_storage_type code
          in
          parse_ty ctxt storage_type_expr
      | Error _ ->
          P.Script.print_expr Format.std_formatter script_expr ;
          assert false

    module BytesSet = Set.Make (Bytes)

    let rec expr_collect_bytes acc expr =
      let open Micheline in
      match expr with
      | Int _ -> acc
      | String (_loc, s) ->
          (* Just in case... *) BytesSet.add (Bytes.of_string s) acc
      | Bytes (_loc, b) -> BytesSet.add b acc
      | Prim (_loc, _, exprs, _) | Seq (_loc, exprs) ->
          List.fold_left expr_collect_bytes acc exprs

    let rec expr_collect_unpack_types ~parse_ty acc expr =
      let open Micheline in
      match expr with
      | Int _ | String _ | Bytes _ -> acc
      | Prim (_, prim, [ty], _) when P.is_unpack prim ->
          let ty_expr = Micheline.strip_locations ty in
          let ty_hash, ty = parse_ty ty_expr in
          ExprMap.add ty_hash ty acc
      | Prim (_, _, exprs, _) | Seq (_, exprs) ->
          List.fold_left (expr_collect_unpack_types ~parse_ty) acc exprs

    let try_decode_expr bytes =
      let try_decode bytes =
        match
          Data_encoding.Binary.of_bytes_opt P.Script.expr_encoding bytes
        with
        | Some x -> [x]
        | None -> []
        | exception _ -> []
      in
      try_decode bytes
      @
      if Bytes.length bytes >= 1 && Bytes.get bytes 0 = '\005' then
        try_decode (Bytes.sub bytes 1 (Bytes.length bytes - 1))
      else []

    let unpack_transitive_closure exprs =
      let bytes =
        ExprMap.fold
          (fun _hash (expr, _from_unpack, _types) bytes ->
            expr_collect_bytes bytes (Micheline.root expr))
          exprs
          BytesSet.empty
      in
      let rec aux exprs bytes =
        let exprs, new_exprs =
          BytesSet.fold
            (fun bytes (exprs, new_exprs) ->
              try_decode_expr bytes
              |> List.fold_left
                   (fun (exprs, new_exprs) expr ->
                     let expr_hash = hash_expr expr in
                     match ExprMap.find expr_hash exprs with
                     | None ->
                         let exprs =
                           ExprMap.add
                             expr_hash
                             (expr, true, ExprMap.empty)
                             exprs
                         in
                         (exprs, expr :: new_exprs)
                     | Some (_, true, _) -> (exprs, new_exprs)
                     | Some (_, false, types) ->
                         let exprs =
                           ExprMap.add expr_hash (expr, true, types) exprs
                         in
                         (exprs, new_exprs))
                   (exprs, new_exprs))
            bytes
            (exprs, [])
        in
        match new_exprs with
        | [] -> exprs
        | _ :: _ ->
            let bytes =
              List.fold_left
                (fun bytes expr ->
                  expr_collect_bytes bytes (Micheline.root expr))
                BytesSet.empty
                new_exprs
            in
            aux exprs bytes
      in
      aux exprs bytes

    let collect_unpack_types ~parse_ty exprs =
      ExprMap.fold
        (fun _hash (expr, _, _) types ->
          expr_collect_unpack_types ~parse_ty types (Micheline.root expr))
        exprs
        ExprMap.empty

    let keep_lambda_types types =
      ExprMap.fold
        (fun hash ty types ->
          match P.Lambda.collect_lambda_tys ty with
          | None -> types
          | Some ex_ty_lambdas -> ExprMap.add hash ex_ty_lambdas types)
        types
        ExprMap.empty

    let collect_lambdas_in_exprs ctxt exprs unpack_types =
      let open Lwt_syntax in
      let unpack_types = keep_lambda_types unpack_types in
      ExprMap.fold
        (fun _hash (expr, from_unpack, types) lambdas_lwt ->
          let* lambdas = lambdas_lwt in
          let types = keep_lambda_types types in
          let types =
            if from_unpack then
              ExprMap.union (fun _k l _r -> Some l) types unpack_types
            else types
          in
          ExprMap.fold
            (fun _ty_hash ex_ty_lambdas lambdas_lwt ->
              let* lambdas = lambdas_lwt in
              P.Lambda.fold_ex_ty_lambdas
                ~ctxt
                ~expr:(Micheline.root expr)
                ~acc:lambdas
                ~f:(fun acc ty_node lam_nodes ->
                  List.fold_left
                    (fun (acc_lambdas, acc_ty, acc_legacy) lambda ->
                      let ty_expr = Micheline.strip_locations ty_node in
                      let lam_expr =
                        Micheline.strip_locations @@ P.Lambda.lam_node lambda
                      in
                      let lam_hash = hash_expr lam_expr in
                      let acc_lambdas =
                        ExprMap.add lam_hash lam_expr acc_lambdas
                      in
                      let acc_ty = ExprMap.add lam_hash ty_expr acc_ty in
                      let acc_legacy =
                        ExprMap.add lam_hash (not from_unpack) acc_legacy
                      in
                      (acc_lambdas, acc_ty, acc_legacy))
                    acc
                    lam_nodes)
                ex_ty_lambdas)
            types
            (Lwt.return lambdas))
        exprs
        (Lwt.return (ExprMap.empty, ExprMap.empty, ExprMap.empty))

    let add_expr_to_map ctxt contract (m, i) =
      let open Lwt_syntax in
      let i = i + 1 in
      if i mod 5000 = 0 then Format.printf "%d@." i ;
      if P.Contract.is_implicit contract then Lwt.return (m, i)
      else
        let* code_opt =
          Storage_helpers.get_lazy_expr
            ~what:"contract code"
            ~getter:P.Contract.get_code
            ~pp:P.Contract.pp
            ctxt
            contract
        in
        match code_opt with
        | Error `AlreadyWarned -> Lwt.return (m, i)
        | Ok (script, code_costs) ->
            let+ add_storage =
              if Config.(collect_lambdas || collect_storage || collect_gas) then
                let+ storage_opt =
                  Storage_helpers.get_lazy_expr
                    ~what:"contract storage"
                    ~getter:P.Contract.get_storage
                    ~pp:P.Contract.pp
                    ctxt
                    contract
                in
                match storage_opt with
                | Error `AlreadyWarned -> fun x -> x
                | Ok (storage, storage_costs) ->
                    let gas =
                      if Config.collect_gas then
                        Some {code_costs; storage_costs}
                      else None
                    in
                    let key = hash_expr storage in
                    fun storages ->
                      ExprMap.add key {contract; storage; gas} storages
              else Lwt.return (fun x -> x)
            in
            let key = hash_expr script in
            ( ExprMap.update
                key
                (fun existing ->
                  let contracts, storages =
                    match existing with
                    | Some {addresses; storages; _} -> (addresses, storages)
                    | None -> ([], ExprMap.empty)
                  in
                  let addresses = contract :: contracts in
                  let storages = add_storage storages in
                  Some {script; addresses; storages})
                m,
              i )
  end

  let big_map_fold :
      P.context * P.Storage.big_map_id ->
      init:'a ->
      f:
        (P.Script.prim Tezos_micheline.Micheline.canonical ->
        'a ->
        'a Error_monad.tzresult Lwt.t) ->
      'a Error_monad.tzresult Lwt.t =
   fun ctxt_i ~init ~f ->
    let open Lwt_result_syntax in
    let* _ctxt, values = P.Storage.list_values ctxt_i in
    List.fold_left_es (fun acc v -> f v acc) init values

  let output_contract_results ctxt output_dir hash ctr total_size =
    let open Lwt_result_syntax in
    let hash_string = P.Script.Hash.to_b58check hash in
    File_helpers.print_expr_file
      ~dirname:output_dir
      ~ext:".tz"
      ~hash_string
      ctr.script ;
    let filename ~ext =
      Filename.concat output_dir (Format.sprintf "%s.%s" hash_string ext)
    in
    File_helpers.print_to_file
      (filename ~ext:"address")
      "%a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline P.Contract.pp)
      ctr.addresses ;
    let* contract_size =
      if Config.measure_code_size then (
        let* script_code =
          P.Translator.parse_code ctxt @@ P.Script.lazy_expr ctr.script
        in
        let size =
          Contract_size.
            {
              expected = P.Translator.expected_code_size script_code;
              actual = P.Translator.actual_code_size script_code;
            }
        in
        File_helpers.print_to_file
          (filename ~ext:"size")
          "%a"
          Contract_size.pp
          size ;
        return size)
      else return Contract_size.zero
    in
    (if Config.collect_storage then
     let dirname = Filename.concat output_dir (hash_string ^ ".storage") in
     let storages = ExprMap.map (fun {storage; _} -> storage) ctr.storages in
     File_helpers.print_expr_dir ~dirname ~ext:".storage" storages) ;
    return @@ Contract_size.add contract_size total_size

  let main ~output_dir ctxt ~head : unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let head_hash, head_level = Tezos_store.Store.Block.descriptor head in
    Format.printf "Head is %a, level %ld\n%!" Block_hash.pp head_hash head_level ;
    let predecessor_timestamp = Tezos_store.Store.Block.timestamp head in
    let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
    print_endline "Preparing raw context..." ;
    let* ctxt =
      P.Context.prepare ~level:head_level ~predecessor_timestamp ~timestamp ctxt
    in
    print_endline "Listing addresses..." ;
    let*! contract_map, _ =
      P.Contract.fold
        ctxt
        ~init:(ExprMap.empty, 0)
        ~f:Michelson_helpers.(add_expr_to_map ctxt)
    in
    print_endline "Listing addresses done." ;
    let* lambda_map, lambda_ty_map, lambda_legacy_map =
      if Config.collect_lambdas then (
        let add_typed_expr hash expr ty_hash ty exprs =
          ExprMap.update
            hash
            (fun existing ->
              let type_map =
                match existing with
                | None -> ExprMap.empty
                | Some (_expr, _from_unpack, type_map) -> type_map
              in
              Some (expr, false, ExprMap.add ty_hash ty type_map))
            exprs
        in
        print_endline "Getting expressions from contracts..." ;
        let* exprs =
          ExprMap.fold_es
            (fun hash {script; storages; _} exprs ->
              let exprs =
                ExprMap.add hash (script, false, ExprMap.empty) exprs
              in
              let*! ty_hash, ty =
                Michelson_helpers.get_script_storage_type ctxt script
              in
              ExprMap.fold_es
                (fun storage_hash {contract = _; storage; gas = _} exprs ->
                  return @@ add_typed_expr storage_hash storage ty_hash ty exprs)
                storages
                exprs)
            contract_map
            ExprMap.empty
        in
        print_endline "Listing big maps..." ;
        let* exprs, _ =
          P.Storage.fold
            ctxt
            ~init:(Ok (exprs, 0))
            ~f:(fun id exprs_i ->
              let* exprs, i = Lwt.return exprs_i in
              let i = i + 1 in
              if i mod 100 = 0 then Format.printf "%d@." i ;
              let*! value_opt =
                Storage_helpers.get_value
                  ~what:"big map value type"
                  ~getter:P.Storage.get
                  ~pp:(fun fmt id -> Z.pp_print fmt (P.Storage.id_to_z id))
                  ctxt
                  id
              in
              match value_opt with
              | Error `AlreadyWarned -> return (exprs, i)
              | Ok value_type_expr ->
                  let ty_hash, ty =
                    Michelson_helpers.parse_ty ctxt value_type_expr
                  in
                  let+ exprs =
                    big_map_fold (ctxt, id) ~init:exprs ~f:(fun v exprs ->
                        let v_hash = Michelson_helpers.hash_expr v in
                        return @@ add_typed_expr v_hash v ty_hash ty exprs)
                  in
                  (exprs, i))
        in
        print_endline "Computing unpack transitive closure of expressions..." ;
        let exprs = Michelson_helpers.unpack_transitive_closure exprs in
        print_endline "Collecting unpack types..." ;
        let unpack_types =
          let parse_ty = Michelson_helpers.parse_ty ctxt in
          Michelson_helpers.collect_unpack_types ~parse_ty exprs
        in
        print_endline "Collecting all lambdas..." ;
        let*! lambda_map =
          Michelson_helpers.collect_lambdas_in_exprs ctxt exprs unpack_types
        in
        return lambda_map)
      else return (ExprMap.empty, ExprMap.empty, ExprMap.empty)
    in
    print_endline "Writing contract files..." ;
    let* total_ir_size =
      ExprMap.fold_es
        (output_contract_results ctxt output_dir)
        contract_map
        Contract_size.zero
    in
    print_endline "Done writing contract files." ;
    if Config.measure_code_size then
      Format.printf
        "@[<v 2>Total IR size:@;%a@]@."
        Contract_size.pp
        total_ir_size ;
    let () =
      if not (ExprMap.is_empty lambda_map) then (
        print_endline "Writing lambda files..." ;
        let dirname = Filename.concat output_dir "lambdas" in
        File_helpers.print_expr_dir ~dirname ~ext:".tz" lambda_map ;
        File_helpers.print_expr_dir ~dirname ~ext:".ty" lambda_ty_map ;
        File_helpers.print_legacy_dir
          ~dirname
          ~ext:".legacy_flag"
          lambda_legacy_map ;
        print_endline "Done writing lambda files.")
      else ()
    in
    return ()
end

let ensure_target_dir_exists () =
  let dir = try Sys.argv.(2) with Invalid_argument _ -> "." in
  try
    if Sys.is_directory dir then dir
    else (
      Printf.printf "Given path: %s points to a file; directory expected!" dir ;
      exit 1)
  with Sys_error _ -> (
    Printf.printf "Directory %s does not exists. Create? [Y/n] " dir ;
    let response = read_line () in
    match response with
    | "" | "y" | "Y" ->
        mkdir dir ;
        dir
    | _ -> exit 0)

let run (module Main : Sigs.MAIN) =
  let open Lwt_result_syntax in
  let data_dir = Sys.argv.(1) in
  let output_dir = ensure_target_dir_exists () in
  Printf.printf "Initializing store from data dir '%s'...\n%!" data_dir ;
  let* store =
    Tezos_store.Store.init
      ~store_dir:(Filename.concat data_dir "store")
      ~context_dir:(Filename.concat data_dir "context")
      ~allow_testchains:true
      ~readonly:true
      Config.mainnet_genesis
  in
  Printf.printf "Getting main chain storage and head...\n%!" ;
  let chain_store = Tezos_store.Store.main_chain_store store in
  let chain_id = Tezos_store.Store.Chain.chain_id chain_store in
  Format.printf "Chain id: %a\n%!" Chain_id.pp chain_id ;
  let*! head = Tezos_store.Store.Chain.current_head chain_store in
  let* proto_hash = Tezos_store.Store.Block.protocol_hash chain_store head in
  Format.printf "Protocol is %a\n%!" Protocol_hash.pp proto_hash ;
  let*! ctxt = Tezos_store.Store.Block.context_exn chain_store head in
  print_endline "Pre-preparing raw context..." ;
  Main.main ~output_dir ctxt ~head

let main (module Main : Sigs.MAIN) =
  match Lwt_main.run (run (module Main)) with
  | Ok () -> ()
  | Error trace -> Format.printf "ERROR: %a%!" Error_monad.pp_print_trace trace
