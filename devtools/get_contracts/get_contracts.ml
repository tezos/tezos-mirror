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

module Options = struct
  let collect_storage = true

  let collect_lambdas = true
end

module Storage_helpers = struct
  let get_value ~what ~getter ~pp ctxt x =
    let open Lwt_syntax in
    let+ value = getter ctxt x in
    match value with
    | Ok x -> Some x
    | Error _ ->
        (* Should not happen *)
        Format.eprintf "Failed getting %s for %a\n" what pp x ;
        None

  let get_lazy_expr ~what ~getter ~pp ctxt x =
    let open Lwt_syntax in
    let+ expr_opt = get_value ~what ~getter ~pp ctxt x in
    Option.map
      (fun (_, expr) ->
        match Data_encoding.force_decode expr with
        | Some expr -> expr
        | None -> assert false)
      expr_opt
end

module Make (P : Sigs.PROTOCOL) = struct
  let ( >>= ) x f =
    let open Lwt_syntax in
    let* r = x in
    f r

  let ( >>=? ) x f =
    let open Lwt_result_syntax in
    let* r = x in
    f r

  let ( >|= ) x f =
    let open Lwt_syntax in
    let+ r = x in
    f r

  let ( >|=? ) x f =
    let open Lwt_result_syntax in
    let+ r = x in
    f r

  let return = Lwt_result_syntax.return

  let ok x = Ok x

  let ( >>?= ) x f = Lwt.return x >>=? f

  let mainnet_genesis =
    {
      Genesis.time = Time.Protocol.of_notation_exn "2018-06-30T16:07:32Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }

  let hangzhounet_genesis =
    {
      Genesis.time = Time.Protocol.of_notation_exn "2021-11-04T15:00:00Z";
      block =
        Block_hash.of_b58check_exn
          "BLockGenesisGenesisGenesisGenesisGenesis7e8c4d4snJW";
      protocol =
        Protocol_hash.of_b58check_exn
          "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P";
    }

  module ExprMap = Map.Make (P.Script.Hash)

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

    let mkdir dirname =
      try Unix.mkdir dirname 0o775
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

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
          ~legacy:true
          ~allow_lazy_storage:true
          ~allow_operation:true
          ~allow_contract:true
          ~allow_ticket:true
          (Micheline.root type_expr)
      with
      | Ok ex_ty -> (hashed_ty, ex_ty)
      | Error _ -> assert false

    let unparse_ty ctxt ty =
      match P.Translator.unparse_ty ctxt ty with
      | Ok node -> node
      | Error _ -> assert false

    let get_script_storage_type ctxt script_expr =
      P.Translator.parse_toplevel ctxt ~legacy:true script_expr >>= function
      | Ok code ->
          let storage_type_expr =
            Micheline.strip_locations @@ P.code_storage_type code
          in
          return (parse_ty ctxt storage_type_expr)
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

    type ex_lambda = P.Unparse_types.ex_lambda

    type ex_ty_lambdas =
      | Ex_ty_lambdas : 'a P.ty * ('a -> ex_lambda list) list -> ex_ty_lambdas

    let keep_lambda_types types =
      let open P.Translator in
      ExprMap.fold
        (fun hash (Ex_ty ty) types ->
          match P.Unparse_types.collect_lambda_tys ty with
          | [] -> types
          | g -> ExprMap.add hash (Ex_ty_lambdas (ty, g)) types)
        types
        ExprMap.empty

    let try_parse_data ctxt ty node =
      P.Translator.parse_data ctxt ~legacy:true ~allow_forged:true ty node
      >|= function
      | Error _ -> None
      | Ok v -> Some v

    let collect_lambdas_in_exprs ctxt exprs unpack_types =
      let unpack_types = keep_lambda_types unpack_types in
      ExprMap.fold
        (fun _hash (expr, from_unpack, types) lambdas_lwt ->
          lambdas_lwt >>= fun lambdas ->
          let types = keep_lambda_types types in
          let types =
            if from_unpack then
              ExprMap.union (fun _k l _r -> Some l) types unpack_types
            else types
          in
          ExprMap.fold
            (fun _ty_hash (Ex_ty_lambdas (ty, getters)) lambdas_lwt ->
              lambdas_lwt >>= fun lambdas ->
              try_parse_data ctxt ty (Micheline.root expr) >|= function
              | None -> lambdas
              | Some v ->
                  List.fold_left
                    (fun acc getter ->
                      let lam_nodes = getter v in
                      List.fold_left
                        (fun (acc_lambdas, acc_ty, acc_legacy)
                             (P.Unparse_types.Ex_lambda (ty, lam)) ->
                          let ty_expr =
                            Micheline.strip_locations (unparse_ty ctxt ty)
                          in
                          let lam_expr =
                            Micheline.strip_locations @@ P.lam_node lam
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
                    lambdas
                    getters)
            types
            (Lwt.return lambdas))
        exprs
        (Lwt.return (ExprMap.empty, ExprMap.empty, ExprMap.empty))

    let add_expr_to_map ctxt contract (m, i) =
      let i = i + 1 in
      if i mod 5000 = 0 then Format.printf "%d@." i ;
      match P.Contract.is_implicit contract with
      | Some _key_hash -> Lwt.return (m, i)
      | None -> (
          Storage_helpers.get_lazy_expr
            ~what:"contract code"
            ~getter:P.Contract.get_code
            ~pp:P.Contract.pp
            ctxt
            contract
          >>= function
          | None -> Lwt.return (m, i) (* Should not happen *)
          | Some code ->
              (if Options.(collect_lambdas || collect_storage) then
               Storage_helpers.get_lazy_expr
                 ~what:"contract storage"
                 ~getter:P.Contract.get_storage
                 ~pp:P.Contract.pp
                 ctxt
                 contract
               >|= function
               | None -> fun x -> x
               | Some storage ->
                   let key = hash_expr storage in
                   fun storages -> ExprMap.add key storage storages
              else Lwt.return (fun x -> x))
              >|= fun add_storage ->
              let key = hash_expr code in
              ( ExprMap.update
                  key
                  (fun existing ->
                    let contracts, storages =
                      match existing with
                      | Some (_code, contracts, storages) ->
                          (contracts, storages)
                      | None -> ([], ExprMap.empty)
                    in
                    let contracts = contract :: contracts in
                    let storages = add_storage storages in
                    Some (code, contracts, storages))
                  m,
                i ))
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
    P.Storage.list_values ctxt_i >>=? fun (_ctxt, values) ->
    List.fold_left_es (fun acc v -> f v acc) init values

  let main () : unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let data_dir = Sys.argv.(1) in
    Printf.printf "Initializing store from data dir '%s'...\n%!" data_dir ;
    let* store =
      Tezos_store.Store.init
        ~store_dir:(Filename.concat data_dir "store")
        ~context_dir:(Filename.concat data_dir "context")
        ~allow_testchains:true
        ~readonly:true
        mainnet_genesis
    in
    Printf.printf "Getting main chain storage and head...\n%!" ;
    let chain_store = Tezos_store.Store.main_chain_store store in
    let chain_id = Tezos_store.Store.Chain.chain_id chain_store in
    Format.printf "Chain id: %a\n%!" Chain_id.pp chain_id ;
    let*! head = Tezos_store.Store.Chain.current_head chain_store in
    let head_hash, head_level = Tezos_store.Store.Block.descriptor head in
    Format.printf "Head is %a, level %ld\n%!" Block_hash.pp head_hash head_level ;
    let* proto_hash = Tezos_store.Store.Block.protocol_hash chain_store head in
    Format.printf "Protocol is %a\n%!" Protocol_hash.pp proto_hash ;
    let*! ctxt = Tezos_store.Store.Block.context_exn chain_store head in
    print_endline "Pre-preparing raw context..." ;
    let ctxt = Tezos_shell_context.Shell_context.wrap_disk_context ctxt in
    let open P in
    let predecessor_timestamp = Tezos_store.Store.Block.timestamp head in
    let fitness = Tezos_store.Store.Block.fitness head in
    let timestamp = Time.Protocol.add predecessor_timestamp 10000L in
    print_endline "Preparing raw context..." ;
    let* ctxt =
      Context.prepare
        ~level:head_level
        ~predecessor_timestamp
        ~timestamp
        ~fitness
        ctxt
    in
    print_endline "Listing addresses..." ;
    let*! contract_map, _ =
      Contract.fold
        ctxt
        ~init:(ExprMap.empty, 0)
        ~f:Michelson_helpers.(add_expr_to_map ctxt)
    in
    print_endline "Listing addresses done." ;
    let* contract_map, (lambda_map, lambda_ty_map, lambda_legacy_map) =
      if Options.collect_lambdas then (
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
            (fun hash (script, _contracts, storages) exprs ->
              let exprs =
                ExprMap.add hash (script, false, ExprMap.empty) exprs
              in
              let* ty_hash, ty =
                Michelson_helpers.get_script_storage_type ctxt script
              in
              ExprMap.fold_es
                (fun storage_hash storage_expr exprs ->
                  return
                  @@ add_typed_expr storage_hash storage_expr ty_hash ty exprs)
                storages
                exprs)
            contract_map
            ExprMap.empty
        in
        print_endline "Listing big maps..." ;
        let* exprs, _ =
          Storage.fold
            ctxt
            ~init:(ok (exprs, 0))
            ~f:(fun id exprs_i ->
              let* exprs, i = Lwt.return exprs_i in
              let i = i + 1 in
              if i mod 100 = 0 then Format.printf "%d@." i ;
              let*! value_opt =
                Storage_helpers.get_value
                  ~what:"big map value type"
                  ~getter:P.Storage.get
                  ~pp:(fun fmt id -> Z.pp_print fmt (Storage.id_to_z id))
                  ctxt
                  id
              in
              match value_opt with
              | None -> return (exprs, i) (* should not happen *)
              | Some value_type_expr ->
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
        return (contract_map, lambda_map))
      else return (contract_map, (ExprMap.empty, ExprMap.empty, ExprMap.empty))
    in
    print_endline "Writing contract files..." ;
    ExprMap.iter
      (fun hash (script, contracts, storages) ->
        let hash_string = P.Script.Hash.to_b58check hash in
        File_helpers.print_expr_file ~dirname:"" ~ext:".tz" ~hash_string script ;
        let filename = hash_string ^ ".addresses" in
        File_helpers.print_to_file
          filename
          "%a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_newline P.Contract.pp)
          contracts ;
        if Options.collect_storage then
          let dirname = hash_string ^ ".storages" in
          File_helpers.print_expr_dir ~dirname ~ext:".storage" storages
        else ())
      contract_map ;
    print_endline "Done writing contract files." ;
    let () =
      if not (ExprMap.is_empty lambda_map) then (
        print_endline "Writing lambda files..." ;
        File_helpers.print_expr_dir ~dirname:"lambdas" ~ext:".tz" lambda_map ;
        File_helpers.print_expr_dir ~dirname:"lambdas" ~ext:".ty" lambda_ty_map ;
        File_helpers.print_legacy_dir
          ~dirname:"lambdas"
          ~ext:".legacy_flag"
          lambda_legacy_map ;
        print_endline "Done writing lambda files.")
      else ()
    in
    return ()

  let main () =
    match Lwt_main.run (main ()) with
    | Ok () -> ()
    | Error trace ->
        Format.printf "ERROR: %a%!" Error_monad.pp_print_trace trace
end
