(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

open Data_encoding

type chain = [`Main | `Test | `Hash of Chain_id.t]

let parse_chain s =
  try
    match s with
    | "main" -> Ok `Main
    | "test" -> Ok `Test
    | h -> Ok (`Hash (Chain_id.of_b58check_exn h))
  with _ -> Error "Cannot parse chain identifier."

let chain_to_string = function
  | `Main -> "main"
  | `Test -> "test"
  | `Hash h -> Chain_id.to_b58check h

let chain_arg =
  let name = "chain_id" in
  let descr =
    "A chain identifier. This is either a chain hash in Base58Check notation \
     or a one the predefined aliases: 'main', 'test'."
  in
  let construct = chain_to_string in
  let destruct = parse_chain in
  RPC_arg.make ~name ~descr ~construct ~destruct ()

type block =
  [ `Genesis
  | `Head of int
  | `Alias of [`Caboose | `Checkpoint | `Savepoint] * int
  | `Hash of Block_hash.t * int
  | `Level of Int32.t ]

let parse_block s =
  let delims = ['~'; '-'; '+'] in
  let count_delims s =
    List.map
      (fun d ->
        (String.fold_left (fun i c -> if c = d then i + 1 else i) 0 s, d))
      delims
  in
  let split_on_delim counts =
    match List.fold_left (fun i (v, _) -> i + v) 0 counts with
    | 0 -> ([s], ' ')
    | 1 ->
        let delim =
          WithExceptions.Option.get ~loc:__LOC__
          @@ List.assoc ~equal:Int.equal 1 counts
        in
        (String.split delim s, delim)
    | _ -> raise Exit
  in
  (* Converts a string representing a block level into a Int32. Fails
     if the resulting integer is negative. *)
  let to_valid_level_id s =
    let l = Int32.of_string s in
    if Compare.Int32.(l < 0l) then raise Exit else l
  in
  (* Converts an Int32 into a level identifier. If [?offset] is given,
     returns the level identifier minus that offset. *)
  let to_level ?offset l =
    if Compare.Int32.(l = 0l) then Ok `Genesis
    else
      match offset with
      | Some ofs -> Ok (`Level Int32.(sub l ofs))
      | None -> Ok (`Level l)
  in
  try
    match split_on_delim (count_delims s) with
    | (["genesis"], _) -> Ok `Genesis
    | (["genesis"; n], '+') -> Ok (`Level (Int32.of_string n))
    | (["head"], _) -> Ok (`Head 0)
    | (["head"; n], '~') | (["head"; n], '-') -> Ok (`Head (int_of_string n))
    | (["checkpoint"], _) -> Ok (`Alias (`Checkpoint, 0))
    | (["checkpoint"; n], '~') | (["checkpoint"; n], '-') ->
        Ok (`Alias (`Checkpoint, int_of_string n))
    | (["checkpoint"; n], '+') -> Ok (`Alias (`Checkpoint, -int_of_string n))
    | (["savepoint"], _) -> Ok (`Alias (`Savepoint, 0))
    | (["savepoint"; n], '~') | (["savepoint"; n], '-') ->
        Ok (`Alias (`Savepoint, int_of_string n))
    | (["savepoint"; n], '+') -> Ok (`Alias (`Savepoint, -int_of_string n))
    | (["caboose"], _) -> Ok (`Alias (`Caboose, 0))
    | (["caboose"; n], '~') | (["caboose"; n], '-') ->
        Ok (`Alias (`Caboose, int_of_string n))
    | (["caboose"; n], '+') -> Ok (`Alias (`Caboose, -int_of_string n))
    | ([hol], _) -> (
        match Block_hash.of_b58check_opt hol with
        | Some h -> Ok (`Hash (h, 0))
        | None -> to_level (to_valid_level_id s))
    | ([hol; n], '~') | ([hol; n], '-') -> (
        match Block_hash.of_b58check_opt hol with
        | Some h -> Ok (`Hash (h, int_of_string n))
        | None ->
            let offset = to_valid_level_id n in
            to_level ~offset (to_valid_level_id hol))
    | ([hol; n], '+') -> (
        match Block_hash.of_b58check_opt hol with
        | Some h -> Ok (`Hash (h, -int_of_string n))
        | None ->
            let offset = Int32.neg (to_valid_level_id n) in
            to_level ~offset (to_valid_level_id hol))
    | _ -> raise Exit
  with _ -> Error ("Cannot parse block identifier: " ^ s)

let alias_to_string = function
  | `Checkpoint -> "checkpoint"
  | `Savepoint -> "savepoint"
  | `Caboose -> "caboose"

let to_string = function
  | `Genesis -> "genesis"
  | `Alias (a, 0) -> alias_to_string a
  | `Alias (a, n) when n < 0 -> Printf.sprintf "%s+%d" (alias_to_string a) (-n)
  | `Alias (a, n) -> Printf.sprintf "%s~%d" (alias_to_string a) n
  | `Head 0 -> "head"
  | `Head n when n < 0 -> Printf.sprintf "head+%d" (-n)
  | `Head n -> Printf.sprintf "head~%d" n
  | `Hash (h, 0) -> Block_hash.to_b58check h
  | `Hash (h, n) when n < 0 ->
      Printf.sprintf "%s+%d" (Block_hash.to_b58check h) (-n)
  | `Hash (h, n) -> Printf.sprintf "%s~%d" (Block_hash.to_b58check h) n
  | `Level i -> Printf.sprintf "%d" (Int32.to_int i)

let blocks_arg =
  let name = "block_id" in
  let descr =
    "A block identifier. This is either a block hash in Base58Check notation, \
     one the predefined aliases: 'genesis', 'head' or a block level (index in \
     the chain). One might also use 'head~N' or '<hash>~N' where N is an \
     integer to denote the Nth predecessor of the designated block.Also, \
     '<hash>+N' denotes the Nth successor of a block."
  in
  let construct = to_string in
  let destruct = parse_block in
  RPC_arg.make ~name ~descr ~construct ~destruct ()

type chain_prefix = unit * chain

type prefix = chain_prefix * block

let chain_path = RPC_path.(root / "chains" /: chain_arg)

let mempool_path p = RPC_path.(p / "mempool")

let live_blocks_path p = RPC_path.(p / "live_blocks")

let dir_path : (chain_prefix, chain_prefix) RPC_path.t =
  RPC_path.(open_root / "blocks")

let path = RPC_path.(dir_path /: blocks_arg)

type operation_list_quota = {max_size : int; max_op : int option}

let operation_list_quota_encoding =
  conv
    (fun {max_size; max_op} -> (max_size, max_op))
    (fun (max_size, max_op) -> {max_size; max_op})
    (obj2 (req "max_size" int31) (opt "max_op" int31))

type raw_context = Key of Bytes.t | Dir of raw_context TzString.Map.t | Cut

let rec raw_context_eq rc1 rc2 =
  match (rc1, rc2) with
  | (Key bytes1, Key bytes2) -> Bytes.equal bytes1 bytes2
  | (Dir dir1, Dir dir2) -> TzString.Map.(equal raw_context_eq dir1 dir2)
  | (Cut, Cut) -> true
  | _ -> false

let rec pp_raw_context ppf = function
  | Cut -> Format.fprintf ppf "..."
  | Key v -> Hex.pp ppf (Hex.of_bytes v)
  | Dir l ->
      Format.fprintf
        ppf
        "{@[<v 1>@,%a@]@,}"
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf (s, t) ->
             Format.fprintf ppf "%s : %a" s pp_raw_context t))
        (TzString.Map.bindings l)

let raw_context_encoding =
  mu "raw_context" (fun encoding ->
      union
        [
          case
            (Tag 0)
            bytes
            ~title:"Key"
            (function Key k -> Some k | _ -> None)
            (fun k -> Key k);
          case
            (Tag 1)
            (assoc encoding)
            ~title:"Dir"
            (function Dir map -> Some (TzString.Map.bindings map) | _ -> None)
            (fun bindings ->
              Dir
                (List.fold_left
                   (fun wip_map (k, v) -> TzString.Map.add k v wip_map)
                   TzString.Map.empty
                   bindings));
          case
            (Tag 2)
            null
            ~title:"Cut"
            (function Cut -> Some () | _ -> None)
            (fun () -> Cut);
        ])

let raw_context_insert =
  let default = Dir TzString.Map.empty in
  (* not tail recursive but over the length of [k], which is small *)
  let rec aux (k, v) ctx =
    let d = match ctx with Dir d -> d | Key _ | Cut -> TzString.Map.empty in
    match k with
    | [] -> v
    | [kh] -> Dir (TzString.Map.add kh v d)
    | kh :: ktl ->
        Dir
          (TzString.Map.update
             kh
             (fun ctxtopt ->
               let ctx' = Option.value ctxtopt ~default in
               Some (aux (ktl, v) ctx'))
             d)
  in
  aux

type merkle_hash_kind = Contents | Node

type merkle_node =
  | Hash of (merkle_hash_kind * string)
  | Data of raw_context
  | Continue of merkle_tree

and merkle_tree = merkle_node TzString.Map.t

let rec merkle_node_eq n1 n2 =
  match (n1, n2) with
  | (Hash (mhk1, s1), Hash (mhk2, s2)) -> mhk1 = mhk2 && String.equal s1 s2
  | (Data rc1, Data rc2) -> raw_context_eq rc1 rc2
  | (Continue mtree1, Continue mtree2) -> merkle_tree_eq mtree1 mtree2
  | _ -> false

and merkle_tree_eq mtree1 mtree2 =
  TzString.Map.equal merkle_node_eq mtree1 mtree2

type merkle_leaf_kind = Hole | Raw_context

let rec pp_merkle_node ppf = function
  | Hash (k, h) ->
      let k_str = match k with Contents -> "Contents" | Node -> "Node" in
      Format.fprintf ppf "Hash(%s, %s)" k_str h
  | Data raw_context -> Format.fprintf ppf "Data(%a)" pp_raw_context raw_context
  | Continue tree -> Format.fprintf ppf "Continue(%a)" pp_merkle_tree tree

and pp_merkle_tree ppf mtree =
  let pairs = TzString.Map.bindings mtree in
  Format.fprintf
    ppf
    "{@[<v 1>@,%a@]@,}"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf (s, t) ->
         Format.fprintf ppf "\"%s\": %a" s pp_merkle_node t))
    pairs

let stringmap_encoding value_encoding =
  let open Data_encoding in
  conv
    TzString.Map.bindings
    (fun l ->
      List.fold_left
        (fun acc (k, v) -> TzString.Map.add k v acc)
        TzString.Map.empty
        l)
    (list (tup2 string value_encoding))

let merkle_tree_encoding : merkle_tree Data_encoding.t =
  let open Data_encoding in
  let hash_tag = 0 and hash_encoding = tup2 bool string in
  let data_tag = 1 and data_encoding = raw_context_encoding in
  let continue_tag = 2 in
  mu "merkle_tree" (fun encoding ->
      let continue_encoding = encoding in
      stringmap_encoding
        (matching
           (function
             | Hash (kind, content) ->
                 matched
                   hash_tag
                   hash_encoding
                   ( (match kind with Contents -> true | Node -> false),
                     content )
             | Data raw_context -> matched data_tag data_encoding raw_context
             | Continue dir -> matched continue_tag encoding dir)
           [
             case
               (Tag hash_tag)
               ~title:"Hash"
               hash_encoding
               (function Hash (k, h) -> Some (k = Contents, h) | _ -> None)
               (fun (k, h) ->
                 let kind = if k then Contents else Node in
                 Hash (kind, h));
             case
               (Tag data_tag)
               data_encoding
               ~title:"Data"
               (function Data raw_context -> Some raw_context | _ -> None)
               (fun raw_context -> Data raw_context);
             case
               (Tag continue_tag)
               continue_encoding
               ~title:"Continue"
               (function Continue dir -> Some dir | _ -> None)
               (fun dir -> Continue dir);
           ]))

module type PROTO = sig
  val hash : Protocol_hash.t

  type block_header_data

  val block_header_data_encoding : block_header_data Data_encoding.t

  type block_header_metadata

  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  type operation_data

  type operation_receipt

  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  val operation_data_encoding : operation_data Data_encoding.t

  val operation_receipt_encoding : operation_receipt Data_encoding.t

  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t
end

type protocols = {
  current_protocol : Protocol_hash.t;
  next_protocol : Protocol_hash.t;
}

let raw_protocol_encoding =
  conv
    (fun {current_protocol; next_protocol} -> (current_protocol, next_protocol))
    (fun (current_protocol, next_protocol) -> {current_protocol; next_protocol})
    (obj2
       (req "protocol" Protocol_hash.encoding)
       (req "next_protocol" Protocol_hash.encoding))

module Make (Proto : PROTO) (Next_proto : PROTO) = struct
  let protocol_hash = Protocol_hash.to_b58check Proto.hash

  let next_protocol_hash = Protocol_hash.to_b58check Next_proto.hash

  type raw_block_header = {
    shell : Block_header.shell_header;
    protocol_data : Proto.block_header_data;
  }

  let raw_block_header_encoding =
    def "raw_block_header"
    @@ conv
         (fun {shell; protocol_data} -> (shell, protocol_data))
         (fun (shell, protocol_data) -> {shell; protocol_data})
         (merge_objs
            Block_header.shell_header_encoding
            Proto.block_header_data_encoding)

  type block_header = {
    chain_id : Chain_id.t;
    hash : Block_hash.t;
    shell : Block_header.shell_header;
    protocol_data : Proto.block_header_data;
  }

  let block_header_encoding =
    def "block_header"
    @@ conv
         (fun {chain_id; hash; shell; protocol_data} ->
           (((), chain_id, hash), {shell; protocol_data}))
         (fun (((), chain_id, hash), {shell; protocol_data}) ->
           {chain_id; hash; shell; protocol_data})
         (merge_objs
            (obj3
               (req "protocol" (constant protocol_hash))
               (req "chain_id" Chain_id.encoding)
               (req "hash" Block_hash.encoding))
            raw_block_header_encoding)

  type block_metadata = {
    protocol_data : Proto.block_header_metadata;
    test_chain_status : Test_chain_status.t;
    (* for the next block: *)
    max_operations_ttl : int;
    max_operation_data_length : int;
    max_block_header_length : int;
    operation_list_quota : operation_list_quota list;
  }

  let block_metadata_encoding =
    def "block_header_metadata"
    @@ conv
         (fun {
                protocol_data;
                test_chain_status;
                max_operations_ttl;
                max_operation_data_length;
                max_block_header_length;
                operation_list_quota;
              } ->
           ( ( (),
               (),
               test_chain_status,
               max_operations_ttl,
               max_operation_data_length,
               max_block_header_length,
               operation_list_quota ),
             protocol_data ))
         (fun ( ( (),
                  (),
                  test_chain_status,
                  max_operations_ttl,
                  max_operation_data_length,
                  max_block_header_length,
                  operation_list_quota ),
                protocol_data ) ->
           {
             protocol_data;
             test_chain_status;
             max_operations_ttl;
             max_operation_data_length;
             max_block_header_length;
             operation_list_quota;
           })
         (merge_objs
            (obj7
               (req "protocol" (constant protocol_hash))
               (req "next_protocol" (constant next_protocol_hash))
               (req "test_chain_status" Test_chain_status.encoding)
               (req "max_operations_ttl" int31)
               (req "max_operation_data_length" int31)
               (req "max_block_header_length" int31)
               (req
                  "max_operation_list_length"
                  (dynamic_size (list operation_list_quota_encoding))))
            Proto.block_header_metadata_encoding)

  let next_operation_encoding =
    let open Data_encoding in
    def "next_operation"
    @@ conv
         (fun Next_proto.{shell; protocol_data} -> ((), (shell, protocol_data)))
         (fun ((), (shell, protocol_data)) -> {shell; protocol_data})
         (merge_objs
            (obj1 (req "protocol" (constant next_protocol_hash)))
            (merge_objs
               (dynamic_size Operation.shell_header_encoding)
               (dynamic_size Next_proto.operation_data_encoding)))

  type operation_receipt =
    | Empty
    | Too_large
    | Receipt of Proto.operation_receipt

  type operation = {
    chain_id : Chain_id.t;
    hash : Operation_hash.t;
    shell : Operation.shell_header;
    protocol_data : Proto.operation_data;
    receipt : operation_receipt;
  }

  let operation_data_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Operation with too large metadata"
          (Tag 0)
          (merge_objs
             Proto.operation_data_encoding
             (obj1 (req "metadata" (constant "too large"))))
          (function
            | (operation_data, Too_large) -> Some (operation_data, ())
            | _ -> None)
          (fun (operation_data, ()) -> (operation_data, Too_large));
        case
          ~title:"Operation without metadata"
          (Tag 1)
          Proto.operation_data_encoding
          (function
            | (operation_data, Empty) -> Some operation_data | _ -> None)
          (fun operation_data -> (operation_data, Empty));
        case
          ~title:"Operation with metadata"
          (Tag 2)
          Proto.operation_data_and_receipt_encoding
          (function
            | (operation_data, Receipt receipt) -> Some (operation_data, receipt)
            | _ -> None)
          (function
            | (operation_data, receipt) -> (operation_data, Receipt receipt));
      ]

  let operation_encoding =
    def "operation"
    @@
    let open Data_encoding in
    conv
      (fun {chain_id; hash; shell; protocol_data; receipt} ->
        (((), chain_id, hash), (shell, (protocol_data, receipt))))
      (fun (((), chain_id, hash), (shell, (protocol_data, receipt))) ->
        {chain_id; hash; shell; protocol_data; receipt})
      (merge_objs
         (obj3
            (req "protocol" (constant protocol_hash))
            (req "chain_id" Chain_id.encoding)
            (req "hash" Operation_hash.encoding))
         (merge_objs
            (dynamic_size Operation.shell_header_encoding)
            (dynamic_size operation_data_encoding)))

  type block_info = {
    chain_id : Chain_id.t;
    hash : Block_hash.t;
    header : raw_block_header;
    metadata : block_metadata option;
    operations : operation list list;
  }

  let block_info_encoding =
    conv
      (fun {chain_id; hash; header; metadata; operations} ->
        ((), chain_id, hash, header, metadata, operations))
      (fun ((), chain_id, hash, header, metadata, operations) ->
        {chain_id; hash; header; metadata; operations})
      (obj6
         (req "protocol" (constant protocol_hash))
         (req "chain_id" Chain_id.encoding)
         (req "hash" Block_hash.encoding)
         (req "header" (dynamic_size raw_block_header_encoding))
         (opt "metadata" (dynamic_size block_metadata_encoding))
         (req "operations" (list (dynamic_size (list operation_encoding)))))

  module S = struct
    let path : prefix RPC_path.context = RPC_path.open_root

    let hash =
      RPC_service.get_service
        ~description:"The block's hash, its unique identifier."
        ~query:RPC_query.empty
        ~output:Block_hash.encoding
        RPC_path.(path / "hash")

    let header =
      RPC_service.get_service
        ~description:"The whole block header."
        ~query:RPC_query.empty
        ~output:block_header_encoding
        RPC_path.(path / "header")

    let raw_header =
      RPC_service.get_service
        ~description:"The whole block header (unparsed)."
        ~query:RPC_query.empty
        ~output:bytes
        RPC_path.(path / "header" / "raw")

    let metadata =
      RPC_service.get_service
        ~description:"All the metadata associated to the block."
        ~query:RPC_query.empty
        ~output:block_metadata_encoding
        RPC_path.(path / "metadata")

    let metadata_hash =
      RPC_service.get_service
        ~description:
          "Hash of the metadata associated to the block. This is only set on \
           blocks starting from environment V1."
        ~query:RPC_query.empty
        ~output:Block_metadata_hash.encoding
        RPC_path.(path / "metadata_hash")

    let protocols =
      RPC_service.get_service
        ~description:"Current and next protocol."
        ~query:RPC_query.empty
        ~output:raw_protocol_encoding
        RPC_path.(path / "protocols")

    module Header = struct
      let path = RPC_path.(path / "header")

      let shell_header =
        RPC_service.get_service
          ~description:"The shell-specific fragment of the block header."
          ~query:RPC_query.empty
          ~output:Block_header.shell_header_encoding
          RPC_path.(path / "shell")

      let protocol_data =
        RPC_service.get_service
          ~description:"The version-specific fragment of the block header."
          ~query:RPC_query.empty
          ~output:
            (conv
               (fun h -> ((), h))
               (fun ((), h) -> h)
               (merge_objs
                  (obj1 (req "protocol" (constant protocol_hash)))
                  Proto.block_header_data_encoding))
          RPC_path.(path / "protocol_data")

      let raw_protocol_data =
        RPC_service.get_service
          ~description:
            "The version-specific fragment of the block header (unparsed)."
          ~query:RPC_query.empty
          ~output:bytes
          RPC_path.(path / "protocol_data" / "raw")
    end

    module Operations = struct
      let path = RPC_path.(path / "operations")

      let operations =
        RPC_service.get_service
          ~description:"All the operations included in the block."
          ~query:RPC_query.empty
          ~output:(list (dynamic_size (list operation_encoding)))
          path

      let list_arg =
        let name = "list_offset" in
        let descr = "Index `n` of the requested validation pass." in
        let construct = string_of_int in
        let destruct s =
          try Ok (int_of_string s)
          with _ -> Error (Format.sprintf "Invalid list offset (%s)" s)
        in
        RPC_arg.make ~name ~descr ~construct ~destruct ()

      let offset_arg =
        let name = "operation_offset" in
        let descr =
          "Index `m` of the requested operation in its validation pass."
        in
        let construct = string_of_int in
        let destruct s =
          try Ok (int_of_string s)
          with _ -> Error (Format.sprintf "Invalid operation offset (%s)" s)
        in
        RPC_arg.make ~name ~descr ~construct ~destruct ()

      let operations_in_pass =
        RPC_service.get_service
          ~description:
            "All the operations included in `n-th` validation pass of the \
             block."
          ~query:RPC_query.empty
          ~output:(list operation_encoding)
          RPC_path.(path /: list_arg)

      let operation =
        RPC_service.get_service
          ~description:
            "The `m-th` operation in the `n-th` validation pass of the block."
          ~query:RPC_query.empty
          ~output:operation_encoding
          RPC_path.(path /: list_arg /: offset_arg)
    end

    module Operation_hashes = struct
      let path = RPC_path.(path / "operation_hashes")

      let operation_hashes =
        RPC_service.get_service
          ~description:"The hashes of all the operations included in the block."
          ~query:RPC_query.empty
          ~output:(list (list Operation_hash.encoding))
          path

      let operation_hashes_in_pass =
        RPC_service.get_service
          ~description:
            "All the operations included in `n-th` validation pass of the \
             block."
          ~query:RPC_query.empty
          ~output:(list Operation_hash.encoding)
          RPC_path.(path /: Operations.list_arg)

      let operation_hash =
        RPC_service.get_service
          ~description:
            "The hash of then `m-th` operation in the `n-th` validation pass \
             of the block."
          ~query:RPC_query.empty
          ~output:Operation_hash.encoding
          RPC_path.(path /: Operations.list_arg /: Operations.offset_arg)
    end

    module Operation_metadata_hashes = struct
      let root =
        RPC_service.get_service
          ~description:
            "The root hash of the operations metadata from the block. This is \
             only set on blocks starting from environment V1."
          ~query:RPC_query.empty
          ~output:Operation_metadata_list_list_hash.encoding
          RPC_path.(path / "operations_metadata_hash")

      let path = RPC_path.(path / "operation_metadata_hashes")

      let operation_metadata_hashes =
        RPC_service.get_service
          ~description:
            "The hashes of all the operation metadata included in the block. \
             This is only set on blocks starting from environment V1."
          ~query:RPC_query.empty
          ~output:(list (list Operation_metadata_hash.encoding))
          path

      let operation_metadata_hashes_in_pass =
        RPC_service.get_service
          ~description:
            "All the operation metadata included in `n-th` validation pass of \
             the block. This is only set on blocks starting from environment \
             V1."
          ~query:RPC_query.empty
          ~output:(list Operation_metadata_hash.encoding)
          RPC_path.(path /: Operations.list_arg)

      let operation_metadata_hash =
        RPC_service.get_service
          ~description:
            "The hash of then `m-th` operation metadata in the `n-th` \
             validation pass of the block. This is only set on blocks starting \
             from environment V1."
          ~query:RPC_query.empty
          ~output:Operation_metadata_hash.encoding
          RPC_path.(path /: Operations.list_arg /: Operations.offset_arg)
    end

    module Helpers = struct
      let path = RPC_path.(path / "helpers")

      module Forge = struct
        let block_header =
          RPC_service.post_service
            ~description:"Forge a block header"
            ~query:RPC_query.empty
            ~input:Block_header.encoding
            ~output:(obj1 (req "block" bytes))
            RPC_path.(path / "forge_block_header")
      end

      module Preapply = struct
        let path = RPC_path.(path / "preapply")

        let block_result_encoding =
          obj2
            (req "shell_header" Block_header.shell_header_encoding)
            (req
               "operations"
               (list (Preapply_result.encoding RPC_error.encoding)))

        type block_param = {
          protocol_data : Next_proto.block_header_data;
          operations : Next_proto.operation list list;
        }

        let block_param_encoding =
          conv
            (fun {protocol_data; operations} -> (protocol_data, operations))
            (fun (protocol_data, operations) -> {protocol_data; operations})
            (obj2
               (req
                  "protocol_data"
                  (conv
                     (fun h -> ((), h))
                     (fun ((), h) -> h)
                     (merge_objs
                        (obj1 (req "protocol" (constant next_protocol_hash)))
                        (dynamic_size Next_proto.block_header_data_encoding))))
               (req
                  "operations"
                  (list (dynamic_size (list next_operation_encoding)))))

        let block_query =
          let open RPC_query in
          query (fun sort timestamp ->
              object
                method sort_operations = sort

                method timestamp = timestamp
              end)
          |+ flag "sort" (fun t -> t#sort_operations)
          |+ opt_field "timestamp" Time.Protocol.rpc_arg (fun t -> t#timestamp)
          |> seal

        let block =
          RPC_service.post_service
            ~description:
              "Simulate the validation of a block that would contain the given \
               operations and return the resulting fitness and context hash."
            ~query:block_query
            ~input:block_param_encoding
            ~output:block_result_encoding
            RPC_path.(path / "block")

        let operations =
          RPC_service.post_service
            ~description:"Simulate the validation of an operation."
            ~query:RPC_query.empty
            ~input:(list next_operation_encoding)
            ~output:
              (list
                 (dynamic_size Next_proto.operation_data_and_receipt_encoding))
            RPC_path.(path / "operations")
      end

      let complete =
        let prefix_arg =
          let destruct s = Ok s and construct s = s in
          RPC_arg.make ~name:"prefix" ~destruct ~construct ()
        in
        RPC_service.get_service
          ~description:
            "Try to complete a prefix of a Base58Check-encoded data. This RPC \
             is actually able to complete hashes of block, operations, \
             public_keys and contracts."
          ~query:RPC_query.empty
          ~output:(list string)
          RPC_path.(path / "complete" /: prefix_arg)
    end

    module Context = struct
      let path = RPC_path.(path / "context")

      let raw_bytes_path = RPC_path.(path / "raw" / "bytes")

      let merkle_tree_path = RPC_path.(path / "merkle_tree")

      let context_path_arg : string RPC_arg.t =
        let name = "context_path" in
        let descr = "A path inside the context" in
        let construct s = s in
        let destruct s = Ok s in
        RPC_arg.make ~name ~descr ~construct ~destruct ()

      let raw_context_query : < depth : int option > RPC_query.t =
        let open RPC_query in
        query (fun depth ->
            object
              method depth = depth
            end)
        |+ opt_field "depth" RPC_arg.uint (fun t -> t#depth)
        |> seal

      let read =
        RPC_service.get_service
          ~description:"Returns the raw context."
          ~query:raw_context_query
          ~output:raw_context_encoding
          RPC_path.(raw_bytes_path /:* context_path_arg)

      let merkle_tree_query : < holey : bool option > RPC_query.t =
        let open RPC_query in
        query (fun holey ->
            object
              method holey = holey
            end)
        |+ opt_field
             ~descr:"Send only hashes, omit data of key"
             "holey"
             RPC_arg.bool
             (fun t -> t#holey)
        |> seal

      let merkle_tree =
        RPC_service.get_service
          ~description:"Returns the merkle tree of a piece of context."
          ~query:merkle_tree_query
          ~output:Data_encoding.(option merkle_tree_encoding)
          RPC_path.(merkle_tree_path /:* context_path_arg)
    end

    let info =
      RPC_service.get_service
        ~description:
          "All the information about a block. The associated metadata may not \
           be present depending on the history mode and block's distance from \
           the head."
        ~query:RPC_query.empty
        ~output:block_info_encoding
        path

    module Mempool = struct
      type t = {
        applied : (Operation_hash.t * Next_proto.operation) list;
        refused : (Next_proto.operation * error list) Operation_hash.Map.t;
        outdated : (Next_proto.operation * error list) Operation_hash.Map.t;
        branch_refused :
          (Next_proto.operation * error list) Operation_hash.Map.t;
        branch_delayed :
          (Next_proto.operation * error list) Operation_hash.Map.t;
        unprocessed : Next_proto.operation Operation_hash.Map.t;
      }

      let version_0_encoding =
        conv
          (fun {
                 applied;
                 refused;
                 outdated;
                 branch_refused;
                 branch_delayed;
                 unprocessed;
               } ->
            ( applied,
              refused,
              outdated,
              branch_refused,
              branch_delayed,
              unprocessed ))
          (fun ( applied,
                 refused,
                 outdated,
                 branch_refused,
                 branch_delayed,
                 unprocessed ) ->
            {
              applied;
              refused;
              outdated;
              branch_refused;
              branch_delayed;
              unprocessed;
            })
          (obj6
             (req
                "applied"
                (list
                   (conv
                      (fun (hash, (op : Next_proto.operation)) ->
                        ((hash, op.shell), op.protocol_data))
                      (fun ((hash, shell), protocol_data) ->
                        (hash, {shell; protocol_data}))
                      (merge_objs
                         (merge_objs
                            (obj1 (req "hash" Operation_hash.encoding))
                            (dynamic_size Operation.shell_header_encoding))
                         (dynamic_size Next_proto.operation_data_encoding)))))
             (req
                "refused"
                (Operation_hash.Map.encoding
                   (merge_objs
                      (dynamic_size next_operation_encoding)
                      (obj1 (req "error" RPC_error.encoding)))))
             (req
                "outdated"
                (Operation_hash.Map.encoding
                   (merge_objs
                      (dynamic_size next_operation_encoding)
                      (obj1 (req "error" RPC_error.encoding)))))
             (req
                "branch_refused"
                (Operation_hash.Map.encoding
                   (merge_objs
                      (dynamic_size next_operation_encoding)
                      (obj1 (req "error" RPC_error.encoding)))))
             (req
                "branch_delayed"
                (Operation_hash.Map.encoding
                   (merge_objs
                      (dynamic_size next_operation_encoding)
                      (obj1 (req "error" RPC_error.encoding)))))
             (req
                "unprocessed"
                (Operation_hash.Map.encoding
                   (dynamic_size next_operation_encoding))))

      let version_1_encoding =
        let operations_with_error_encoding kind =
          req
            kind
            (conv
               (fun map -> Operation_hash.Map.bindings map)
               (fun list -> list |> List.to_seq |> Operation_hash.Map.of_seq)
               (list
                  (merge_objs
                     (obj1 (req "hash" Operation_hash.encoding))
                     (merge_objs
                        next_operation_encoding
                        (obj1 (req "error" RPC_error.encoding))))))
        in
        conv
          (fun {
                 applied;
                 refused;
                 outdated;
                 branch_refused;
                 branch_delayed;
                 unprocessed;
               } ->
            ( applied,
              refused,
              outdated,
              branch_refused,
              branch_delayed,
              unprocessed ))
          (fun ( applied,
                 refused,
                 outdated,
                 branch_refused,
                 branch_delayed,
                 unprocessed ) ->
            {
              applied;
              refused;
              outdated;
              branch_refused;
              branch_delayed;
              unprocessed;
            })
          (obj6
             (req
                "applied"
                (list
                   (conv
                      (fun (hash, (op : Next_proto.operation)) ->
                        ((hash, op.shell), op.protocol_data))
                      (fun ((hash, shell), protocol_data) ->
                        (hash, {shell; protocol_data}))
                      (merge_objs
                         (merge_objs
                            (obj1 (req "hash" Operation_hash.encoding))
                            Operation.shell_header_encoding)
                         (dynamic_size Next_proto.operation_data_encoding)))))
             (operations_with_error_encoding "refused")
             (operations_with_error_encoding "outdated")
             (operations_with_error_encoding "branch_refused")
             (operations_with_error_encoding "branch_delayed")
             (req
                "unprocessed"
                (conv
                   (fun map -> Operation_hash.Map.bindings map)
                   (fun list ->
                     list |> List.to_seq |> Operation_hash.Map.of_seq)
                   (list
                      (merge_objs
                         (obj1 (req "hash" Operation_hash.encoding))
                         next_operation_encoding)))))

      (* This encoding should be always the one by default. *)
      let encoding = version_0_encoding

      (* If you change this value, also change [encoding]. *)
      let default_pending_operations_version = 0

      let pending_query =
        let open RPC_query in
        query (fun version ->
            object
              method version = version
            end)
        |+ field
             "version"
             RPC_arg.int
             default_pending_operations_version
             (fun t -> t#version)
        |> seal

      (* If you update this datatype, please update also [supported_version]. *)
      type t_with_version = Version_0 of t | Version_1 of t

      (* This value should be consistent with [t_with_version]. *)
      let supported_version = [0; 1]

      let pending_operations_encoding =
        union
          [
            case
              ~title:"new_encoding_pending_operations"
              Json_only
              version_1_encoding
              (function
                | Version_1 pending_operations -> Some pending_operations
                | Version_0 _ -> None)
              (fun pending_operations -> Version_1 pending_operations);
            case
              ~title:"old_encoding_pending_operations"
              Json_only
              version_0_encoding
              (function
                | Version_0 pending_operations -> Some pending_operations
                | Version_1 _ -> None)
              (fun pending_operations -> Version_0 pending_operations);
          ]

      let pending_operations_version_dispatcher ~version pending_operations =
        if version = 0 then RPC_answer.return (Version_0 pending_operations)
        else if version = 1 then
          RPC_answer.return (Version_1 pending_operations)
        else
          RPC_answer.fail
            (RPC_error.bad_version ~given:version ~supported:supported_version)

      let pending_operations path =
        RPC_service.get_service
          ~description:"List the prevalidated operations."
          ~query:pending_query
          ~output:pending_operations_encoding
          RPC_path.(path / "pending_operations")

      let ban_operation path =
        RPC_service.post_service
          ~description:
            "Remove an operation from the mempool if present, reverting its \
             effect if it was applied. Add it to the set of banned operations \
             to prevent it from being fetched/processed/injected in the \
             future. Note: If the baker has already received the operation, \
             then it's necessary to restart it to flush the operation from it."
          ~query:RPC_query.empty
          ~input:Operation_hash.encoding
          ~output:unit
          RPC_path.(path / "ban_operation")

      let unban_operation path =
        RPC_service.post_service
          ~description:
            "Remove an operation from the set of banned operations (nothing \
             happens if it was not banned)."
          ~query:RPC_query.empty
          ~input:Operation_hash.encoding
          ~output:unit
          RPC_path.(path / "unban_operation")

      let unban_all_operations path =
        RPC_service.post_service
          ~description:"Clear the set of banned operations."
          ~query:RPC_query.empty
          ~input:Data_encoding.empty
          ~output:unit
          RPC_path.(path / "unban_all_operations")

      let mempool_query =
        let open RPC_query in
        query (fun applied refused outdated branch_refused branch_delayed ->
            object
              method applied = applied

              method refused = refused

              method outdated = outdated

              method branch_refused = branch_refused

              method branch_delayed = branch_delayed
            end)
        |+ field
             ~descr:"Include applied operations (set by default)"
             "applied"
             RPC_arg.bool
             true
             (fun t -> t#applied)
        |+ field
             ~descr:"Include refused operations"
             "refused"
             RPC_arg.bool
             false
             (fun t -> t#refused)
        |+ field
             ~descr:"Include outdated operations"
             "outdated"
             RPC_arg.bool
             false
             (fun t -> t#outdated)
        |+ field
             ~descr:"Include branch refused operations"
             "branch_refused"
             RPC_arg.bool
             false
             (fun t -> t#branch_refused)
        |+ field
             ~descr:"Include branch delayed operations (set by default)"
             "branch_delayed"
             RPC_arg.bool
             true
             (fun t -> t#branch_delayed)
        |> seal

      (* We extend the object so that the fields of 'next_operation'
         stay toplevel, for backward compatibility. *)
      let processed_operation_encoding =
        merge_objs
          (merge_objs
             (obj1 (req "hash" Operation_hash.encoding))
             next_operation_encoding)
          (obj1 (dft "error" RPC_error.opt_encoding None))

      let monitor_operations path =
        RPC_service.get_service
          ~description:"Monitor the mempool operations."
          ~query:mempool_query
          ~output:(list processed_operation_encoding)
          RPC_path.(path / "monitor_operations")

      let get_filter_query =
        let open RPC_query in
        query (fun include_default ->
            object
              method include_default = include_default
            end)
        |+ field
             ~descr:"Show fields equal to their default value (set by default)"
             "include_default"
             RPC_arg.bool
             true
             (fun t -> t#include_default)
        |> seal

      let get_filter path =
        RPC_service.get_service
          ~description:
            {|Get the configuration of the mempool filter. The minimal_fees are in mutez. Each field minimal_nanotez_per_xxx is a rational number given as a numerator and a denominator, e.g. "minimal_nanotez_per_gas_unit": [ "100", "1" ].|}
          ~query:get_filter_query
          ~output:json
          RPC_path.(path / "filter")

      let set_filter path =
        RPC_service.post_service
          ~description:
            {|Set the configuration of the mempool filter. **If any of the fields is absent from the input JSON, then it is set to the default value for this field (i.e. its value in the default configuration), even if it previously had a different value.** If the input JSON does not describe a valid configuration, then the configuration is left unchanged. Also return the new configuration (which may differ from the input if it had omitted fields or was invalid). You may call [./tezos-client rpc get '/chains/main/mempool/filter?include_default=true'] to see an example of JSON describing a valid configuration.|}
          ~query:RPC_query.empty
          ~input:json
          ~output:json
          RPC_path.(path / "filter")

      let request_operations_query =
        let open RPC_query in
        query (fun peer_id ->
            object
              method peer_id = peer_id
            end)
        |+ opt_field "peer_id" P2p_peer_id.rpc_arg (fun t -> t#peer_id)
        |> seal

      let request_operations path =
        RPC_service.post_service
          ~description:
            "Request the operations of our peers or a specific peer if \
             specified via a query parameter."
          ~input:Data_encoding.empty
          ~query:request_operations_query
          ~output:Data_encoding.empty
          RPC_path.(path / "request_operations")
    end

    let live_blocks =
      RPC_service.get_service
        ~description:
          "List the ancestors of the given block which, if referred to as the \
           branch in an operation header, are recent enough for that operation \
           to be included in the current block."
        ~query:RPC_query.empty
        ~output:Block_hash.Set.encoding
        RPC_path.(live_blocks_path open_root)
  end

  let path = RPC_path.prefix chain_path path

  let make_call0 s ctxt a b q p =
    let s = RPC_service.prefix path s in
    RPC_context.make_call2 s ctxt a b q p

  let make_call1 s ctxt a b c q p =
    let s = RPC_service.prefix path s in
    RPC_context.make_call3 s ctxt a b c q p

  let make_call2 s ctxt a b c d q p =
    let s = RPC_service.prefix path s in
    RPC_context.make_call s ctxt (((((), a), b), c), d) q p

  let hash ctxt =
    let f = make_call0 S.hash ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () ->
      match block with `Hash (h, 0) -> return h | _ -> f chain block () ()

  let header ctxt =
    let f = make_call0 S.header ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

  let raw_header ctxt =
    let f = make_call0 S.raw_header ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

  let metadata ctxt =
    let f = make_call0 S.metadata ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

  let metadata_hash ctxt =
    let f = make_call0 S.metadata_hash ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

  let protocols ctxt =
    let f = make_call0 S.protocols ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

  module Header = struct
    module S = S.Header

    let shell_header ctxt =
      let f = make_call0 S.shell_header ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

    let protocol_data ctxt =
      let f = make_call0 S.protocol_data ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

    let raw_protocol_data ctxt =
      let f = make_call0 S.raw_protocol_data ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()
  end

  module Operations = struct
    module S = S.Operations

    let operations ctxt =
      let f = make_call0 S.operations ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

    let operations_in_pass ctxt =
      let f = make_call1 S.operations_in_pass ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n -> f chain block n () ()

    let operation ctxt =
      let f = make_call2 S.operation ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n m -> f chain block n m () ()
  end

  module Operation_hashes = struct
    module S = S.Operation_hashes

    let operation_hashes ctxt =
      let f = make_call0 S.operation_hashes ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

    let operation_hashes_in_pass ctxt =
      let f = make_call1 S.operation_hashes_in_pass ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n -> f chain block n () ()

    let operation_hash ctxt =
      let f = make_call2 S.operation_hash ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n m -> f chain block n m () ()
  end

  module Operation_metadata_hashes = struct
    module S = S.Operation_metadata_hashes

    let root ctxt =
      let f = make_call0 S.root ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

    let operation_metadata_hashes ctxt =
      let f = make_call0 S.operation_metadata_hashes ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

    let operation_metadata_hashes_in_pass ctxt =
      let f = make_call1 S.operation_metadata_hashes_in_pass ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n -> f chain block n () ()

    let operation_metadata_hash ctxt =
      let f = make_call2 S.operation_metadata_hash ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) n m -> f chain block n m () ()
  end

  module Context = struct
    module S = S.Context

    let read ctxt =
      let f = make_call1 S.read ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) ?depth path ->
        f
          chain
          block
          path
          (object
             method depth = depth
          end)
          ()

    let merkle_tree ctxt =
      let f = make_call1 S.merkle_tree ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) ?holey path ->
        f
          chain
          block
          path
          (object
             method holey = holey
          end)
          ()
  end

  module Helpers = struct
    module S = S.Helpers

    module Forge = struct
      module S = S.Forge

      let block_header ctxt =
        let f = make_call0 S.block_header ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) header ->
          f chain block () header
    end

    module Preapply = struct
      module S = S.Preapply

      let block ctxt =
        let f = make_call0 S.block ctxt in
        fun ?(chain = `Main)
            ?(block = `Head 0)
            ?(sort = false)
            ?timestamp
            ~protocol_data
            operations ->
          f
            chain
            block
            (object
               method sort_operations = sort

               method timestamp = timestamp
            end)
            {protocol_data; operations}

      let operations ctxt =
        let f = make_call0 S.operations ctxt in
        fun ?(chain = `Main) ?(block = `Head 0) operations ->
          f chain block () operations
    end

    let complete ctxt =
      let f = make_call1 S.complete ctxt in
      fun ?(chain = `Main) ?(block = `Head 0) s -> f chain block s () ()
  end

  let info ctxt =
    let f = make_call0 S.info ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()

  module Mempool = struct
    type t = S.Mempool.t = {
      applied : (Operation_hash.t * Next_proto.operation) list;
      refused : (Next_proto.operation * error list) Operation_hash.Map.t;
      outdated : (Next_proto.operation * error list) Operation_hash.Map.t;
      branch_refused : (Next_proto.operation * error list) Operation_hash.Map.t;
      branch_delayed : (Next_proto.operation * error list) Operation_hash.Map.t;
      unprocessed : Next_proto.operation Operation_hash.Map.t;
    }

    type t_with_version = S.Mempool.t_with_version =
      | Version_0 of t
      | Version_1 of t

    let pending_operations_version_dispatcher =
      S.Mempool.pending_operations_version_dispatcher

    let pending_operations ctxt ?(chain = `Main)
        ?(version = S.Mempool.default_pending_operations_version) () =
      RPC_context.make_call1
        (S.Mempool.pending_operations (mempool_path chain_path))
        ctxt
        chain
        (object
           method version = version
        end)
        ()
      >>=? function
      | Version_1 pending_operations | Version_0 pending_operations ->
          return pending_operations

    let ban_operation ctxt ?(chain = `Main) op_hash =
      let s = S.Mempool.ban_operation (mempool_path chain_path) in
      RPC_context.make_call1 s ctxt chain () op_hash

    let unban_operation ctxt ?(chain = `Main) op_hash =
      let s = S.Mempool.unban_operation (mempool_path chain_path) in
      RPC_context.make_call1 s ctxt chain () op_hash

    let unban_all_operations ctxt ?(chain = `Main) () =
      let s = S.Mempool.unban_all_operations (mempool_path chain_path) in
      RPC_context.make_call1 s ctxt chain () ()

    let monitor_operations ctxt ?(chain = `Main) ?(applied = true)
        ?(branch_delayed = true) ?(branch_refused = false) ?(refused = false)
        ?(outdated = false) () =
      let s = S.Mempool.monitor_operations (mempool_path chain_path) in
      RPC_context.make_streamed_call
        s
        ctxt
        ((), chain)
        (object
           method applied = applied

           method refused = refused

           method outdated = outdated

           method branch_refused = branch_refused

           method branch_delayed = branch_delayed
        end)
        ()

    let request_operations ctxt ?(chain = `Main) ?peer_id () =
      let s = S.Mempool.request_operations (mempool_path chain_path) in
      RPC_context.make_call1
        s
        ctxt
        chain
        (object
           method peer_id = peer_id
        end)
        ()
  end

  let live_blocks ctxt =
    let f = make_call0 S.live_blocks ctxt in
    fun ?(chain = `Main) ?(block = `Head 0) () -> f chain block () ()
end

module Fake_protocol = struct
  let hash = Protocol_hash.zero

  type block_header_data = unit

  let block_header_data_encoding = Data_encoding.empty

  type block_header_metadata = unit

  let block_header_metadata_encoding = Data_encoding.empty

  type operation_data = unit

  type operation_receipt = unit

  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  let operation_data_encoding = Data_encoding.empty

  let operation_receipt_encoding = Data_encoding.empty

  let operation_data_and_receipt_encoding =
    Data_encoding.conv
      (fun ((), ()) -> ())
      (fun () -> ((), ()))
      Data_encoding.empty
end

module Empty = Make (Fake_protocol) (Fake_protocol)

let () =
  Printexc.register_printer (function
      | ( Json_schema.Cannot_parse _ | Json_schema.Dangling_reference _
        | Json_schema.Bad_reference _ | Json_schema.Unexpected _
        | Json_schema.Duplicate_definition _ ) as exn ->
          Some
            (Format.asprintf "%a" (fun ppf -> Json_schema.print_error ppf) exn)
      | _ -> None)

let protocols = Empty.protocols
