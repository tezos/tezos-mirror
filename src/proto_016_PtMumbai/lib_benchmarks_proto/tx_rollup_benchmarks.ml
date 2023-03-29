(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let ns = Namespace.make Registration_helpers.ns "tx_rollup"

let fv s = Free_variable.of_namespace (ns s)

module Inbox_add_message : Benchmark.S = struct
  open Tx_rollup_inbox_repr

  let name = ns "Inbox_add_message"

  let info = "Benchmark for Merkle.add_message"

  let module_filename = __FILE__

  let tags = ["tx_rollup"; "merkle"; "inbox"; "add_message"]

  type config = {max_messages : int}

  let default_config =
    {
      (* Quite conservative, as in practive we will see no more
         [tx_rollup_max_messages_per_inbox] messages, and that is
         currently set to [1010]. *)
      max_messages = 10000;
    }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_messages} -> max_messages)
      (fun max_messages -> {max_messages})
      int31

  (* The workload is a pair [(messages, message)]: we measure the time
     necessary to [add_message (merkleize messages) message].

     Note: we have no encoding for [Merkle.tree], and so we store the
     unmerkleize message list instead. Below, after generating the
     workload, we merkleize the list _before_ running benchmarking
     [add_message].
  *)
  type workload =
    Tx_rollup_message_hash_repr.t list * Tx_rollup_message_hash_repr.t

  let workload_encoding =
    let open Data_encoding in
    tup2
      (list Tx_rollup_message_hash_repr.encoding)
      Tx_rollup_message_hash_repr.encoding

  let workload_to_vector (messages, _) =
    (* the size of each message already in the inbox and the size of
       the new message is constant (a hash) and therefore we consider
       only the length of the inbox in number of messages as a
       parameter to the cost model. *)
    Sparse_vec.String.of_list
      [("n_messages", float_of_int @@ List.length messages)]

  let models =
    let conv (messages, _) = (List.length messages, ()) in
    [
      ( "tx_rollup",
        Model.make ~conv ~model:(Model.logn ~name ~coeff:(fv "n_message_coeff"))
      );
    ]

  let create_benchmarks ~rng_state ~bench_num ({max_messages} : config) =
    List.repeat bench_num @@ fun () ->
    let n_messages =
      Base_samplers.sample_in_interval
        ~range:{min = 1; max = max_messages}
        rng_state
    in
    let message_hash rng_state : Tx_rollup_message_hash_repr.t =
      let size = Tx_rollup_message_hash_repr.size in
      let bytes =
        Base_samplers.bytes ~size:{min = size; max = size} rng_state
      in
      Tx_rollup_message_hash_repr.of_bytes_exn bytes
    in

    let messages = List.repeat n_messages (message_hash rng_state) in
    let message = message_hash rng_state in
    let workload = (messages, message) in
    let tree = List.fold_left Merkle.add_message Merkle.empty messages in
    let closure () = ignore (Merkle.add_message tree message : Merkle.tree) in
    Generator.Plain {workload; closure}
end

module Commitment_full_compact_bench : Benchmark.S = struct
  open Tx_rollup_commitment_repr

  let name = ns "Commitment_full_compact_bench"

  let info = "Benchmark for Tx_rollup_commitment_repr.Full.compact"

  let module_filename = __FILE__

  let tags = ["tx_rollup"; "merkle"; "commitment"; "compact"]

  type config = {max_messages : int}

  let default_config = {max_messages = 10000}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_messages} -> max_messages)
      (fun max_messages -> {max_messages})
      int31

  type workload = Tx_rollup_commitment_repr.Full.t

  let workload_encoding = Tx_rollup_commitment_repr.Full.encoding

  let workload_to_vector {messages; _} =
    Sparse_vec.String.of_list
      [("n_messages", float_of_int @@ List.length messages)]

  let models =
    let conv {messages; _} = (List.length messages, ()) in
    [
      ( "tx_rollup",
        Model.make
          ~conv
          ~model:
            (Model.affine
               ~name
               ~intercept:(fv "full_compact_bench_intercept")
               ~coeff:(fv "n_messages_coeff")) );
    ]

  let create_benchmarks ~rng_state ~bench_num ({max_messages} : config) =
    List.repeat bench_num @@ fun () ->
    let n_messages =
      Base_samplers.sample_in_interval
        ~range:{min = 1; max = max_messages}
        rng_state
    in
    let message_result_hash rng_state : Tx_rollup_message_result_hash_repr.t =
      let size = Tx_rollup_message_result_hash_repr.size in
      let bytes =
        Base_samplers.bytes ~size:{min = size; max = size} rng_state
      in
      Tx_rollup_message_result_hash_repr.of_bytes_exn bytes
    in
    let workload =
      let level = Tx_rollup_level_repr.root in
      let predecessor = None in
      let inbox_merkle_root =
        Tx_rollup_inbox_repr.Merkle.root Tx_rollup_inbox_repr.Merkle.empty
      in
      (* AFAICT, the values above have no influence on the execution time of [compact] *)
      let messages = List.repeat n_messages (message_result_hash rng_state) in
      {level; messages; predecessor; inbox_merkle_root}
    in
    let closure () =
      ignore
        (Tx_rollup_commitment_repr.Full.compact workload
          : Tx_rollup_commitment_repr.Compact.t)
    in
    Generator.Plain {workload; closure}
end

module Irmin_context = Tezos_context_memory.Context_binary

exception Error of Environment.Error_monad.error

module Prover_storage :
  Tx_rollup_l2_storage_sig.STORAGE
    with type t = Irmin_context.tree
     and type 'a m = 'a Lwt.t = struct
  type t = Irmin_context.tree

  type 'a m = 'a Lwt.t

  module Syntax = struct
    include Lwt.Syntax

    let return = Lwt.return

    let fail e = Lwt.fail (Error e)

    let catch (m : 'a m) k h =
      Lwt.catch
        (fun () -> m >>= k)
        (function Error e -> h e | e -> Lwt.fail e)

    let list_fold_left_m = Lwt_list.fold_left_s
  end

  let path k = [Bytes.to_string k]

  let get store key = Irmin_context.Tree.find store (path key)

  let set store key value = Irmin_context.Tree.add store (path key) value

  let remove store key = Irmin_context.Tree.remove store (path key)
end

module Prover_context = Tx_rollup_l2_context.Make (Prover_storage)
module Prover_apply = Tx_rollup_l2_apply.Make (Prover_context)

type address = {
  sk : Bls12_381_signature.sk;
  pk : Bls12_381_signature.MinPk.pk;
  addr : Tx_rollup_l2_address.t;
  index : Tx_rollup_l2_context_sig.address_index;
  mutable counter : int;
}

type ticket = {
  hash : Alpha_context.Ticket_hash.t;
  index : Tx_rollup_l2_context_sig.ticket_index;
}

type couple = {addr1 : address; addr2 : address; common_tickets : ticket list}

let address sk pk addr i =
  {sk; pk; addr; index = Indexable.index_exn i; counter = 1}

let ticket hash i = {hash; index = Indexable.index_exn i}

let couple a b l = {addr1 = a; addr2 = b; common_tickets = l}

let unique_ticket_id =
  let x = ref 0 in
  fun () ->
    let ticket = Printf.sprintf "ticket%d" !x in
    let () = incr x in
    ticket

let gen_l2_account rng_state =
  let seed = Base_samplers.uniform_bytes ~nbytes:32 rng_state in
  Tezos_crypto.Signature.Bls.generate_key ~seed ()

let hash_key_exn ctxt ~ticketer ~typ ~contents ~owner =
  let ticketer = Micheline.root @@ Expr.from_string ticketer in
  let ty = Micheline.root @@ Expr.from_string typ in
  let contents = Micheline.root @@ Expr.from_string contents in
  let owner = Micheline.root @@ Expr.from_string owner in
  match Alpha_context.Ticket_hash.make ctxt ~ticketer ~ty ~contents ~owner with
  | Ok x -> x
  | Error _ -> raise (Invalid_argument "hash_key_exn")

let make_key ctxt content =
  hash_key_exn
    ctxt
    ~ticketer:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}
    ~typ:"string"
    ~contents:(Printf.sprintf {|"%s"|} content)
      (* In practice, the owner is a rollup address, but this is important only
         for the table of tickets *)
    ~owner:{|"KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq"|}

let make_ticket str =
  match
    Lwt_main.run
      ( Context.init1 () >>=? fun (blk, _) ->
        Incremental.begin_construction blk >>=? fun incr ->
        let ctxt = Incremental.alpha_ctxt incr in
        let ticket, _ctxt = make_key ctxt str in
        return ticket )
  with
  | Ok x -> x
  | Error _ -> raise (Invalid_argument "make_ticket")

let gen_ticket () =
  let ticket = unique_ticket_id () in
  make_ticket ticket

(** [input ~rng_state nb_of_couple_addr nb_of_ticket_per_couple] creates
    [nb_of_couple_addr] of {!couple} where each couple owns
    [nb_of_ticket_per_couple] of {!ticket} in common. It can later on be used
    to create transfers between addresses in the same couple of a ticket they
    both own (that helps to create larger proofs). *)
let input ~rng_state nb_of_couple_addr nb_of_ticket_per_couple =
  (* Prevent an infinite loop, but 0 is semantically stupid as well. *)
  assert (nb_of_couple_addr >= 0) ;
  assert (nb_of_ticket_per_couple >= 0) ;
  let idx_addr = ref 0 in
  let idx_ticket = ref 0 in
  let rec fold_ticket acc = function
    | 0 -> acc
    | n ->
        let tidx = Int32.of_int !idx_ticket in
        let () = incr idx_ticket in
        let ticket = ticket (gen_ticket ()) tidx in
        fold_ticket (ticket :: acc) (n - 1)
  in
  let rec fold_couple acc = function
    | 0 -> acc
    | n ->
        (* Generate random identities *)
        let addr1, pk1, sk1 = gen_l2_account rng_state in
        let addr2, pk2, sk2 = gen_l2_account rng_state in
        (* Pick indexes *)
        let aidx = Int32.of_int !idx_addr in
        let () = incr idx_addr in
        let bidx = Int32.of_int !idx_addr in
        let () = incr idx_addr in
        (* Build addresses *)
        let a = address sk1 pk1 addr1 aidx in
        let b = address sk2 pk2 addr2 bidx in
        (* Generate common tickets *)
        let tickets = List.rev @@ fold_ticket [] nb_of_ticket_per_couple in
        (* Build the couple *)
        fold_couple (couple a b tickets :: acc) (n - 1)
  in
  fold_couple [] nb_of_couple_addr |> List.rev

(** [init_ctxt input] initializes a real [Irmin_context.t] context with
    the abstract data generated in {!input}. *)
let init_ctxt input =
  let open Prover_context in
  let open Syntax in
  let* index = Irmin_context.init "/tmp" in
  let empty_store = Irmin_context.empty index in
  let empty_tree = Irmin_context.Tree.empty empty_store in
  let qty = Tx_rollup_l2_qty.of_int64_exn 1_000_000L in
  let* tree =
    list_fold_left_m
      (fun tree couple ->
        let* tree, _, idx1 =
          Address_index.get_or_associate_index tree couple.addr1.addr
        in
        let* tree =
          Address_metadata.init_with_public_key tree idx1 couple.addr1.pk
        in
        let* tree, _, idx2 =
          Address_index.get_or_associate_index tree couple.addr2.addr
        in
        let* tree =
          Address_metadata.init_with_public_key tree idx2 couple.addr2.pk
        in
        let* tree =
          list_fold_left_m
            (fun tree ticket ->
              let* tree, _, tidx =
                Ticket_index.get_or_associate_index tree ticket.hash
              in
              let* tree = Ticket_ledger.credit tree tidx idx1 qty in
              let* tree = Ticket_ledger.credit tree tidx idx2 qty in
              return tree)
            tree
            couple.common_tickets
        in
        return tree)
      empty_tree
      input
  in
  let* store = Irmin_context.add_tree empty_store [] tree in
  let* _ = Irmin_context.commit ~time:Time.Protocol.epoch store in
  return store

(** [create_operation ~rng_state input senders] creates an operation based
    on [input].
    The generated transfer is random in the sense that:
    * A couple is randomly taken to be the source of the transfer.
    * The values are randomly replaced by their indexes.
    * The destination is randomly taken between:
      * The source couple.
      * Another address in the context.
      * A new generated address.
    * The transfered quantity is randomly generated and can be greater than
      the source balance to make the transfer fail.
*)
let create_operation ~rng_state input senders =
  let either a b = if Base_samplers.uniform_bool rng_state then a else b in
  let index_or_value idx value =
    let idx = Indexable.forget idx in
    let value = Indexable.from_value value in
    either idx value
  in
  let couple, source =
    (* The source must be unique in the transfer. The l2 operation forbids
       operation to have multiple transfers from the same source. *)
    let rec pick_until_new () =
      let couple =
        Stdlib.List.nth
          input
          (Base_samplers.sample_in_interval
             ~range:{min = 0; max = List.length input - 1}
             rng_state)
      in
      let source = couple.addr1 in
      if
        List.mem
          ~equal:(fun x y -> Tx_rollup_l2_address.( = ) x.addr y.addr)
          source
          senders
      then pick_until_new ()
      else (couple, source)
    in
    pick_until_new ()
  in
  let signer =
    index_or_value
      Indexable.(to_int32 source.index |> index_exn)
      (Tx_rollup_l2_batch.Bls_pk source.pk)
  in
  let destination =
    let x =
      Base_samplers.sample_in_interval ~range:{min = 0; max = 99} rng_state
    in
    if x >= 0 && x < 40 then
      (* couple.B, he has the ticket *)
      index_or_value couple.addr2.index couple.addr2.addr
    else if x >= 40 && x < 80 then
      (* other couple, he does not have the ticket *)
      let i =
        Base_samplers.sample_in_interval
          ~range:{min = 0; max = List.length input - 1}
          rng_state
      in
      let couple = Stdlib.List.nth input i in
      index_or_value couple.addr2.index couple.addr2.addr
    else
      (* create new address *)
      let addr, _pk, _sk = gen_l2_account rng_state in
      Indexable.from_value addr
  in
  let qty =
    let x =
      Base_samplers.sample_in_interval ~range:{min = 0; max = 99} rng_state
    in
    (* Low probably to take more than the balance and make the operation fail. *)
    if x <= 2 then Tx_rollup_l2_qty.of_int64_exn 1_000_001L
    else Tx_rollup_l2_qty.one
  in
  let {hash = ticket_hash; index = ticket_index} =
    let n = List.length couple.common_tickets in
    let x =
      Base_samplers.sample_in_interval ~range:{min = 0; max = n - 1} rng_state
    in
    Stdlib.List.nth couple.common_tickets x
  in
  let counter = source.counter in
  let () = source.counter <- counter + 1 in
  let ticket_hash = index_or_value ticket_index ticket_hash in
  ( Tx_rollup_l2_batch.V1.
      {
        signer;
        counter = 1L;
        contents = [Transfer {destination; ticket_hash; qty}];
      },
    source.sk,
    source :: senders )

let create_transaction ~rng_state input nb_op =
  (* Prevent an infinite loop, but 0 is semantically stupid as well. *)
  assert (nb_op >= 0) ;
  let rec aux acc senders = function
    | 0 -> acc
    | n ->
        let op, signer, senders = create_operation ~rng_state input senders in
        let acc = (op, signer) :: acc in
        aux acc senders (n - 1)
  in
  let acc = [] in
  let senders = [] in
  aux acc senders nb_op

let make_msg ~rng_state input nb_op =
  let transaction, signers =
    create_transaction ~rng_state input nb_op |> List.split
  in
  let buf =
    (Data_encoding.Binary.to_bytes_exn
       Data_encoding.Compact.(
         make ~tag_size:`Uint8 Tx_rollup_l2_batch.V1.compact_transaction))
      transaction
  in
  let signatures =
    List.map (fun sk -> Bls12_381_signature.MinPk.Aug.sign sk buf) signers
  in
  let aggregated_signature =
    match Bls12_381_signature.MinPk.aggregate_signature_opt signatures with
    | Some res -> res
    | None -> assert false
  in
  let batch =
    Tx_rollup_l2_batch.V1.{contents = [transaction]; aggregated_signature}
  in
  let batch_string =
    Data_encoding.Binary.to_string_exn
      Tx_rollup_l2_batch.encoding
      Tx_rollup_l2_batch.(V1 batch)
  in
  Alpha_context.Tx_rollup_message.make_batch batch_string |> fst

let get_tree_from_store store =
  let open Prover_context.Syntax in
  let* tree_opt = Irmin_context.find_tree store [] in
  match tree_opt with Some x -> return x | None -> assert false

let hash_tree_from_store store =
  let open Prover_context.Syntax in
  let+ tree = get_tree_from_store store in
  Irmin_context.Tree.hash tree

let create_proof store max_withdrawals msg =
  let open Prover_context.Syntax in
  let index = Irmin_context.index store in
  let* hash = hash_tree_from_store store in
  let* proof, _ =
    Irmin_context.produce_stream_proof index (`Node hash) (fun tree ->
        Prover_apply.(
          catch
            (apply_message
               tree
               Tx_rollup_l2_apply.
                 {tx_rollup_max_withdrawals_per_batch = max_withdrawals}
               msg)
            (fun (tree, _) -> return (tree, ()))
            (fun _error ->
              (* With the context and message generation we should not reach
                 this case. *)
              assert false)))
  in
  return proof

module Verify_proof_compute_bench : Benchmark.S = struct
  let name = ns "Tx_rollup_verify_proof"

  let info = "Benchmark for Tx_rollup.verify_proof"

  let module_filename = __FILE__

  let tags = ["tx_rollup"; "merkle"; "verify"; "proof"]

  type config = {max_withdrawals : int}

  let default_config = {max_withdrawals = 255}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_withdrawals} -> max_withdrawals)
      (fun max_withdrawals -> {max_withdrawals})
      (obj1 (req "max_withdrawals" int31))

  type workload = {proof_size : int; message_size : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {proof_size; message_size} -> (proof_size, message_size))
      (fun (proof_size, message_size) -> {proof_size; message_size})
      (obj2 (req "proof_size" int31) (req "message_size" int31))

  let workload_to_vector {proof_size; message_size} =
    Sparse_vec.String.of_list
      [
        ("proof_size", float_of_int proof_size);
        ("message_size", float_of_int message_size);
      ]

  let models =
    let conv {proof_size; message_size} = (proof_size, (message_size, ())) in
    [
      ( "tx_rollup",
        Model.make
          ~conv
          ~model:
            (Model.bilinear
               ~name
               ~coeff1:(fv "proof_size_coeff")
               ~coeff2:(fv "message_size_coeff")) );
    ]

  let proof_size proof =
    Data_encoding.Binary.length Tx_rollup_l2_proof.encoding proof

  let message_size message =
    Data_encoding.Binary.length Alpha_context.Tx_rollup_message.encoding message

  let bench_verify_proof rng_state {max_withdrawals} () =
    let n_couple_addr =
      Base_samplers.sample_in_interval rng_state ~range:{min = 100; max = 1_000}
    in
    let n_ticket_per_couple =
      Base_samplers.sample_in_interval rng_state ~range:{min = 1; max = 6}
    in
    let n_ops =
      Base_samplers.sample_in_interval rng_state ~range:{min = 5; max = 30}
    in
    let input = input ~rng_state n_couple_addr n_ticket_per_couple in
    let message = make_msg ~rng_state input n_ops in
    let proof =
      Lwt_main.run
      @@
      let open Lwt_syntax in
      let* store = init_ctxt input in
      create_proof store max_withdrawals message
    in
    let closure () =
      Lwt_main.run
      @@
      let open Prover_context.Syntax in
      (* Account the time it takes to calculate the length of the proof and
         message done during the proof verification. *)
      let (_ : int) = proof_size proof in
      let (_ : int) = message_size message in
      (* Account the time it takes to verify the proof *)
      let _ =
        Tx_rollup_l2_verifier.Internal_for_tests.verify_l2_proof
          proof
          Tx_rollup_l2_apply.
            {tx_rollup_max_withdrawals_per_batch = max_withdrawals}
          message
      in
      return ()
    in
    let proof_size = proof_size proof in
    let message_size = message_size message in
    Generator.Plain {workload = {proof_size; message_size}; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (bench_verify_proof rng_state config)
end

let () = Registration_helpers.register (module Inbox_add_message)

let () = Registration_helpers.register (module Commitment_full_compact_bench)

let () = Registration_helpers.register (module Verify_proof_compute_bench)
