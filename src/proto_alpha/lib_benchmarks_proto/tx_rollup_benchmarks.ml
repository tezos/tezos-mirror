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

module Inbox_add_message : Benchmark.S = struct
  open Tx_rollup_inbox_repr

  let name = "Inbox_add_message"

  let info = "Benchmark for Merkle.add_message"

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
        Model.make
          ~conv
          ~model:(Model.logn ~coeff:(Free_variable.of_string "n_message_coeff"))
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

let () = Registration_helpers.register (module Inbox_add_message)
