(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

module Context_size_dependent_read_bench : sig
  open Base_samplers

  type config = {
    depth : range;
    storage_chunk_bytes : int;
    storage_chunks : range;
    insertions : range;
    key_card : int;
    commit_batch_size : int;
    temp_dir : string option;
  }

  include Benchmark.Simple with type config := config
end

module Context_size_dependent_write_bench : sig
  open Base_samplers

  type config = {
    depth : range;
    storage_chunk_bytes : int;
    storage_chunks : range;
    insertions : range;
    key_card : int;
    commit_batch_size : int;
    temp_dir : string option;
  }

  include Benchmark.Simple with type config := config
end

module Irmin_pack_read_bench : sig
  open Base_samplers

  type config = {
    depth : range;
    insertions : range;
    key_card : int;
    irmin_pack_max_width : int;
    storage_chunk_bytes : int;
    storage_chunks : range;
    default_storage_bytes : int;
    commit_batch_size : int;
    temp_dir : string option;
  }

  include Benchmark.Simple with type config := config
end

module Irmin_pack_write_bench : sig
  open Base_samplers

  type config = {
    depth : range;
    insertions : range;
    key_card : int;
    irmin_pack_max_width : int;
    storage_chunk_bytes : int;
    storage_chunks : range;
    default_storage_bytes : int;
    commit_batch_size : int;
    temp_dir : string option;
  }

  include Benchmark.Simple with type config := config
end

module Read_random_key_bench : sig
  type config = {
    existing_context : string * Context_hash.t;
    subdirectory : string;
  }

  include Benchmark.Simple_with_num with type config := config
end

module Write_random_keys_bench : sig
  open Base_samplers

  type config = {
    existing_context : string * Context_hash.t;
    storage_chunk_bytes : int;
    storage_chunks : range;
    max_written_keys : int;
    temp_dir : string option;
    subdirectory : string;
  }

  include Benchmark.Simple_with_num with type config := config
end
