(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
open Tezos_scoru_wasm
module Wasmer = Tezos_wasmer
module Lazy_containers = Tezos_lazy_containers

module Kernel_cache = Cache.Make (struct
  module Key = Context_hash

  type value = Wasmer.Module.t

  let delete = Wasmer.Module.delete
end)

let kernel_cache = Kernel_cache.create 2

let load_parse_module store key durable =
  let open Lwt.Syntax in
  let* kernel = Durable.find_value_exn durable key in
  let+ kernel = Lazy_containers.Chunked_byte_vector.to_string kernel in
  Wasmer.Module.(create store Binary kernel)

let load_module store key durable =
  let open Lwt.Syntax in
  let* kernel_hash = Durable.hash_exn ~kind:Value durable key in
  let md = Kernel_cache.find_opt kernel_cache kernel_hash in
  match md with
  | None ->
      let* md = load_parse_module store key durable in
      Kernel_cache.replace kernel_cache kernel_hash md ;
      Lwt.return md
  | Some md -> Lwt.return md

let load_kernel store durable = load_module store Constants.kernel_key durable
