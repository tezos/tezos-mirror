(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

open Environment

module Registration = struct
  let register0_noctxt ~chunked s f dir =
    RPC_directory.register ~chunked dir s (fun _rpc_ctxt q i -> f q i)
end

module DAC = struct
  module Hashing_scheme = Dac_pages_encoding.Merkle_tree.V0
  module Hash_storage = Dac_preimage_data_manager.Reveal_hash

  module S = struct
    let dac_store_preimage =
      RPC_service.put_service
        ~description:"Split DAC reveal data"
        ~query:RPC_query.empty
        ~input:Data_encoding.(bytes Hex)
        ~output:Hashing_scheme.hash_encoding
        RPC_path.(open_root / "dac" / "store_preimage")
  end

  let handle_serialize_dac_store_preimage reveal_data_dir data =
    let for_each_page (hash, page_contents) =
      Hash_storage.save_bytes reveal_data_dir hash page_contents
    in
    let size = Protocol.Alpha_context.Constants.sc_rollup_message_size_limit in
    Hashing_scheme.serialize_payload ~max_page_size:size data ~for_each_page

  let register_serialize_dac_store_preimage reveal_data_dir =
    Registration.register0_noctxt
      ~chunked:false
      S.dac_store_preimage
      (fun () input ->
        handle_serialize_dac_store_preimage reveal_data_dir input)

  let register reveal_data_dir =
    (RPC_directory.empty : unit RPC_directory.t)
    |> register_serialize_dac_store_preimage reveal_data_dir
end

let rpc_services ~reveal_data_dir = DAC.register reveal_data_dir
