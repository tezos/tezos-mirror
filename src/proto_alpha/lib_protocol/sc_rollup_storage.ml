(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let originate ctxt ~kind ~boot_sector =
  Raw_context.increment_origination_nonce ctxt >>?= fun (ctxt, nonce) ->
  Sc_rollup_repr.Address.from_nonce nonce >>?= fun address ->
  Storage.Sc_rollup.PVM_kind.add ctxt address kind >>= fun ctxt ->
  Storage.Sc_rollup.Boot_sector.add ctxt address boot_sector >>= fun ctxt ->
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let boot_sector_size =
    Data_encoding.Binary.length
      Sc_rollup_repr.PVM.boot_sector_encoding
      boot_sector
  in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + boot_sector_size + addresses_size)
  in
  return (ctxt, address, size)
