(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module manages the cache for smart contracts.

    This cache must be consistent with the on-disk representation
    of the smart contracts. In particular, [update] must be called
    each time a contract storage is updated.

*)

open Alpha_context

(** Each cached script has a unique identifier in the cache. *)
type identifier

(** The cache holds the unparsed and the internal representation of
   the contract. *)
type cached_contract = Script.t * Script_ir_translator.ex_script

(** [find ctxt contract] returns [(ctxt', identifier, script)] where:
   - [ctxt'] is [ctxt] with less gas;
   - [identifier] is the identifier identifying the [contract] in the cache;
   - [script = None] if there is no such contract in [ctxt];
   - [script = Some (unparsed_script, ir_script)] where
     - [unparsed_script] is the contract code and storage;
     - [script_ir] is a typed internal representation of the contract, i.e.,
       the abstract syntax tree of its code as well as its storage.

   This function consumes gas depending on the cache. If the contract is not
   in the cache, then the function also consumes the gas of [Contract.get_script]
   and [Script_ir_translator.parse_script]. *)
val find :
  context ->
  Contract_hash.t ->
  (context * identifier * cached_contract option) tzresult Lwt.t

(** [update ctxt identifier unparsed_script ir_script size] refreshes the
   cached contract identified by [identifier] with a new [unparsed_script],
   a new [ir_script], and a new size. *)
val update : context -> identifier -> cached_contract -> int -> context tzresult

(** [entries ctxt] returns the contracts in the cache as well as their
   respective size. The list is sorted by date of last modification:
   the least recently updated entry comes first. *)
val entries : context -> (Contract_hash.t * int) list tzresult

(** [contract_rank ctxt contract] returns the number of contracts
    older than [contract] in the cache of [ctxt]. This function
    returns [None] if [contract] does not exist in the cache of
    [ctxt]. *)
val contract_rank : context -> Contract_hash.t -> int option

(** [size ctxt] is an overapproximation of the cache size in
   memory (in bytes). *)
val size : context -> int

(** [size_limit ctxt] is the maximal size of the cache (in bytes). *)
val size_limit : context -> int
