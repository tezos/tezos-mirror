(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* Tezos Protocol Implementation - Random number generation *)

type seed = B of State_hash.t

type t = T of State_hash.t

type sequence = S of State_hash.t

type nonce = bytes

let nonce_encoding = Data_encoding.Fixed.bytes Constants_repr.nonce_length

let initial_seed = "Laissez-faire les proprietaires."

let zero_bytes = Bytes.make Nonce_hash.size '\000'

let state_hash_encoding =
  let open Data_encoding in
  conv State_hash.to_bytes State_hash.of_bytes_exn (Fixed.bytes Nonce_hash.size)

let seed_encoding =
  let open Data_encoding in
  conv (fun (B b) -> b) (fun b -> B b) state_hash_encoding

let empty = B (State_hash.hash_string [initial_seed])

let nonce (B state) nonce =
  B (State_hash.hash_bytes [State_hash.to_bytes state; nonce])

let initialize_new (B state) append =
  T (State_hash.hash_bytes (State_hash.to_bytes state :: zero_bytes :: append))

let xor_higher_bits i b =
  let higher = TzEndian.get_int32 b 0 in
  let r = Int32.logxor higher i in
  let res = Bytes.copy b in
  TzEndian.set_int32 res 0 r ;
  res

let sequence (T state) n =
  State_hash.to_bytes state |> xor_higher_bits n |> fun b ->
  S (State_hash.hash_bytes [b])

let take (S state) =
  let b = State_hash.to_bytes state in
  let h = State_hash.hash_bytes [b] in
  (State_hash.to_bytes h, S h)

let take_int32 s bound =
  if Compare.Int32.(bound <= 0l) then invalid_arg "Seed_repr.take_int32"
    (* FIXME *)
  else
    let drop_if_over =
      Int32.sub Int32.max_int (Int32.rem Int32.max_int bound)
    in
    let rec loop s =
      let bytes, s = take s in
      let r = TzEndian.get_int32 bytes 0 in
      (* The absolute value of min_int is min_int.  Also, every
           positive integer is represented twice (positive and negative),
           but zero is only represented once.  We fix both problems at
           once. *)
      let r = if Compare.Int32.(r = Int32.min_int) then 0l else Int32.abs r in
      if Compare.Int32.(r >= drop_if_over) then loop s
      else
        let v = Int32.rem r bound in
        (v, s)
    in
    loop s

let take_int64 s bound =
  if Compare.Int64.(bound <= 0L) then invalid_arg "Seed_repr.take_int64"
    (* FIXME *)
  else
    let drop_if_over =
      Int64.sub Int64.max_int (Int64.rem Int64.max_int bound)
    in

    let rec loop s =
      let bytes, s = take s in
      let r = TzEndian.get_int64 bytes 0 in
      (* The absolute value of min_int is min_int.  Also, every
           positive integer is represented twice (positive and negative),
           but zero is only represented once.  We fix both problems at
           once. *)
      let r = if Compare.Int64.(r = Int64.min_int) then 0L else Int64.abs r in
      if Compare.Int64.(r >= drop_if_over) then loop s
      else
        let v = Int64.rem r bound in
        (v, s)
    in
    loop s

type error += Unexpected_nonce_length (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"unexpected_nonce_length"
    ~title:"Unexpected nonce length"
    ~description:"Nonce length is incorrect."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Nonce length is not %i bytes long as it should."
        Constants_repr.nonce_length)
    Data_encoding.empty
    (function Unexpected_nonce_length -> Some () | _ -> None)
    (fun () -> Unexpected_nonce_length)

let make_nonce nonce =
  if Compare.Int.(Bytes.length nonce <> Constants_repr.nonce_length) then
    error Unexpected_nonce_length
  else ok nonce

let hash nonce = Nonce_hash.hash_bytes [nonce]

let check_hash nonce hash =
  Compare.Int.(Bytes.length nonce = Constants_repr.nonce_length)
  && Nonce_hash.equal (Nonce_hash.hash_bytes [nonce]) hash

let nonce_hash_key_part = Nonce_hash.to_path

let initial_nonce_0 = zero_bytes

let initial_nonce_hash_0 = hash initial_nonce_0

let deterministic_seed seed = nonce seed zero_bytes

let initial_seeds ?initial_seed n =
  let[@coq_struct "i"] rec loop acc elt i =
    if Compare.Int.(i = 1) then List.rev (elt :: acc)
    else loop (elt :: acc) (deterministic_seed elt) (i - 1)
  in
  let first_seed =
    match initial_seed with
    | Some initial_seed -> nonce (B initial_seed) initial_nonce_0
    | None -> B (State_hash.hash_bytes [])
  in
  loop [] first_seed n
