(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

module S = Csir.Scalar

(* Difference between the scalar order and the succeeding power of 2 *)
let alpha = Z.(shift_left one (Z.numbits S.order) - S.order)

(*
  Plompiler uses lists of bits where the lower positions are less significant
  e.g. the least significant bit of [bs] is [List.nth bs 0].

  [bitlist] default endianess follows the semantics of [get_uint16] that can be
  tested in utop:

  Bytes.get_uint16_le (Bytes.of_string "\001\000") 0;;
  - : int = 1

  Bytes.get_uint16_be (Bytes.of_string "\001\000") 0;;
  - : int = 256
*)

let bitlist : ?le:bool -> bytes -> bool list =
 fun ?(le = false) b ->
  let l = Bytes.length b in
  (* Depending on endianess we start, stop and step in different directions. *)
  let start = if le then 0 else l - 1 in
  let stop = if le then l else -1 in
  let next = if le then succ else pred in
  let rec loop_byte acc n =
    if n = stop then acc
    else
      let byte = Bytes.get_uint8 b n in
      let rec loop_bit acc m =
        if m = 8 then acc
        else
          (* For each position in a byte, a mask is built where all bits are
             zero except the one at position. The masked byte is compute by
             locagical AND of the mask and the current byte. If the masked
             result is zero the bit at position is zero, otherwise it's one. *)
          let mask = 1 lsl m in
          let bit = byte land mask in
          let bit = if bit = 0 then false else true in
          loop_bit (bit :: acc) (m + 1)
      in
      let acc = loop_bit acc 0 in
      loop_byte acc (next n)
  in
  List.rev @@ loop_byte [] start

(* Takes a list of booleans (typically from the Plompiler Bytes representation)
   and returns OCaml Bytes. Works only if the input length is a multiple of a
   byte. *)
let of_bitlist : ?le:bool -> bool list -> bytes =
 fun ?(le = false) bl ->
  assert (List.length bl mod 8 = 0) ;
  let rec loop_byte acc rest =
    match rest with
    | [] ->
        let res = if le then List.rev acc else acc in
        Bytes.(concat empty res)
    | _ ->
        let rec loop_bit acc pos rest =
          if pos = 8 then (acc, rest)
          else
            match rest with
            | [] -> assert false
            | bit :: rest ->
                (* For each position in a byte, a mask is built where all bits
                   are zero except the one at position. The mask is then summed
                   to the accumulator using a logical OR. *)
                let mask = if bit then 1 lsl pos else 0 in
                let acc = acc lor mask in
                loop_bit acc (succ pos) rest
        in
        (* Each sequence of 8 bits is converted to an integer in the previous
           loop and here it is interpreted as a uint8. *)
        let byte_as_int, rest = loop_bit 0 0 rest in
        let byte = Bytes.create 1 in
        Bytes.set_uint8 byte 0 byte_as_int ;
        loop_byte (byte :: acc) rest
  in
  loop_byte [] bl

let bytes_of_hex hs =
  let h = `Hex hs in
  Hex.to_bytes h

let hex_of_bytes bs = Hex.of_bytes bs |> Hex.show

let bool_list_to_scalar : bool list -> S.t =
 fun b_list ->
  let res, _ =
    List.fold_left
      (fun (acc_res, acc_p) b ->
        let acc_res = if b then S.(acc_res + acc_p) else acc_res in
        let acc_p = S.double acc_p in
        (acc_res, acc_p))
      (S.zero, S.one)
      b_list
  in
  res

(* We use little endian notation (the lsb is on the head) *)
let bool_list_to_z : bool list -> Z.t =
 fun b_list ->
  let res, _ =
    List.fold_left
      (fun (acc_res, acc_p) b ->
        let acc_res = if b then Z.(acc_res + acc_p) else acc_res in
        let acc_p = Z.(acc_p + acc_p) in
        (acc_res, acc_p))
      (Z.zero, Z.one)
      b_list
  in
  res

(* We use little endian notation (the lsb is on the head) *)
let bool_list_of_z : ?nb_bits:int -> Z.t -> bool list =
 fun ?nb_bits z ->
  let two = Z.of_int 2 in
  let rec aux bits z = function
    | 0 -> List.rev bits
    | n ->
        let b = Z.(equal (z mod two) one) in
        aux (b :: bits) (Z.div z two) (n - 1)
  in
  aux [] z @@ Option.value ~default:(Z.numbits z) nb_bits

module Z = struct
  include Z

  let t : t Repr.t =
    Repr.(
      map
        bytes
        (fun bs -> Z.of_bits (Bytes.unsafe_to_string bs))
        (fun s -> Z.to_bits s |> Bytes.of_string))
end

let ( %! ) = Z.rem

(* [next_multiple_of k n] is the first multiple of [k : int] greater than
   or equal to [n : int] *)
let next_multiple_of k n = k * (1 + ((n - 1) / k))

(* [is_power_of_2 n] returns [true] iff [n : Z.t] is a perfect power of 2 *)
let is_power_of_2 n = Z.log2 n = Z.log2up n

(* [min_nb_limbs ~modulus ~base] is the smallest integer k such that
   base^k >= modulus *)
let min_nb_limbs ~modulus ~base =
  assert (Z.(modulus > one)) ;
  assert (Z.(base > one)) ;
  (* we want to compute ceil(log_base(modulus)), but we use this iterative
     method as we only have support for log2 (and not log_base) over Z.t *)
  let rec aux acc k =
    if acc >= modulus then k else aux Z.(acc * base) (k + 1)
  in
  aux base 1

(* [z_to_limbs ~len ~base n] takes an integer (n : Z.t) and returns a Z.t list
   of [len] elements encoding its big-endian representation in base [base].
   It fails if [n < 0 or n >= base^len]. *)
let z_to_limbs ~len ~base n =
  let rec aux output n =
    let q, r = Z.div_rem n base in
    if Z.(q = zero) then r :: output else aux (r :: output) q
  in
  if n < Z.zero then
    raise @@ Failure "z_to_limbs: n must be greater than or equal to zero" ;
  let limbs = aux [] n in
  let nb_limbs = List.length limbs in
  if nb_limbs > len then
    raise @@ Failure "z_to_limbs: n must be strictly lower than base^len"
  else List.init (len - nb_limbs) (Fun.const Z.zero) @ limbs

(* [z_of_limbs ~base ls] returns the Z.t encoded in the given Z.t list [ls],
   its big-endian representation in base [base]. *)
let z_of_limbs ~base limbs =
  List.fold_left (fun acc x -> Z.((base * acc) + x)) Z.zero limbs

(* [mod_add_limbs ~modulus ~base xs ys] returns the result of adding [xs]
   and [ys] modulo [modulus], where the inputs and the output are in big-endian
   form in base [base]. *)
let mod_add_limbs ~modulus ~base xs ys =
  let nb_limbs = List.length xs in
  assert (List.compare_length_with ys nb_limbs = 0) ;
  let x = z_of_limbs ~base xs in
  let y = z_of_limbs ~base ys in
  let z = Z.((x + y) %! modulus) in
  let z = if z < Z.zero then Z.(z + modulus) else z in
  z_to_limbs ~len:nb_limbs ~base z

let mod_sub_limbs ~modulus ~base xs ys =
  mod_add_limbs ~modulus ~base xs (List.map Z.neg ys)

(* [mod_mul_limbs ~modulus ~base xs ys] returns the result of multiplying [xs]
   by [ys] modulo [modulus], where the inputs and the output are in big-endian
   form in base [base].
   It performs modular division instead if the optional argument [division] is
   set to true. *)
let mod_mul_limbs ?(division = false) ~modulus ~base xs ys =
  let nb_limbs = List.length xs in
  assert (List.compare_length_with ys nb_limbs = 0) ;
  let x = z_of_limbs ~base xs in
  let y = z_of_limbs ~base ys in
  let y =
    if division then
      let _d, y_inv, _v = Z.gcdext y modulus in
      y_inv
    else y
  in
  let z = Z.(x * y %! modulus) in
  let z = if z < Z.zero then Z.(z + modulus) else z in
  z_to_limbs ~len:nb_limbs ~base z

let mod_div_limbs ~modulus ~base xs ys =
  mod_mul_limbs ~division:true ~modulus ~base xs ys

let rec transpose = function
  | [] | [] :: _ -> []
  | rows -> List.(map hd rows :: (transpose @@ map tl rows))

let of_bytes repr bs =
  Stdlib.Result.get_ok
  @@ Repr.(unstage @@ of_bin_string repr) (Bytes.unsafe_to_string bs)

let to_bytes repr e =
  Bytes.unsafe_of_string @@ Repr.(unstage @@ to_bin_string repr) e

let tables_cs_encoding_t : (string list * Csir.CS.t) Repr.t =
  let open Repr in
  pair (list string) Csir.CS.t

let save_cs_to_file path tables cs =
  (*   let outc = open_out path in *)
  (*   let encoder = Jsonm.encoder (`Channel outc) in *)
  (*   Repr.encode_json tables_cs_encoding_t encoder (tables, cs); *)
  (*   close_out outc *)
  let s = Repr.to_json_string tables_cs_encoding_t (tables, cs) in
  let outc = open_out path in
  output_string outc s ;
  close_out outc

let load_cs_from_file path =
  if not (Sys.file_exists path) then
    raise
    @@ Invalid_argument
         (Printf.sprintf "load_cs_from_file: %s does not exist." path) ;
  (*   let inc = open_in path in *)
  (*   let decoder = Jsonm.decoder (`Channel inc) in *)
  (*   let res = *)
  (*     Repr.decode_json tables_cs_encoding_t decoder |> Stdlib.Result.get_ok *)
  (*   in *)
  (*   close_in inc; *)
  (*   res *)
  let inc = open_in path in
  let content = really_input_string inc (in_channel_length inc) in
  let res =
    Repr.of_json_string tables_cs_encoding_t content |> Stdlib.Result.get_ok
  in
  close_in inc ;
  res

let get_circuit_id cs =
  let serialized_bytes = to_bytes Csir.CS.t cs in
  Hacl_star.Hacl.Blake2b_32.hash serialized_bytes 32 |> Hex.of_bytes |> Hex.show

let circuit_dir =
  match Sys.getenv_opt "TMPDIR" with
  | None -> "/tmp/plompiler"
  | Some dir -> dir ^ "/plompiler"

let circuit_path s =
  if not @@ Sys.file_exists circuit_dir then Sys.mkdir circuit_dir 0o755 ;
  circuit_dir ^ "/" ^ s

let dump_label_traces path (cs : Csir.CS.t) =
  let outc = open_out path in
  List.iter
    Csir.CS.(
      Array.iter (fun c ->
          Printf.fprintf outc "%s 1\n" @@ String.concat "; " (List.rev c.label)))
    cs ;
  close_out outc
