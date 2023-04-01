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

let map5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

(* Difference between the scalar order and the succeeding power of 2 *)
let alpha = Z.(shift_left one (Z.numbits S.order) - S.order)

let bitlist : ?le:bool -> bytes -> bool list =
 fun ?(le = false) b ->
  let l = Bytes.length b in
  let start = if le then 0 else l - 1 in
  let stop = if le then l else -1 in
  let next a = if le then a + 1 else a - 1 in
  let rec loop_byte acc n =
    if n = stop then acc
    else
      let byte = Bytes.get_uint8 b n in
      let rec loop_bit acc m =
        if m = 8 then acc
        else
          let mask = 1 lsl m in
          let bit = byte land mask in
          let bit = if bit = 0 then false else true in
          loop_bit (bit :: acc) (m + 1)
      in
      let acc = loop_bit acc 0 in
      loop_byte acc (next n)
  in
  List.rev @@ loop_byte [] start

let bytes_of_hex hs =
  let h = `Hex hs in
  Hex.to_bytes h

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
