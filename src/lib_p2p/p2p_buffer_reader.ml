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

type error +=
  | Invalid_read_request of {
      expected : string;
      pos : int64;
      length_to_copy : int64;
      buflen : int64;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"p2p_io_scheduler.invalid_read_request"
    ~title:"Read request is erroneously specified"
    ~description:
      "When about to read incoming data, read parameters are erroneous"
    ~pp:(fun fmt (expected, pos, length_to_copy, buflen) ->
      Format.fprintf
        fmt
        "%s should hold, but pos=%Ld length_to_copy=%Ld buflen=%Ld"
        expected
        pos
        length_to_copy
        buflen)
    Data_encoding.(
      obj4
        (req "expected" string)
        (req "pos" int64)
        (req "length_to_copy" int64)
        (req "buflen" int64))
    (function
      | Invalid_read_request {expected; pos; length_to_copy; buflen} ->
          Some (expected, pos, length_to_copy, buflen)
      | _ -> None)
    (fun (expected, pos, length_to_copy, buflen) ->
      Invalid_read_request {expected; pos; length_to_copy; buflen})

type readable = {
  read_buffer : Circular_buffer.t;
  read_queue : Circular_buffer.data tzresult Lwt_pipe.Maybe_bounded.t;
  mutable partial_read : Circular_buffer.data option;
}

let mk_readable ~read_buffer ~read_queue =
  {read_buffer; read_queue; partial_read = None}

(** Container to write bytes to when reading [length_to_copy] bytes from a readable.
    Bytes read are written to [buf] starting at offset [pos].
    Values of this type guarantee the invariants:

    - [0 <= length_to_copy]
    - [0 <= pos]
    - [pos + length_to_copy <= (Bytes.length buf)]

    This is ensured by the smart constructor [mk_buffer] and
    the type being abstract. *)
type buffer = {length_to_copy : int; pos : int; buf : Bytes.t}

let mk_buffer ?pos ?length_to_copy buf : buffer tzresult =
  let open Tzresult_syntax in
  let buflen = Bytes.length buf in
  let pos = Option.value ~default:0 pos in
  let length_to_copy = Option.value ~default:(buflen - pos) length_to_copy in
  let check cond ~expected =
    if cond then return_unit
    else
      fail
        (Invalid_read_request
           {
             expected;
             pos = Int64.of_int pos;
             length_to_copy = Int64.of_int pos;
             buflen = Int64.of_int buflen;
           })
  in
  let* () = check (0 <= length_to_copy) ~expected:"0 <= length_to_copy" in
  let* () = check (0 <= pos) ~expected:"0 <= pos" in
  let* () =
    check
      (pos + length_to_copy <= buflen)
      ~expected:"pos + length_to_copy <= buflen"
  in
  Ok {length_to_copy; pos; buf}

let mk_buffer_safe buf : buffer =
  {length_to_copy = Bytes.length buf; pos = 0; buf}

(** [shift amount buffer] returns a variant of [buffer] with its [pos] shifted
    [amount] cells to the right and [length_to_copy] adapted accordingly. *)
let shift amount {pos; length_to_copy; buf} =
  mk_buffer ~pos:(pos + amount) ~length_to_copy:(length_to_copy - amount) buf

(** Copy [length_to_copy] bytes from [data] starting at [pos] into [buf].

   If not all [data] is read, the remainder is put back in
   [readable.partial_read] *)
let read_from readable {pos = offset; length_to_copy; buf} data =
  match data with
  | Ok data ->
      let read_len = min length_to_copy (Circular_buffer.length data) in
      Option.iter
        (fun data -> readable.partial_read <- Some data)
        (Circular_buffer.read
           data
           readable.read_buffer
           ~len:read_len
           ~into:buf
           ~offset) ;
      Ok read_len
  | Error _ -> error P2p_errors.Connection_closed

let read ?canceler readable buffer =
  let open Lwt_syntax in
  match readable.partial_read with
  | Some msg ->
      readable.partial_read <- None ;
      Lwt.return (read_from readable buffer (Ok msg))
  | None ->
      let+ m =
        protect ?canceler (fun () ->
            Lwt_pipe.Maybe_bounded.pop readable.read_queue)
      in
      read_from readable buffer m

let read_full ?canceler readable buffer =
  let open Lwt_result_syntax in
  let rec loop ({length_to_copy; _} as buffer) =
    if length_to_copy = 0 then return_unit
    else
      let* read_len = read ?canceler readable buffer in
      (* This is safe - even if the initial [length_to_copy] is not a multiple of the
         readable's pending bytes - because [read] reads *at most*
         the requested number of bytes: it doesn't try to read more
         than available. *)
      let*? buffer = shift read_len buffer in
      loop buffer
  in
  loop buffer

module Internal_for_tests = struct
  let destruct_buffer {pos; length_to_copy; buf} = (pos, length_to_copy, buf)
end
