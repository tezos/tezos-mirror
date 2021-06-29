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
      pos : int32;
      len : int32;
      buflen : int32;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"p2p_io_scheduler.invalid_read_request"
    ~title:"Read request is erroneously specified"
    ~description:
      "When about to read incoming data, read parameters are erroneous"
    ~pp:(fun fmt (expected, pos, len, buflen) ->
      Format.fprintf
        fmt
        "%s should hold, but pos=%ld len=%ld buflen=%ld"
        expected
        pos
        len
        buflen)
    Data_encoding.(
      obj4
        (req "expected" string)
        (req "pos" int32)
        (req "len" int32)
        (req "buflen" int32))
    (function
      | Invalid_read_request {expected; pos; len; buflen} ->
          Some (expected, pos, len, buflen)
      | _ -> None)
    (fun (expected, pos, len, buflen) ->
      Invalid_read_request {expected; pos; len; buflen})

type readable = {
  read_buffer : Circular_buffer.t;
  read_queue : Circular_buffer.data tzresult Lwt_pipe.t;
  mutable partial_read : Circular_buffer.data option;
}

let mk_readable ~read_buffer ~read_queue =
  {read_buffer; read_queue; partial_read = None}

(** Container to write bytes to when reading [len] bytes from a connection.
    Bytes read are written to [buf] starting at offset [pos].
    Values of this type guarantee that [0 <= len], [0 <= pos], and
    [pos + len <= (Bytes.length buf)].

    This is ensured by the smart constructor [mk_buffer] and
    the type's implementation not being exposed. *)
type buffer = {len : int; pos : int; buf : Bytes.t}

(** [mk_buffer ?pos ?len buf] creates an instance of {!buffer},
    making sure its invariant holds; or fails with [Invalid_read_request]. *)
let mk_buffer ?pos ?len buf : (buffer, tztrace) result =
  let buflen = Bytes.length buf in
  let pos = Option.value ~default:0 pos in
  let len = Option.value ~default:(buflen - pos) len in
  let check cond ~expected =
    if cond then ok ()
    else
      error
        Int32.(
          Invalid_read_request
            {
              expected;
              pos = of_int pos;
              len = of_int pos;
              buflen = of_int buflen;
            })
  in
  check (0 <= len) ~expected:"0 <= len" >>? fun () ->
  check (0 <= pos) ~expected:"0 <= pos" >>? fun () ->
  check (pos + len <= buflen) ~expected:"pos + len <= buflen" >>? fun () ->
  Ok {len; pos; buf}

let mk_buffer_safe buf : buffer = {len = Bytes.length buf; pos = 0; buf}

(** [shift amount buffer] returns a variant of buffer whose next read
    will write to [buffer.buf] [amount] cells to the right. *)
let shift amount {pos; len; buf} =
  mk_buffer ~pos:(pos + amount) ~len:(len - amount) buf

(* Copy [len] bytes from [data] starting at [pos] into [buf].

   If not all [data] is read, the remainder is put back in
   [conn.partial_read] *)
let read_from conn {pos = offset; len; buf} data =
  match data with
  | Ok data ->
      let read_len = min len (Circular_buffer.length data) in
      Option.iter
        (fun data -> conn.partial_read <- Some data)
        (Circular_buffer.read
           data
           conn.read_buffer
           ~len:read_len
           ~into:buf
           ~offset) ;
      Ok read_len
  | Error _ -> error P2p_errors.Connection_closed

(* Read available data or wait for it. *)
let read ?canceler conn buffer =
  match conn.partial_read with
  | Some msg ->
      conn.partial_read <- None ;
      Lwt.return (read_from conn buffer (Ok msg))
  | None ->
      Lwt.catch
        (fun () ->
          protect ?canceler (fun () -> Lwt_pipe.pop conn.read_queue)
          >|= read_from conn buffer)
        (fun _ -> fail P2p_errors.Connection_closed)

(* fill [buf] with data *)
let read_full ?canceler conn buffer =
  let rec loop ({len; _} as buffer) =
    if len = 0 then return_unit
    else
      read ?canceler conn buffer >>=? fun read_len ->
      (* This is safe - even if the initial ~len is not a multiple of the
         connection's pending bytes - because the low-level read function
         [read_from] reads *at most* the requested number of bytes:
         it doesn't try to read more than available. *)
      shift read_len buffer >>?= loop
  in
  loop buffer

module Internal = struct
  let destruct_buffer {pos; len; buf} = (pos, len, buf)
end
