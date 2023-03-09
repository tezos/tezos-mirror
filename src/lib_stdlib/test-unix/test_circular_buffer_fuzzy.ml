(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:    stdlib
    Invocation:   dune exec src/lib_stdlib/test-unix/main.exe
    Subject:      Test the circular buffer with a reference implementation
 *)

(* This test implement a fuzzy testing where we check that the
   `circular_buffer` behaves similarly than a reference implementation
   of the same interface. *)

open Lwt.Syntax
open Qcheck2_helpers

module type S = sig
  type t

  type data

  val create : ?maxlength:int -> ?fresh_buf_size:int -> unit -> t

  (* Write the output of [fill_using] in [data]. *)
  val write :
    maxlen:int ->
    fill_using:(bytes -> int -> int -> (int, 'err) result Lwt.t) ->
    t ->
    (data, 'err) result Lwt.t

  (* Read the value of [data]. The read may be partial if the [data]
     is not fully read. We return the [data] part which was not
     read. *)
  val read : data -> ?len:int -> t -> into:Bytes.t -> offset:int -> data option

  val length : data -> int
end

module Reference : S = struct
  (* There is not buffer, hence the type [t] is not necessary. For
     each [data] we create a new buffer. *)
  type t = unit

  type data = Bytes.t

  let create ?maxlength:_ ?fresh_buf_size:_ () = ()

  let write ~maxlen
      ~(fill_using : data -> int -> int -> (int, 'err) result Lwt.t) () =
    let bytes = Bytes.create maxlen in
    let* written_bytes = fill_using bytes 0 maxlen in
    match written_bytes with
    | Error e -> Lwt.return_error e
    | Ok written_bytes -> Lwt.return_ok (Bytes.sub bytes 0 written_bytes)

  let read data ?(len = Bytes.length data) () ~into ~offset =
    let data_length = Bytes.length data in
    if len > data_length then
      raise (Invalid_argument "Circular_buffer.read: len > (length data).") ;
    Bytes.blit data 0 into offset len ;
    if len = data_length then None
    else Some (Bytes.sub data len (data_length - len))

  let length = Bytes.length
end

(* Check that the circular buffer as the expected interface *)
module Circular_buffer : S = Circular_buffer

(* A scenario will be generate as a sequence of write/read such that
   at each moment, there is more writes than reads. Details are made
   precise in the [pp_op] function below. *)
type op = Write of int * int | Read of int

let pp_op fmt = function
  | Write (write_len, len) ->
      Format.fprintf
        fmt
        "Write %d bytes into a buffer of maxlen %d bytes."
        (min write_len len)
        len
  | Read read_len ->
      (* if [read_len] is too long, we may truncate to the correct size
         depending on the test (see below). *)
      Format.fprintf fmt "Read at most %d bytes." read_len

let pp = Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_op

let write_op =
  let open QCheck2.Gen in
  map (fun (write_len, len) -> Write (write_len, len)) (pair uint8 uint8)

let read_op =
  let open QCheck2.Gen in
  map (fun len -> Read len) uint8

let op = QCheck2.Gen.oneof [write_op; read_op]

(* We record the number of writes to ensure the invariant [nb_writes >
   nb_reads]. *)
let rec ops_gen acc i =
  if i = 0 then acc
  else
    let open QCheck2.Gen in
    ops_gen
      (let* nb_writes, ops = acc in
       let gen = if nb_writes > 0 then op else write_op in
       map
         (fun op ->
           let delta = match op with Write _ -> 1 | Read _ -> -1 in
           (nb_writes + delta, op :: ops))
         gen)
      (i - 1)

(* Scenarios start with a write operation. *)
let ops_gen size =
  let open QCheck2.Gen in
  let gen = ops_gen (map (fun v -> (1, [v])) write_op) size in
  map (fun (_, ops) -> ops) gen

let values =
  let open QCheck2 in
  (* 1000 is a good trade-off between:
     - testing long scenarii using a long sequence of operations
     - quick execution
  *)
  let size_gen = Gen.int_range 0 1000 in
  Gen.(
    let* size = size_gen in
    ops_gen size)

let print_values = Format.asprintf "%a" pp

(* To generate random bytes in a buffer. *)
let random_bytes =
  let state = Random.State.make_self_init () in
  fun size ->
    let buff = Bytes.create size in
    let rec fill_random size offset buff =
      let data = Random.State.int64 state Int64.max_int in
      if size < 8 then
        for i = 0 to size - 1 do
          Bytes.set_int8
            buff
            (offset + i)
            (Int64.to_int (Int64.shift_right data i))
        done
      else (
        Bytes.set_int64_ne buff offset data ;
        fill_random (size - 8) (offset + 8) buff)
    in
    fill_random size 0 buff ;
    buff

let pp_buf fmt buf =
  Format.fprintf fmt "Length: %d@." (Bytes.length buf) ;
  Bytes.iter (fun c -> Format.fprintf fmt "%02x" (Char.code c)) buf

type state =
  | E : {
      implementation : (module S with type t = 'a and type data = 'b);
      internal_state : 'a;
      data_to_be_read : 'b Queue.t;
      mutable partial_read : 'b option;
    }
      -> state

let () =
  (* The module Circular buffer should have the same semantics as the
     reference implementation given in the Reference module. We use
     qcheck to generate write and reads, then check that both
     implementations send the same result. *)
  let fill_using write_len fresh_bytes bytes offset maxlen =
    let len = min write_len maxlen in
    Bytes.blit fresh_bytes 0 bytes offset len ;
    Lwt.return_ok len
  in
  let write_data write_len maxlen bytes_to_write (E state) =
    let (module M) = state.implementation in
    let* data =
      M.write
        ~maxlen
        ~fill_using:(fill_using write_len bytes_to_write)
        state.internal_state
    in
    match data with
    | Error _ -> failwith "read_invalid: fill_using_error"
    | Ok data ->
        Queue.add data state.data_to_be_read ;
        Lwt.return_unit
  in
  let read_data ~without_invalid_argument read_len (E state) =
    let (module M) = state.implementation in
    let data_to_read =
      match state.partial_read with
      | None -> Queue.take state.data_to_be_read
      | Some p ->
          state.partial_read <- None ;
          p
    in
    let len =
      (* to avoid the invalid_argument we take the minimum between the
         size of the data to read and the one generated by the
         [QCheck2] generator. *)
      if without_invalid_argument then min (M.length data_to_read) read_len
      else read_len
    in
    let buf = Bytes.create len in
    try
      state.partial_read <-
        M.read data_to_read ~len state.internal_state ~into:buf ~offset:0 ;
      (false, buf)
    with Invalid_argument _ -> (true, Bytes.create 0)
  in
  let update_state ?(without_invalid_argument = false) left_state right_state
      value =
    match value with
    | Write (write_len, maxlen) ->
        let len = min write_len maxlen in
        let bytes_to_write = random_bytes len in
        let* () = write_data write_len maxlen bytes_to_write left_state in
        let* () = write_data write_len maxlen bytes_to_write right_state in
        Lwt.return_false
    | Read read_len -> (
        try
          let left_has_raised, left_buf =
            read_data ~without_invalid_argument read_len left_state
          in
          let right_has_raised, right_buf =
            read_data ~without_invalid_argument read_len right_state
          in
          if left_has_raised then
            if right_has_raised then Lwt.return true
            else
              QCheck2.Test.fail_report "Different behaviors (invalid_argument)"
          else
            let _ =
              qcheck_eq' ~pp:pp_buf ~expected:left_buf ~actual:right_buf ()
            in
            Lwt.return_false
        with Queue.Empty -> QCheck2.assume_fail ())
  in
  let test_invalid_argument =
    QCheck2.Test.make
      ~name:
        "Stdlib.circular_buffer.equivalence-with-reference-implementation-without-invalid-argument"
      ~print:print_values
      values
      (fun ops ->
        (* To ensure that the number of [write] is greater than the
           number of [read] we reverse the list. *)
        let ops = List.rev ops in
        let left_state =
          E
            {
              implementation = (module Circular_buffer);
              internal_state = Circular_buffer.create ~maxlength:(1 lsl 10) ();
              data_to_be_read = Queue.create ();
              partial_read = None;
            }
        in
        let right_state =
          E
            {
              implementation = (module Reference);
              internal_state = Reference.create ~maxlength:(1 lsl 10) ();
              data_to_be_read = Queue.create ();
              partial_read = None;
            }
        in
        Lwt_main.run
          (Lwt_list.iter_s
             (fun value ->
               let* _ =
                 update_state
                   ~without_invalid_argument:true
                   left_state
                   right_state
                   value
               in
               Lwt.return_unit)
             ops) ;
        true)
  in
  let test_with_reference =
    (* The test below do not try to avoid the `invalid_argument`
       exception. It checks that both implementations raise this
       exception at the same time. *)
    QCheck2.Test.make
      ~name:"Stdlib.circular_buffer.equivalence-with-reference-implementation"
      ~print:print_values
      values
      (fun ops ->
        let ops = List.rev ops in
        let left_state =
          E
            {
              implementation = (module Circular_buffer);
              internal_state = Circular_buffer.create ();
              data_to_be_read = Queue.create ();
              partial_read = None;
            }
        in
        let right_state =
          E
            {
              implementation = (module Reference);
              internal_state = Reference.create ();
              data_to_be_read = Queue.create ();
              partial_read = None;
            }
        in
        let _ =
          Lwt_main.run
            (Lwt_list.fold_left_s
               (fun raised value ->
                 if raised then Lwt.return raised
                 else
                   let* raised' = update_state left_state right_state value in
                   Lwt.return (raised || raised'))
               false
               ops)
        in
        true)
  in
  Alcotest.run
    ~__FILE__
    "Stdlib.circular_buffer"
    [
      ("Invalid argument", qcheck_wrap [test_invalid_argument]);
      ("With reference", qcheck_wrap [test_with_reference]);
    ]
