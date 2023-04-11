(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Invocation:   dune exec src/lib_stdlib/test-unix/main.exe \
                  -- --file test_circular_buffer.ml
    Subject:      On circular buffer
*)

open Lwt.Syntax
module Buff = Circular_buffer

let buffer_size = ref (1 lsl 14)

let random_bytes size =
  let buff = Bytes.create size in
  let rec fill_random size offset buff =
    let data = Random.int64 Int64.max_int in
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

(** Tests with constant size allocation requests  *)
module Constant = struct
  (**{1} Helpers functions  *)

  (** Writes [chunks_count] chunks into the [circular_buffer] and
  returns an array  with the returned [data] witnesses.
  Data for chunk [i] are copied from the chunk in  [seed_chunks] at
  position [i] modulo the array size.

  Assumes that [seed_chunks] are bigger than [chunks_size]
    *)
  let fill_full seed_chunks circular_buffer chunks_size chunks_count =
    let seed_count = Array.length seed_chunks in
    let data_store =
      Array.init chunks_count (fun i ->
          let* res =
            Buff.write
              ~maxlen:chunks_size
              ~fill_using:(fun buff off maxlen ->
                Bytes.blit seed_chunks.(i mod seed_count) 0 buff off maxlen ;
                Lwt.return_ok maxlen)
              circular_buffer
          in
          match res with
          | Error _ -> failwith "fill_full: fill_using_error"
          | Ok res -> Lwt.return res)
    in
    data_store

  (** Same as [fill_full] but a only a random count of bytes are
      copied into the circular buffer, not the whole required size.  *)
  let fill_partial seed_chunks circular_buffer chunks_size chunks_count =
    let seed_count = Array.length seed_chunks in
    let data_store =
      Array.init chunks_count (fun i ->
          let* res =
            Buff.write
              ~maxlen:chunks_size
              ~fill_using:(fun buff off maxlen ->
                let length = Random.int maxlen in
                Bytes.blit seed_chunks.(i mod seed_count) 0 buff off length ;
                Lwt.return_ok length)
              circular_buffer
          in
          match res with
          | Error _ -> failwith "fill_partial: fill_using_error"
          | Ok res -> Lwt.return res)
    in
    data_store

  (** Reads each data chunk in one shot, and assert that they match
      the chunk at the expected position in seed_chunks.
   *)
  let read_all seed_chunks circular_buffer chunks_size data_store =
    let seed_count = Array.length seed_chunks in
    let buff = random_bytes chunks_size in
    Array.iteri
      (fun i data ->
        ignore
          (let* data in
           let _ = Buff.read data circular_buffer ~into:buff ~offset:0 in
           assert (
             Bytes.sub buff 0 (Buff.length data)
             = Bytes.sub seed_chunks.(i mod seed_count) 0 (Buff.length data)) ;
           Lwt.return_unit))
      data_store

  (** Reads each data chunk fragement by fragment (10 fragments max,
      each of random size), and assert that they match
      the chunk at the expected position in seed_chunks.
   *)
  let read_by_chunk seed_chunks circular_buffer chunks_size data_store =
    let seed_count = Array.length seed_chunks in
    let buff = random_bytes chunks_size in
    Array.iter
      (fun (i, data) ->
        ignore
          (let* data in
           let max_iter = 10 in
           let rec exhaust data offset iter =
             let length = Buff.length data in
             let len =
               if iter > max_iter then length
               else try Random.int length with _ -> length
               (* length might be 0 *)
             in
             let remainder =
               Buff.read ~len data circular_buffer ~into:buff ~offset
             in
             let offset = offset + len in
             match remainder with
             | None -> offset
             | Some remainder -> exhaust remainder offset (iter + 1)
           in
           let read_length = exhaust data 0 0 in
           assert (read_length = Buff.length data) ;
           assert (
             Bytes.sub buff 0 (Buff.length data)
             = Bytes.sub seed_chunks.(i mod seed_count) 0 (Buff.length data)) ;
           Lwt.return_unit))
      data_store

  (**{1} Tests  *)

  (** [run ~buffer_size ~chunk_size ~chunks_count ~iter] fills the
     buffer with ~chunks_count then read all the chunks and check that
     it retrieved the right data.

      Data are randoms bits.  *)
  let run ~buffer_size ~chunks_size ~chunks_count ~iter () =
    let seed_count = 20 in
    let seed_chunks =
      Array.init seed_count (fun _ -> random_bytes chunks_size)
    in
    let circular_buffer = Buff.create ~maxlength:buffer_size () in
    for _i = 0 to iter do
      Format.eprintf "iteration %d@." _i ;
      read_all
        seed_chunks
        circular_buffer
        chunks_size
        (fill_full seed_chunks circular_buffer chunks_size chunks_count)
    done ;
    Lwt.return_unit

  (** [run ~buffer_size ~chunk_size ~chunks_count ~iter] fills the
     buffer with ~chunks_count. The writing function does not always
     write ~chunk_size bytes.  Then all the chunks are fully read and
     checked to be the same as written data.

      Data are randoms bits.  *)
  let run_partial_write ~buffer_size ~chunks_size ~chunks_count ~iter () =
    let seed_count = 20 in
    let seed_chunks =
      Array.init seed_count (fun _ -> random_bytes chunks_size)
    in
    let circular_buffer = Buff.create ~maxlength:buffer_size () in
    for _i = 0 to iter do
      Format.eprintf "iteration %d@." _i ;
      read_all
        seed_chunks
        circular_buffer
        chunks_size
        (fill_partial seed_chunks circular_buffer chunks_size chunks_count)
    done ;
    Lwt.return_unit

  (** [run ~buffer_size ~chunk_size ~chunks_count ~iter] fills the
     buffer with ~chunks_count. The writing function does not always
     write ~chunk_size bytes.  Then all the chunks are read by smaller
     chunk until exhaustion and checked to be the same as written
     data.

      Data are randoms bits.  *)
  let run_partial_read_writes ~buffer_size ~chunks_size ~chunks_count ~iter () =
    let seed_count = 20 in
    let seed_chunks =
      Array.init seed_count (fun _ -> random_bytes chunks_size)
    in
    let circular_buffer = Buff.create ~maxlength:buffer_size () in
    for _i = 0 to iter do
      Format.eprintf "iteration %d@." _i ;
      read_by_chunk
        seed_chunks
        circular_buffer
        chunks_size
        (Array.mapi
           (fun i d -> (i, d))
           (fill_partial seed_chunks circular_buffer chunks_size chunks_count))
    done ;
    Lwt.return_unit

  (** [run_partial_read_writes_interleaving ~buffer_size ~chunk_size
     ~chunks_count ~iter] interleaves writes of chunks_count chunks
     and and reads (by chunks) of a subsets of already written data.
     The writing function does not always write ~chunk_size bytes.
     The chunks are read by smaller chunk until exhaustion
     and checked to be the same as written data.

      Data are randoms bits.  *)
  let run_partial_read_writes_interleaving ~buffer_size ~chunks_size
      ~chunks_count ~iter () =
    let seed_count = 20 in
    let seed_chunks =
      Array.init seed_count (fun _ -> random_bytes chunks_size)
    in
    let circular_buffer = Buff.create ~maxlength:buffer_size () in
    let data_store =
      Array.mapi
        (fun i d -> (i, d))
        (fill_partial seed_chunks circular_buffer chunks_size chunks_count)
    in
    let rec loop iter data_store =
      if iter <= 0 then ()
      else (
        Format.eprintf "iteration %d@." iter ;
        let length = Array.length data_store in
        let to_read = Random.int length in
        read_by_chunk
          seed_chunks
          circular_buffer
          chunks_size
          (Array.sub data_store 0 to_read) ;
        loop
          (iter - 1)
          (Array.append
             (Array.sub data_store to_read (length - to_read))
             (Array.mapi
                (fun i d -> (i, d))
                (fill_partial
                   seed_chunks
                   circular_buffer
                   chunks_size
                   chunks_count))))
    in
    loop iter data_store ;
    Lwt.return_unit
end

module Fail_Test = struct
  (** Expected fail on write.  *)
  let write_invalid ~max_buffer_len ~max_data_size ~actual_data_size =
    let circular_buffer = Buff.create ~maxlength:max_buffer_len () in
    let tmp_buff = Bytes.create (max max_data_size actual_data_size) in
    Lwt.catch
      (fun () ->
        let* _data =
          Buff.write
            ~maxlen:max_data_size
            ~fill_using:(fun buff off _maxlen ->
              Bytes.blit tmp_buff 0 buff off actual_data_size ;
              Lwt.return_ok actual_data_size)
            circular_buffer
        in
        assert false)
      (function Invalid_argument _ -> Lwt.return_unit | exn -> raise exn)

  (** Fail read too long.  *)
  let read_invalid ~max_buffer_len ~max_data_size ~actual_data_size =
    let circular_buffer = Buff.create ~maxlength:max_buffer_len () in
    let tmp_buff = Bytes.create (max max_data_size actual_data_size) in
    let* data =
      Buff.write
        ~maxlen:max_data_size
        ~fill_using:(fun buff off _maxlen ->
          Bytes.blit tmp_buff 0 buff off actual_data_size ;
          Lwt.return_ok actual_data_size)
        circular_buffer
    in
    match data with
    | Error _ -> failwith "read_invalid: fill_using_error"
    | Ok data -> (
        try
          let _ =
            Buff.read
              data
              ~len:(actual_data_size + 1)
              circular_buffer
              ~into:tmp_buff
              ~offset:0
          in
          assert false
        with Invalid_argument _ -> Lwt.return_unit)

  let run_write_too_long_in_buffer () =
    write_invalid ~max_buffer_len:10 ~max_data_size:5 ~actual_data_size:6

  let run_write_too_long_extra_alloc () =
    write_invalid ~max_buffer_len:4 ~max_data_size:5 ~actual_data_size:6

  let run_read_too_long_in_buffer () =
    let* () =
      read_invalid ~max_buffer_len:10 ~max_data_size:10 ~actual_data_size:5
    in
    read_invalid ~max_buffer_len:10 ~max_data_size:0 ~actual_data_size:0

  let run_read_too_long_extra_alloc () =
    let* () =
      read_invalid ~max_buffer_len:4 ~max_data_size:10 ~actual_data_size:5
    in
    read_invalid ~max_buffer_len:4 ~max_data_size:0 ~actual_data_size:0
end

let spec =
  Arg.[("--buffer-size", Set_int buffer_size, " Size of the read buffers")]

let () =
  let anon_fun _num_peers = raise (Arg.Bad "No anonymous argument.") in
  let usage_msg = "Usage: %s <num_peers>.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg

let wrap n f =
  Alcotest.test_case n `Quick (fun () ->
      match
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let* x = f () in
               Lwt.return @@ `Ok x)
             (fun exn -> Lwt.return @@ `Exn exn))
      with
      | `Ok _ -> ()
      | `Exn exn -> raise exn)

let () =
  Alcotest.run
    ~__FILE__
    "tezos-stdlib"
    (List.map
       (fun (run, descr) ->
         ( "circular_buffer.constant-chunks." ^ descr,
           [
             wrap "bad-fit" (fun () ->
                 let buffer_size = !buffer_size in
                 let chunks_size = (buffer_size / 13) + 1 in
                 let chunks_count = buffer_size / chunks_size in
                 run ~buffer_size ~chunks_size ~chunks_count ~iter:15 ());
             wrap "bad-fit-underflow" (fun () ->
                 let buffer_size = !buffer_size in
                 let chunks_size = (buffer_size / 13) + 1 in
                 let chunks_count = (buffer_size / chunks_size) - 1 in
                 run
                   ~buffer_size
                   ~chunks_size
                   ~chunks_count
                   ~iter:(chunks_count + 2)
                   ());
             wrap "bad-fit-overflow" (fun () ->
                 let buffer_size = !buffer_size in
                 let chunks_size = (buffer_size / 13) + 1 in
                 let chunks_count = (buffer_size / chunks_size) + 3 in
                 run ~buffer_size ~chunks_size ~chunks_count ~iter:15 ());
             wrap "perfect-fit" (fun () ->
                 let buffer_size = !buffer_size in
                 let chunks_size = buffer_size / (buffer_size lsr 3) in
                 let chunks_count = buffer_size / chunks_size in
                 run ~buffer_size ~chunks_size ~chunks_count ~iter:2 ());
             wrap "perfect-fit-underflow" (fun () ->
                 let buffer_size = !buffer_size in
                 let chunks_size = buffer_size / (buffer_size lsr 3) in
                 let chunks_count = (buffer_size / chunks_size) - 1 in
                 run
                   ~buffer_size
                   ~chunks_size
                   ~chunks_count
                   ~iter:(chunks_count + 2)
                   ());
             wrap "perfect-fit-overflow" (fun () ->
                 let buffer_size = !buffer_size in
                 let chunks_size = buffer_size / (buffer_size lsr 3) in
                 let chunks_count = (buffer_size / chunks_size) + 2 in
                 run
                   ~buffer_size
                   ~chunks_size
                   ~chunks_count
                   ~iter:((1 lsl 3) + 1)
                   ());
           ] ))
       [
         (Constant.run, "full");
         (Constant.run_partial_write, "partial-write");
         (Constant.run_partial_read_writes, "partial-read-write");
         ( Constant.run_partial_read_writes_interleaving,
           "partial-read-write-interleaving" );
       ]
    @ [
        ( "circular_buffer.fail_cases",
          [
            wrap
              "Write_too_long-in_buffer"
              Fail_Test.run_write_too_long_in_buffer;
            wrap
              "Write_too_long-extra_allocated_buffer"
              Fail_Test.run_write_too_long_extra_alloc;
            wrap "Read_too_long-in_buffer" Fail_Test.run_read_too_long_in_buffer;
            wrap
              "Read_too_long-extra_allocated_buffer"
              Fail_Test.run_read_too_long_extra_alloc;
          ] );
      ])
