(* The MIT License

Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                   Thomas Gazagnaire <thomas@tarides.com>
                   Ioana Cristescu <ioana@tarides.com>
                   Clément Pascutto <clement@tarides.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software. *)

module type ELT = sig
  type t

  val encoded_size : int

  val decode : Bytes.t -> int -> t
end

module type S = sig
  include Search.ARRAY

  type io

  val v : io -> t
end

module Make (IO : Io.S) (Elt : ELT) :
  S with type io = IO.t and type elt = Elt.t = struct
  module Elt = struct
    include Elt

    let encoded_sizeL = Int64.of_int encoded_size
  end

  type io = IO.t

  type elt = Elt.t

  type buffer = { buf : bytes; low_off : int64; high_off : int64 }

  type t = { io : IO.t; mutable buffer : buffer option }

  let v io = { io; buffer = None }

  let get_entry_from_io io off =
    let buf = Bytes.create Elt.encoded_size in
    let n = IO.read io ~off ~len:Elt.encoded_size buf in
    assert (n = Elt.encoded_size);
    Elt.decode buf 0

  let ( -- ) = Int64.sub

  let get_entry_from_buffer buf off =
    let buf_off = Int64.(to_int @@ (off -- buf.low_off)) in
    assert (buf_off <= Bytes.length buf.buf);
    Elt.decode buf.buf buf_off

  let is_in_buffer t off =
    match t.buffer with
    | None -> false
    | Some b ->
        Int64.compare off b.low_off >= 0 && Int64.compare off b.high_off <= 0

  let get t i =
    let off = Int64.(mul i Elt.encoded_sizeL) in
    match t.buffer with
    | Some b when is_in_buffer t off -> (
        try get_entry_from_buffer b off with _ -> assert false )
    | _ -> get_entry_from_io t.io off

  let length t = Int64.(div (IO.offset t.io) Elt.encoded_sizeL)

  let max_buffer_size = 4096

  let buf = Bytes.create max_buffer_size

  let set_buffer t ~low ~high =
    let range = Elt.encoded_size * (1 + Int64.to_int (high -- low)) in
    let low_off = Int64.mul low Elt.encoded_sizeL in
    let high_off = Int64.mul high Elt.encoded_sizeL in
    let n = IO.read t.io ~off:low_off ~len:range buf in
    assert (n = range);
    t.buffer <- Some { buf; low_off; high_off }

  let pre_fetch t ~low ~high =
    let range = Elt.encoded_size * (1 + Int64.to_int (high -- low)) in
    if Int64.compare low high > 0 then
      Log.warn (fun m ->
          m "Requested pre-fetch region is empty: [%Ld, %Ld]" low high)
    else if range > max_buffer_size then
      Log.warn (fun m ->
          m "Requested pre-fetch [%Ld, %Ld] is larger than %d" low high
            max_buffer_size)
    else
      match t.buffer with
      | Some b ->
          let low_buf, high_buf =
            Int64.
              (div b.low_off Elt.encoded_sizeL, div b.high_off Elt.encoded_sizeL)
          in
          if low >= low_buf && high <= high_buf then
            Log.debug (fun m ->
                m
                  "Pre-existing buffer [%Ld, %Ld] encloses requested pre-fetch \
                   [%Ld, %Ld]"
                  low_buf high_buf low high)
          else (
            Log.warn (fun m ->
                m
                  "Current buffer [%Ld, %Ld] insufficient. Prefetching in \
                   range [%Ld, %Ld]"
                  low_buf high_buf low high);
            set_buffer t ~low ~high )
      | None ->
          Log.debug (fun m ->
              m "No existing buffer. Prefetching in range [%Ld, %Ld]" low high);
          set_buffer t ~low ~high
end
