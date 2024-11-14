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

include Io_intf
open! Import

module Extend (S : S) = struct
  include S

  let iter ~page_size ?min:(min_off = Int63.zero) ?max:max_off f io =
    let max_off = match max_off with None -> offset io | Some m -> m in
    let rec aux offset =
      let remaining = Int63.sub max_off offset in
      if remaining <= Int63.zero then ()
      else
        let len = Int63.to_int_exn (min remaining page_size) in
        let raw = Bytes.create len in
        let n = read io ~off:offset ~len raw in
        let rec read_page page off =
          if off = n then ()
          else
            let read =
              f ~off:Int63.(add (of_int off) offset) ~buf:page ~buf_off:off
            in
            (read_page [@tailcall]) page (off + read)
        in
        read_page (Bytes.unsafe_to_string raw) 0;
        (aux [@tailcall]) Int63.(add offset page_size)
    in
    (aux [@tailcall]) min_off
end
