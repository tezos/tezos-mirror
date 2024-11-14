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

open! Import

module type ELT = sig
  type t

  val encoded_size : int
  val decode : string -> int -> t
end

module type S = sig
  include Search.ARRAY

  type io

  val v : io -> t
end

(** Takes an IO instance and wraps it in an Array interface with support for
    prefetching sections of the array. *)
module Make (IO : Io.S) (Elt : ELT) : S with type io = IO.t and type elt = Elt.t
