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

module type ARRAY = sig
  type t
  type elt

  val get : t -> int63 -> elt
  val length : t -> int63
  val pre_fetch : t -> low:int63 -> high:int63 -> unit
end

module type ENTRY = sig
  type t

  module Key : sig
    type t

    val equal : t -> t -> bool
  end

  module Value : sig
    type t
  end

  val to_key : t -> Key.t
  val to_value : t -> Value.t
end

module type METRIC = sig
  type t

  module Entry : ENTRY

  val compare : t -> t -> int
  val of_entry : Entry.t -> t
  val of_key : Entry.Key.t -> t
  val linear_interpolate : low:int63 * t -> high:int63 * t -> t -> int63
end

module type S = sig
  module Entry : ENTRY
  module Array : ARRAY with type elt = Entry.t

  val interpolation_search :
    Array.t -> Entry.Key.t -> low:int63 -> high:int63 -> Entry.Value.t
end

module type Search = sig
  module type ARRAY = ARRAY
  module type ENTRY = ENTRY
  module type METRIC = METRIC
  module type S = S

  module Make
      (Entry : ENTRY)
      (Array : ARRAY with type elt = Entry.t)
      (Metric : METRIC with module Entry := Entry) :
    S with module Entry := Entry and module Array := Array
end
