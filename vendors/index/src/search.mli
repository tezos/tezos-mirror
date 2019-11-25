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

module type ARRAY = sig
  type t

  type elt

  val get : t -> int64 -> elt

  val length : t -> int64

  val pre_fetch : t -> low:int64 -> high:int64 -> unit
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

  val linear_interpolate : low:int64 * t -> high:int64 * t -> t -> int64
end

module type S = sig
  module Entry : ENTRY

  module Array : ARRAY with type elt = Entry.t

  val interpolation_search :
    Array.t -> Entry.Key.t -> low:int64 -> high:int64 -> Entry.Value.t
end

module Make
    (Entry : ENTRY)
    (Array : ARRAY with type elt = Entry.t)
    (Metric : METRIC with module Entry := Entry) :
  S with module Entry := Entry and module Array := Array
