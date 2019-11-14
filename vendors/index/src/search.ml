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

(* Metrics must be
    - totally ordered
    - computable from entries and (potentially redundantly) from keys
    - linearly interpolate-able on the int64 type *)
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
  S with module Entry := Entry and module Array := Array = struct
  module Entry = Entry
  module Array = Array
  module Value = Entry.Value

  module Key = struct
    include Entry.Key

    let ( = ) a b = compare a b = 0
  end

  module Metric = struct
    include Metric

    let ( < ) a b = compare a b < 0

    let ( = ) a b = compare a b = 0

    let ( > ) a b = compare a b > 0
  end

  let look_around array key key_metric index =
    let rec search (op : int64 -> int64) curr =
      let i = op curr in
      if i < 0L || i >= Array.length array then raise Not_found
      else
        let e = array.(i) in
        let e_metric = Metric.of_entry e in
        if not Metric.(key_metric = e_metric) then raise Not_found
        else if Key.equal (Entry.to_key e) key then Entry.to_value e
        else (search [@tailcall]) op i
    in
    try search Int64.pred index
    with Not_found -> (search [@tailcall]) Int64.succ index

  (** Improves over binary search in cases where the values in some array are
      uniformly distributed according to some metric (such as a hash). *)
  let interpolation_search array key ~low ~high =
    let key_metric = Metric.of_key key in
    (* The core of the search *)
    let rec search low high lowest_entry highest_entry =
      if high < low then raise Not_found
      else (
        Array.pre_fetch array ~low ~high;
        let lowest_entry = Lazy.force lowest_entry in
        if high = low then
          if Key.(key = Entry.to_key lowest_entry) then
            Entry.to_value lowest_entry
          else raise Not_found
        else
          let lowest_metric = Metric.of_entry lowest_entry in
          if Metric.(lowest_metric > key_metric) then raise Not_found
          else
            let highest_entry = Lazy.force highest_entry in
            let highest_metric = Metric.of_entry highest_entry in
            if Metric.(highest_metric < key_metric) then raise Not_found
            else
              let next_index =
                Metric.linear_interpolate ~low:(low, lowest_metric)
                  ~high:(high, highest_metric) key_metric
              in
              let e = array.(next_index) in
              let e_metric = Metric.of_entry e in
              if Metric.(key_metric = e_metric) then
                if Key.(key = Entry.to_key e) then Entry.to_value e
                else look_around array key key_metric next_index
              else if Metric.(key_metric > e_metric) then
                (search [@tailcall])
                  Int64.(succ next_index)
                  high
                  (lazy array.(Int64.(succ next_index)))
                  (Lazy.from_val highest_entry)
              else
                (search [@tailcall]) low (Int64.pred next_index)
                  (Lazy.from_val lowest_entry)
                  (lazy array.(Int64.(pred next_index))) )
    in
    if high < 0L then raise Not_found
    else (search [@tailcall]) low high (lazy array.(low)) (lazy array.(high))
end
