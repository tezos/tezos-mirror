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

(* Metrics must be
    - totally ordered
    - computable from entries and (potentially redundantly) from keys
    - linearly interpolate-able on the int63 type *)

include Search_intf
open! Import

module Make
    (Entry : ENTRY)
    (Array : ARRAY with type elt = Entry.t)
    (Metric : METRIC with module Entry := Entry) :
  S with module Entry := Entry and module Array := Array = struct
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
    let rec search (op : int63 -> int63) curr =
      let i = op curr in
      if i < Int63.zero || i >= Array.length array then raise Not_found
      else
        let e = array.(i) in
        let e_metric = Metric.of_entry e in
        if not Metric.(key_metric = e_metric) then raise Not_found
        else if Key.equal (Entry.to_key e) key then Entry.to_value e
        else (search [@tailcall]) op i
    in
    try search Int63.pred index
    with Not_found -> (search [@tailcall]) Int63.succ index

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
                  Int63.(succ next_index)
                  high
                  (lazy array.(Int63.(succ next_index)))
                  (Lazy.from_val highest_entry)
              else
                (search [@tailcall]) low (Int63.pred next_index)
                  (Lazy.from_val lowest_entry)
                  (lazy array.(Int63.(pred next_index))))
    in
    if high < Int63.zero then raise Not_found
    else (search [@tailcall]) low high (lazy array.(low)) (lazy array.(high))
end
