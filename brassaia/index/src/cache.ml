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

module type S = sig
  type ('k, 'v) t
  (** A cache of values of type ['v], indexed by keys of type ['k]. *)

  val create : unit -> (_, _) t
  val add : ('k, 'v) t -> 'k -> 'v -> unit
  val find : ('k, 'v) t -> 'k -> 'v option
  val remove : ('k, _) t -> 'k -> unit
end

(** Cache implementation that always misses. *)
module Noop : S = struct
  type (_, _) t = unit

  let create () = ()
  let add () _ _ = ()
  let find () _ = None
  let remove () _ = ()
end

(** Cache implementation that always finds previously-added values, and grows
    indefinitely. *)
module Unbounded : S = struct
  include Hashtbl

  let create () = create 0
  let find = find_opt
end
