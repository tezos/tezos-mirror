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

type t

val equal : t -> t -> bool
(** The equality function for fan-out. *)

val v : hash_size:int -> entry_size:int -> int -> t
(** [v ~hash_size ~entry_size n] creates a fan_out for an index with [hash_size]
    and [entry_size], containing [n] elements. *)

val nb_fans : t -> int
(** [nb_fans t] is the number of fans in [t]. *)

val search : t -> int -> int64 * int64
(** [search t hash] is the interval of offsets containing [hash], if present. *)

val update : t -> int -> int64 -> unit
(** [update t hash off] updates [t] so that [hash] is registered to be at offset
    [off]. *)

val finalize : t -> unit
(** Finalizes the update of the fanout. This is mendatory before any [search]
    query. *)

val exported_size : t -> int
(** [exported_size t] is the size of [export t]. This does not actually compute
    the encoding of [t]. *)

val export : t -> string
(** [export t] is a string encoded form of [t]. *)

val import : hash_size:int -> string -> t
(** [import ~hash_size buf] decodes [buf] such that
    [import ~hash_size (export t) = t] if [t] was initially created with
    ~hash_size. *)
