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

(** Index

    [Index] is a scalable implementation of persistent indices in OCaml.

    [Index] provides the standard key-value interface: [find], [mem] and
    [replace]. It requires three IO instances:

    - A `log` IO containing all of the recently-added bindings; this is also
      kept in memory.

    - When the `log` IO is full, it is merged into the `index` IO. Search is
      done first in `log` then in `index`, which makes recently added bindings
      search faster.

    - A `lock` IO to ensure safe concurrent access. *)

include Index_intf.Index
(** @inline *)
