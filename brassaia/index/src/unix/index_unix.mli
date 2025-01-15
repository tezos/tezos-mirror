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
open Brassaia_index

module Make (K : Index.Key.S) (V : Index.Value.S) (C : Index.Cache.S) :
  Index.S with type key = K.t and type value = V.t

module Syscalls = Syscalls
(** Bindings to Unix system calls. *)

(** These modules should not be used. They are exposed purely for testing
    purposes. *)
module Private : sig
  module Platform : Brassaia_index.Platform.S
  module IO : Brassaia_index.Platform.IO
  module Raw = Raw

  module Make (K : Index.Key.S) (V : Index.Value.S) (C : Index.Cache.S) :
    Index.Private.S with type key = K.t and type value = V.t
end
