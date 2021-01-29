(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Sigs = struct
  module Monad = Bare_sigs.Monad
  module Hashtbl = Bare_sigs.Hashtbl
  module List = Bare_sigs.List
  module Map = Bare_sigs.Map
  module Option = Bare_sigs.Option
  module Result = Bare_sigs.Result
  module Seq = Bare_sigs.Seq
  module Set = Bare_sigs.Set
  module WithExceptions = Bare_sigs.WithExceptions
end

module Structs = struct
  module Monad = Bare_structs.Monad
  module Hashtbl = Bare_structs.Hashtbl
  module List = Bare_structs.List
  module Map = Bare_structs.Map
  module Option = Bare_structs.Option
  module Result = Bare_structs.Result
  module Seq = Bare_structs.Seq
  module Set = Bare_structs.Set
  module WithExceptions = Bare_structs.WithExceptions
end
