(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

include Stdlib.Array

let get = `You_cannot_access_array_content_in_the_protocol

let unsafe_get = `You_cannot_access_array_content_in_the_protocol

let set = `You_cannot_modify_array_content_in_the_protocol

let unsafe_set = `You_cannot_modify_array_content_in_the_protocol

let to_seq = `You_cannot_traverse_arrays_lazily_in_the_protocol

let to_seqi = `You_cannot_traverse_arrays_lazily_in_the_protocol

let make = `You_cannot_build_arrays_with_implicit_sharing_in_the_protocol

let create = `You_cannot_build_arrays_with_implicit_sharing_in_the_protocol

let make_matrix = `You_cannot_build_arrays_with_implicit_sharing_in_the_protocol

let create_float = `You_cannot_use_floats_in_the_protocol

let make_float = `You_cannot_use_floats_in_the_protocol

let sub = `You_cannot_cut_arrays_in_the_protocol

let fill = `You_cannot_fill_arrays_in_the_protocol

let blit = `You_cannot_blit_arrays_in_the_protocol

let iter2 = `You_cannot_traverse_2_arrays_at_once_in_the_protocol

let map2 = `You_cannot_traverse_2_arrays_at_once_in_the_protocol

let combine = `You_cannot_traverse_2_arrays_at_once_in_the_protocol

let sort = `You_cannot_sort_arrays_in_the_protocol

let stable_sort = `You_cannot_sort_arrays_in_the_protocol

let fast_sort = `You_cannot_sort_arrays_in_the_protocol

module Floatarray = struct end
