(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make
    (Monad : Traced_sigs.Monad.S)
    (Seq : Traced_sigs.Seq.S with type 'error trace := 'error Monad.trace) =
struct
  let hash = Stdlib.Hashtbl.hash

  let seeded_hash = Stdlib.Hashtbl.seeded_hash

  let hash_param ~meaningful ~total v =
    Stdlib.Hashtbl.hash_param meaningful total v

  let seeded_hash_param ~meaningful ~total seed v =
    Stdlib.Hashtbl.seeded_hash_param meaningful total seed v

  module type S =
    Traced_functor_outputs.Hashtbl.S
      with type 'error trace := 'error Monad.trace

  module Make (H : Stdlib.Hashtbl.HashedType) : S with type key = H.t = struct
    include Bare_structs.Hashtbl.Make (H)

    let iter_ep f t = Seq.iter_ep (fun (k, v) -> f k v) (to_seq t)
  end

  module type SeededS =
    Traced_functor_outputs.Hashtbl.SeededS
      with type 'error trace := 'error Monad.trace

  module MakeSeeded (H : Stdlib.Hashtbl.SeededHashedType) :
    SeededS with type key = H.t = struct
    include Bare_structs.Hashtbl.MakeSeeded (H)

    let iter_ep f t = Seq.iter_ep (fun (k, v) -> f k v) (to_seq t)
  end

  module type S_ES =
    Traced_functor_outputs.Hashtbl.S_ES
      with type 'error trace := 'error Monad.trace

  module Make_es (H : Stdlib.Hashtbl.HashedType) : S_ES with type key = H.t =
  struct
    include Bare_structs.Hashtbl.Make_es (H)

    let iter_with_waiting_ep f t =
      let open Monad.Lwt_traced_result_syntax in
      join
      @@ fold_promises
           (fun k p acc ->
             let promise =
               Lwt.try_bind
                 (fun () -> p)
                 (function Error _ -> return_unit | Ok v -> f k v)
                 (fun _ -> return_unit)
             in
             promise :: acc)
           t
           []
  end
end
