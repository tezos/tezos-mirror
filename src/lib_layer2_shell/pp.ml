(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type PP = sig
  val pp : Format.formatter -> bytes -> unit
end

type t = (module PP)

let registered_pp : (string * t) list ref = ref []

let register_pp ~name pp =
  let pp : (module PP) =
    (module struct
      let pp = pp
    end)
  in
  registered_pp := (name, pp) :: !registered_pp

let of_string str = List.assoc_opt ~equal:String.equal str !registered_pp

let pp (module P : PP) = P.pp

let supported_pp () = List.map fst !registered_pp

let () =
  register_pp ~name:"hex" (fun fmt bytes ->
      let hex = Hex.of_bytes bytes in
      Hex.pp fmt hex) ;
  register_pp ~name:"ascii" (fun fmt bytes ->
      let ascii = String.of_bytes bytes in
      Format.pp_print_string fmt ascii)
