(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type t = {
  staking_over_baking_limit_millionth : int32;
  baking_over_staking_edge_billionth : int32;
}

let maximum_baking_over_staking_edge_billionth =
  (* max is 1 (1_000_000_000 billionth) *)
  1_000_000_000l

let default =
  {
    staking_over_baking_limit_millionth = 0l;
    baking_over_staking_edge_billionth = 1_000_000_000l;
  }

type error += Invalid_staking_parameters

let () =
  register_error_kind
    `Permanent
    ~id:"operations.invalid_staking_parameters"
    ~title:"Invalid parameters for staking parameters"
    ~description:"The staking parameters are invalid."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid staking parameters")
    Data_encoding.empty
    (function Invalid_staking_parameters -> Some () | _ -> None)
    (fun () -> Invalid_staking_parameters)

let make ~staking_over_baking_limit_millionth
    ~baking_over_staking_edge_billionth =
  if
    Compare.Int32.(staking_over_baking_limit_millionth < 0l)
    || Compare.Int32.(baking_over_staking_edge_billionth < 0l)
    || Compare.Int32.(
         baking_over_staking_edge_billionth
         > maximum_baking_over_staking_edge_billionth)
  then Error ()
  else
    Ok {staking_over_baking_limit_millionth; baking_over_staking_edge_billionth}

let encoding =
  let open Data_encoding in
  conv_with_guard
    (fun {
           staking_over_baking_limit_millionth;
           baking_over_staking_edge_billionth;
         } ->
      (staking_over_baking_limit_millionth, baking_over_staking_edge_billionth))
    (fun ( staking_over_baking_limit_millionth,
           baking_over_staking_edge_billionth ) ->
      Result.map_error
        (fun () -> "Invalid staking parameters")
        (make
           ~staking_over_baking_limit_millionth
           ~baking_over_staking_edge_billionth))
    (obj2
       (req "staking_over_baking_limit_millionth" int32)
       (req "baking_over_staking_edge_billionth" int32))

let make ~staking_over_baking_limit_millionth
    ~baking_over_staking_edge_billionth =
  match
    make
      ~staking_over_baking_limit_millionth
      ~baking_over_staking_edge_billionth
  with
  | Error () -> Result_syntax.tzfail Invalid_staking_parameters
  | Ok _ as ok -> ok
