(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
  chain_name : Distributed_db_version.Name.t;
  distributed_db_version : Distributed_db_version.t;
  p2p_version : P2p_version.t;
}

let pp ppf {chain_name; distributed_db_version; p2p_version} =
  Format.fprintf
    ppf
    "%a.%a (p2p: %a)"
    Distributed_db_version.Name.pp
    chain_name
    Distributed_db_version.pp
    distributed_db_version
    P2p_version.pp
    p2p_version

let encoding =
  let open Data_encoding in
  def
    "network_version"
    ~description:
      "A version number for the network protocol (includes distributed DB \
       version and p2p version)"
  @@ conv
       (fun {chain_name; distributed_db_version; p2p_version} ->
         (chain_name, distributed_db_version, p2p_version))
       (fun (chain_name, distributed_db_version, p2p_version) ->
         {chain_name; distributed_db_version; p2p_version})
       (obj3
          (req "chain_name" Distributed_db_version.Name.encoding)
          (req "distributed_db_version" Distributed_db_version.encoding)
          (req "p2p_version" P2p_version.encoding))

let greatest = function
  | [] -> raise (Invalid_argument "Network_version.greatest")
  | h :: t -> List.fold_left max h t

let announced ~chain_name ~distributed_db_versions ~p2p_versions =
  assert (distributed_db_versions <> []) ;
  assert (p2p_versions <> []) ;
  {
    chain_name;
    distributed_db_version = greatest distributed_db_versions;
    p2p_version = greatest p2p_versions;
  }

let () = Data_encoding.Registration.register ~pp encoding

module Internal_for_tests = struct
  let mock () =
    {
      chain_name = Distributed_db_version.Name.of_string "";
      distributed_db_version = Distributed_db_version.zero;
      p2p_version = P2p_version.zero;
    }
end
