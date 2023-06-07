(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = {expected_env : env_version; components : component list}

and component = {
  name : string;
  interface : string option;
  implementation : string;
}

and env_version = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10

let compare_version = Stdlib.compare

include Compare.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
end)

let component_encoding =
  let open Data_encoding in
  conv
    (fun {name; interface; implementation} -> (name, interface, implementation))
    (fun (name, interface, implementation) -> {name; interface; implementation})
    (obj3
       (req "name" string)
       (opt "interface" string)
       (req "implementation" string))

let module_name_of_env_version = function
  | V0 -> "V0"
  | V1 -> "V1"
  | V2 -> "V2"
  | V3 -> "V3"
  | V4 -> "V4"
  | V5 -> "V5"
  | V6 -> "V6"
  | V7 -> "V7"
  | V8 -> "V8"
  | V9 -> "V9"
  | V10 -> "V10"

let env_version_encoding =
  let open Data_encoding in
  def "protocol.environment_version"
  @@ conv
       (function
         | V0 -> 0
         | V1 -> 1
         | V2 -> 2
         | V3 -> 3
         | V4 -> 4
         | V5 -> 5
         | V6 -> 6
         | V7 -> 7
         | V8 -> 8
         | V9 -> 9
         | V10 -> 10)
       (function
         | 0 -> V0
         | 1 -> V1
         | 2 -> V2
         | 3 -> V3
         | 4 -> V4
         | 5 -> V5
         | 6 -> V6
         | 7 -> V7
         | 8 -> V8
         | 9 -> V9
         | 10 -> V10
         | _ -> failwith "unexpected environment version")
       uint16

let encoding =
  let open Data_encoding in
  def
    "protocol"
    ~description:
      "The environment a protocol relies on and the components a protocol is \
       made of."
  @@ conv
       (fun {expected_env; components} -> (expected_env, components))
       (fun (expected_env, components) -> {expected_env; components})
       (obj2
          (req "expected_env_version" env_version_encoding)
          (req "components" (list component_encoding)))

let bounded_encoding ?max_size () =
  match max_size with
  | None -> encoding
  | Some max_size -> Data_encoding.check_size max_size encoding

let pp ppf op =
  Data_encoding.Json.pp ppf (Data_encoding.Json.construct encoding op)

let pp_ocaml_component ppf {name; interface; implementation} =
  Format.fprintf
    ppf
    "@[{@[<v 1> name = %S ;@ interface = %a ;@ implementation = %S ;@]@ }@]"
    name
    (fun ppf -> function
      | None -> Format.fprintf ppf "None"
      | Some s -> Format.fprintf ppf "Some %S" s)
    interface
    implementation

let pp_ocaml ppf {expected_env; components} =
  Format.fprintf
    ppf
    "@[{@[<v 1> expected_env = %s ;@ components = [@[<v>%a@]] ;@]@ }@]"
    (module_name_of_env_version expected_env)
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf " ;@ ")
       pp_ocaml_component)
    components

let to_bytes v = Data_encoding.Binary.to_bytes_exn encoding v

let of_bytes b = Data_encoding.Binary.of_bytes_opt encoding b

let of_string b = Data_encoding.Binary.of_string_opt encoding b

let of_bytes_exn b = Data_encoding.Binary.of_bytes_exn encoding b

let of_string_exn b = Data_encoding.Binary.of_string_exn encoding b

let hash proto = Tezos_crypto.Hashed.Protocol_hash.hash_bytes [to_bytes proto]

let hash_raw proto = Tezos_crypto.Hashed.Protocol_hash.hash_bytes [proto]

module Meta = struct
  type t = {
    hash : Tezos_crypto.Hashed.Protocol_hash.t option;
    expected_env_version : env_version option;
    modules : string list;
  }

  let encoding =
    let open Data_encoding in
    def
      "protocol.meta"
      ~description:
        "Protocol metadata: the hash of the protocol, the expected environment \
         version and the list of modules comprising the protocol."
    @@ conv
         (fun {hash; expected_env_version; modules} ->
           (hash, expected_env_version, modules))
         (fun (hash, expected_env_version, modules) ->
           {hash; expected_env_version; modules})
    @@ obj3
         (opt
            "hash"
            ~description:"Used to force the hash of the protocol"
            Tezos_crypto.Hashed.Protocol_hash.encoding)
         (opt "expected_env_version" env_version_encoding)
         (req
            "modules"
            ~description:"Modules comprising the protocol"
            (list string))
end

let () =
  Data_encoding.Registration.register ~pp:pp_ocaml encoding ;
  Data_encoding.Registration.register Meta.encoding
