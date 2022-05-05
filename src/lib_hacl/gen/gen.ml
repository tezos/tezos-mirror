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

(* This file implements the strict minimum to geneate hacl-star
   bindings automatically based on ctypes bindings and the api.json
   file from the hacl-star project.

   All unsuported features will fail with [assert false] and should be
   added as needed. *)

module String_set = Set.Make (String)

module Entry : sig
  type t =
    | Proxy of {arity : int; ctypes_name : string; name : string}
    | From_spec of {
        ctypes_name : string;
        name : string;
        unprefixed_alias : bool;
        spec : Api_json.t;
      }
    | Constant of {ctypes_name : string; value : string}
    | Unimplemented of {arity : int; ctypes_name : string; name : string}
    | Error of {ctypes_name : string; name : string}

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    | Proxy of {arity : int; ctypes_name : string; name : string}
    | From_spec of {
        ctypes_name : string;
        name : string;
        unprefixed_alias : bool;
        spec : Api_json.t;
      }
    | Constant of {ctypes_name : string; value : string}
    | Unimplemented of {arity : int; ctypes_name : string; name : string}
    | Error of {ctypes_name : string; name : string}

  let compare a b =
    match (a, b) with
    | (Error {name = x; _}, Error {name = y; _}) -> compare x y
    | (From_spec {name = x; _}, From_spec {name = y; _}) -> compare x y
    | (Proxy {name = x; _}, Proxy {name = y; _}) -> compare x y
    | (Unimplemented {name = x; _}, Unimplemented {name = y; _}) -> compare x y
    | (a, b) -> compare a b

  let size_to_js s args =
    match s with
    | None -> "null"
    | Some (`Absolute i) -> string_of_int i
    | Some (`Relative (s, 0)) -> (
        let a = List.find (fun u -> u.Api_json.name = s) args in
        match a.typ with
        | Uint32 -> Printf.sprintf "integers_int32_of_uint32(%s)" a.name
        | Uint8 -> Printf.sprintf "%s" a.name
        | Void | Buffer | Int | Bool -> assert false)
    | Some (`Relative (s, i)) -> (
        let a = List.find (fun u -> u.Api_json.name = s) args in
        match a.typ with
        | Uint32 -> Printf.sprintf "integers_int32_of_uint32(%s)%+d" a.name i
        | Uint8 -> Printf.sprintf "%s%+d" a.name i
        | Void | Buffer | Int | Bool -> assert false)

  let pp fmt t =
    let f s = Format.fprintf fmt s in
    let gen_args arity =
      String.concat ", " (List.init arity (fun i -> Printf.sprintf "x%d" i))
    in
    let max_arity = 5 in
    let gen_byte_ arity ctypes_name =
      if arity > max_arity then (
        let args = gen_args arity in
        f "//Provides: %s_byte%d@." ctypes_name arity ;
        f "//Requires: %s@." ctypes_name ;
        f "function %s_byte%d (%s) {@." ctypes_name arity args ;
        f "  return %s(%s);@." ctypes_name args ;
        f "}@.@.")
    in
    match t with
    | Proxy {arity; ctypes_name; name} ->
        (* Manually written stubs, we just need to proxy from the ctypes_name to the real name *)
        let args = gen_args arity in
        f "//Provides: %s@." ctypes_name ;
        f "//Requires: %s@." name ;
        f "function %s (%s) {@." ctypes_name args ;
        f "  return %s(%s);@." name args ;
        f "}@.@." ;
        gen_byte_ arity ctypes_name
    | From_spec {ctypes_name; name; spec; unprefixed_alias} ->
        let vars = List.map (fun x -> x.Api_json.name) spec.args in
        let vars = String.concat ", " vars in
        if unprefixed_alias then (
          (* We still expose an unprefixed version because hand-written
             stubs depend on it *)
          f "//Provides: %s@." name ;
          f "//Requires: %s@." ctypes_name ;
          f "var %s = %s@.@." name ctypes_name) ;
        (* Geneate the binding *)
        f "//Provides: %s@." ctypes_name ;
        f "//Requires: hacl_create_buffer, hacl_blit_buf_to_bytes@." ;
        f "//Requires: integers_int32_of_uint32, integers_uint32_of_int32@." ;
        f "function %s (%s) {@." ctypes_name vars ;
        f "  var H = globalThis._HACL;@." ;
        (* We convert first convert inputs *)
        List.iter
          (fun (v : Api_json.arg) ->
            match (v.index, v.typ) with
            | (None, _) -> ()
            | (Some _i, Buffer) ->
                f
                  "  var a_%s = hacl_create_buffer(%s,%s)@."
                  v.name
                  v.name
                  (size_to_js v.size spec.args)
            | (Some _i, Uint32) ->
                f "  var i_%s = integers_int32_of_uint32(%s)@." v.name v.name
            | (Some _, Uint8) -> ()
            | _ -> assert false)
          spec.args ;
        (* Call the underlying api *)
        let () =
          let args =
            List.filter_map
              (function
                | {Api_json.index = None; _} -> None
                | {index = Some _; typ = Buffer; name; _} -> Some ("a_" ^ name)
                | {index = Some _; typ = Uint32; name; _} -> Some ("i_" ^ name)
                | {index = Some _; typ = Uint8; name; _} -> Some name
                | {index = Some _; _} -> assert false)
              spec.args
          in
          f
            "  var ret = H.%s.%s(%s);@."
            spec.js_mod_name
            spec.js_fun_name
            (String.concat ", " args)
        in
        (* blit results to output buffers *)
        let () =
          let pos = ref (if spec.return = Void then 0 else 1) in
          List.iter
            (fun (v : Api_json.arg) ->
              match v.kind with
              | `Input -> ()
              | `Output ->
                  f
                    "  hacl_blit_buf_to_bytes(ret[%d], %s, %s)@."
                    !pos
                    v.name
                    (size_to_js v.size spec.args) ;
                  incr pos)
            spec.args
        in
        (* return results *)
        let () =
          match spec.return with
          | Void -> f "  return 0;@."
          | Uint32 -> f "  return integers_uint32_of_int32(ret[0]);@."
          | Uint8 -> f "  return ret[0];@."
          | Bool -> f "  return (ret[0]?1:0);@."
          | Int -> assert false
          | Buffer -> assert false
        in
        f "}@." ;
        gen_byte_ (List.length spec.args) ctypes_name
    | Constant {ctypes_name; value} ->
        f "//Provides: %s@." ctypes_name ;
        f "//Requires: integers_uint32_of_int32@." ;
        f "function %s(_unit) { return %s }@.@." ctypes_name value
    | Unimplemented {arity; ctypes_name; name} ->
        let args = gen_args arity in
        f "//Provides: %s@." ctypes_name ;
        f "//Requires: caml_failwith@." ;
        f "function %s (%s) {@." ctypes_name args ;
        f "  caml_failwith('%s unimplemetned');@." name ;
        f "}@.@." ;
        gen_byte_ arity ctypes_name
    | Error {ctypes_name; name} ->
        Printf.eprintf
          "%s was manually provided but one should provide %s instead@."
          ctypes_name
          name
end

let rec compute_arity : 'a. 'a Ctypes_static.fn -> int =
  fun (type a) (t : a Ctypes_static.fn) ->
   match t with
   | Ctypes_static.Returns _ -> 0
   | Function (_, x) -> 1 + compute_arity x

let unify_type (type a) (typ : a Ctypes_static.typ) (api : Api_json.typ) :
    Api_json.typ =
  match (typ, api) with
  | (Void, Void) -> Void
  | (Primitive Uint32_t, Int) -> Uint32
  | (Primitive Uint8_t, Int) -> Uint8
  | (Primitive Bool, Bool) -> Bool
  | (OCaml Bytes, Buffer) -> Buffer
  | (Void, _) -> assert false
  | (Primitive Char, _) -> assert false
  | (Primitive Schar, _) -> assert false
  | (Primitive Uchar, _) -> assert false
  | (Primitive Bool, _) -> assert false
  | (Primitive Short, _) -> assert false
  | (Primitive Int, _) -> assert false
  | (Primitive Long, _) -> assert false
  | (Primitive Llong, _) -> assert false
  | (Primitive Ushort, _) -> assert false
  | (Primitive Sint, _) -> assert false
  | (Primitive Uint, _) -> assert false
  | (Primitive Ulong, _) -> assert false
  | (Primitive Ullong, _) -> assert false
  | (Primitive Size_t, _) -> assert false
  | (Primitive Int8_t, _) -> assert false
  | (Primitive Int16_t, _) -> assert false
  | (Primitive Int32_t, _) -> assert false
  | (Primitive Int64_t, _) -> assert false
  | (Primitive Uint8_t, _) -> assert false
  | (Primitive Uint16_t, _) -> assert false
  | (Primitive Uint32_t, _) -> assert false
  | (Primitive Uint64_t, _) -> assert false
  | (Primitive Camlint, _) -> assert false
  | (Primitive Nativeint, _) -> assert false
  | (Primitive Float, _) -> assert false
  | (Primitive Double, _) -> assert false
  | (Primitive LDouble, _) -> assert false
  | (Primitive Complex32, _) -> assert false
  | (Primitive Complex64, _) -> assert false
  | (Primitive Complexld, _) -> assert false
  | (Pointer _t, _) -> assert false
  | (Funptr _fn, _) -> assert false
  | (Struct _, _) -> assert false
  | (Union _, _) -> assert false
  | (Abstract _, _) -> assert false
  | (View _, _) -> assert false
  | (Array _, _) -> assert false
  | (Bigarray _, _) -> assert false
  | (OCaml String, _) -> assert false
  | (OCaml Bytes, _) -> assert false
  | (OCaml FloatArray, _) -> assert false

let rec unify_types :
          'a.
          Api_json.arg list ->
          'a Ctypes_static.fn ->
          Api_json.arg list ->
          Api_json.typ ->
          Api_json.arg list * Api_json.typ =
  fun (type a) acc (t : a Ctypes_static.fn) args return ->
   match (t, args) with
   | (Ctypes_static.Returns t, []) -> (List.rev acc, unify_type t return)
   | (Ctypes_static.Returns _, _) -> assert false
   | (Function (t, x), a :: args) ->
       let typ = unify_type t a.Api_json.typ in
       unify_types ({a with typ} :: acc) x args return
   | (Function _, []) -> assert false

let gen_fn ~api ~manually_implemented ~required ~name ~ctypes_name add fn : unit
    =
  let unit_to_bool (type a) (fn : a Ctypes_static.fn) b =
    match fn with
    | Ctypes_static.Function (Void, Returns (Primitive Bool)) ->
        add (Entry.Constant {ctypes_name; value = (if b then "1" else "0")})
    | _ -> assert false
  in
  let unit_to_uint32 (type a) (fn : a Ctypes_static.fn) v =
    match fn with
    | Ctypes_static.Function (Void, Returns (Primitive Uint32_t)) ->
        add
          (Entry.Constant
             {
               ctypes_name;
               value = Printf.sprintf "integers_uint32_of_int32(%d)" v;
             })
    | _ -> assert false
  in
  match name with
  | "EverCrypt_AutoConfig2_wants_hacl" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_shaext" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_aesni" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_pclmulqdq" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_avx2" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_avx" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_bmi2" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_adx" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_sse" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_movbe" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_rdrand" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_avx512" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_wants_vale" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_wants_openssl" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_wants_bcrypt" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_vec128" -> unit_to_bool fn false
  | "EverCrypt_AutoConfig2_has_vec256" -> unit_to_bool fn false
  (* EverCrypt_StaticConfig_bindings.ml *)
  | "EverCrypt_StaticConfig_hacl" -> unit_to_bool fn false
  | "EverCrypt_StaticConfig_vale" -> unit_to_bool fn false
  | "EverCrypt_StaticConfig_openssl" -> unit_to_bool fn false
  | "EverCrypt_StaticConfig_bcrypt" -> unit_to_bool fn false
  (* EverCrypt_DRBG_bindings.ml *)
  | "EverCrypt_DRBG_reseed_interval" -> unit_to_uint32 fn 1024
  | "EverCrypt_DRBG_max_output_length" -> unit_to_uint32 fn 65536
  | "EverCrypt_DRBG_max_length" -> unit_to_uint32 fn 65536
  | "EverCrypt_DRBG_max_personalization_string_length" ->
      unit_to_uint32 fn 65536
  | "EverCrypt_DRBG_max_additional_input_length" -> unit_to_uint32 fn 65536
  (* Hacl_Poly1305_N_bindings.ml *)
  | "Hacl_Poly1305_32_blocklen" -> unit_to_uint32 fn 16
  | "Hacl_Poly1305_128_blocklen" -> unit_to_uint32 fn 16
  | "Hacl_Poly1305_256_blocklen" -> unit_to_uint32 fn 16
  (* Hacl_HMAC_DRBG_bindings.ml *)
  | "Hacl_HMAC_DRBG_reseed_interval" -> unit_to_uint32 fn 1024
  | "Hacl_HMAC_DRBG_max_output_length" -> unit_to_uint32 fn 65536
  | "Hacl_HMAC_DRBG_max_length" -> unit_to_uint32 fn 65536
  | "Hacl_HMAC_DRBG_max_personalization_string_length" ->
      unit_to_uint32 fn 65536
  | "Hacl_HMAC_DRBG_max_additional_input_length" -> unit_to_uint32 fn 65536
  (* Hacl_Frodo64_bindings.ml *)
  | "Hacl_Frodo64_crypto_bytes" -> unit_to_uint32 fn 16
  | "Hacl_Frodo64_crypto_publickeybytes" -> unit_to_uint32 fn 976
  | "Hacl_Frodo64_crypto_secretkeybytes" -> unit_to_uint32 fn 2032
  | "Hacl_Frodo64_crypto_ciphertextbytes" -> unit_to_uint32 fn 1080
  (* Hacl_Frodo640_bindings.ml *)
  | "Hacl_Frodo640_crypto_bytes" -> unit_to_uint32 fn 16
  | "Hacl_Frodo640_crypto_publickeybytes" -> unit_to_uint32 fn 9616
  | "Hacl_Frodo640_crypto_secretkeybytes" -> unit_to_uint32 fn 19888
  | "Hacl_Frodo640_crypto_ciphertextbytes" -> unit_to_uint32 fn 9720
  (* Hacl_Frodo976_bindings.ml *)
  | "Hacl_Frodo976_crypto_bytes" -> unit_to_uint32 fn 24
  | "Hacl_Frodo976_crypto_publickeybytes" -> unit_to_uint32 fn 15632
  | "Hacl_Frodo976_crypto_secretkeybytes" -> unit_to_uint32 fn 31296
  | "Hacl_Frodo976_crypto_ciphertextbytes" -> unit_to_uint32 fn 15744
  (* Hacl_Frodo1344_bindings.ml *)
  | "Hacl_Frodo1344_crypto_bytes" -> unit_to_uint32 fn 32
  | "Hacl_Frodo1344_crypto_publickeybytes" -> unit_to_uint32 fn 21520
  | "Hacl_Frodo1344_crypto_secretkeybytes" -> unit_to_uint32 fn 43088
  | "Hacl_Frodo1344_crypto_ciphertextbytes" -> unit_to_uint32 fn 21632
  | _ -> (
      let arity = compute_arity fn in
      if String_set.mem name manually_implemented then
        add (Entry.Proxy {arity; ctypes_name; name})
      else if String_set.mem ctypes_name manually_implemented then
        add (Entry.Error {ctypes_name; name})
      else
        try
          let api_spec =
            List.find (fun api -> name = api.Api_json.wasm_fun_name) api
          in
          let (args, return) =
            unify_types [] fn api_spec.args api_spec.return
          in
          let unprefixed_alias = String_set.mem name required in
          add
            (Entry.From_spec
               {
                 ctypes_name;
                 name;
                 unprefixed_alias;
                 spec = {api_spec with args; return};
               })
        with Not_found -> add (Entry.Unimplemented {arity; ctypes_name; name}))

let gen_value ~api ~manually_implemented ~required ~name ~ctypes_name add typ =
  gen_fn
    ~api
    ~manually_implemented
    ~required
    ~name
    ~ctypes_name
    add
    (Ctypes_static.Function (Void, Returns typ))

module type FOREIGN = Ctypes.FOREIGN

module type FOREIGN' = FOREIGN with type 'a result = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

let collect ~api ~manually_implemented ~required add (module B : BINDINGS) =
  let prefix = "" in
  let module M = B (struct
    let counter = ref 0

    let var prefix name =
      incr counter ;
      Printf.sprintf "%s_%d_%s" prefix !counter name

    type 'a fn = 'a Ctypes.fn

    type 'a return = 'a

    type 'a result = unit

    let foreign name fn =
      gen_fn
        ~api
        ~manually_implemented
        ~required
        ~name
        ~ctypes_name:(var prefix name)
        add
        fn

    let foreign_value name typ =
      gen_value
        ~api
        ~manually_implemented
        ~required
        ~name
        ~ctypes_name:(var prefix name)
        add
        typ

    let returning = Ctypes.returning

    let ( @-> ) = Ctypes.( @-> )
  end) in
  ()

let collect_all ~api ~manually_implemented ~required =
  let l = ref [] in
  let add x = l := x :: !l in
  let collect m = collect ~api ~manually_implemented ~required add m in
  collect (module EverCrypt_AEAD_bindings.Bindings) ;
  collect (module EverCrypt_AutoConfig2_bindings.Bindings) ;
  collect (module EverCrypt_Chacha20Poly1305_bindings.Bindings) ;
  collect (module EverCrypt_Cipher_bindings.Bindings) ;
  collect (module EverCrypt_CTR_bindings.Bindings) ;
  collect (module EverCrypt_Curve25519_bindings.Bindings) ;
  collect (module EverCrypt_DRBG_bindings.Bindings) ;
  collect (module EverCrypt_Ed25519_bindings.Bindings) ;
  collect (module EverCrypt_Error_bindings.Bindings) ;
  collect (module EverCrypt_Hash_bindings.Bindings) ;
  collect (module EverCrypt_HKDF_bindings.Bindings) ;
  collect (module EverCrypt_HMAC_bindings.Bindings) ;
  collect (module EverCrypt_Poly1305_bindings.Bindings) ;
  collect (module EverCrypt_StaticConfig_bindings.Bindings) ;
  collect (module EverCrypt_Vale_bindings.Bindings) ;
  collect (module Hacl_Bignum25519_51_bindings.Bindings) ;
  collect (module Hacl_Bignum256_32_bindings.Bindings) ;
  collect (module Hacl_Bignum256_bindings.Bindings) ;
  collect (module Hacl_Bignum32_bindings.Bindings) ;
  collect (module Hacl_Bignum4096_32_bindings.Bindings) ;
  collect (module Hacl_Bignum4096_bindings.Bindings) ;
  collect (module Hacl_Bignum64_bindings.Bindings) ;
  collect (module Hacl_Bignum_Base_bindings.Bindings) ;
  collect (module Hacl_Bignum_bindings.Bindings) ;
  collect (module Hacl_Chacha20_bindings.Bindings) ;
  collect (module Hacl_Chacha20Poly1305_128_bindings.Bindings) ;
  collect (module Hacl_Chacha20Poly1305_256_bindings.Bindings) ;
  collect (module Hacl_Chacha20Poly1305_32_bindings.Bindings) ;
  collect (module Hacl_Chacha20_Vec128_bindings.Bindings) ;
  collect (module Hacl_Chacha20_Vec256_bindings.Bindings) ;
  collect (module Hacl_Chacha20_Vec32_bindings.Bindings) ;
  collect (module Hacl_Curve25519_51_bindings.Bindings) ;
  collect (module Hacl_Curve25519_64_bindings.Bindings) ;
  collect (module Hacl_Curve25519_64_Slow_bindings.Bindings) ;
  collect (module Hacl_EC_Ed25519_bindings.Bindings) ;
  collect (module Hacl_Ed25519_bindings.Bindings) ;
  collect (module Hacl_FFDHE_bindings.Bindings) ;
  collect (module Hacl_Frodo1344_bindings.Bindings) ;
  collect (module Hacl_Frodo640_bindings.Bindings) ;
  collect (module Hacl_Frodo64_bindings.Bindings) ;
  collect (module Hacl_Frodo976_bindings.Bindings) ;
  collect (module Hacl_Frodo_KEM_bindings.Bindings) ;
  collect (module Hacl_GenericField32_bindings.Bindings) ;
  collect (module Hacl_GenericField64_bindings.Bindings) ;
  collect (module Hacl_Hash_Base_bindings.Bindings) ;
  collect (module Hacl_Hash_Blake2b_256_bindings.Bindings) ;
  collect (module Hacl_Hash_Blake2_bindings.Bindings) ;
  collect (module Hacl_Hash_Blake2s_128_bindings.Bindings) ;
  collect (module Hacl_Hash_MD5_bindings.Bindings) ;
  collect (module Hacl_Hash_SHA1_bindings.Bindings) ;
  collect (module Hacl_Hash_SHA2_bindings.Bindings) ;
  collect (module Hacl_HKDF_bindings.Bindings) ;
  collect (module Hacl_HKDF_Blake2b_256_bindings.Bindings) ;
  collect (module Hacl_HKDF_Blake2s_128_bindings.Bindings) ;
  collect (module Hacl_HMAC_bindings.Bindings) ;
  collect (module Hacl_HMAC_Blake2b_256_bindings.Bindings) ;
  collect (module Hacl_HMAC_Blake2s_128_bindings.Bindings) ;
  collect (module Hacl_HMAC_DRBG_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve51_CP128_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve51_CP128_SHA512_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve51_CP256_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve51_CP256_SHA512_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve51_CP32_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve51_CP32_SHA512_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve64_CP128_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve64_CP128_SHA512_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve64_CP256_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve64_CP256_SHA512_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve64_CP32_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_Curve64_CP32_SHA512_bindings.Bindings) ;
  collect (module Hacl_HPKE_P256_CP128_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_P256_CP256_SHA256_bindings.Bindings) ;
  collect (module Hacl_HPKE_P256_CP32_SHA256_bindings.Bindings) ;
  collect (module Hacl_IntTypes_Intrinsics_128_bindings.Bindings) ;
  collect (module Hacl_IntTypes_Intrinsics_bindings.Bindings) ;
  collect (module Hacl_NaCl_bindings.Bindings) ;
  collect (module Hacl_P256_bindings.Bindings) ;
  collect (module Hacl_Poly1305_128_bindings.Bindings) ;
  collect (module Hacl_Poly1305_256_bindings.Bindings) ;
  collect (module Hacl_Poly1305_32_bindings.Bindings) ;
  collect (module Hacl_RSAPSS_bindings.Bindings) ;
  collect (module Hacl_Salsa20_bindings.Bindings) ;
  collect (module Hacl_SHA2_Scalar32_bindings.Bindings) ;
  collect (module Hacl_SHA2_Vec128_bindings.Bindings) ;
  collect (module Hacl_SHA2_Vec256_bindings.Bindings) ;
  collect (module Hacl_SHA3_bindings.Bindings) ;
  collect (module Hacl_Spec_bindings.Bindings) ;
  collect (module Hacl_Streaming_Blake2_bindings.Bindings) ;
  collect (module Hacl_Streaming_MD5_bindings.Bindings) ;
  collect (module Hacl_Streaming_Poly1305_32_bindings.Bindings) ;
  collect (module Hacl_Streaming_SHA1_bindings.Bindings) ;
  collect (module Hacl_Streaming_SHA2_bindings.Bindings) ;
  collect (module Lib_RandomBuffer_System_bindings.Bindings) ;
  !l

let entries =
  let api_json = ref "./api.json" in

  let js_stubs = ref [] in

  let usage () =
    print_endline "gen.exe -api api.json -stubs runtime1.js -stubs runtime1.js"
  in
  let () =
    Arg.parse
      [
        ("-api", Arg.Set_string api_json, "FILE the location the api.json file");
        ( "-stubs",
          Arg.String (fun stubs -> js_stubs := stubs :: !js_stubs),
          "FILE manually written stubs" );
      ]
      (fun _ -> usage ())
      ""
  in

  let api = Api_json.parse_file !api_json in

  let (manually_implemented, required) =
    let provides_r = Str.regexp "//Provides: *\\([a-zA-z0-9_]*\\)" in
    let requires_r = Str.regexp "//Requires: *\\([a-zA-z0-9_, ]*\\)" in
    let implemented = ref String_set.empty in
    let required = ref String_set.empty in
    let parse f =
      let ic = open_in_bin f in
      try
        while true do
          let line = input_line ic in
          if Str.string_match provides_r line 0 then
            implemented :=
              String_set.add (Str.matched_group 1 line) !implemented
          else if Str.string_match requires_r line 0 then
            let l = String.split_on_char ',' (Str.matched_group 1 line) in
            List.iter
              (fun r -> required := String_set.add (String.trim r) !required)
              l
        done
      with End_of_file -> ()
    in
    List.iter parse !js_stubs ;
    (!implemented, !required)
  in
  collect_all ~api ~manually_implemented ~required

let () =
  let entries = List.sort Entry.compare entries in
  let fmt = Format.std_formatter in
  Format.fprintf fmt "// This file was automatically generated, do not edit.@." ;
  Format.fprintf fmt "// Edit file src/lib_hacl/gen/gen.ml instead@.@.@." ;
  List.iter (Entry.pp fmt) entries ;
  if List.exists (function Entry.Error _ -> true | _ -> false) entries then
    exit 1
