(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

module Shell_error_monad = Error_monad

type shell_error = error = ..

open Environment_context
open Environment_protocol_T

module type T = sig
  include
    Tezos_protocol_environment_sigs.V8.T
      with type Format.formatter = Format.formatter
       and type 'a Seq.node = 'a Seq.node
       and type 'a Seq.t = unit -> 'a Seq.node
       and type 'a Data_encoding.t = 'a Data_encoding.t
       and type 'a Data_encoding.Compact.t = 'a Data_encoding.Compact.t
       and type 'a Data_encoding.lazy_t = 'a Data_encoding.lazy_t
       and type 'a Lwt.t = 'a Lwt.t
       and type ('a, 'b) Pervasives.result = ('a, 'b) result
       and type Chain_id.t = Tezos_crypto.Hashed.Chain_id.t
       and type Block_hash.t = Tezos_crypto.Hashed.Block_hash.t
       and type Operation_hash.t = Tezos_crypto.Hashed.Operation_hash.t
       and type Operation_list_hash.t =
        Tezos_crypto.Hashed.Operation_list_hash.t
       and type Operation_list_list_hash.t =
        Tezos_crypto.Hashed.Operation_list_list_hash.t
       and type Context.t = Context.t
       and type Context.cache_key = Environment_context.Context.cache_key
       and type Context.cache_value = Environment_context.Context.cache_value
       and type Context_hash.t = Tezos_crypto.Hashed.Context_hash.t
       and type Context_hash.Version.t =
        Tezos_crypto.Hashed.Context_hash.Version.t
       and type Context.config = Tezos_context_sigs.Config.t
       and module Context.Proof = Environment_context.Context.Proof
       and type Protocol_hash.t = Tezos_crypto.Hashed.Protocol_hash.t
       and type Time.t = Time.Protocol.t
       and type Operation.shell_header = Operation.shell_header
       and type Operation.t = Operation.t
       and type Block_header.shell_header = Block_header.shell_header
       and type Block_header.t = Block_header.t
       and type 'a RPC_directory.t = 'a Tezos_rpc.Directory.t
       and type Ed25519.Public_key_hash.t = Signature.Ed25519.Public_key_hash.t
       and type Ed25519.Public_key.t = Signature.Ed25519.Public_key.t
       and type Ed25519.t = Signature.Ed25519.t
       and type Secp256k1.Public_key_hash.t =
        Signature.Secp256k1.Public_key_hash.t
       and type Secp256k1.Public_key.t = Signature.Secp256k1.Public_key.t
       and type Secp256k1.t = Signature.Secp256k1.t
       and type P256.Public_key_hash.t = Signature.P256.Public_key_hash.t
       and type P256.Public_key.t = Signature.P256.Public_key.t
       and type P256.t = Signature.P256.t
       and type Bls.Public_key_hash.t = Signature.Bls.Public_key_hash.t
       and type Bls.Public_key.t = Signature.Bls.Public_key.t
       and type Bls.t = Signature.Bls.t
       and type Signature.public_key_hash = Signature.V1.public_key_hash
       and type Signature.public_key = Signature.V1.public_key
       and type Signature.signature = Signature.V1.signature
       and type Signature.t = Signature.V1.t
       and type Signature.watermark = Signature.V1.watermark
       and type Micheline.canonical_location = Micheline.canonical_location
       and type 'a Micheline.canonical = 'a Micheline.canonical
       and type Z.t = Z.t
       and type Q.t = Q.t
       and type ('a, 'b) Micheline.node = ('a, 'b) Micheline.node
       and type Data_encoding.json_schema = Data_encoding.json_schema
       and type ('a, 'b) RPC_path.t = ('a, 'b) Tezos_rpc.Path.t
       and type RPC_service.meth = Tezos_rpc.Service.meth
       and type (+'m, 'pr, 'p, 'q, 'i, 'o) RPC_service.t =
        ('m, 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t
       and type Error_monad.shell_tztrace = Error_monad.tztrace
       and type 'a Error_monad.shell_tzresult = ('a, Error_monad.tztrace) result
       and type Timelock.chest = Tezos_crypto.Timelock_legacy.chest
       and type Timelock.chest_key = Tezos_crypto.Timelock_legacy.chest_key
       and type Timelock.opening_result =
        Tezos_crypto.Timelock_legacy.opening_result
       and module Sapling = Tezos_sapling.Core.Validator
       and type ('a, 'b) Either.t = ('a, 'b) Stdlib.Either.t
       and type Bls.Primitive.Fr.t = Bls12_381.Fr.t
       and type Plonk.proof = Tezos_protocol_environment_structs.V8.Plonk.proof
       and type Plonk.public_parameters =
        Tezos_protocol_environment_structs.V8.Plonk.verifier_public_parameters
       and type Dal.parameters = Tezos_crypto_dal.Cryptobox.Verifier.parameters
       and type Dal.commitment = Tezos_crypto_dal.Cryptobox.Verifier.commitment
       and type Dal.commitment_proof =
        Tezos_crypto_dal.Cryptobox.Verifier.commitment_proof
       and type Dal.page_proof = Tezos_crypto_dal.Cryptobox.Verifier.page_proof
       and type Bounded.Non_negative_int32.t =
        Tezos_base.Bounded.Non_negative_int32.t
       and type Wasm_2_0_0.reveal =
        Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.reveal
       and type Wasm_2_0_0.input = Tezos_scoru_wasm.Wasm_pvm_state.input_info
       and type Wasm_2_0_0.output = Tezos_scoru_wasm.Wasm_pvm_state.output_info
       and type Wasm_2_0_0.reveal_hash =
        Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.reveal_hash

  type error += Ecoproto_error of Error_monad.error

  val wrap_tzerror : Error_monad.error -> error

  val wrap_tztrace : Error_monad.error Error_monad.trace -> error trace

  val wrap_tzresult : 'a Error_monad.tzresult -> 'a tzresult

  module Lift (P : Updater.PROTOCOL) :
    PROTOCOL
      with type block_header_data = P.block_header_data
       and type block_header_metadata = P.block_header_metadata
       and type block_header = P.block_header
       and type operation_data = P.operation_data
       and type operation_receipt = P.operation_receipt
       and type operation = P.operation
       and type validation_state = P.validation_state
       and type application_state = P.application_state

  class ['chain, 'block] proto_rpc_context :
    Tezos_rpc.Context.t ->
    (unit, (unit * 'chain) * 'block) RPC_path.t ->
    ['chain * 'block] RPC_context.simple

  class ['block] proto_rpc_context_of_directory :
    ('block -> RPC_context.t) ->
    RPC_context.t RPC_directory.t ->
    ['block] RPC_context.simple
end

module Make
    (Param : sig
      val name : string
    end)
    () =
struct
  (* The protocol V8 only supports 64-bits architectures. We ensure this the
     hard way with a dynamic check. *)
  let () =
    match Sys.word_size with
    | 32 ->
        Printf.eprintf
          "FAILURE: Environment V8 does not support 32-bit architectures\n%!" ;
        Stdlib.exit 1
    | 64 -> ()
    | n ->
        Printf.eprintf
          "FAILURE: Unknown, unsupported architecture (%d bits)\n%!"
          n ;
        Stdlib.exit 1

  module CamlinternalFormatBasics = CamlinternalFormatBasics
  include Stdlib
  module Pervasives = Stdlib

  module Logging = struct
    type level = Internal_event.level =
      | Debug
      | Info
      | Notice
      | Warning
      | Error
      | Fatal

    let logging_function = ref None

    let name_colon_space = Param.name ^ ": "

    let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

    let log (level : Internal_event.level) =
      match !logging_function with
      | None -> Format.ikfprintf ignore null_formatter
      | Some f -> Format.kasprintf (fun s -> f level (name_colon_space ^ s))

    let log_string (level : Internal_event.level) s =
      match !logging_function with
      | None -> ()
      | Some f -> f level (name_colon_space ^ s)
  end

  module Compare = Compare
  module Either = Either
  module Seq = Tezos_protocol_environment_structs.V8.Seq

  module List = struct
    include Tezos_error_monad.TzLwtreslib.List

    let fold_right_es f l x = fold_left_es (fun acc x -> f x acc) x (rev l)
  end

  module Array = Tezos_protocol_environment_structs.V8.Array
  module Char = Char
  module Bytes = Tezos_base.TzPervasives.Bytes
  module Hex = Tezos_stdlib.Hex
  module String = String
  module Bits = Bits
  module TzEndian = TzEndian
  module Set = Tezos_error_monad.TzLwtreslib.Set
  module Map = Tezos_error_monad.TzLwtreslib.Map
  module Int32 = Int32
  module Int64 = Int64
  module Format = Format
  module FallbackArray = FallbackArray

  let not_a_sys_exc next_classifier = function
    | Unix.Unix_error _ | UnixLabels.Unix_error _ | Sys_error _ -> false
    | e -> next_classifier e

  module Option = struct
    include Tezos_error_monad.TzLwtreslib.Option

    (* This as well as the catchers in [Result] and [Error_monad] are different
       from the ones in Lwtreslib/Error Monad in that they also hide the Unix
       and System errors. This is because, from the point-of-view of the
       protocol, these exceptions are too abstract and too indeterministic. *)
    let catch ?(catch_only = fun _ -> true) f =
      (* Note that [catch] also special-cases its own set of exceptions. *)
      catch ~catch_only:(not_a_sys_exc catch_only) f

    let catch_s ?(catch_only = fun _ -> true) f =
      catch_s ~catch_only:(not_a_sys_exc catch_only) f
  end

  module Result = struct
    include Tezos_error_monad.TzLwtreslib.Result

    let catch ?(catch_only = fun _ -> true) f =
      catch ~catch_only:(not_a_sys_exc catch_only) f

    let catch_f ?(catch_only = fun _ -> true) f =
      catch_f ~catch_only:(not_a_sys_exc catch_only) f

    let catch_s ?(catch_only = fun _ -> true) f =
      catch_s ~catch_only:(not_a_sys_exc catch_only) f
  end

  module Raw_hashes = struct
    let sha256 = Tezos_crypto.Hacl.Hash.SHA256.digest

    let sha512 = Tezos_crypto.Hacl.Hash.SHA512.digest

    let blake2b msg =
      Tezos_crypto.Blake2B.to_bytes (Tezos_crypto.Blake2B.hash_bytes [msg])

    let keccak256 msg = Tezos_crypto.Hacl.Hash.Keccak_256.digest msg

    let sha3_256 msg = Tezos_crypto.Hacl.Hash.SHA3_256.digest msg

    let sha3_512 msg = Tezos_crypto.Hacl.Hash.SHA3_512.digest msg
  end

  module Z = Z
  module Q = Q
  module Lwt = Lwt

  module Data_encoding = struct
    include Tezos_protocol_environment_structs.V8.Data_encoding

    type tag_size = [`Uint8 | `Uint16]

    let def name ?title ?description encoding =
      def (Param.name ^ "." ^ name) ?title ?description encoding

    (* TODO: https://gitlab.com/nomadic-labs/data-encoding/-/issues/58
       Remove when fix is integrated in data-encoding. *)
    let splitted ~json ~binary =
      let open Data_encoding__.Encoding in
      let e = splitted ~json ~binary in
      {
        e with
        encoding =
          (match e.encoding with
          | Splitted {encoding; json_encoding; _} ->
              Splitted
                {
                  encoding;
                  json_encoding;
                  is_obj = is_obj json && is_obj binary;
                  is_tup = is_tup json && is_tup binary;
                }
          | desc -> desc);
      }
  end

  module Time = Time.Protocol
  module Ed25519 = Signature.Ed25519
  module Secp256k1 = Signature.Secp256k1
  module P256 = Signature.P256

  module Bls = struct
    include Tezos_crypto.Signature.Bls

    let aggregate_signature_opt = aggregate_signature_opt ~subgroup_check:true
  end

  module Signature = Signature.V1
  module Timelock = Tezos_crypto.Timelock_legacy
  module Vdf = Class_group_vdf.Vdf_self_contained

  module S = struct
    module type T = Tezos_base.S.T

    module type HASHABLE = Tezos_base.S.HASHABLE

    module type MINIMAL_HASH = Tezos_crypto.Intfs.MINIMAL_HASH

    module type B58_DATA = sig
      type t

      val to_b58check : t -> string

      val to_short_b58check : t -> string

      val of_b58check_exn : string -> t

      val of_b58check_opt : string -> t option

      type Tezos_crypto.Base58.data += Data of t

      val b58check_encoding : t Tezos_crypto.Base58.encoding
    end

    module type RAW_DATA = sig
      type t

      val size : int (* in bytes *)

      val to_bytes : t -> Bytes.t

      val of_bytes_opt : Bytes.t -> t option

      val of_bytes_exn : Bytes.t -> t
    end

    module type ENCODER = sig
      type t

      val encoding : t Data_encoding.t

      val rpc_arg : t Tezos_rpc.Arg.t
    end

    module type INDEXES_SET = sig
      include Set.S

      val random_elt : t -> elt

      val encoding : t Data_encoding.t
    end

    module type INDEXES_MAP = sig
      include Map.S

      val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
    end

    module type INDEXES = sig
      type t

      module Set : INDEXES_SET with type elt = t

      module Map : INDEXES_MAP with type key = t
    end

    module type HASH = sig
      include MINIMAL_HASH

      include RAW_DATA with type t := t

      include B58_DATA with type t := t

      include ENCODER with type t := t

      include INDEXES with type t := t
    end

    module type MERKLE_TREE = sig
      type elt

      include HASH

      val compute : elt list -> t

      val empty : t

      type path = Left of path * t | Right of t * path | Op

      val compute_path : elt list -> int -> path

      val check_path : path -> elt -> t * int

      val path_encoding : path Data_encoding.t
    end

    module type SIGNATURE_PUBLIC_KEY_HASH = sig
      type t

      val pp : Format.formatter -> t -> unit

      val pp_short : Format.formatter -> t -> unit

      include Compare.S with type t := t

      include RAW_DATA with type t := t

      include B58_DATA with type t := t

      include ENCODER with type t := t

      include INDEXES with type t := t

      val zero : t
    end

    module type SIGNATURE_PUBLIC_KEY = sig
      type t

      val pp : Format.formatter -> t -> unit

      include Compare.S with type t := t

      include B58_DATA with type t := t

      include ENCODER with type t := t

      type public_key_hash_t

      val hash : t -> public_key_hash_t

      val size : t -> int (* in bytes *)

      val of_bytes_without_validation : bytes -> t option
    end

    module type SIGNATURE = sig
      module Public_key_hash : SIGNATURE_PUBLIC_KEY_HASH

      module Public_key :
        SIGNATURE_PUBLIC_KEY with type public_key_hash_t := Public_key_hash.t

      type t

      val pp : Format.formatter -> t -> unit

      include RAW_DATA with type t := t

      include Compare.S with type t := t

      include B58_DATA with type t := t

      include ENCODER with type t := t

      val zero : t

      type watermark

      (** Check a signature *)
      val check : ?watermark:watermark -> Public_key.t -> t -> Bytes.t -> bool
    end

    module type AGGREGATE_SIGNATURE = sig
      include SIGNATURE

      val aggregate_check :
        (Public_key.t * watermark option * bytes) list -> t -> bool

      val aggregate_signature_opt : t list -> t option
    end

    module type SPLIT_SIGNATURE = sig
      include SIGNATURE

      type prefix

      type splitted = {prefix : prefix option; suffix : Bytes.t}

      val split_signature : t -> splitted

      val of_splitted : splitted -> t option

      val prefix_encoding : prefix Data_encoding.t
    end

    module type FIELD = sig
      type t

      (** The order of the finite field *)
      val order : Z.t

      (** Minimal number of bytes required to encode a value of the field. *)
      val size_in_bytes : int

      (** [check_bytes bs] returns [true] if [bs] is a correct byte
          representation of a field element *)
      val check_bytes : Bytes.t -> bool

      (** The neutral element for the addition *)
      val zero : t

      (** The neutral element for the multiplication *)
      val one : t

      (** [add a b] returns [a + b mod order] *)
      val add : t -> t -> t

      (** [mul a b] returns [a * b mod order] *)
      val mul : t -> t -> t

      (** [eq a b] returns [true] if [a = b mod order], else [false] *)
      val eq : t -> t -> bool

      (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
          unique [y] such that [x + y mod order = 0]
      *)
      val negate : t -> t

      (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)
      val inverse_opt : t -> t option

      (** [pow x n] returns [x^n] *)
      val pow : t -> Z.t -> t

      (** From a predefined bytes representation, construct a value t. It is not
          required that to_bytes [(Option.get (of_bytes_opt t)) = t]. By default, little endian encoding
          is used and the given element is modulo the prime order *)
      val of_bytes_opt : Bytes.t -> t option

      (** Convert the value t to a bytes representation which can be used for
          hashing for instance. It is not required that [Option.get (to_bytes
          (of_bytes_opt t)) = t]. By default, little endian encoding is used, and
          length of the resulting bytes may vary depending on the order.
      *)
      val to_bytes : t -> Bytes.t
    end

    (** Module type for the prime fields GF(p) *)
    module type PRIME_FIELD = sig
      include FIELD

      (** Actual number of bytes allocated for a value of type t *)
      val size_in_memory : int

      (** [of_z x] builds an element t from the Zarith element [x]. [mod order] is
          applied if [x >= order] or [x < 0]. *)
      val of_z : Z.t -> t

      (** [to_z x] builds a Zarith element, using the decimal representation.
          Arithmetic on the result can be done using the modular functions on
          integers *)
      val to_z : t -> Z.t
    end

    module type CURVE = sig
      (** The type of the element in the elliptic curve *)
      type t

      (** Actual number of bytes allocated for a value of type t *)
      val size_in_memory : int

      (** The size of a point representation, in bytes *)
      val size_in_bytes : int

      module Scalar : FIELD

      (** Check if a point, represented as a byte array, is on the curve **)
      val check_bytes : Bytes.t -> bool

      (** Attempt to construct a point from a byte array *)
      val of_bytes_opt : Bytes.t -> t option

      (** Return a representation in bytes *)
      val to_bytes : t -> Bytes.t

      (** Zero of the elliptic curve *)
      val zero : t

      (** A fixed generator of the elliptic curve *)
      val one : t

      (** Return the addition of two element *)
      val add : t -> t -> t

      (** Double the element *)
      val double : t -> t

      (** Return the opposite of the element *)
      val negate : t -> t

      (** Return [true] if the two elements are algebraically the same *)
      val eq : t -> t -> bool

      (** Multiply an element by a scalar *)
      val mul : t -> Scalar.t -> t
    end
  end

  module Error_core = struct
    include
      Tezos_error_monad.Core_maker.Make
        (struct
          let id = Format.asprintf "proto.%s." Param.name
        end)
        (struct
          type t =
            [ `Branch  (** Errors that may not happen in another context *)
            | `Temporary  (** Errors that may not happen in a later context *)
            | `Permanent  (** Errors that will happen no matter the context *)
            | `Outdated  (** Errors that happen when the context is too old *)
            ]

          let default_category = `Temporary

          let string_of_category = function
            | `Permanent -> "permanent"
            | `Outdated -> "outdated"
            | `Branch -> "branch"
            | `Temporary -> "temporary"

          let classify = function
            | `Permanent -> Tezos_error_monad.Error_classification.Permanent
            | `Branch -> Branch
            | `Temporary -> Temporary
            | `Outdated -> Outdated
        end)
  end

  type error_category = Error_core.error_category

  type shell_error += Ecoproto_error of Error_core.error

  module Wrapped_error_monad = struct
    type unwrapped = Error_core.error = ..

    include (
      Error_core :
        sig
          include
            Tezos_error_monad.Sig.CORE
              with type error := unwrapped
               and type error_category = error_category
        end)

    let unwrap = function Ecoproto_error ecoerror -> Some ecoerror | _ -> None

    let wrap ecoerror = Ecoproto_error ecoerror
  end

  module Error_monad = struct
    type shell_tztrace = Error_monad.tztrace

    type 'a shell_tzresult = ('a, Error_monad.tztrace) result

    include Error_core
    include Tezos_error_monad.TzLwtreslib.Monad
    include
      Tezos_error_monad.Monad_maker.Make (Error_core) (TzTrace)
        (Tezos_error_monad.TzLwtreslib.Monad)

    (* Backwards compatibility additions (dont_wait, trace helpers) *)
    include Tezos_protocol_environment_structs.V8.Error_monad_infix_globals

    let tzfail e = Lwt.return_error (TzTrace.make e)

    let error e = Error (TzTrace.make e)

    let dont_wait ex er f = dont_wait f er ex

    let trace_of_error e = TzTrace.make e

    let make_trace_encoding e = TzTrace.encoding e

    let pp_trace = pp_print_trace

    type 'err trace = 'err TzTrace.trace

    (* Shadowing catch to prevent catching system exceptions *)
    type error += Exn of exn

    let () =
      register_error_kind
        `Temporary
        ~id:"failure"
        ~title:"Exception"
        ~description:"Exception safely wrapped in an error"
        ~pp:(fun ppf s ->
          Format.fprintf ppf "@[<h 0>%a@]" Format.pp_print_text s)
        Data_encoding.(obj1 (req "msg" @@ string Plain))
        (function
          | Exn (Failure msg) -> Some msg
          | Exn exn -> Some (Printexc.to_string exn)
          | _ -> None)
        (fun msg -> Exn (Failure msg))

    let error_of_exn e = TzTrace.make @@ Exn e

    let catch ?catch_only f =
      Result.catch ?catch_only f |> Result.map_error error_of_exn

    let catch_f ?catch_only f h =
      Result.catch ?catch_only f
      |> Result.map_error (fun e -> trace_of_error (h e))

    let catch_s ?catch_only f =
      let open Lwt_syntax in
      let+ r = Result.catch_s ?catch_only f in
      Result.map_error (fun e -> error_of_exn e) r

    let both_e = Tezos_error_monad.TzLwtreslib.Monad.Traced_result_syntax.both

    let join_e = Tezos_error_monad.TzLwtreslib.Monad.Traced_result_syntax.join

    let all_e = Tezos_error_monad.TzLwtreslib.Monad.Traced_result_syntax.all
  end

  let () =
    let id = Format.asprintf "proto.%s.wrapper" Param.name in
    Shell_error_monad.register_wrapped_error_kind
      (module Wrapped_error_monad)
      ~id
      ~title:("Error returned by protocol " ^ Param.name)
      ~description:("Wrapped error for economic protocol " ^ Param.name ^ ".")

  let wrap_tzerror error = Ecoproto_error error

  let wrap_tztrace t = List.map wrap_tzerror t

  let wrap_tzresult r = Result.map_error wrap_tztrace r

  module Chain_id = Tezos_crypto.Hashed.Chain_id
  module Block_hash = Tezos_crypto.Hashed.Block_hash
  module Operation_hash = Tezos_crypto.Hashed.Operation_hash
  module Operation_list_hash = Tezos_crypto.Hashed.Operation_list_hash
  module Operation_list_list_hash = Tezos_crypto.Hashed.Operation_list_list_hash
  module Context_hash = Tezos_crypto.Hashed.Context_hash
  module Protocol_hash = Tezos_crypto.Hashed.Protocol_hash
  module Blake2B = Tezos_crypto.Blake2B
  module Fitness = Fitness
  module Operation = Operation
  module Block_header = Block_header
  module Bounded = Bounded
  module Protocol = Protocol
  module RPC_arg = Tezos_rpc.Arg
  module RPC_path = Tezos_rpc.Path
  module RPC_query = Tezos_rpc.Query
  module RPC_service = Tezos_rpc.Service

  module RPC_answer = struct
    type 'o t =
      [ `Ok of 'o (* 200 *)
      | `OkChunk of 'o (* 200 but with chunked transfer encoding *)
      | `OkStream of 'o stream (* 200 *)
      | `Created of string option (* 201 *)
      | `No_content (* 204 *)
      | `Unauthorized of Error_monad.error list option (* 401 *)
      | `Forbidden of Error_monad.error list option (* 403 *)
      | `Not_found of Error_monad.error list option (* 404 *)
      | `Conflict of Error_monad.error list option (* 409 *)
      | `Error of Error_monad.error list option (* 500 *) ]

    and 'a stream = 'a Resto_directory.Answer.stream = {
      next : unit -> 'a option Lwt.t;
      shutdown : unit -> unit;
    }

    let return x = Lwt.return (`Ok x)

    let return_chunked x = Lwt.return (`OkChunk x)

    let return_stream x = Lwt.return (`OkStream x)

    let not_found = Lwt.return (`Not_found None)

    let fail err = Lwt.return (`Error (Some err))
  end

  module RPC_directory = struct
    include Tezos_rpc.Directory

    let gen_register dir service handler =
      let open Lwt_syntax in
      gen_register dir service (fun p q i ->
          let* r = handler p q i in
          match r with
          | `Ok o -> RPC_answer.return o
          | `OkChunk o -> RPC_answer.return_chunked o
          | `OkStream s -> RPC_answer.return_stream s
          | `Created s -> Lwt.return (`Created s)
          | `No_content -> Lwt.return `No_content
          | `Unauthorized e ->
              let e = Option.map (List.map (fun e -> Ecoproto_error e)) e in
              Lwt.return (`Unauthorized e)
          | `Forbidden e ->
              let e = Option.map (List.map (fun e -> Ecoproto_error e)) e in
              Lwt.return (`Forbidden e)
          | `Not_found e ->
              let e = Option.map (List.map (fun e -> Ecoproto_error e)) e in
              Lwt.return (`Not_found e)
          | `Conflict e ->
              let e = Option.map (List.map (fun e -> Ecoproto_error e)) e in
              Lwt.return (`Conflict e)
          | `Error e ->
              let e = Option.map (List.map (fun e -> Ecoproto_error e)) e in
              Lwt.return (`Error e))

    let register ~chunked dir service handler =
      let open Lwt_syntax in
      gen_register dir service (fun p q i ->
          let* r = handler p q i in
          match r with
          | Ok o when chunked -> RPC_answer.return_chunked o
          | Ok o (* otherwise *) -> RPC_answer.return o
          | Error e -> RPC_answer.fail e)

    let opt_register ~chunked dir service handler =
      let open Lwt_syntax in
      gen_register dir service (fun p q i ->
          let* r = handler p q i in
          match r with
          | Ok (Some o) when chunked -> RPC_answer.return_chunked o
          | Ok (Some o) (* otherwise *) -> RPC_answer.return o
          | Ok None -> RPC_answer.not_found
          | Error e -> RPC_answer.fail e)

    let lwt_register ~chunked dir service handler =
      let open Lwt_syntax in
      gen_register dir service (fun p q i ->
          let* o = handler p q i in
          if chunked then RPC_answer.return_chunked o else RPC_answer.return o)

    open Curry

    let register0 ~chunked root s f = register ~chunked root s (curry Z f)

    let register1 ~chunked root s f = register ~chunked root s (curry (S Z) f)

    let register2 ~chunked root s f =
      register ~chunked root s (curry (S (S Z)) f)

    let register3 ~chunked root s f =
      register ~chunked root s (curry (S (S (S Z))) f)

    let register4 ~chunked root s f =
      register ~chunked root s (curry (S (S (S (S Z)))) f)

    let register5 ~chunked root s f =
      register ~chunked root s (curry (S (S (S (S (S Z))))) f)

    let opt_register0 ~chunked root s f =
      opt_register ~chunked root s (curry Z f)

    let opt_register1 ~chunked root s f =
      opt_register ~chunked root s (curry (S Z) f)

    let opt_register2 ~chunked root s f =
      opt_register ~chunked root s (curry (S (S Z)) f)

    let opt_register3 ~chunked root s f =
      opt_register ~chunked root s (curry (S (S (S Z))) f)

    let opt_register4 ~chunked root s f =
      opt_register ~chunked root s (curry (S (S (S (S Z)))) f)

    let opt_register5 ~chunked root s f =
      opt_register ~chunked root s (curry (S (S (S (S (S Z))))) f)

    let gen_register0 root s f = gen_register root s (curry Z f)

    let gen_register1 root s f = gen_register root s (curry (S Z) f)

    let gen_register2 root s f = gen_register root s (curry (S (S Z)) f)

    let gen_register3 root s f = gen_register root s (curry (S (S (S Z))) f)

    let gen_register4 root s f = gen_register root s (curry (S (S (S (S Z)))) f)

    let gen_register5 root s f =
      gen_register root s (curry (S (S (S (S (S Z))))) f)

    let lwt_register0 ~chunked root s f =
      lwt_register ~chunked root s (curry Z f)

    let lwt_register1 ~chunked root s f =
      lwt_register ~chunked root s (curry (S Z) f)

    let lwt_register2 ~chunked root s f =
      lwt_register ~chunked root s (curry (S (S Z)) f)

    let lwt_register3 ~chunked root s f =
      lwt_register ~chunked root s (curry (S (S (S Z))) f)

    let lwt_register4 ~chunked root s f =
      lwt_register ~chunked root s (curry (S (S (S (S Z)))) f)

    let lwt_register5 ~chunked root s f =
      lwt_register ~chunked root s (curry (S (S (S (S (S Z))))) f)
  end

  module RPC_context = struct
    type t = rpc_context

    class type ['pr] simple = object
      method call_proto_service0 :
        'm 'q 'i 'o.
        ( ([< Tezos_rpc.Service.meth] as 'm),
          t,
          t,
          'q,
          'i,
          'o )
        Tezos_rpc.Service.t ->
        'pr ->
        'q ->
        'i ->
        'o Error_monad.shell_tzresult Lwt.t

      method call_proto_service1 :
        'm 'a 'q 'i 'o.
        ( ([< Tezos_rpc.Service.meth] as 'm),
          t,
          t * 'a,
          'q,
          'i,
          'o )
        Tezos_rpc.Service.t ->
        'pr ->
        'a ->
        'q ->
        'i ->
        'o Error_monad.shell_tzresult Lwt.t

      method call_proto_service2 :
        'm 'a 'b 'q 'i 'o.
        ( ([< Tezos_rpc.Service.meth] as 'm),
          t,
          (t * 'a) * 'b,
          'q,
          'i,
          'o )
        Tezos_rpc.Service.t ->
        'pr ->
        'a ->
        'b ->
        'q ->
        'i ->
        'o Error_monad.shell_tzresult Lwt.t

      method call_proto_service3 :
        'm 'a 'b 'c 'q 'i 'o.
        ( ([< RPC_service.meth] as 'm),
          t,
          ((t * 'a) * 'b) * 'c,
          'q,
          'i,
          'o )
        RPC_service.t ->
        'pr ->
        'a ->
        'b ->
        'c ->
        'q ->
        'i ->
        'o Error_monad.shell_tzresult Lwt.t
    end

    let make_call0 s (ctxt : _ simple) = ctxt#call_proto_service0 s

    let make_call0 = (make_call0 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

    let make_call1 s (ctxt : _ simple) = ctxt#call_proto_service1 s

    let make_call1 = (make_call1 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

    let make_call2 s (ctxt : _ simple) = ctxt#call_proto_service2 s

    let make_call2 = (make_call2 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

    let make_call3 s (ctxt : _ simple) = ctxt#call_proto_service3 s

    let make_call3 = (make_call3 : _ -> _ simple -> _ :> _ -> _ #simple -> _)

    let make_opt_call0 s ctxt block q i =
      let open Lwt_syntax in
      let* r = make_call0 s ctxt block q i in
      match r with
      | Error [Tezos_rpc.Context.Not_found _] -> Lwt.return_ok None
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return_ok (Some v)

    let make_opt_call1 s ctxt block a1 q i =
      let open Lwt_syntax in
      let* r = make_call1 s ctxt block a1 q i in
      match r with
      | Error [Tezos_rpc.Context.Not_found _] -> Lwt.return_ok None
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return_ok (Some v)

    let make_opt_call2 s ctxt block a1 a2 q i =
      let open Lwt_syntax in
      let* r = make_call2 s ctxt block a1 a2 q i in
      match r with
      | Error [Tezos_rpc.Context.Not_found _] -> Lwt.return_ok None
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return_ok (Some v)

    let make_opt_call3 s ctxt block a1 a2 a3 q i =
      let open Lwt_syntax in
      let* r = make_call3 s ctxt block a1 a2 a3 q i in
      match r with
      | Error [Tezos_rpc.Context.Not_found _] -> Lwt.return_ok None
      | Error _ as v -> Lwt.return v
      | Ok v -> Lwt.return_ok (Some v)
  end

  module Sapling = Tezos_sapling.Core.Validator

  module Micheline = struct
    include Micheline
    include Micheline_encoding

    (* The environment exposes a single canonical encoding for Micheline
       expression. Since env-V4, it is encoding-v2 because this is the most
       recent, most correct-at-time-of-writing encoding. For backwards
       compatibility reason, you should never upgrade (nor downgrade) this.
       Future fixes and improvements of the encoding should be made available in
       future environments only. *)
    let canonical_encoding ~variant encoding =
      canonical_encoding_v2 ~variant:(Param.name ^ "." ^ variant) encoding
  end

  module Updater = struct
    type nonrec validation_result = legacy_validation_result = {
      context : Context.t;
      fitness : Fitness.t;
      message : string option;
      max_operations_ttl : int;
      last_allowed_fork_level : Int32.t;
    }

    type nonrec quota = quota = {max_size : int; max_op : int option}

    type nonrec rpc_context = rpc_context = {
      block_hash : Block_hash.t;
      block_header : Block_header.shell_header;
      context : Context.t;
    }

    let activate = Context.set_protocol

    module type PROTOCOL =
      Environment_protocol_T_V7.T
        with type context := Context.t
         and type cache_value := Environment_context.Context.cache_value
         and type cache_key := Environment_context.Context.cache_key
         and type quota := quota
         and type validation_result := legacy_validation_result
         and type rpc_context := rpc_context
         and type tztrace := Error_monad.tztrace
         and type 'a tzresult := 'a Error_monad.tzresult
  end

  module Base58 = struct
    include Tezos_crypto.Base58

    let simple_encode enc s = simple_encode enc s

    let simple_decode enc s = simple_decode enc s

    include Make (struct
      type context = Context.t
    end)

    let decode s = decode s
  end

  module Context = struct
    include Context
    include Environment_context.V8

    module type PROOF_ENCODING = Tezos_context_sigs.Context.PROOF_ENCODING

    module Proof_encoding =
      Tezos_context_merkle_proof_encoding.Merkle_proof_encoding

    let complete ctxt s = Base58.complete ctxt s
  end

  module Wasm_2_0_0 = struct
    type input = Tezos_scoru_wasm.Wasm_pvm_state.input_info = {
      inbox_level : Bounded.Non_negative_int32.t;
      message_counter : Z.t;
    }

    type output = Tezos_scoru_wasm.Wasm_pvm_state.output_info = {
      outbox_level : Bounded.Non_negative_int32.t;
      message_index : Z.t;
    }

    type reveal_hash = Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.reveal_hash

    type reveal = Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.reveal =
      | Reveal_raw_data of reveal_hash
      | Reveal_metadata

    type input_request =
      | No_input_required
      | Input_required
      | Reveal_required of reveal

    type info = {
      current_tick : Z.t;
      last_input_read : input option;
      input_request : input_request;
    }

    module Make
        (Tree : Context.TREE with type key = string list and type value = bytes) =
    struct
      type Tezos_tree_encoding.tree_instance += PVM_tree of Tree.tree

      include Tezos_scoru_wasm.Wasm_pvm.Make (struct
        include Tree

        let select = function
          | PVM_tree t -> t
          | _ -> raise Tezos_tree_encoding.Incorrect_tree_type

        let wrap t = PVM_tree t
      end)

      let initial_state = initial_state V0

      let compute_step =
        compute_step ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint

      let reveal_compat reveal =
        match
          Tezos_scoru_wasm.Wasm_pvm_state.Compatibility.of_current_opt reveal
        with
        | Some r -> r
        | None ->
            (* The WASM PVM before environment V11 will not request a value
               outside of the [Compatibility.reveal] domain. As a consequence,
               the only execution path leading to executing this branch is
               by having two dishonest nodes playing against each other.

               As a consequence, it is safe to return an arbitrary value here,
               and we do so to avoid raising an exception. *)
            Reveal_raw_data "this line costs 10k XTZ to execute"

      let input_request_compat = function
        | Tezos_scoru_wasm.Wasm_pvm_state.No_input_required -> No_input_required
        | Input_required -> Input_required
        | Reveal_required req -> Reveal_required (reveal_compat req)

      let info_compat
          Tezos_scoru_wasm.Wasm_pvm_state.
            {current_tick; last_input_read; input_request} =
        {
          current_tick;
          last_input_read;
          input_request = input_request_compat input_request;
        }

      let get_info tree =
        let open Lwt_syntax in
        let+ info = get_info tree in
        info_compat info
    end
  end

  module Lift (P : Updater.PROTOCOL) = struct
    let environment_version = Protocol.V8

    let expected_context_hash = Predecessor_resulting_context

    include P

    let value_of_key ~chain_id ~predecessor_context ~predecessor_timestamp
        ~predecessor_level ~predecessor_fitness ~predecessor ~timestamp =
      let open Lwt_result_syntax in
      let*! r =
        value_of_key
          ~chain_id
          ~predecessor_context
          ~predecessor_timestamp
          ~predecessor_level
          ~predecessor_fitness
          ~predecessor
          ~timestamp
      in
      let*? f = wrap_tzresult r in
      return (fun x ->
          let*! r = f x in
          Lwt.return (wrap_tzresult r))

    (** Ensure that the cache is correctly loaded in memory
        before running any operations. *)
    let load_predecessor_cache predecessor_context chain_id mode
        (predecessor_header : Block_header.shell_header) cache =
      let open Lwt_result_syntax in
      let predecessor_hash, timestamp =
        match mode with
        | Application block_header | Partial_validation block_header ->
            (block_header.shell.predecessor, block_header.shell.timestamp)
        | Construction {predecessor_hash; timestamp; _}
        | Partial_construction {predecessor_hash; timestamp} ->
            (predecessor_hash, timestamp)
      in
      let* value_of_key =
        value_of_key
          ~chain_id
          ~predecessor_context
          ~predecessor_timestamp:predecessor_header.timestamp
          ~predecessor_level:predecessor_header.level
          ~predecessor_fitness:predecessor_header.fitness
          ~predecessor:predecessor_hash
          ~timestamp
      in
      Context.load_cache predecessor_hash predecessor_context cache value_of_key

    let begin_validation ctxt chain_id mode ~predecessor ~cache =
      let open Lwt_result_syntax in
      let* ctxt = load_predecessor_cache ctxt chain_id mode predecessor cache in
      let*! validation_state =
        begin_validation ctxt chain_id mode ~predecessor
      in
      Lwt.return (wrap_tzresult validation_state)

    let validate_operation ?check_signature validation_state oph operation =
      let open Lwt_syntax in
      let+ validation_state =
        validate_operation ?check_signature validation_state oph operation
      in
      wrap_tzresult validation_state

    let finalize_validation validation_state =
      let open Lwt_syntax in
      let+ res = finalize_validation validation_state in
      wrap_tzresult res

    let begin_application ctxt chain_id mode ~predecessor ~cache =
      let open Lwt_result_syntax in
      let* ctxt = load_predecessor_cache ctxt chain_id mode predecessor cache in
      let*! application_state =
        begin_application ctxt chain_id ~predecessor mode
      in
      Lwt.return (wrap_tzresult application_state)

    let apply_operation application_state oph operation =
      let open Lwt_syntax in
      let+ application_state =
        apply_operation application_state oph operation
      in
      wrap_tzresult application_state

    let finalize_application state shell_header =
      let open Lwt_syntax in
      let* r = finalize_application state shell_header in
      match r with
      | Ok (vr, metadata) ->
          Lwt.return_ok (lift_legacy_validation_result vr, metadata)
      | Error e -> Lwt.return (wrap_tzresult (Error e))

    let init chain_id c bh =
      let open Lwt_syntax in
      let* r = init chain_id c bh in
      match r with
      | Ok vr -> Lwt.return_ok (lift_legacy_validation_result vr)
      | Error e -> Lwt.return (wrap_tzresult (Error e))

    let set_log_message_consumer f = Logging.logging_function := Some f

    module Mempool = struct
      include Mempool

      type add_error =
        | Validation_error of Error_monad.shell_tztrace
        | Add_conflict of operation_conflict

      let unsupported_feature name =
        let msg =
          Printf.sprintf
            "The current protocol does not support the '%s' feature."
            name
        in
        [Exn (Failure msg)]

      let partial_op_validation ?check_signature:_ _ _ =
        Lwt.return_error (unsupported_feature "partial_op_validation")

      let add_valid_operation ?conflict_handler:_ _ _ =
        Error (Validation_error (unsupported_feature "add_valid_operation"))

      let add_operation ?check_signature ?conflict_handler info mempool op :
          (t * add_result, add_error) result Lwt.t =
        let open Lwt_syntax in
        let+ r =
          Mempool.add_operation
            ?check_signature
            ?conflict_handler
            info
            mempool
            op
        in
        match r with
        | Ok v -> Ok v
        | Error (Mempool.Validation_error e) ->
            Error (Validation_error (wrap_tztrace e))
        | Error (Mempool.Add_conflict c) -> Error (Add_conflict c)

      let init ctxt chain_id ~head_hash ~head ~cache =
        let open Lwt_result_syntax in
        let* ctxt =
          load_predecessor_cache
            ctxt
            chain_id
            (Partial_construction
               {
                 predecessor_hash = head_hash;
                 timestamp = head.Block_header.timestamp;
               })
            head
            cache
        in
        let*! r = init ctxt chain_id ~head_hash ~head in
        Lwt.return (wrap_tzresult r)
    end
  end

  class ['chain, 'block] proto_rpc_context (t : Tezos_rpc.Context.t)
    (prefix : (unit, (unit * 'chain) * 'block) RPC_path.t) =
    object
      method call_proto_service0 :
          'm 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            RPC_context.t,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'chain * 'block ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s (chain, block) q i ->
          let s = RPC_service.subst0 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s (((), chain), block) q i

      method call_proto_service1 :
          'm 'a 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            RPC_context.t * 'a,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'chain * 'block ->
          'a ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s (chain, block) a1 q i ->
          let s = RPC_service.subst1 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s ((((), chain), block), a1) q i

      method call_proto_service2 :
          'm 'a 'b 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            (RPC_context.t * 'a) * 'b,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'chain * 'block ->
          'a ->
          'b ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s (chain, block) a1 a2 q i ->
          let s = RPC_service.subst2 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s (((((), chain), block), a1), a2) q i

      method call_proto_service3 :
          'm 'a 'b 'c 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            ((RPC_context.t * 'a) * 'b) * 'c,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'chain * 'block ->
          'a ->
          'b ->
          'c ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s (chain, block) a1 a2 a3 q i ->
          let s = RPC_service.subst3 s in
          let s = RPC_service.prefix prefix s in
          t#call_service s ((((((), chain), block), a1), a2), a3) q i
    end

  class ['block] proto_rpc_context_of_directory conv dir :
    ['block] RPC_context.simple =
    let lookup = new Tezos_rpc.Context.of_directory dir in
    object
      method call_proto_service0 :
          'm 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            RPC_context.t,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'block ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s block q i ->
          let rpc_context = conv block in
          lookup#call_service s rpc_context q i

      method call_proto_service1 :
          'm 'a 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            RPC_context.t * 'a,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'block ->
          'a ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s block a1 q i ->
          let rpc_context = conv block in
          lookup#call_service s (rpc_context, a1) q i

      method call_proto_service2 :
          'm 'a 'b 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            (RPC_context.t * 'a) * 'b,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'block ->
          'a ->
          'b ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s block a1 a2 q i ->
          let rpc_context = conv block in
          lookup#call_service s ((rpc_context, a1), a2) q i

      method call_proto_service3 :
          'm 'a 'b 'c 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            RPC_context.t,
            ((RPC_context.t * 'a) * 'b) * 'c,
            'q,
            'i,
            'o )
          RPC_service.t ->
          'block ->
          'a ->
          'b ->
          'c ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
        fun s block a1 a2 a3 q i ->
          let rpc_context = conv block in
          lookup#call_service s (((rpc_context, a1), a2), a3) q i
    end

  module Equality_witness = Environment_context.Equality_witness
  module Plonk = Tezos_protocol_environment_structs.V8.Plonk

  module Dal = struct
    include Tezos_crypto_dal.Cryptobox.Verifier

    let verify_page t commitment ~page_index page page_proof =
      match verify_page t commitment ~page_index page page_proof with
      | Error `Page_length_mismatch -> Error `Page_length_mismatch
      | Error `Page_index_out_of_range -> Error `Segment_index_out_of_range
      | Error (`Invalid_page | `Invalid_degree_strictly_less_than_expected _) ->
          Ok false
      | Ok () -> Ok true
  end
end
