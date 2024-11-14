open Brassaia

(** A store in which all values are stored as immediates inside their respective
    keys. The store itself keeps no information, except for the bookkeeping
    needed to handle [clear]-ing the in-memory keys. *)
module Keyed_by_value = struct
  type (_, 'v) key = { value : 'v }

  module Key (Hash : Hash.S) (Value : Type.S) = struct
    type t = (Hash.t, Value.t) key
    type hash = Hash.t [@@deriving brassaia ~pre_hash]
    type value = Value.t [@@deriving brassaia ~pre_hash]

    let value_to_hash t = Hash.hash (fun f -> pre_hash_value t f)
    let to_hash t = value_to_hash t.value

    let (t : t Type.t) =
      let open Type in
      map
        ~pre_hash:(fun t f ->
          let hash = value_to_hash t.value in
          pre_hash_hash hash f)
        Value.t
        (fun _ ->
          Alcotest.fail ~pos:__POS__ "Key implementation is non-serialisable")
        (fun t -> t.value)

    let pp ppf kt = Type.(pp t) ppf kt

    let encoding =
      Data_encoding.conv (Type.to_string t)
        Brassaia.Type.(
          of_string_exn
            ~path:
              "test/brassaia/generic_key/test_inlined_contents.ml/Keyed_by_value/of_string"
            t)
        Data_encoding.string
  end

  module Make (Hash : Hash.S) (Value : Type.S) = struct
    type _ t = { instance : unit option ref }
    type hash = Hash.t
    type value = Value.t

    module Key = Key (Hash) (Value)

    type key = Key.t

    let check_not_closed t =
      match !(t.instance) with None -> raise Closed | Some t -> t

    let init _ = Lwt.return { instance = ref (Some ()) }

    let mem t _ =
      let _ = check_not_closed t in
      Lwt.return_true

    let unsafe_add t _ value =
      let _ = check_not_closed t in
      Lwt.return { value }

    let add t v = unsafe_add t () v

    let find t k =
      let _ = check_not_closed t in
      Lwt.return_some k.value

    let index _ _ = Lwt.return_none
    let batch t f = f (t :> Perms.read_write t)

    let close t =
      t.instance := None;
      Lwt.return_unit
  end
end

module Plain = struct
  type 'h key = 'h

  module Key = Key.Of_hash

  module Make (H : Hash.S) (V : Type.S) = struct
    module CA =
      Content_addressable.Check_closed (Brassaia_mem.Content_addressable) (H)
        (V)

    include Indexable.Of_content_addressable (H) (CA)

    let init = CA.init
  end
end

module Store_maker = Generic_key.Maker (struct
  module Contents_store = Keyed_by_value
  module Node_store = Plain
  module Commit_store = Plain
  module Branch_store = Atomic_write.Check_closed (Brassaia_mem.Atomic_write)
end)

module Store = Store_maker.Make (Schema.KV (Contents.String))

let suite =
  let store =
    (module Store : Brassaia_test_helpers.Brassaia_test.Generic_key)
  in
  let config = Brassaia_mem.config () in
  Brassaia_test_helpers.Brassaia_test.Suite.create_generic_key
    ~name:"inlined_contents" ~store ~config ~import_supported:false ()
