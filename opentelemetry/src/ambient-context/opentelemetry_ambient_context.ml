module TLS = Thread_local_storage
include Opentelemetry_ambient_context_types

type 'a key = int * 'a Hmap.key

let debug =
  match Sys.getenv_opt "OCAML_AMBIENT_CONTEXT_DEBUG" with
  | Some ("1" | "true") -> true
  | _ -> false

let _debug_id_ = Atomic.make 0

let[@inline] generate_debug_id () = Atomic.fetch_and_add _debug_id_ 1

let compare_key : int -> int -> int = Stdlib.compare

module Storage_tls_hmap = struct
  let[@inline] ( let* ) o f =
    match o with
    | None -> None
    | Some x -> f x

  let key : Hmap.t TLS.t = Hmap_key_.key

  let name = "Storage_tls"

  let[@inline] get_map () = TLS.get_opt key

  let[@inline] with_map m cb =
    let old = TLS.get_opt key |> Option.value ~default:Hmap.empty in
    TLS.set key m;
    Fun.protect ~finally:(fun () -> TLS.set key old) cb

  let create_key = Hmap.Key.create

  let get k =
    let* context = get_map () in
    Hmap.find k context

  let with_binding k v cb =
    let new_context =
      match get_map () with
      | None -> Hmap.singleton k v
      | Some old_context -> Hmap.add k v old_context
    in
    with_map new_context @@ fun _context -> cb ()

  let without_binding k cb =
    match get_map () with
    | None -> cb ()
    | Some old_context ->
      let new_context = Hmap.rem k old_context in
      with_map new_context @@ fun _context -> cb ()
end

let default_storage : storage = (module Storage_tls_hmap)

let k_current_storage : storage TLS.t = TLS.create ()

let get_current_storage () =
  match TLS.get_exn k_current_storage with
  | v -> v
  | exception TLS.Not_set ->
    let v = default_storage in
    TLS.set k_current_storage v;
    v

let create_key () =
  let (module Store : STORAGE) = get_current_storage () in
  if not debug then
    0, Store.create_key ()
  else (
    let id = generate_debug_id () in
    Printf.printf "%s: create_key %i\n%!" Store.name id;
    id, Store.create_key ()
  )

let get (id, k) =
  let (module Store : STORAGE) = get_current_storage () in
  if not debug then
    Store.get k
  else (
    let rv = Store.get k in
    (match rv with
    | Some _ -> Printf.printf "%s: get %i -> Some\n%!" Store.name id
    | None -> Printf.printf "%s: get %i -> None\n%!" Store.name id);
    rv
  )

let with_binding : 'a key -> 'a -> (unit -> 'r) -> 'r =
 fun (id, k) v cb ->
  let (module Store : STORAGE) = get_current_storage () in
  if not debug then
    Store.with_binding k v cb
  else (
    Printf.printf "%s: with_binding %i enter\n%!" Store.name id;
    let rv = Store.with_binding k v cb in
    Printf.printf "%s: with_binding %i exit\n%!" Store.name id;
    rv
  )

let without_binding (id, k) cb =
  let (module Store : STORAGE) = get_current_storage () in
  if not debug then
    Store.without_binding k cb
  else (
    Printf.printf "%s: without_binding %i enter\n%!" Store.name id;
    let rv = Store.without_binding k cb in
    Printf.printf "%s: without_binding %i exit\n%!" Store.name id;
    rv
  )

let set_storage_provider store_new =
  let store_before = get_current_storage () in
  if store_new == store_before then
    ()
  else
    TLS.set k_current_storage store_new;
  if debug then (
    let (module Store_before : STORAGE) = store_before in
    let (module Store_new : STORAGE) = store_new in
    Printf.printf "set_storage_provider %s (previously %s)\n%!" Store_new.name
      Store_before.name
  )
