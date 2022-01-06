(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type size = int

type index = int

type identifier = string

type key = {identifier : identifier; cache_index : index}

let key_encoding =
  Data_encoding.(
    conv
      (fun key -> (key.identifier, key.cache_index))
      (fun (identifier, cache_index) -> {identifier; cache_index})
      (tup2 string int16))

module Key = struct
  type t = key

  let compare k1 k2 = String.compare k1.identifier k2.identifier
end

module KeyMap = Map.Make (Key)
module KeySet = Set.Make (Key)

type value_metadata = {size : int; birth : int64; cache_nonce : Bytes.t}

let value_metadata_encoding : value_metadata Data_encoding.t =
  Data_encoding.(
    conv
      (fun entry -> (entry.size, entry.birth, entry.cache_nonce))
      (fun (size, birth, cache_nonce) -> {size; birth; cache_nonce})
      (tup3 int31 int64 Variable.bytes))

let pp_entry ppf (entry : value_metadata) =
  Format.fprintf
    ppf
    "%d/%Ld/%a"
    entry.size
    entry.birth
    Hex.pp
    (Hex.of_bytes entry.cache_nonce)

module Int64Map = Map.Make (Int64)

type 'a cache = {
  (* Each cache has a handle in the context caches. *)
  index : index;
  (* [map] collects the cache entries. *)
  map : ('a * value_metadata) KeyMap.t;
  (* [lru] maintains a fast index from [birth] to entries. In
     particular, it provides a logarithmic access to the Least
     Recently Used entry. *)
  lru : key Int64Map.t;
  (* [size] is the sum of all entry sizes. *)
  size : int;
  (* [limit] is the maximal size of the cache in memory.  This [limit]
     MUST be greater than any entry size added in cache. This assumption
     is used for the correctness of the implementation. We enforce
     this property by preventing any too large entry from entering
     the cache. Similarly, we enforce the invariant that no entry of null
     size can enter the cache. *)
  limit : int;
  (* [counter] is the maximal age of entries that have been inserted
     in the cache since its creation. Assuming 100_000 new entries per
     second, [counter] will not overflow before ~3 million years. *)
  counter : int64;
  (* [removed_entries] maintains the keys removed since last
     synchronization. *)
  removed_entries : KeySet.t;
  (* [entries_removals] maintains the last numbers of entries removal
     per block. This list cannot be longer than
     [entries_removals_window_width]. *)
  entries_removals : int list;
}

type 'a t = 'a cache FunctionalArray.t option

let string_of_key {identifier; _} = identifier

let pp_cache fmt {index; map; size; limit; counter; _} =
  Format.fprintf
    fmt
    "@[<v 0>Index: %d@,Cardinal: %d@,Size limit: %d@,Size: %d@,Counter: %Ld%a@]"
    index
    (KeyMap.cardinal map)
    limit
    size
    counter
    (fun ppf map ->
      KeyMap.iter
        (fun k (_, entry) ->
          Format.fprintf ppf "@,Element %s: %a" (string_of_key k) pp_entry entry)
        map)
    map

let invalid_arg_with_callstack msg =
  let cs = Printexc.get_callstack 15 in
  Format.kasprintf
    invalid_arg
    "Internal error: %s\nCall stack:\n%s\n"
    msg
    (Printexc.raw_backtrace_to_string cs)

let with_caches cache f =
  match cache with
  | None -> invalid_arg_with_callstack "uninitialized caches"
  | Some caches -> f caches

let cache_of_index t index =
  with_caches t (fun caches -> FunctionalArray.get caches index)

let cache_of_key caches key = cache_of_index caches key.cache_index

let lookup_entry cache key = KeyMap.find key cache.map

let lookup_value cache key =
  match lookup_entry cache key with Some (e, _) -> Some e | None -> None

let lookup t key = lookup_entry (cache_of_key t key) key

let update_cache_with t index cache =
  with_caches t (fun caches -> Some (FunctionalArray.set caches index cache))

let empty_cache =
  {
    index = -1;
    map = KeyMap.empty;
    lru = Int64Map.empty;
    size = 0;
    counter = 0L;
    removed_entries = KeySet.empty;
    entries_removals = [];
    limit = -1;
  }

let make_caches (layout : size list) =
  let default = FunctionalArray.make (List.length layout) empty_cache in
  let folder index array limit =
    FunctionalArray.set array index {empty_cache with limit; index}
  in
  List.fold_left_i folder default layout

(*

   When an entry is fresh, it is assigned a [fresh_entry_nonce].

   The actual nonce for this entry will be known only when its block
   is finalized: it is only in function [sync] that
   [fresh_entry_nonce] is substituted by a valid [nonce].

*)
let fresh_entry_nonce = Bytes.of_string "__FRESH_ENTRY_NONCE__"

let remove_cache_entry cache key entry =
  {
    cache with
    map = KeyMap.remove key cache.map;
    size = cache.size - entry.size;
    lru = Int64Map.remove entry.birth cache.lru;
    removed_entries = KeySet.add key cache.removed_entries;
  }

(* The dean is the oldest entry.

   The complexity of this operation is logarithmic in the number of
   entries in the cache. Along a given chain, [dean cache] only
   increases. *)
let dean cache : (int64 * key) option = Int64Map.min_binding cache.lru

let remove_dean cache =
  match dean cache with
  | None ->
      (* This case is unreachable because [remove_dean] is always called
         by [enforce_size_limit] with a nonempty cache. *)
      cache
  | Some (_, key) -> (
      match KeyMap.find key cache.map with
      | None -> assert false
      (* because [lru] must point to keys that are in [map]. *)
      | Some (_, entry) -> remove_cache_entry cache key entry)

let rec enforce_size_limit cache =
  if cache.size > cache.limit then
    remove_dean cache
    (* [size] has decreased strictly because if size > limit, then the
       cache cannot be empty. Hence, this recursive call will
       converge. *)
    |> enforce_size_limit
  else cache

let insert_cache_entry cache key ((_, {size; birth; _}) as entry) =
  {
    cache with
    map = KeyMap.add key entry cache.map;
    size = cache.size + size;
    counter = max cache.counter birth;
    lru = Int64Map.add birth key cache.lru;
    removed_entries = KeySet.remove key cache.removed_entries;
  }
  |> enforce_size_limit

let insert_cache cache key value size cache_nonce =
  (* Conforming to entry size invariant: we need this size to be
     strictly positive. *)
  let size = max 1 size in
  let entry = {size; birth = Int64.add cache.counter 1L; cache_nonce} in
  insert_cache_entry cache key (value, entry)

let update_cache cache key entry =
  let cache =
    match lookup_entry cache key with
    | None -> cache
    | Some (_, old_entry) -> remove_cache_entry cache key old_entry
  in
  match entry with
  | None -> cache
  | Some (entry, size) -> insert_cache cache key entry size fresh_entry_nonce

let update t key entry =
  let cache = cache_of_key t key in
  update_cache_with t key.cache_index (update_cache cache key entry)

(* We maintain the number of entries removal for the last
   [entries_removals_window_width] blocks to determine the life
   expectancy of cache entries. *)
let entries_removals_window_width = 5

let median_entries_removals cache =
  let median l = List.(nth (sort Int.compare l) (length l / 2)) in
  match median cache.entries_removals with None -> 0 | Some x -> x

let uninitialised = None

let key_of_identifier ~cache_index identifier = {identifier; cache_index}

let identifier_of_key {identifier; _} = identifier

let pp fmt = function
  | None -> Format.fprintf fmt "Unitialised cache"
  | Some caches -> FunctionalArray.iter (pp_cache fmt) caches

let find t key = lookup_value (cache_of_key t key) key

let compatible_layout t layout =
  with_caches t (fun caches ->
      Compare.List_length_with.(layout = FunctionalArray.length caches)
      && List.fold_left_i
           (fun idx r len -> r && (FunctionalArray.get caches idx).limit = len)
           true
           layout)

let from_layout layout = Some (make_caches layout)

let future_cache_expectation t ~time_in_blocks =
  Some
    (with_caches t (fun caches ->
         FunctionalArray.map
           (fun cache ->
             let oldness = time_in_blocks * median_entries_removals cache in
             Utils.fold_n_times oldness remove_dean cache)
           caches))

let record_entries_removals cache =
  let entries_removals =
    if
      List.compare_length_with
        cache.entries_removals
        entries_removals_window_width
      >= 0
    then
      match cache.entries_removals with
      | [] -> assert false
      | _ :: entries_removals -> entries_removals
    else cache.entries_removals
  in
  let entries_removals =
    entries_removals @ [KeySet.cardinal cache.removed_entries]
  in
  {cache with entries_removals; removed_entries = KeySet.empty}

(* [update_entry ctxt cache key entry nonce] stores the [entry]
   identified by [key] in a [cache] of the context. Each fresh entry
   is marked with the [nonce] to characterize the block that has
   introduced it. *)
let update_entry entry nonce =
  let element_nonce =
    if Bytes.equal entry.cache_nonce fresh_entry_nonce then nonce
    else entry.cache_nonce
  in
  {entry with cache_nonce = element_nonce}

(* [finalize_cache ctxt cache nonce] sets the cache nonce for the new
   entries. This function returns the cache for the next block. *)
let finalize_cache ({map; _} as cache) nonce =
  let map = KeyMap.map (fun (e, entry) -> (e, update_entry entry nonce)) map in
  let metamap = KeyMap.map snd map in
  ({cache with map}, metamap)

(**

   A subcache has a domain composed of:

   - [keys] to restore the in-memory representation of the subcache at
     loading time ;

   - [counter] to restart the generation of "birth dates" for new entries
     at the right counter.

   [counter] is important because restarting from [0] does not work.
   Indeed, a baker that reloads the cache from the domain must be
   able to reconstruct the exact same cache as the validator. The
   validator maintains a cache in memory by inheriting it from the
   predecessor block: hence its counter is never reset.

*)
type subcache_domain = {keys : value_metadata KeyMap.t; counter : int64}

type domain = subcache_domain list

let sync_cache cache ~cache_nonce =
  let cache = enforce_size_limit cache in
  let cache = record_entries_removals cache in
  let (cache, new_entries) = finalize_cache cache cache_nonce in
  (cache, {keys = new_entries; counter = cache.counter})

let subcache_keys_encoding : value_metadata KeyMap.t Data_encoding.t =
  Data_encoding.(
    conv
      KeyMap.bindings
      (fun b -> KeyMap.of_seq (List.to_seq b))
      (list (dynamic_size (tup2 key_encoding value_metadata_encoding))))

let subcache_domain_encoding : subcache_domain Data_encoding.t =
  Data_encoding.(
    conv
      (fun {keys; counter} -> (keys, counter))
      (fun (keys, counter) -> {keys; counter})
      (obj2 (req "keys" subcache_keys_encoding) (req "counter" int64)))

let domain_encoding : domain Data_encoding.t =
  Data_encoding.(list subcache_domain_encoding)

let empty_domain = List.is_empty

let sync t ~cache_nonce =
  with_caches t @@ fun caches ->
  FunctionalArray.fold_map
    (fun acc cache ->
      let (cache, domain) = sync_cache cache ~cache_nonce in
      (domain :: acc, cache))
    caches
    []
    empty_cache
  |> fun (rev_domains, caches) -> (Some caches, List.rev rev_domains)

let update_cache_key t key value meta =
  with_caches t @@ fun caches ->
  let cache = FunctionalArray.get caches key.cache_index in
  let cache = insert_cache_entry cache key (value, meta) in
  update_cache_with t key.cache_index cache

let clear_cache cache =
  {
    index = cache.index;
    limit = cache.limit;
    map = KeyMap.empty;
    size = 0;
    counter = 0L;
    lru = Int64Map.empty;
    entries_removals = [];
    removed_entries = KeySet.empty;
  }

let clear t =
  Some (with_caches t (fun caches -> FunctionalArray.map clear_cache caches))

let from_cache initial domain ~value_of_key =
  let domain' = Array.of_list domain in
  let cache =
    with_caches (clear initial) @@ fun caches ->
    FunctionalArray.mapi
      (fun i (cache : 'a cache) ->
        if i = -1 then cache
        else if i >= Array.length domain' then
          (* By precondition: the layout of [domain] and [initial]
               must be the same. *)
          invalid_arg_with_callstack "invalid usage of from_cache"
        else
          let subdomain = domain'.(i) in
          {cache with counter = subdomain.counter})
      caches
  in
  let fold_cache_keys subdomain cache =
    KeyMap.fold_es
      (fun key entry cache ->
        (match lookup initial key with
        | None -> value_of_key key
        | Some (value, entry') ->
            if Bytes.equal entry.cache_nonce entry'.cache_nonce then
              return value
            else value_of_key key)
        >>=? fun value -> return (update_cache_key cache key value entry))
      subdomain.keys
      cache
  in
  List.fold_left_es
    (fun cache subdomain -> fold_cache_keys subdomain cache)
    (Some cache)
    domain

let number_of_caches t = with_caches t FunctionalArray.length

let on_cache t cache_index f =
  if cache_index < number_of_caches t && cache_index >= 0 then
    Some (f (cache_of_index t cache_index))
  else None

let cache_size t ~cache_index =
  on_cache t cache_index @@ fun cache -> cache.size

let cache_size_limit t ~cache_index =
  on_cache t cache_index @@ fun cache -> cache.limit

let list_keys t ~cache_index =
  on_cache t cache_index @@ fun cache ->
  let xs =
    KeyMap.fold
      (fun k (_, {size; birth; _}) acc -> (k, size, birth) :: acc)
      cache.map
      []
  in
  xs
  |> List.sort (fun (_, _, b1) (_, _, b2) -> Int64.compare b1 b2)
  |> List.map (fun (k, s, _) -> (k, s))

let key_rank ctxt key =
  let cache = cache_of_key ctxt key in
  let rec length_until x n = function
    | [] -> Some n
    | y :: ys ->
        if Key.compare x y = 0 then Some n else length_until x (n + 1) ys
  in
  if not @@ KeyMap.mem key cache.map then None
  else Int64Map.bindings cache.lru |> List.map snd |> length_until key 0
