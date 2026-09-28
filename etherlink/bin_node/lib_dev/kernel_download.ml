(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Reveal_hash = Tezos_raw_protocol_alpha.Sc_rollup_reveal_hash

type error += Invalid_preimage_for_hash of Hex.t * string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_invalid_preimage"
    ~title:"Preimage has not the expected hash"
    ~description:
      "The EVM node could not apply a blueprint on top of its local EVM state."
    ~pp:(fun ppf (hash, _preimage) ->
      Format.fprintf
        ppf
        "The preimage received for %s doesn't return the same hash"
        hash)
    Data_encoding.(obj2 (req "expected_hash" string) (req "preimage" string))
    (function
      | Invalid_preimage_for_hash (`Hex hash, preimage) -> Some (hash, preimage)
      | _ -> None)
    (fun (hash, preimage) -> Invalid_preimage_for_hash (`Hex hash, preimage))

type error += Cannot_fetch_preimage of Hex.t * int

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_cannot_fetch_preimage"
    ~title:"Preimage could not be fetched"
    ~description:
      "The EVM node could not fetch a preimage from the preimages endpoint."
    ~pp:(fun ppf (hash, status) ->
      Format.fprintf
        ppf
        "The preimages endpoint answered %d for preimage %s"
        status
        hash)
    Data_encoding.(obj2 (req "hash" string) (req "status" int31))
    (function
      | Cannot_fetch_preimage (`Hex hash, status) -> Some (hash, status)
      | _ -> None)
    (fun (hash, status) -> Cannot_fetch_preimage (`Hex hash, status))

type preimages = Contents of bytes | Hashes of string list

let preimages_encoding =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"content"
          (Tag 0)
          bytes
          (function Contents _payload -> assert false | _ -> None)
          (fun payload -> Contents payload);
        case
          ~title:"hashes"
          (Tag 1)
          (list Reveal_hash.encoding)
          (function Hashes _hashes -> assert false | _ -> None)
          (fun hashes -> Hashes (List.map Reveal_hash.to_hex hashes));
      ])

let check_preimage (`Hex hash) preimage =
  let computed_hash =
    Reveal_hash.hash_string ~scheme:Reveal_hash.Blake2B [preimage]
    |> Reveal_hash.to_hex
  in
  hash = computed_hash

let delete_preimage preimages (`Hex hash) =
  Lwt_unix.unlink (Filename.concat preimages hash)

(* Runs [k] with a pool of connections onto [endpoint], closing them on the way
   out whether [k] succeeded or not. *)
let with_connection_pool ~n endpoint k =
  let pool = Octez_connpool.make ~n endpoint in
  Lwt.finalize (fun () -> k pool) (fun () -> Octez_connpool.clear pool)

(* Reads a preimage from the [preimages] directory if it is already there, and
   fetches it from the endpoint otherwise, writing it down on the way. The
   requests go through a pool of connections kept open from one preimage to
   the next: a kernel is a couple of thousand of them, so opening a connection
   -- and, over https, running a handshake -- for each one is a cost of its
   own, on top of the round-trip it is paid for. *)
let fetch_preimage pool ~preimages (`Hex hash) =
  let open Lwt_result_syntax in
  let path = Filename.concat preimages hash in
  let*! known = Lwt_unix.file_exists path in
  if known then
    let*! preimage = Lwt_utils_unix.read_file path in
    return preimage
  else
    let* response, preimage = Octez_connpool.get pool hash in
    match response.status with
    | `OK ->
        (* Written through a temporary file and renamed into place. A plain
           write truncates the target as it opens it, and the open and the
           write are two scheduling points apart, so a fetch interrupted in
           between -- cancelled, or the process killed -- would leave an empty
           file under a name that is a valid preimage hash, which the kernel's
           own reveal rejects without removing it, leaving the node to fail on
           it again at every attempt. The rename is atomic, and the temporary file is created
           in the preimages directory itself, so it is on the same partition
           and its name, ending in [.tmp], cannot be taken for a preimage. *)
        let*! written =
          Lwt_utils_unix.with_atomic_open_out path (fun fd ->
              Lwt_utils_unix.write_string fd preimage)
        in
        let*? () =
          match written with
          | Ok () -> Ok ()
          | Error io_error -> Lwt_utils_unix.tzfail_of_io_error io_error
        in
        return preimage
    | status ->
        tzfail
          (Cannot_fetch_preimage (`Hex hash, Cohttp.Code.code_of_status status))

let rec reveal_and_check pool ~preimages ~num_download_retries hash =
  let open Lwt_result_syntax in
  let* preimage = fetch_preimage pool ~preimages hash in
  if not (check_preimage hash preimage) then
    (* The preimage has been written to the directory before being checked, so
       it is deleted before giving up too: a body that does not hash to its own
       name must not be left behind under that name, where whatever reads the
       directory next would take it for the real thing. *)
    let*! () = delete_preimage preimages hash in
    if num_download_retries <= 0 then
      tzfail (Invalid_preimage_for_hash (hash, preimage))
    else
      reveal_and_check
        pool
        ~preimages
        ~num_download_retries:(pred num_download_retries)
        hash
  else return preimage

(* The preimages of a kernel form a balanced tree: the content pages holding
   the kernel itself are the leaves, and each level above them lists the hashes
   of the level below (see [prepare_preimages] in the kernel SDK). Walking that
   tree breadth-first therefore reaches every hash page before the first
   content page, so the number of preimages the kernel is made of is known as
   soon as a content page is reached: the hash pages downloaded so far, plus
   the content pages of the last level. *)
let download ~preimages_endpoint ~preimages ~(root_hash : Hex.t)
    ?(num_download_retries = 1) ?(concurrency = 1) ?(progress = false) () =
  let open Lwt_result_syntax in
  (* [Octez_connpool.make] raises on a pool of zero connections, and it is
     called before anything that would turn that into an error. *)
  let*? () =
    error_when
      Compare.Int.(concurrency < 1)
      (error_of_fmt "concurrency must be at least 1, got %d." concurrency)
  in
  with_connection_pool ~n:concurrency preimages_endpoint @@ fun pool ->
  (* Decoding a page that is not one raises, and so does reading a preimage
     file that disappeared under us. Both are turned into errors here: with
     several fetches in flight, an escaping exception would leave the other
     workers running against an endpoint that is already known to be failing. *)
  let fetch (`Hex hex as hash) =
    protect @@ fun () ->
    let* preimage =
      reveal_and_check pool ~preimages ~num_download_retries hash
    in
    match Data_encoding.Binary.of_string preimages_encoding preimage with
    | Ok page -> return page
    | Error err ->
        failwith
          "Preimage %s is not a valid kernel page: %a"
          hex
          Data_encoding.Binary.pp_read_error
          err
  in
  let hashes_of_page hashes = List.map (fun hash -> `Hex hash) hashes in
  (* Downloads the levels of hash pages, from the root down. Returns how many
     pages were downloaded, together with the hashes of the last level, which
     are the content pages. *)
  let rec download_hash_pages ~downloaded ~level ~next_level =
    match (level, next_level) with
    | [], [] -> return (downloaded, [])
    | [], _ :: _ ->
        (download_hash_pages [@tailcall])
          ~downloaded
          ~level:(List.rev next_level)
          ~next_level:[]
    | hash :: level, _ -> (
        let* page = fetch hash in
        let downloaded = downloaded + 1 in
        match page with
        | Hashes page_hashes ->
            (download_hash_pages [@tailcall])
              ~downloaded
              ~level
              ~next_level:
                (List.rev_append (hashes_of_page page_hashes) next_level)
        | Contents _content -> return (downloaded, level))
  in
  (* Downloads the content pages of the last level, [concurrency] of them at a
     time. Fetching a preimage is a request to a remote endpoint followed by a
     hash check over 4kB, so the download is bound by the latency of the
     endpoint rather than by anything this process computes: overlapping the
     requests is what makes it faster, and Lwt concurrency is enough for that.

     The pages are only safe to fetch out of order because the walk above has
     already revealed every hash page: what is left is a flat list of
     independent content pages, each stored under the name of its own hash.

     The connection pool already holds the number of requests in flight down to
     its size. The tokens are what bounds the rest of a fetch: a preimage that
     is already on disk is read without going through the pool at all, and
     [iter_ep] starts every fetch at once, so a preimages directory that is
     already populated would otherwise open one file per page in one go. *)
  let download_content_pages ~concurrency report hashes =
    let tokens = Lwt_pool.create concurrency (fun () -> Lwt.return_unit) in
    (* A page of hashes is not expected at this level, but is walked rather than
       dropped so that an unbalanced tree still yields a complete kernel instead
       of one silently missing preimages -- only the total the progress bar
       counts towards is then off. *)
    let rec download_page hash =
      (* [protect] covers the report as well as the fetch: the reporter writes
         to the terminal, so it rejects on a closed standard error, and a
         rejection here would escape [iter_ep] instead of stopping it. *)
      protect @@ fun () ->
      let* page = Lwt_pool.use tokens (fun () -> fetch hash) in
      let*! () = report 1 in
      match page with
      | Contents _content -> return_unit
      | Hashes page_hashes ->
          List.iter_ep download_page (hashes_of_page page_hashes)
    in
    let downloads = List.map download_page hashes in
    let all = List.iter_ep Fun.id downloads in
    (* Every download is started at once, and [Lwt.all] below would wait for
       all of them: without this, an endpoint that just failed a request would
       still be asked for the two thousand pages in flight and queued behind
       the tokens. The first failure cancels the rest instead. Both pools cope
       with that: a waiter queued on [Lwt_pool.use] is removed when cancelled,
       and one cancelled while holding a connection releases it through the
       handler [Octez_connpool] wraps its calls in. *)
    let first_error = ref None in
    List.iter
      (fun download ->
        Lwt.on_success download (function
          | Ok () -> ()
          | Error trace ->
              (* Only the first failure cancels, and it cancels the aggregate
                 rather than the downloads one by one: [iter_ep] is a map over
                 [Lwt.all], and bind, join and map each propagate cancellation
                 to what they wait on, so one cancel reaches all of them. *)
              if Option.is_none !first_error then (
                first_error := Some trace ;
                Lwt.cancel all)))
      downloads ;
    (* The cancelled downloads resolve on [Exn Lwt.Canceled], which says
       nothing; the error reported is the one that stopped the download. *)
    let*! (_ : unit tzresult) = all in
    match !first_error with
    | None -> return_unit
    | Some trace -> Lwt.return (Error trace)
  in
  (* [Progress_bar.Lwt]'s reporter is not safe to call concurrently: it renders
     the bar into a formatter it shares with the cursor bookkeeping of the line
     it is redrawing, and writing that line to the terminal yields. Two workers
     reporting at once interleave a half-drawn bar with the cursor move meant
     to overwrite it, which shows on the terminal as duplicated lines and a
     count that goes backwards. *)
  let serialized report =
    let mutex = Lwt_mutex.create () in
    fun n -> Lwt_mutex.with_lock mutex (fun () -> report n)
  in
  let* downloaded, content_pages =
    let listing =
      download_hash_pages ~downloaded:0 ~level:[root_hash] ~next_level:[]
    in
    if progress then
      Progress_bar.Lwt.with_background_spinner
        ~no_tty_quiet:true
        ~message:"Listing the preimages of the kernel"
        listing
    else listing
  in
  let* () =
    if progress then
      let total = downloaded + List.length content_pages in
      Progress_bar.Lwt.with_reporter
        (Progress_bar.progress_bar
           ~update_interval:0.5
           ~message:"Downloading kernel"
           ~counter:`Int
           total)
        (fun report ->
          let*! () = report downloaded in
          download_content_pages ~concurrency (serialized report) content_pages)
    else
      download_content_pages
        ~concurrency
        (fun _ -> Lwt.return_unit)
        content_pages
  in
  let*! () = Events.predownload_kernel root_hash in
  return_unit
