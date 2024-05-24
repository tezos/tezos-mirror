include Checks_intf
open! Import

module Make (K : Data.Key) (V : Data.Value) (Platform : Platform_args) = struct
  open Platform
  module Entry = Data.Entry.Make (K) (V)

  module IO = struct
    include Io.Extend (IO)

    (** This module never makes persistent changes *)
    let v = v_readonly

    let page_size = Int63.of_int (Entry.encoded_size * 1000)

    let iter ?min ?max f =
      iter ?min ?max ~page_size (fun ~off ~buf ~buf_off ->
          let entry = Entry.decode buf buf_off in
          f off entry;
          Entry.encoded_size)

    let read_entry io off =
      let buf = Bytes.create Entry.encoded_size in
      let (_ : int) = IO.read io ~off ~len:Entry.encoded_size buf in
      Entry.decode (Bytes.unsafe_to_string buf) 0
  end

  type size = Bytes of int63 [@@deriving repr]

  let size_t =
    let pp = Fmt.using (fun (Bytes b) -> b) Progress.Units.Bytes.pp_int63 in
    Repr.like
      ~json:
        ( (fun e t ->
            ignore @@ Jsonm.encode e (`Lexeme (`String (Fmt.to_to_string pp t)))),
          fun _ -> assert false )
      size_t

  let path =
    let open Cmdliner.Arg in
    required
    @@ pos 0 (some string) None
    @@ info ~doc:"Path to the Index store on disk" ~docv:"PATH" []

  module Stat = struct
    type io = {
      size : size;
      offset : int64;
      generation : int64;
      fanout_size : size;
    }
    [@@deriving repr]

    type files = {
      data : io option;
      log : io option;
      log_async : io option;
      merge : io option;
      lock : string option;
    }
    [@@deriving repr]

    type t = { entry_size : size; files : files } [@@deriving repr]

    let with_io : type a. string -> (IO.t -> a) -> a option =
     fun path f ->
      match IO.v path with
      | Error `No_file_on_disk -> None
      | Ok io ->
          let a = f io in
          IO.close io;
          Some a

    let io path =
      with_io path @@ fun io ->
      let IO.Header.{ offset; generation } = IO.Header.get io in
      let fanout_size = Bytes (IO.get_fanout_size io) in
      let size = Bytes (IO.size io |> Int63.of_int) in
      let offset = Int63.to_int64 offset in
      let generation = Int63.to_int64 generation in
      { size; offset; generation; fanout_size }

    let run ~root =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      let data = io (Layout.data ~root) in
      let log = io (Layout.log ~root) in
      let log_async = io (Layout.log_async ~root) in
      let merge = io (Layout.merge ~root) in
      let lock =
        IO.Lock.pp_dump (Layout.lock ~root)
        |> Option.map (fun f ->
               f Format.str_formatter;
               Format.flush_str_formatter ())
      in
      let entry_size = K.encoded_size + V.encoded_size |> Int63.of_int in
      {
        entry_size = Bytes entry_size;
        files = { data; log; log_async; merge; lock };
      }
      |> Repr.pp_json ~minify:false t Fmt.stdout

    let term = Cmdliner.Term.(const (fun root () -> run ~root) $ path)
  end

  module Integrity_check = struct
    let encoded_sizeL = Int63.of_int Entry.encoded_size
    let encoded_sizeLd = Int64.of_int Entry.encoded_size

    let print_window_around central_offset io context =
      let window_size = (2 * context) + 1 in
      List.init window_size (fun i ->
          let index = i - context in
          Int63.(add central_offset (mul (of_int index) encoded_sizeL)))
      |> List.filter (fun off -> Int63.compare off Int63.zero >= 0)
      |> List.map (fun off ->
             let entry = IO.read_entry io off in
             let highlight =
               if off = central_offset then Fmt.(styled (`Fg `Red)) else Fun.id
             in
             highlight (fun ppf () -> (Repr.pp Entry.t) ppf entry))
      |> Fmt.(concat ~sep:cut)

    let run ~root =
      let context = 2 in
      match IO.v (Layout.data ~root) with
      | Error `No_file_on_disk -> Fmt.failwith "No data file in %s" root
      | Ok io ->
          let io_offset = IO.offset io in
          if Int63.compare io_offset encoded_sizeL < 0 then (
            if not (Int63.equal io_offset Int63.zero) then
              Fmt.failwith
                "Non-integer number of entries in file: { offset = %a; \
                 entry_size = %d }"
                Int63.pp io_offset Entry.encoded_size)
          else
            let first_entry = IO.read_entry io Int63.zero in
            let previous = ref first_entry in
            Format.eprintf "\n%!";
            Progress.(
              with_reporter
                (counter ~style:`UTF8 ~message:"Scanning store for faults"
                   ~pp:Progress.Units.Bytes.of_int64 (Int63.to_int64 io_offset)))
            @@ fun report ->
            io
            |> IO.iter ~min:encoded_sizeL (fun off e ->
                   report encoded_sizeLd;
                   if !previous.key_hash > e.key_hash then
                     Log.err (fun f ->
                         f "Found non-monotonic region:@,%a@,"
                           (print_window_around off io context)
                           ());
                   previous := e)

    let term = Cmdliner.Term.(const (fun root () -> run ~root) $ path)
  end

  module Cli = struct
    open Cmdliner

    let reporter =
      let pp_header ppf = function
        | Logs.App, header ->
            Fmt.(styled `Bold (styled (`Fg `Cyan) string)) ppf ">> ";
            Fmt.(option string) ppf header
        | _, header -> Fmt.(option string) ppf header
      in
      Logs_fmt.reporter ~pp_header ()

    let main () : empty =
      let default = Term.(ret (const (`Help (`Auto, None)))) in
      let info =
        let doc = "Check and repair Index data-stores." in
        Cmd.info ~doc "index-fsck"
      in
      let commands =
        [
          ( Term.(
              Stat.term
              $ Log.setup_term ~reporter (module Clock) (module Fmt_tty)),
            Cmd.info ~doc:"Print high-level statistics about the store." "stat"
          );
          ( Term.(
              Integrity_check.term
              $ Log.setup_term ~reporter (module Clock) (module Fmt_tty)),
            Cmd.info
              ~doc:"Search the store for integrity faults and corruption."
              "integrity-check" );
        ]
      in
      let commands = List.map (fun (term, info) -> Cmd.v info term) commands in
      exit @@ Cmd.eval (Cmd.group ~default info commands)
  end

  let cli = Cli.main
end
