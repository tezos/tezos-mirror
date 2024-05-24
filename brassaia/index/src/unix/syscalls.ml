open! Import

external pread_int : Unix.file_descr -> int -> bytes -> int -> int -> int
  = "caml_index_pread_int"

external pread_int64 : Unix.file_descr -> int64 -> bytes -> int -> int -> int
  = "caml_index_pread_int64"

let pread : fd:_ -> fd_offset:int63 -> _ =
  match Int63.is_immediate with
  | True ->
      fun ~fd ~fd_offset ~buffer ~buffer_offset ~length ->
        pread_int fd fd_offset buffer buffer_offset length
  | False ->
      fun ~fd ~fd_offset ~buffer ~buffer_offset ~length ->
        pread_int64 fd
          (Int63.Boxed.to_int64 fd_offset)
          buffer buffer_offset length

external pwrite_int : Unix.file_descr -> int -> bytes -> int -> int -> int
  = "caml_index_pwrite_int"

external pwrite_int64 : Unix.file_descr -> int64 -> bytes -> int -> int -> int
  = "caml_index_pwrite_int64"

let pwrite : fd:_ -> fd_offset:int63 -> _ =
  match Int63.is_immediate with
  | True ->
      fun ~fd ~fd_offset ~buffer ~buffer_offset ~length ->
        pwrite_int fd fd_offset buffer buffer_offset length
  | False ->
      fun ~fd ~fd_offset ~buffer ~buffer_offset ~length ->
        pwrite_int64 fd
          (Int63.Boxed.to_int64 fd_offset)
          buffer buffer_offset length
