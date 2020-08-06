type t = string

let mockup_subdirectory = "mockup"

let mockup_directory ~dirname = Filename.concat dirname mockup_subdirectory

let has_mockup_directory ~dirname =
  let filename = mockup_directory ~dirname in
  Sys.file_exists filename && Sys.is_directory filename

let context_basename = "context.json"

let mempool_basename = "mempool.json"

let subdir_file = Filename.concat mockup_subdirectory

let context_file = subdir_file context_basename

let mempool_file = subdir_file mempool_basename

let dir_file ~dirname (file : t) = Filename.concat dirname file

let mempool ~dirname = dir_file ~dirname mempool_file

let context ~dirname = dir_file ~dirname context_file

let has_context ~dirname = Sys.file_exists @@ context ~dirname

let has_mempool ~dirname = Sys.file_exists @@ mempool ~dirname
