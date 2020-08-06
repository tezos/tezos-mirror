type t = private string

val mockup_directory : dirname:string -> t

val has_mockup_directory : dirname:string -> bool

val context : dirname:string -> t

val has_context : dirname:string -> bool

val mempool : dirname:string -> t

val has_mempool : dirname:string -> bool

val dir_file : dirname:string -> t -> t
