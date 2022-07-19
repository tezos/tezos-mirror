type host_func =
  Input_buffer.t ->
  Instance.module_inst ref ->
  Values.value list ->
  Values.value list Lwt.t

val register : module_name:string -> func_name:string -> host_func -> unit

val lookup : module_name:string -> func_name:string -> host_func
