exception Invalid_process_status of Unix.process_status

val impl : string -> (string, exn) result

val intf : string -> (string, exn) result
