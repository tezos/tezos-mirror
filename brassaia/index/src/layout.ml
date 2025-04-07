open! Import

let toplevel ~root name = Filename.(concat (concat root "index") name)
let log = toplevel "log"
let log_async = toplevel "log_async"
let data = toplevel "data"
let lock = toplevel "lock"
let merge = toplevel "merge"
