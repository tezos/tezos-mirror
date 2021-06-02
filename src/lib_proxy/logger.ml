module type S = sig
  type 'a t = 'a Internal_event.Simple.t

  val emit : 'a t -> 'a -> unit Lwt.t

  val proxy_getter_created : (string * string) t

  val proxy_block_header : (string * string) t

  val proxy_block_rpc : (string * string * string list) t

  val no_tree_received : unit t

  val tree_received : int64 t
end

let logger ~protocol_name : (module S) =
  (module struct
    include Internal_event.Simple

    let section = [protocol_name; "proxy_rpc"]

    let mk_name event = protocol_name ^ "-" ^ event

    let proxy_getter_created =
      declare_2
        ~section
        ~name:(mk_name "proxy_getter_created")
        ~level:Internal_event.Debug
        ~msg:"proxy cache created for chain {chain} and block {block}"
        ("chain", Data_encoding.string)
        ("block", Data_encoding.string)

    let proxy_block_header =
      declare_2
        ~section
        ~name:(mk_name "proxy_block_header")
        ~level:Internal_event.Debug
        ~msg:"chains/<{chain}>/blocks/<{block}>/header"
        ("chain", Data_encoding.string)
        ("block", Data_encoding.string)

    let proxy_block_rpc =
      declare_3
        ~section
        ~name:(mk_name "proxy_block_rpc")
        ~level:Internal_event.Debug
        ~msg:"/chains/<{chain}>/blocks/<{block}>/context/raw/bytes/{key}"
        ("chain", Data_encoding.string)
        ("block", Data_encoding.string)
        ~pp3:(fun ppf strings ->
          Format.fprintf ppf "%s" (String.concat "/" strings))
        ("key", Data_encoding.Variable.list Data_encoding.string)

    let no_tree_received =
      declare_0
        ~section
        ~name:(mk_name "no_tree_received")
        ~level:Internal_event.Debug
        ~msg:"no tree received"
        ()

    let tree_received =
      declare_1
        ~section
        ~name:(mk_name "tree_received")
        ~level:Internal_event.Debug
        ~msg:"received tree of size {size}"
        ("size", Data_encoding.int64)
  end)
