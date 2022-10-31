include Internal_event.Simple

let section = ["proxy_rpc"]

let proxy_getter_created =
  declare_2
    ~section
    ~name:"proxy_getter_created"
    ~level:Internal_event.Debug
    ~msg:"proxy cache created for chain {chain} and block {block}"
    ("chain", Data_encoding.string)
    ("block", Data_encoding.string)

let proxy_block_rpc =
  declare_3
    ~section
    ~name:"proxy_block_rpc"
    ~level:Internal_event.Debug
    ~msg:"/chains/<{chain}>/blocks/<{block}>/context/raw/bytes/{key}"
    ("chain", Data_encoding.string)
    ("block", Data_encoding.string)
    ~pp3:(fun ppf strings ->
      Format.fprintf ppf "%s" (String.concat "/" strings))
    ("key", Data_encoding.Variable.list Data_encoding.string)

let tree_received =
  declare_1
    ~section
    ~name:"tree_received"
    ~level:Internal_event.Debug
    ~msg:"received tree of size {size}"
    ("size", Data_encoding.int64)
