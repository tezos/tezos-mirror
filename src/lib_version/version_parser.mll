{
  type additional_info = Dev | RC of int | Release
  type t = {major : int; minor : int; additional_info : additional_info}
  let int s = int_of_string_opt s |> Option.value ~default: 0
}

let num = ['0'-'9']+

rule version_tag = parse
  | 'v'? (num as major) '.' (num as minor) ".0"?
      { match extra lexbuf with
          | None -> None
          | Some extra -> Some { major = int major; minor = int minor; additional_info = extra } }
  | _ | eof
      { None }

and extra = parse
  | "-rc" (num as rc) eof
      { Some (RC (int rc)) }
  | "+dev" eof
      { Some Dev }
  | eof
      { Some Release }
  | _
      { None }
