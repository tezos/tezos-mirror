open Types
open Yaml

let scalar value =
  `Scalar
    {
      anchor = None;
      tag = None;
      value;
      plain_implicit = true;
      quoted_implicit = false;
      style = `Any;
    }

let sequence l =
  `A {s_anchor = None; s_tag = None; s_implicit = true; s_members = l}

let mapping l =
  `O
    {
      m_anchor = None;
      m_tag = None;
      m_implicit = true;
      m_members = List.map (fun (k, v) -> (scalar k, v)) l;
    }

let metaSpec (t : MetaSpec.t) =
  mapping
    (List.filter_map
       (fun x -> x)
       [(match t.id with None -> None | Some id -> Some ("id", scalar id))])

let classSpec _ = mapping [("test", scalar "test")]

let instanceSpec _ = mapping [("test", scalar "test")]

let enumSpec _ = mapping [("test", scalar "test")]

let if_not_empty = function [] -> false | _ -> true

let to_yaml (t : ClassSpec.t) =
  mapping
    (List.filter_map
       (fun (b, n, v) -> if b then Some (n, v) else None)
       [
         (true, "meta", metaSpec t.meta);
         ( if_not_empty t.types,
           "types",
           mapping (t.types |> List.map (fun (k, v) -> (k, classSpec v))) );
         ( if_not_empty t.instances,
           "instances",
           mapping (t.instances |> List.map (fun (k, v) -> (k, instanceSpec v)))
         );
         ( if_not_empty t.enums,
           "enums",
           mapping (t.enums |> List.map (fun (k, v) -> (k, enumSpec v))) );
         ( if_not_empty t.seq,
           "seq",
           sequence
             (t.seq
             |> List.map (fun v -> mapping [("id", scalar v.AttrSpec.id)])) );
       ])

let print t =
  let y = to_yaml t in
  match Yaml.yaml_to_string y with Ok x -> x | Error (`Msg m) -> failwith m
