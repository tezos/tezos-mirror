(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

type reason = Explicitly_requested | Dependency_of of string * Version.t

let show_reason = function
  | Explicitly_requested -> "explicitly requested"
  | Dependency_of (name, version) ->
      sf "dependency of %s.%s" name (Version.show version)

let show_reasons = function
  | [] -> "no reason"
  | list -> String.concat ", " (List.map show_reason list)

type component_with_reason = {component : Component.t; reasons : reason list}

(* Solving a system with only equality constraints is a simple topological sort. *)
let solve (components : (string * Version.t) list) =
  (* [seen] is the set of components that we already decided we need to install.
     Components are added to [seen] before recursing.
     [seen] can thus be used to detect that we do not need to traverse a component again.
     As a map, [seen] does not contain information about order, but it is efficient
     to query whether a component needs to be traversed.
     [seen] cannot be used to detect cycles because components may have been added
     from another branch of the dependency tree.
     [seen] also stores the version numbers that were chosen, to detect conflicts. *)
  let seen : component_with_reason String_map.t ref = ref String_map.empty in
  (* [topological_sort] is also a set of "seen" components.
     Contrary to [seen]:
     - it is sorted: it contains the result of the algorithm, in reverse order;
     - components are added to it *after* recursing, so that their dependencies
       are added first. *)
  let topological_sort : string list ref = ref [] in
  (* [dependency_path] is yet another set of "seen" components.
     Contrary to [seen], it only contains components that have been seen on the current
     branch of the dependency tree. It can thus be used to detect cycles.
     It would be more efficient if it was a set, but by having it be a list
     we can output error messages that describe the cycle. *)
  let rec traverse dependency_path reason (name, version) =
    (* Check for cycles using [dependency_path]. *)
    if List.exists (fun (name', _) -> name' = name) dependency_path then
      fail
        "cycle detected: %s depends on itself (%s)"
        name
        (List.map
           (fun (name, version) -> sf "%s.%s" name (Version.show version))
           (List.rev ((name, version) :: dependency_path))
        |> String.concat " -> ")
    else
      (* From now on, on this dependency branch, we are not allowed to see this component
         again, as it would be a cycle. *)
      let dependency_path = (name, version) :: dependency_path in
      (* Check whether we already saw this component. *)
      match String_map.find_opt name !seen with
      | Some component ->
          (* We already saw this component in another dependency branch
             (if it was in the same branch it would be a cycle).
             We do not need to traverse it again.
             But we do need to check that the version is the same as the one
             we already decided to use. *)
          if Version.equal component.component.version version then (
            (* Add this dependency relationship as another reason to install. *)
            seen :=
              String_map.add
                name
                {component with reasons = reason :: component.reasons}
                !seen ;
            unit)
          else
            fail
              "component %s.%s is required (%s) but is also required in \
               version %s (%s)"
              name
              (Version.show component.component.version)
              (show_reasons component.reasons)
              (Version.show version)
              (show_reason reason)
      | None ->
          (* We never saw this component. *)
          let* component = Component.load name version in
          seen := String_map.add name {component; reasons = [reason]} !seen ;
          (* Recursively traverse its dependencies. *)
          let* () =
            list_iter_r component.dependencies @@ function
            | External _ ->
                (* We don't care about external dependencies as we are not going
                   to install them ourselves and they cannot depend on internal
                   dependencies. *)
                unit
            | Internal {with_test = true; _} ->
                (* We don't care about test dependencies because we are not building
                   to run tests. *)
                unit
            | Internal
                {
                  name = dependency_name;
                  version = dependency_version;
                  with_test = false;
                } ->
                traverse
                  dependency_path
                  (Dependency_of (name, version))
                  (dependency_name, dependency_version)
          in
          (* This component can now be made part of the topological sort.
             Note: one could think that the component could be added before
             traversing its dependencies, and that one would just have to remove
             the final [List.rev], simplifying the algorithm (with the possibility
             of merging [seen] and [topological_sort] into one variable as well).
             But this does not work.

             Consider the case where we are requesting to install A and B which
             both depend on C. Let's say we traverse A first, then B.
             - We traverse A and add it to the list.
             - We traverse its dependency C and add it to the list.
             - We traverse B and add it to the list.
             - We already saw its dependency C so we do not traverse C.
             The list now looks like this: [ B; C; A ].
             C is installed after B, which is a mistake.
             (And reversing the list does not help.)

             Instead, what the algorithm does is:
             - We traverse A.
               - We traverse C and add it to the list.
               - We can now add A to the list.
             - We traverse B.
               - We already traversed C so we do nothing.
               - We can now add B to the list.
             The list now looks like this: [ B; A; C ].
             C is thus first to be installed (after the list is reversed). *)
          topological_sort := name :: !topological_sort ;
          unit
  in
  (* Traverse all requested components. *)
  let* () = list_iter_r components (traverse [] Explicitly_requested) in
  (* Return the topological sort. [topological_sort] only contains component names;
     the [component_with_reason]s are in [seen], so we find them there. *)
  Ok
    ( Fun.flip List.rev_map !topological_sort @@ fun name ->
      match String_map.find_opt name !seen with
      | None ->
          (* We always add to [seen] before we add to [topological_sort]. *)
          assert false
      | Some x -> x )
