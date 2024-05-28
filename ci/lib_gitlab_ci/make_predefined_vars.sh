#!/bin/sh

if [ "$1" = "--help" ] || [ "$#" -gt 1 ]; then
  cat << EOT
Usage: $0

Writes the files 'predefined_vars.ml' and 'predefined_vars.mli' based
on the contents of the files 'predefined_vars.csv' and
'predefined_mr_vars.csv'.
EOT
  exit 1
fi

rm -f predefined_vars.ml predefined_vars.mli

cat >> predefined_vars.mli << EOT
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Predefined CI/CD variables in all pipelines.

    This contains a subset of the
    {{:https://docs.gitlab.com/ee/ci/variables/predefined_variables.html}
    predefined variables}. *)

(** String representation of a variable with the sigil-sign.

    A handy alias of [If.encode_var]. *)
val show : Var.t -> string

EOT

cat >> predefined_vars.ml << EOT
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file is generated.

   See [make_predefined_vars.sh] in this directory for more information. *)

open Var

let show = encode

EOT

grep -v Deprecated predefined_vars.csv | while read -r line; do
  var=$(echo "$line" | cut -f 1 | xargs)
  var_lower=$(echo "$var" | tr '[:upper:]' '[:lower:]')
  defined_for=$(echo "$line" | cut -f 2 | xargs)
  gitlab_version=$(echo "$line" | cut -f 3 | xargs)
  runner=$(echo "$line" | cut -f 4 | xargs)
  description=$(echo "$line" | cut -f 5 | xargs | fold --spaces --width 76 | sed "s/^/    /g")

  cat >> predefined_vars.mli << EOT
(** Corresponds to [${var}].

${description}

    Context: [${defined_for}]. Available since GitLab [${gitlab_version}].
    Available in [${runner}] runners. *)
val ${var_lower} : Var.t

EOT
  cat >> predefined_vars.ml << EOT
let ${var_lower} = make "${var}"

EOT
done

echo "(** {2 Predefined variables for merge request pipelines} *)" >> predefined_vars.mli
echo >> predefined_vars.mli

grep -v Deprecated predefined_mr_vars.csv | while read -r line; do
  var=$(echo "$line" | cut -f 1 | xargs)
  var_lower=$(echo "$var" | tr '[:upper:]' '[:lower:]')
  gitlab_version=$(echo "$line" | cut -f 2 | xargs)
  runner=$(echo "$line" | cut -f 3 | xargs)
  description=$(echo "$line" | cut -f 4 | xargs | fold --spaces --width 76 | sed "s/^/    /g")

  cat >> predefined_vars.mli << EOT
(** Corresponds to [${var}].

${description}

    Context: merge requests. Available since GitLab [${gitlab_version}].
    Available in [${runner}] runners. *)
val ${var_lower} : Var.t

EOT
  cat >> predefined_vars.ml << EOT
let ${var_lower} = make "${var}"

EOT
done

sed -i 's/ \+$//' predefined_vars.ml predefined_vars.mli
ocamlformat -i predefined_vars.ml predefined_vars.mli
