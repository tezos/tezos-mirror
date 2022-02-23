#!/usr/bin/env python3
# Check cross-references within the doc of a given protocol-specific dir

# Overview

# This script checks only local protocol references, that is, refs in
# protocol NNN to labels defined in the protocol pages.
# All these refs must be versioned, i.e. suffixed by _NNN.
# With option -l, also checks that all defined labels are versioned.
# You should use this option for all protocols except the active one
# (which may contain unversioned labels).

# The script warns about refs to section headings (having the form
# :ref:`Section Name`), because these ones cannot be versioned (besides being
# very fragile).

# Implementation details

# In order to distinguish local refs from external refs, we build a symbol
# table in a first pass over all the .rst files in the protocol-specific
# dir.

# A second pass checks only local refs, and complains about unversioned ones.

import sys
import re
import os.path
import glob
import argparse

parser = argparse.ArgumentParser()
parser.add_argument(
    "proto_dir", help="a protocol directory containing its documentation"
)
parser.add_argument(
    "-l",
    "--labels",
    action="store_true",
    help="check that all labels are versioned",
)
args = parser.parse_args()

# pylint: disable=superfluous-parens
if not (m := re.search(r'([^\/]+)\/?$', args.proto_dir)):
    parser.print_help()
    sys.exit(1)

proto = m.group(1)
directory = args.proto_dir

# set of all the defined labels
labels = set()

if not os.path.isdir(directory):
    print(f"directory {directory} not found", file=sys.stderr)
    sys.exit(1)

# First pass: Scan labels: check & register
def_lbl_pat = re.compile(r' *[.][.]  *_([a-zA-Z0-9_-]*) *:')
for file in glob.glob(f"{directory}/*.rst"):
    with open(file, mode="r", encoding="utf-8") as f:
        while line := f.readline():
            if m := re.match(def_lbl_pat, line):
                label = m.group(1)
                labels.add(label.lower())
                if args.labels and not label.endswith('_' + proto):
                    print(
                        (
                            f"{file}: unversioned label {label} found, "
                            + "please remove or rewrite"
                        ),
                        file=sys.stderr,
                    )

# Second pass: Scan refs: check
ref_pat = re.compile(r':ref:`([^`]+)`', flags=re.DOTALL)
ref_lbl_desc_pat = re.compile(r'<(.+)>$')
ref_file_pat = re.compile(r'[.](rst|html)$')
sphinx_id_pat = re.compile(r'[\w.-]+')
for file in glob.glob(f"{directory}/*.rst"):
    with open(file, mode="r", encoding="utf-8") as f:
        txt = f.read()  # read all file contents
    for i in re.finditer(ref_pat, txt):
        ref = i.group(1)
        start = i.start(0)  # offset in file (unused for now)
        if m := re.search(ref_lbl_desc_pat, ref):
            # ref to label with description
            label = m.group(1)
        else:
            # ref with no description
            label = ref

        # Check the target label:
        if re.search(ref_file_pat, label):
            continue  # ref to a file = ok
        # warn on :ref:`Section Heading`:
        if not (m := re.fullmatch(sphinx_id_pat, label)):
            print(
                f"{file}: :ref:`{ref}`: rewrite as ref to a label",
                file=sys.stderr,
            )
            continue

        if label.endswith(f"_{proto}"):
            continue  # versioned label = ok
        lclabel = label.lower()
        # external ref ok:
        if lclabel not in labels and f"lclabel_{proto}" not in labels:
            continue
        print(
            (
                f"{file}: :ref:`{ref}`: label {label} should be"
                f"versioned as {label}_{proto}"
            ),
            file=sys.stderr,
        )
    # end for ref
