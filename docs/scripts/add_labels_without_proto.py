#!/usr/bin/env python3
# Add unversioned labels to a page, before each versioned label

# Overview

# This script adds unversioned labels (i.e. Sphinx labels without the _N suffix,
# where N is the name of the protocol) to one doc page before every versioned
# label, unless an unversioned label already exists.

# If it finds an unversioned label, it leaves it as is, but prints a warning.

import sys
import re
import fileinput

USAGE = f"usage: {sys.argv[0]} <proto-dir>/<rst-file>"

if len(sys.argv) != 2:
    print(USAGE, file=sys.stderr)
    sys.exit(1)

# Parse the argument to separate the protocol directory and the page file
if not (m := re.search(r'([^\/]+)\/([^\/]+)$', sys.argv[1])):
    print(USAGE, file=sys.stderr)
    sys.exit(1)

proto = m.group(1)

# Recognize a label definition:
def_lbl_pat = re.compile(r' *[.][.]  *_([a-zA-Z0-9_-]*) *:')
for line in fileinput.input():
    if m := re.match(def_lbl_pat, line):
        # label definition
        label = m.group(1)
        if label.endswith('_' + proto):  # versioned label => add before
            # drop "_proto" suffix:
            print(f".. _{label[:-len(proto)-1]}:")
        else:  # unversioned already there => warn
            print(
                f"warning: unversioned label {label} found, may conflict",
                file=sys.stderr,
            )
    print(line, end='')
