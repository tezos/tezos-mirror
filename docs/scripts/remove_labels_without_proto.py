#!/usr/bin/env python3
# Remove unversioned links from a page

## Overview

# This script removes unversioned labels (i.e. Sphinx labels without the _N
# suffix, where N is the name of the protocol) from one doc page.

# If this was a local label (i.e. referred in this or other pages of protocol
# _N) a versioned label surely exists before or after it, and will remain.
# Otherwise, this is rather an external label, which must not be referred to
# anymore because protocol _N is no more the active protocol.

import sys
import re
import fileinput

USAGE = f"usage: {sys.argv[0]} <proto-dir>/<rst-file>"

if len(sys.argv) != 2:
    print(USAGE, file = sys.stderr)
    exit(1)

# Parse the argument to separate the protocol directory and the page file
if not (m := re.search(r'([^\/]+)\/([^\/]+)$', sys.argv[1])):
    print(USAGE, file = sys.stderr)
    exit(1)

proto = m.group(1)

# Recognize a label definition:
def_lbl_pat = re.compile(r' *[.][.]  *_([a-zA-Z0-9_-]*) *:')
for line in fileinput.input():
    if m := re.match(def_lbl_pat, line):
        # label definition
        label = m.group(1)
        if not label.endswith('_' + proto): # unversioned label => drop
            #print("delete " + label)
            continue
    print(line, end = '')
