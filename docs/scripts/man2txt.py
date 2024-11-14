#!/usr/bin/env python3
# Convert man pages to simple text by removing formatting directives
# (e.g., bold characters are replaced: x^Hx -> x)
# With option -r, further make output compatible with RST syntax

import re
import fileinput
import argparse

parser = argparse.ArgumentParser()
parser.add_argument(
    "-r",
    "--RST",
    action="store_true",
    help="make output compatible with ReS",
)
parser.add_argument(
    'files',
    metavar='FILE',
    nargs='*',
    help='files to read, if empty, stdin is used',
)
args = parser.parse_args()

# To prevent line-breaks to bother sphinx with two consecutive lines with the
# same indentation, we remove keep on one line those paragraphs.

# If indentation decreases, we add a newline
lastLines = []
INDENT = 0
PREVIOUS_INDENT = 0
for line in fileinput.input(files=args.files):
    line = line.rstrip('\n')
    # get rid of the overwritten letters
    line = re.sub(r'.[\b]', '', line)
    INDENT = len(line) - len(line.lstrip())
    if args.RST:
        # protect double dashes from being interpreted as a long dash
        line = re.sub(r'--', r'-\-', line)
    if INDENT > 0 and PREVIOUS_INDENT == INDENT:
        lastLines.append(line)
    elif INDENT > 0 and PREVIOUS_INDENT < INDENT:
        print(
            " ".join(
                [lastLines[0]] + list(map(lambda x: x.lstrip(), lastLines[1:]))
            )
        )
        lastLines = [line]
    elif 0 < INDENT < PREVIOUS_INDENT:
        print("\n".join(lastLines))
        print("")
        lastLines = [line]
    else:  # INDENT == 0
        if len(lastLines) > 0:
            print("\n".join(lastLines))
        lastLines = [line]
    PREVIOUS_INDENT = INDENT
print("\n".join(lastLines))
