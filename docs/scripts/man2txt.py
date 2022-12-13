#!/usr/bin/env python3
# Convert man pages to simple text by removing formatting directives

# Currently, only bold characters are replaced: x^Hx -> x

import re
import fileinput

for line in fileinput.input():
    line = line.rstrip('\n')
    # get rid of the overwritten letters
    line = re.sub(r'.[\b]', '', line)
    # protect double dashes from being interpreted as a long dash
    line = re.sub(r'--', r'-\-', line)
    print(line)
