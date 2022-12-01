#!/usr/bin/env python3
# Convert man pages to simple text by removing formatting directives

# Currently, only bold characters are replaced: x^Hx -> x

import re
import fileinput

for line in fileinput.input():
    print(re.sub(r'.[\b]', '', line.rstrip('\n')))
