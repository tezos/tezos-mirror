#!/bin/bash

#############################################################################
#                                                                           #
# Open Source License                                                       #
# Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                #
#                                                                           #
# Permission is hereby granted, free of charge, to any person obtaining a   #
# copy of this software and associated documentation files (the "Software"),#
# to deal in the Software without restriction, including without limitation #
# the rights to use, copy, modify, merge, publish, distribute, sublicense,  #
# and/or sell copies of the Software, and to permit persons to whom the     #
# Software is furnished to do so, subject to the following conditions:      #
#                                                                           #
# The above copyright notice and this permission notice shall be included   #
# in all copies or substantial portions of the Software.                    #
#                                                                           #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR#
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   #
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER#
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       #
# DEALINGS IN THE SOFTWARE.                                                 #
#                                                                           #
#############################################################################

# This script is a wrapper for git-gas-diff. See the README.md for more
# information about the tool.

# The script accepts zero, one or two commit hash arguments.
# If no commit is provided, the script will use git-gas-diff between the
# last merge commit and HEAD.
# If one commit is provided, the script will use git-gas-diff between this
# commit and HEAD.
# If two commits are provided, the script will use git-gas-diff between the
# first and the second.

# The script saves the git diff between the two commits to a temporary file
# that will be applied to git-gas-diff.
# The diff is restricted to regression test output files, does not consider
# added and deleted files (option `--diff-filter=ad`), and does not carry the
# context around modified lines (option `U0`).

case "$#" in
  0)
    OLD=$(git log --pretty=format:"%H" --merges -n 1)
    NEW='HEAD'
    ;;

  1)
    OLD=$1
    NEW='HEAD'
    ;;

  2)
    OLD=$1
    NEW=$2
    ;;

  *)
    echo "** Error: wrong number of arguments (0, 1 or 2 expected, found $#)."
    echo 'Usage: ./git-gas-diff.sh to use git-gas-diff between the last merge commit and HEAD;'
    echo '       ./git-gas-diff.sh [commit] to use git-gas-diff between [commit] and HEAD;'
    echo '       ./git-gas-diff.sh [commit1] [commit2] to use git-gas-diff between [commit1] and [commit2].'
    exit 1

esac

DIFF_FILE=$(mktemp)

MSG="-- LINE NUMBERS REFER TO TEMPORARY FILE ${DIFF_FILE}. --"

echo -e "$MSG\n"

git diff --no-ext-diff --diff-filter=ad -U0 --output="$DIFF_FILE" "$OLD" "$NEW" \
  ../../tezt/tests/expected/
dune exec git-gas-diff "$DIFF_FILE"

echo -e "\n$MSG"
