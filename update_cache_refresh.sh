#!/bin/sh

git fetch --all
git rebase origin/master
make -C ci
git status
