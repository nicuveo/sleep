#! /usr/bin/env bash

set -e

FILES=$(find src -type f -iname "*.hs")


# assert we are at the repo root

if [ ! -r "stack.yaml" ] ; then
    echo "not in repo root, aborting"
    exit 1
fi


# assert that the repo is clean

if [ -n "$(git st --porcelain | egrep -v '^\?\?')" ] ; then
    echo "repo not clean, aborting"
    exit 1
fi


# checking out new branch

git checkout -B autoclean
stack clean


# do the thing

for f in $FILES ; do
    exts=$(sed -n 's/{-# LANGUAGE \([^ ]*\) *#-}/\1/p' < $f)
    if [ -n "$exts" ] ; then
        echo "# $f"
        for ext in $exts ; do
            sed -i "/LANGUAGE $ext/d" $f
            set +e
            rm -f *.tix
            stack test --fast --ghc-options '-Werror' &> /dev/null
            res=$?
            set -e
            if [ $res -eq 0 ] ; then
                echo "  removing $ext from $f"
                git commit $f -m "removed $ext from $f" &> /dev/null
            else
                git reset --hard &> /dev/null
            fi
        done
    fi
done
