#! /usr/bin/env bash

set -e

FILES=$(find src -type f -iname "*.hs")

cat $FILES | sed -n 's/{-# LANGUAGE \([^ ]*\) *#-}/\1/p' | sort | uniq -c | sort -rn
