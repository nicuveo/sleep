#! /usr/bin/env bash

set -e

FILES=$(find src -type f -iname "*.hs")

echo 'digraph "haskell-imports" {'

for f in $FILES; do
    module=$(sed -n 's/^module \([^ ]*\).*/\1/p' $f)
    module=${module:-$f}
    for import in $(sed -n 's/^import \(qualified\)\? *\([^ ]*\).*/\2/p' $f | grep 'Sleep') ; do
        echo "    \"$module\" -> \"$import\""
    done
done

echo '}'
