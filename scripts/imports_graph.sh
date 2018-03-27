#! /usr/bin/env bash

set -e

FILES=$(find src -type f \( -iname "*.hs" -o -iname "*.lhs" \))
TESTS=""
MAINS=""
TUTOS=""
TUMBLR=""
COMMON=""
OTHERS=""

function module() {
    m=$(sed -n 's/^\(> \)\?module \([^ ]*\).*/\2/p' "$1")
    echo ${m:-$1}
}

for f in $FILES ; do
    if echo $f | grep "Test" > /dev/null; then
        TESTS="$TESTS $f"
    elif echo $f | grep "/Tutorial/" > /dev/null; then
        TUTOS="$TUTOS $f"
    elif grep 'main :: IO ()' "$f" > /dev/null; then
        MAINS="$MAINS $f"
    elif echo $f | grep "/Tumblr/" > /dev/null; then
        TUMBLR="$TUMBLR $f"
    elif echo $f | grep "/Common/" > /dev/null; then
        COMMON="$COMMON $f"
    else
        OTHERS="$OTHERS $f"
    fi
done


echo 'digraph "haskell-imports" {'

for f in $COMMON; do
    echo "    \"$(module $f)\" [color=green]"
done

for f in $TUTOS; do
    echo "    \"$(module $f)\" [color=gold]"
done

for f in $MAINS; do
    echo "    \"$(module $f)\" [color=red]"
done

for f in $TESTS; do
    echo "    \"$(module $f)\" [color=blue]"
done

for f in $FILES; do
    for import in $(sed -n 's/^\(> \)\?import \(qualified\)\? *\([^ ]*\).*/\3/p' $f | grep 'Sleep') ; do
        echo "    \"$(module $f)\" -> \"$import\""
    done
done

echo '}'
