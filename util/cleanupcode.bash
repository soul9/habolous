#!/bin/bash

longlines() {
  find src/ -type f | \
    while read file < /dev/stdin; do
      egrep -n '.{80,}' "$file" \
          | sed -r "s,([0-9]+):.*,${file}:\1,g"
    done | sed -r 's,^,Long Line: ,g'
}

trailingblanks() {
  find src/ -type f | \
    while read file < /dev/stdin; do
      egrep -n '[ 	]+$' "$file" \
          | sed -r "s,([0-9]+):.*,${file}:\1,g"
    done | sed -r 's,^,Trailing Blank: ,g'
}

todosandfixme() {
  find src/ -type f | \
    while read file < /dev/stdin; do
      egrep -n '(FIXME|TODO|BUG)' "$file" \
        | sed -r "s,([0-9]+):.*,${file}:\1,g"
    done | sed -r 's,^,Devel Tag: ,g'
}

illegalimports() {
    find src/ -type f | \
      while read file < /dev/stdin; do
        egrep -n '^import' "$file" |grep -v "qualified"\
        | sed -r "s,([0-9]+):.*,${file}:\1,g"
    done | sed -r 's,^,Unqualified import: ,g'
}

hlint src -u --hint=docs/hlint.ignore
longlines
trailingblanks
todosandfixme
illegalimports
echo "Try to build app? (^c to cancel, enter to continue)"
if read; then
    cabal-dev clean
    rm -r dist/ cabal-dev/
    cabal-dev install
fi
