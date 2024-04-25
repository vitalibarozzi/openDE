#!/usr/bin/env sh

BASE_DIR=src/Physics/ODE
GHC_FLAGS="-lode -fglasgow-exts -ignore-package HODE -cpp -isrc"

# The order of these matters
MODULES="Mass World Body Geom Joint Objects Space"
HSCFILES="Types.hsc Hsc.hsc"


ZEROTH=zeroth


for hsc in $HSCFILES; do
  hsc2hs $BASE_DIR/$hsc
done

for module in $MODULES; do
  hsFile=$BASE_DIR/$module.hs
  thFile=$BASE_DIR/$module.th
  if test ! -e $hsFile || test $hsFile -ot $thFile
  then
    echo Preprocessing: $thFile
    $ZEROTH --input=$thFile --output=$hsFile "--ghc-args=$GHC_FLAGS"
  fi
done
