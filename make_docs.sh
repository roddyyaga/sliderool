#!/usr/bin/env bash
# Build the docs and move them to the correct place
rm -r docs/sliderool
dune clean && dune build @doc
cp -r _build/default/_doc/_html/sliderool docs/sliderool
