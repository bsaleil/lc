#!/usr/bin/env bash

# Declarations
echo "(declare (standard-bindings) (extended-bindings) (inlining-limit 0) (not inline-primitives) (block) (not safe))" > "$2"

# Library
if [[ "$3" == "lib" ]]
then
  cat ./lib.scm >> "$2"
fi

# Code
if [[ "$1" != "" ]]
then
  cat "$1" >> "$2"
fi
