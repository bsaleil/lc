#!/usr/bin/env bash

# Declarations
echo "$4" > "$2"

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
