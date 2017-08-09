#!/usr/bin/env bash

if [ "$2" == "--strat3" ]; then
    make debug -j8 LC_STRAT=strat3
    ./lazy-comp --enable-const-vers --time --enable-cxoverflow-fallback $1
else
    make debug -j8 LC_STRAT=strat1
    ./lazy-comp --time --enable-cxoverflow-fallback $1
fi
