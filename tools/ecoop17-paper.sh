#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT="$SCRIPT_DIR/../"

BENCH_PATH="$ROOT/tools/benchtimes/result/LC/"
LC_PATH="$ROOT/lazy-comp"
KEY_TO_CSV_PATH="$ROOT/tools/graphs/lckey_to_csv.py"
GET_CTIME_PATH="$ROOT/tools/graphs/get_ctime.py"
GET_TTIME_PATH="$ROOT/tools/graphs/get_ttime.py"
GET_ETIME_PATH="$ROOT/tools/benchtimes/run.py"

NEXEC="3"

function type_checks {
    echo "## Number of type checks"
    python $KEY_TO_CSV_PATH \
    $BENCH_PATH \
    $LC_PATH \
    "Executed tests" \
    "Max=5, Intraprocedural;--max-versions 5;--disable-entry-points;--disable-return-points" \
    "Max=5, Return points only;--max-versions 5;--disable-entry-points" \
    "Max=5, Entry points only;--max-versions 5;--disable-return-points" \
    "Max=5, Entry and return points;--max-versions 5"
    exit
}

function code_size {
    echo "## Code size (bytes)"
    python $KEY_TO_CSV_PATH \
    $BENCH_PATH \
    $LC_PATH \
    "Code size (bytes)" \
    "Max=5, Intraprocedural;--max-versions 5;--disable-entry-points;--disable-return-points" \
    "Max=5, Return points only;--max-versions 5;--disable-entry-points" \
    "Max=5, Entry points only;--max-versions 5;--disable-return-points" \
    "Max=5, Entry and return points;--max-versions 5" \
    exit
}

function tables_size {
    echo "## Function entry point tables size (kbytes)"
    python $KEY_TO_CSV_PATH \
    $BENCH_PATH \
    $LC_PATH \
    "CC table space (kbytes)" \
    "Max=5, Entry and return points;--max-versions 5"

    echo "## Continuation entry point tables size (kbytes)"
    python $KEY_TO_CSV_PATH \
    $BENCH_PATH \
    $LC_PATH \
    "CR table space (kbytes)" \
    "Max=5, Entry and return points;--max-versions 5"
    exit
}

function exec_time {
    echo "## Execution time"
    python $GET_ETIME_PATH $NEXEC --exec-only
    exit
}

function compil_time {
    echo "## Compilation time"
    python $GET_CTIME_PATH $NEXEC \
    $BENCH_PATH \
    $LC_PATH \
    "Max=5, Intraprocedural;--ctime;--max-versions 5;--disable-entry-points;--disable-return-points" \
    "Max=5, Return points only;--ctime;--max-versions 5;--disable-entry-points" \
    "Max=5, Entry points only;--ctime;--max-versions 5;--disable-return-points" \
    "Max=5, Entry and return points;--ctime;--max-versions 5"
    exit
}

function total_time {
    echo "## Total time"
    python $GET_TTIME_PATH $NEXEC \
    $BENCH_PATH \
    $LC_PATH \
    "m5intra;--max-versions 5;--disable-entry-points;--disable-return-points" \
    "m5rponly;--max-versions 5;--disable-entry-points" \
    "m5eponly;--max-versions 5;--disable-return-points" \
    "m5inter;--max-versions 5"
    exit
}

function help {
    echo "This script is used to extract raw data (using csv format) used in the experiment section of the paper:"
    echo "'Interprocedural Specialization of Higher-Order Dynamic Languages Without Static Analysis'"
    echo "To extract data, run:"
    echo
    echo "  ./ecoop17-paper [option]"
    echo
    echo "  The option is one of the following:"
    echo
    echo "    --help"
    echo "      Print this help"
    echo
    echo "    --type-checks"
    echo "      Extract the number of executed type checks"
    echo
    echo "    --code-size"
    echo "      Extract the generated code size"
    echo
    echo "    --tables-size"
    echo "      Extract the size of the function entry point tables"
    echo "      and the continuation entry point tables"
    echo
    echo "    --execution-time"
    echo "      Extract the execution time. This option may be followed by the number of executions (default is 3)."
    echo "      Because min and max values are removed, the number of executions must be greater than 2."
    echo "      Example: ./ecoop17-paper --execution-time 10"
    echo
    echo "    --compilation-time"
    echo "      Extract the compilation time. This option may be followed by the number of executions (default is 3)."
    echo "      Because min and max values are removed, the number of executions must be greater than 2."
    echo "      Example: ./ecoop17-paper --compilation-time 10"
    echo
    echo "    --total-time"
    echo "      Extract the total time. This option may be followed by the number of executions (default is 3)."
    echo "      Because min and max values are removed, the number of executions must be greater than 2."
    echo "      Example: ./ecoop17-paper --total-time 10"
    echo
    echo "    --all"
    echo "      Extract all of the data with a single option. This option may be followed by the number of executions to extract time values (default is 3)."
    echo "      Because min and max values are removed, the number of executions must be greater than 2."
    echo "      Example: ./ecoop17-paper --all 10"
    exit
}

if [ "$1" == "--all" ]; then
    if [ "$2" != "" ]; then
        NEXEC="$2"
    fi
    type_checks
    code_size
    tables_size
    exec_time
    compil_time
    total_time
elif [ "$1" == "--type-checks" ]; then
    type_checks
elif [ "$1" == "--code-size" ]; then
    code_size
elif [ "$1" == "--tables-size" ]; then
    tables_size
elif [ "$1" == "--execution-time" ]; then
    if [ "$2" != "" ]; then
        NEXEC="$2"
    fi
    exec_time
elif [ "$1" == "--compilation-time" ]; then
    if [ "$2" != "" ]; then
        NEXEC="$2"
    fi
    compil_time
elif [ "$1" == "--total-time" ]; then
    if [ "$2" != "" ]; then
        NEXEC="$2"
    fi
    total_time
elif [ "$1" == "--help" ]; then
    help
else
    echo "Invalid option $1"
    echo "Try '--help' option"
    exit
fi
