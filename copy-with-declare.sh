echo "(declare (standard-bindings) (extended-bindings) (inlining-limit 100) (not safe))"|cat - "$1" > "$2"
