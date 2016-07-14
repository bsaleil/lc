echo "(declare (standard-bindings) (extended-bindings) (block) (inlining-limit 0) (not safe))"|cat - "$1" > "$2"
