echo "(declare (standard-bindings) (extended-bindings) (inlining-limit 0) (block) (not safe))"|cat - "$1" > "$2"
