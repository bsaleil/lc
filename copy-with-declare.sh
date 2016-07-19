echo "(declare (standard-bindings) (extended-bindings) (block) (inlining-limit 300) (not safe))"|cat - "$1" > "$2"
