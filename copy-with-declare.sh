echo "(declare (standard-bindings) (extended-bindings) (block) (inlining-limit 00) (not safe))"|cat - "$1" > "$2"
