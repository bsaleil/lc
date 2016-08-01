echo "(declare (standard-bindings) (extended-bindings) (not inline-primitives) (block) (not safe))"|cat - "$1" > "$2"
