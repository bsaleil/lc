echo "(declare (standard-bindings) (inlining-limit 0) (block) (fixnum) (not safe))"|cat - "$1" > "$2"
