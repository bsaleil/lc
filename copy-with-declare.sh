
# Declarations
echo "(declare (standard-bindings) (extended-bindings) (not inline-primitives) (block) (not safe))" > "$2"

# Library
if [ "$3" == "lib" ]; then
  cat ./lib.scm >> "$2"
fi

# Code
cat "$1" >> "$2"
