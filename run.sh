for bench in ./tools/benchtimes/result/tag-inter-opt/*.scm
do
    # Get benchmark name from path
    NAME=$(basename $bench)
    NAME="${NAME%.*}"

    # Run LC with options and grep using key (e.g. "Executed tests: 120")
    OUT=$(time ls | grep "real")

    # Split using ":" and take 2nd element (e.g. 120)
    RES="$(cut -d'l' -f2 <<<"$OUT")"

    printf "$OUT"
    printf "$RES"
    # # Print "NAME:RES"
    # printf "$NAME"
    # printf ":$RES"
    # printf "\n"
done
