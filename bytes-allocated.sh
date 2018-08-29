
#------------------------------------------------------------------------------
# Script params

# Path of compiled benchmarks
BENCH_PATH="./tools/benchtimes/result/tag-inter-opt/*.scm"

# LC key to extract from --stats option
LC_KEY="Bytes allocated"

# Name of configurations used to extract lc key
CONFIG_NAMES=(
    "tag-intra-noopt"
    "tag-inter-noopt"
    "nan-intra-noopt"
    "nan-inter-noopt"
    "nan-intra-opt"
    "nan-inter-opt"
    "tag-intra-opt"
    "tag-inter-opt"
)

# Options of configurations used to extract lc key
CONFIGS=(
    "--disable-float-unboxing --disable-entry-points  --disable-return-points"
    "--disable-float-unboxing"
    "--disable-float-unboxing --disable-entry-points  --disable-return-points --nan-boxing"
    "--disable-float-unboxing --nan-boxing"
    "--disable-entry-points   --disable-return-points --nan-boxing"
    "--nan-boxing"
    "--disable-entry-points   --disable-return-points"
    ""
)

#------------------------------------------------------------------------------

# $1 benchmark
# $2 lc options
print_for_config()
{
    result=$(./lc $1 $2 --disable-pair-tag --stats --max-versions 5 | grep "$LC_KEY")
    IFS=':' read -r -a result <<< "$result"
    result="${result[1]}"
    result="${result// /}"
    printf ":"
    printf "$result"
}

# Print CSV header
printf "benchmark"
for config_name in "${CONFIG_NAMES[@]}"; do printf ":" && printf $config_name; done
printf "\n"

# Extract and print key for each benchmark
for bench in $BENCH_PATH
do
    name=$(basename $bench)
    name=${name::-8}
    printf $name
    for config in "${CONFIGS[@]}"; do print_for_config $bench "$config"; done
    printf "\n"
done
