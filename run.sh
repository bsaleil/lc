function run {
    ARGS="$2 --time"
    echo "Building..."
    $1 > /dev/null
    echo "Running with args '$ARGS'..."
    echo "#name:#exec:#total"
    STR=""
    for i in ./tools/benchtimes/result/inter/*.scm; do
        OUT="$(./lazy-comp $i $ARGS)"

        # Get exec time
        EXEC="$(echo "$OUT" | sed -n 1p | cut -c 11-)"
        #EXEC="$(echo "$EXEC" | grep -Poi '^\S*')"
        # Get total time
        TOTAL="$(echo "$OUT" | sed -n 9p | cut -c 11-)"
        #TOTAL="$(echo "$TOTAL" | grep -Poi '^\S*')"

        #
        FILE="$(basename $i)"
        NAME="${FILE%.*}"
        NAME="${NAME%.*}"
        echo "$NAME:$EXEC:$TOTAL"
    done
}

# Interprocedural
echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat1" ""

echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat1" "--max-versions 5"

echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat1" "--cc-max 500 --cr-max 500 --enable-cxoverflow-fallback"

echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 500 --cr-max 500  --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"

echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 500 --cr-max 500  --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --call-max-len 10 --const-vers-types cha voi nul boo vec sym clo"

echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 500 --cr-max 500  --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --call-max-len 8 --const-vers-types cha voi nul boo vec sym clo"

echo "Temps d'exécution interprocédural"
run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 500 --cr-max 500  --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --call-max-len 5 --const-vers-types cha voi nul boo vec sym clo"

# # Interprocedural
# echo "Temps d'exécution interprocédural"
# run "make LC_STRAT=strat1" ""
#
# echo "Temps d'exécution interprocédural"
# run "make LC_STRAT=strat1" "--max-versions 5"
#
# # Interprocedural strat3 + no nums
# echo "Temps d'exécution interprocédural + constantes"
# run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 500 --cr-max 500 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"
#
# # Interprocedural strat3 + no nums
# echo "Temps d'exécution interprocédural + constantes"
# run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"
#
# # Interprocedural strat3 + no nums
# echo "Temps d'exécution interprocédural + constantes"
# run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"
#
# # Interprocedural strat3 + no nums
# echo "Temps d'exécution interprocédural + constantes"
# run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"

# # Interprocedural strat1
# echo "Temps d'exécution interprocédural + constantes"
# run "make" "--enable-const-vers --cc-max 3000 --cr-max 3000 --enable-cxoverflow-fallback --max-versions 100"
#
# # Interprocedural strat3 + all consts
# echo "Temps d'exécution interprocédural + constantes"
# run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 3000 --cr-max 3000 --enable-cxoverflow-fallback"
#
# # Interprocedural strat3 + no nums
# echo "Temps d'exécution interprocédural + constantes"
# run "make LC_STRAT=strat3" "--enable-const-vers --cc-max 3000 --cr-max 3000 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"
