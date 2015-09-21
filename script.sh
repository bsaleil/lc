#! /bin/bash

## Special files:
## * mazefun-cps.scm: 
##      Need to replace "append '() lists" by "CPSappendTwo '() lists" because
##      append is used in a foldr call. Foldr send a continuation to the function.
##      However, the script does not translate append calls to CPS. So we need an 
##      Alternative CPS version for this special case 

containsElement () {
  local e
  for e in "${@:2}"; do [[ "$e" == "$1" ]] && return 0; done
  return 1
}

# Copy files and convert to CPS
echo "Starting conversion..."

rm ./CPS -rf
mkdir CPS
for bench in ./unit-tests/benchmarks/*.scm
do
	FILE=$(basename $bench)
	echo "$bench "
	cp $bench ./CPS/ -f
	gsi /home/bapt/Bureau/CPS/CPSCOM.scm ./CPS/$FILE

	# Handle special files
	if [ $FILE == "mazefun.scm" ]
	then
		echo "Special: MAZEFUN"
		NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"
		echo $OUTFILE
		sed -i "s/append '() lists/CPSappendTwo '() lists/g" $OUTFILE
    fi
done

notConverted=()
nbNotConverted=0

for bench in ./unit-tests/benchmarks/*.scm
do
	NAME=$(basename $bench .scm)
	OUTFILE="./CPS/$NAME-cps.scm"
	if [ ! -e "$OUTFILE" ]
	then
		notConverted[$nbNotConverted]=$bench
		nbNotConverted=$(( $nbNotConverted + 1))
	fi
done

notEqual=()
nbNotEqual=0

# Compare executions with/without using CPS
echo "Starting execution..."

for bench in ./unit-tests/benchmarks/*.scm
do
	containsElement $bench "${notConverted[@]}"
	if [ $? == 1 ] # We have a -cps file
	then
		NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"

		NCPS=$(./lazy-comp $bench --disable-functionid-propagation)
		CPS=$(./lazy-comp $OUTFILE --disable-functionid-propagation)

		if [ "$NCPS" != "$CPS" ]
		then
			notEqual[$nbNotEqual]=$bench
			nbNotEqual=$(( $nbNotEqual + 1))
		fi
	fi
done

clear

echo "REPORT"
echo ""
echo "Conversion status:"
for i in ${notConverted[@]}
do
	echo "KO - $i"
done

echo ""
echo "Execution status:"
for i in ${notEqual[@]}
do
	echo "KO - $i"
done