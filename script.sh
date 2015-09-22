#! /bin/bash

## Special files:
## * mazefun-cps.scm: 
##      Need to replace "append '() lists" by "CPSappendTwo '() lists" because
##      append is used in a foldr call. Foldr send a continuation to the function.
##      However, the script does not translate append calls to CPS. So we need an 
##      Alternative CPS version for this special case
## * triangl.scm and browse.scm:
##      Convert do bindings with only (variable init) to (variable init variable)
##      Because CPS script does not handle (variable init) bindings 
## * compiler.scm
##		Copy modified compiler-cps.scm instead of use cps.scm script
## * conform.scm
##		Need to create a CPS versions of vector because vector is used as a constructor
##		Then, the constructor is called as (constr k el1 el2 ...) and k is considered as
##      an element of the array instead of a continuation
##      then replace all " vector" to " CPSvector" (3 constructors)
## * graphs.scm
##		Need to replace the two cons given as parameters from cons to (lambda (k a b) (k (cons a b)))
##      Because they are called using a continuation (because they are function parameters, the function does not
##      know that it is cons) then replace all " cons" to " (lambda (k a b) (k (cons a b)))" (2 occurrences)
## * peval.scm
##		Need to replace all 'number?' by '(lambda (k n) (k (number? n)))' because thay are all send as parameters
##      and called like (number? k n)
##      Replace the only " car" by " (lambda (k l) (k (car l)))" for the same reason
##      Finally replace the only " list" by " (lambda (k . l) (k l))" which is a CPS version of list
##      for the same reason as the one for vector in conform.scm

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
	
	if [ $FILE == "BST.scm" ] || [ $FILE == "pyramid.scm" ]; then continue; fi

	echo "$bench "
	cp $bench ./CPS/ -f

	# Handle special files before conversion
	if [ $FILE == "triangl.scm" ]
	then
		echo "Special: TRIANGL"
		OUTFILE="./CPS/$FILE"
		echo $OUTFILE
		sed -i "s/depth (+ depth 1)/depth (+ depth 1) depth/g" $OUTFILE
	elif [ $FILE == "browse.scm" ]
	then
		echo "Special: BROWSE"
		OUTFILE="./CPS/$FILE"
		echo $OUTFILE
		sed -i "s/a '()/a '() a/g" $OUTFILE
    fi

    # Handle special files instead of convert
    if [ $FILE == "compiler.scm" ]
    then
    	echo "Special: COMPILER"
    	NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"
		cp /home/bapt/Bureau/CPS/specials/compiler-cps.scm $OUTFILE
	else
		gsi /home/bapt/Bureau/CPS/CPSCOM.scm ./CPS/$FILE
	fi

	# Handle special files after conversion
	if [ $FILE == "mazefun.scm" ]
	then
		echo "Special: MAZEFUN"
		NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"
		echo $OUTFILE
		sed -i "s/append '() lists/CPSappendTwo '() lists/g" $OUTFILE
	elif [ $FILE == "conform.scm" ]
	then
		echo "Special: CONFORM"
		NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"
		echo $OUTFILE
		sed -i "s/ vector/ CPSvector/g" $OUTFILE
	elif [ $FILE == "graphs.scm" ]
	then
		echo "Special: GRAPHS"
		NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"
		echo $OUTFILE
		sed -i "s/ cons/ (lambda (k a b) (k (cons a b)))/g" $OUTFILE
	elif [ $FILE == "peval.scm" ]
	then
		echo "Special: PEVAL"
		NAME=$(basename $bench .scm)
		OUTFILE="./CPS/$NAME-cps.scm"
		echo $OUTFILE
		sed -i "s/number?/(lambda (k n) (k (number? n)))/g" $OUTFILE
		sed -i "s/ car/ (lambda (k l) (k (car l)))/g" $OUTFILE
		sed -i "s/ list/ (lambda (k . l) (k l))/g" $OUTFILE
	fi
done

notConverted=()
nbNotConverted=0

for bench in ./unit-tests/benchmarks/*.scm
do
	NAME=$(basename $bench .scm)
	
	FILE=$(basename $bench)
	if [ $FILE == "BST.scm" ] || [ $FILE == "pyramid.scm" ]; then continue; fi

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
	FILE=$(basename $bench)
	if [ $FILE == "BST.scm" ] || [ $FILE == "pyramid.scm" ]; then continue; fi

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