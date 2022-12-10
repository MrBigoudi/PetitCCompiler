#!/bin/bash

# for absolute paths
cd "$(dirname "$0")"
BASE_DIR=$PWD

shopt -s nullglob

# script de test pour le projet de compilation

option=$1
compilo=${BASE_DIR}/../$2
score=0
max=0
bonus=0
verbose=0

echo "Test de $2"

echo

# tous les tests passent avec gcc
test_gcc() {
for f in ${BASE_DIR}/syntax/bad/*.c; do
    if gcc -c $f > /dev/null 2>&1 ; then
      echo "succÃ¨s de gcc -c sur $f"
    fi
done

for f in ${BASE_DIR}/typing/good/*.c ${BASE_DIR}/exec/*.c ${BASE_DIR}/exec-fail/*.c; do
    gcc -c $f > /dev/null 2>&1 ||
     echo "echec de gcc sur $f"
done
for f in ${BASE_DIR}/typing/bad/*.c; do
    if gcc -c $f > /dev/null 2>&1 ; then
      echo "succÃ¨s de gcc -c sur $f"
    fi
done
for f in ${BASE_DIR}/exec/*.c; do
    echo "test gcc sur $f"
    expected=${BASE_DIR}/exec/`basename $f .c`.out
    if gcc $f > /dev/null 2>&1 ; then
      ./a.out > out
      if ! cmp --quiet out $expected; then
          echo "mauvaise sortie de gcc sur $f"
      fi
    else
      echo "Ã©chec de gcc -c sur $f"
    fi
done
}

compile () {
if [[ $verbose != 0 ]]; then
  echo Compile $1 $2
  $compilo $1 $2;
else
  $compilo $1 $2 > /dev/null 2>&1;
fi;
}


# partie 1 : tests d'analyse syntaxique

partie1 () {

score=0
max=0

echo "Partie 1"

# les mauvais
echo -n "mauvais "
for f in ${BASE_DIR}/syntax/bad/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait Ã©chouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in ${BASE_DIR}/syntax/good/*.c ${BASE_DIR}/typing/bad/*.c ${BASE_DIR}/typing/good/*.c ${BASE_DIR}/exec/*.c ${BASE_DIR}/exec-fail/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Syntaxe : $score/$max : $percent%"; }

# partie 2 : tests d'analyse sÃ©mantique


partie2 () {
echo
echo "Partie 2"

score=0
max=0

# les mauvais
echo -n "mauvais "
for f in ${BASE_DIR}/typing/bad/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait Ã©chouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in ${BASE_DIR}/typing/good/*.c ${BASE_DIR}/exec/*.c ${BASE_DIR}/exec-fail/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo    "Typage  : $score/$max : $percent%";
}


# partie 3 : tests d'exÃ©cution

partie3 () {

score_comp=0
score_out=0
score_test=0
max=0

echo
echo "Partie 3"
echo "Execution normale"
echo "-----------------"

# timeout="why3-cpulimit 30 0 -h"
# spim="spim -ldata 20000000 -lstack 20000000"

for f in ${BASE_DIR}/exec/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n "."
    asm=${BASE_DIR}/exec/`basename $f .c`.s
    rm -f $asm
    expected=${BASE_DIR}/exec/`basename $f .c`.out
    max=`expr $max + 1`;
    if compile $f; then
	rm -f out
	score_comp=`expr $score_comp + 1`;
	if gcc -no-pie $asm && ./a.out > out; then
	    score_out=`expr $score_out + 1`;
	    if cmp --quiet out $expected; then
		score_test=`expr $score_test + 1`;
	    else
		echo
		echo "ECHEC : mauvaise sortie pour $f"
	    fi
	else
		echo
		echo "ECHEC du code produit pour $f"
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait rÃ©ussir)"
    fi
done
echo

echo "Execution conduisant Ã  un Ã©chec"
echo "-------------------------------"

for f in ${BASE_DIR}/exec-fail/*.c; do
    b=`basename $f`
    if [[ $bonus == 0 ]] && [[ $b == nested* ]]; then
        continue
    fi
    echo -n "."
    asm=${BASE_DIR}/exec-fail/`basename $f .c`.s
    rm -f $asm
    max=`expr $max + 1`;
    if compile $f && gcc -no-pie $asm; then
	score_comp=`expr $score_comp + 1`;
	if ./a.out > out; then
	    echo
	    echo "ECHEC : devrait Ã©chouer sur $f"
	else
	    score_test=`expr $score_test + 1`;
	    score_out=`expr $score_out + 1`;
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait rÃ©ussir)"
    fi
done

echo
percent=`expr 100 \* $score / $max`;

echo "Compilation:";
percent=`expr 100 \* $score_comp / $max`;
echo "Compilation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Code produit : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Comportement du code : $score_test/$max : $percent%";}


case $option in
    "-1" )
        partie1;;
    "-2" )
        partie2;;
    "-3" )
        partie3;;
    "-v1" )
        verbose=1;
        partie1;;
    "-v2" )
        verbose=1;
        partie2;;
    "-v3" )
        verbose=1;
        partie3;;
    "-1b" )
	    bonus=1;
	    partie1;;
    "-2b" )
    	bonus=1;
        partie2;;
    "-3b" )
    	bonus=1;
        partie3;;
    "-v1b" )
        verbose=1;
	    bonus=1;
	    partie1;;
    "-v2b" )
        verbose=1;
    	bonus=1;
        partie2;;
    "-v3b" )
        verbose=1;
    	bonus=1;
        partie3;;
    "-all" )
    	partie1;
    	partie2;
    	partie3;;
    "-allb" )
        bonus=1;
    	partie1;
    	partie2;
    	partie3;;
    "-vall" )
        verbose=1;
    	partie1;
    	partie2;
    	partie3;;
    "-vallb" )
        verbose=1;
        bonus=1;
    	partie1;
    	partie2;
    	partie3;;
    "-gcc" )
        test_gcc;;
    "-part1" )
    	partie1;
    	partie2;
        bonus=1;
    	partie1;
    	partie2;;
    * )
        echo "usage : $0 <option> <compilo>"
        echo "spÃ©cifier une option parmi : "
        echo "-1      : tester la partie 1"
        echo "-2      : tester la partie 2"
        echo "-3      : tester la partie 3"
        echo "-1b     : tester la partie 1 en comptant le bonus"
        echo "-2b     : tester la partie 2 en comptant le bonus"
        echo "-3b     : tester la partie 3 en comptant le bonus"
        echo "-v1     : tester la partie 1 (verbose)"
        echo "-v2     : tester la partie 2 (verbose)"
        echo "-v3     : tester la partie 3 (verbose)"
        echo "-v1b    : tester la partie 1 en comptant le bonus (verbose)"
        echo "-v2b    : tester la partie 2 en comptant le bonus (verbose)"
        echo "-v3b    : tester la partie 3 en comptant le bonus (verbose)"
        echo "-gcc    : tester avec gcc"
        echo "-all    : tout tester"
        echo "-allb   : tout tester en comptant le bonus"
        echo "-vall   : tout tester (verbose)"
        echo "-vallb  : tout tester en comptant le bonus (verbose)"
        echo "-part1  : tester la première partie du projet";;

esac
echo