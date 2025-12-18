#!/bin/bash
AUTFILT="autfilt"
KOFOLA="../verifit_kofola/kofola/build/src/kofola"

# Check the number of command-line arguments
if [ \( "$#" -ne 1 \) -a \( "$#" -ne 2 -o "$1" != "--csv" \) ] ; then
	echo "usage: ${0} [--csv] <input-ba>"
	echo "usage: ${0} --print-header"
	exit 1
fi

if [ \( $1 = "--print-header" \) ] ; then
	echo -n "name;"
	echo -n "empty;"
	echo -n "deterministic;"
	echo -n "inherently weak;"
	echo -n "semi deterministic;"
	echo -n "terminal;"
	echo -n "unambiguous;"
	echo -n "weak;"
	echo -n "very weak;"
	echo -n "elevator;"
	echo -n "one-state;"
	echo
	exit 0
fi

INPUT=$1
WANTS_CSV=0

if [ \( "$#" -eq 2 \) -a \( "$1" = "--csv" \) ] ; then
	WANTS_CSV=1
	INPUT=$2
fi

run_autfilt () {
	local flags=$1

	cat ${INPUT} | ${AUTFILT} ${flags} > /dev/null
	if [ \( $? == 1 \) ] ; then
		echo 0
	else
		echo 1
	fi
}

run_kofola_elevator () {
	timeout 60 ${KOFOLA} --is-elevator ${INPUT} 
	if [ \( $? == 1 \) ] ; then
		echo 0
	else
		echo 1
	fi
}

run_one_state () {
	cat ${INPUT} | grep '^States: 1$' > /dev/null
	if [ \( $? == 0 \) ] ; then
		echo 1
	else
		echo 0
	fi
}

is_empty=$(run_autfilt "--is-empty")
is_deterministic=$(run_autfilt "--is-deterministic")
is_inherently_weak=$(run_autfilt "--is-inherently-weak")
is_semi_deterministic=$(run_autfilt "--is-semi-deterministic")
is_terminal=$(run_autfilt "--is-terminal")
is_unambiguous=$(run_autfilt "--is-unambiguous")
is_weak=$(run_autfilt "--is-weak")
is_very_weak=$(run_autfilt "--is-very-weak")
is_elevator=$(run_kofola_elevator)
is_one_state=$(run_one_state)

if [ ${WANTS_CSV} == 1 ] ; then
	echo -n "${INPUT};"
	echo -n "${is_empty};"
	echo -n "${is_deterministic};"
	echo -n "${is_inherently_weak};"
	echo -n "${is_semi_deterministic};"
	echo -n "${is_terminal};"
	echo -n "${is_unambiguous};"
	echo -n "${is_weak};"
	echo -n "${is_very_weak};"
	echo -n "${is_elevator};"
	echo -n "${is_one_state};"
	echo
else
	echo "name: ${INPUT}"
	echo "empty: ${is_empty}"
	echo "deterministic: ${is_deterministic}"
	echo "inherently weak: ${is_inherently_weak}"
	echo "semi deterministic: ${is_semi_deterministic}"
	echo "terminal: ${is_terminal}"
	echo "unambiguous: ${is_unambiguous}"
	echo "weak: ${is_weak}"
	echo "very weak: ${is_very_weak}"
	echo "elevator: ${is_elevator}"
	echo "one-state: ${is_one_state}"
fi
