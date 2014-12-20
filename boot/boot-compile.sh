#!/bin/bash

cd `dirname $0`

usage="$0 <source file> [<output file>]"

if [ "$#" -lt 1 ]; then
	echo "$usage"
	exit 1
fi

input="$1"
input_basename=$(basename "$input")
output=a.out

if [ "$#" -gt 1 ]; then
	output="$2"
fi

bc=$(mktemp ${input%%.*}_XXX.bc)

sbcl --script boot-compile.lisp "$input" "$bc"
if [ $? -ne 0 ]; then
	rm "$bc"
	exit 1
fi

obj=${input%%.*}.o
llc-3.5 -filetype=obj "$bc" -o "$obj"

rm "$bc"

gcc "$obj" -o "$output"

rm "$obj"
exit
