#!/bin/bash

usage() {
	cat << EOF
Usage: nucc.sh [-hai] <source file> [<output file>]

Invoke the bootstrap compiler to compile the given nucleus source file.

    -h    display this help message
    -a    produce assembly output
    -i    produce LLVM IR output
    -r    run the program immediately
EOF
}

output_ir=false
output_asm=false
run=false

script_dir=$(dirname "$(realpath "$0")")

OPTIND=1
while getopts 'hair' opt; do
	case "$opt" in
		h)
			usage
			exit 0
			;;
		a)
			output_asm=true
			;;
		i)
			output_ir=true
			;;
		r)
			run=true
			;;
		'?')
			usage >&2
			exit 1
			;;
	esac
done
shift "$((OPTIND-1))"

if [ $output_ir = true ] && [ $output_asm = true ]; then
	echo "Error: only one of {-a, -i} can be specified"
	exit 1
fi

if [ "$#" -lt 1 ]; then
	usage >&2
	exit 1
fi

input="$(realpath $1)"
input_basename="$(realpath "$(basename "$input")")"

if [ $output_ir = true ]; then
	output="${input_basename%%.*}.ll"
elif [ $output_asm = true ]; then
	output="${input_basename%%.*}.s"
else
	output="$(realpath a.out)"
fi

if [ "$#" -gt 1 ]; then
	output="$(realpath $2)"
fi

cd "$script_dir"

make -C ../runtime > /dev/null
if [ $? -ne 0 ]; then
	echo Compiling the runtime failed, aborting
	exit 1
fi

bc=$(mktemp ${input%%.*}_XXX.bc)

sbcl --script nucc.lisp "$input" "$bc"
if [ $? -ne 0 ]; then
	rm "$bc"
	exit 1
fi

if [ $output_ir = true ]; then
	llvm-dis-3.5 "$bc" -o "$output"
	rm "$bc"
	exit 0
fi

if [ $output_asm = true ]; then
	llc-3.5 -filetype=asm "$bc" -o "$output"
	rm "$bc"
	exit 0
fi

llvm-link-3.5 "$bc" ../runtime/nuc-runtime.bc -o "$bc"

obj=${input%%.*}.o
llc-3.5 -filetype=obj "$bc" -o "$obj"

rm "$bc"

# TODO: ld?
gcc "$obj" -o "$output"
if [ $? -ne 0 ]; then
	echo Compilation failed, aborting
	exit 1
fi

rm "$obj"

if [ $run = true ]; then
	"$output"
	rm "$output"
fi

exit 0
