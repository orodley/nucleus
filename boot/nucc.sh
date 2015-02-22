#!/bin/bash

usage() {
	cat << EOF
Usage: nucc.sh [-hairl] <source file> [<output file>]

Invoke the bootstrap compiler to compile the given nucleus source file.

    -h    display this help message
    -a    produce assembly output
    -i    produce LLVM IR output
    -r    run the resulting executable immediately
    -l    link against llvm
EOF
}

output_ir=false
output_asm=false
run=false
link_llvm=false

script_dir=$(dirname "$(realpath "$0")")

OPTIND=1
while getopts 'hairl' opt; do
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
		l)
			link_llvm=true
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
	llvm-dis "$bc" -o "$output"
	rm "$bc"
	exit 0
fi

if [ $output_asm = true ]; then
	llc -filetype=asm "$bc" -o "$output"
	rm "$bc"
	exit 0
fi

llvm-link "$bc" ../runtime/nuc-runtime.bc -o "$bc"

obj=${input%%.*}.o
llc -filetype=obj "$bc" -o "$obj"

rm "$bc"

link_flags="-lm $obj -o $output"
if [ $link_llvm = true ]; then
	llvm_cmpts='core bitwriter analysis'
	link_flags="$link_flags $(llvm-config --ldflags --libs $llvm_cmpts --system-libs)"
fi

# We have to use g++ as the linker when linking against LLVM
g++ $link_flags
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
