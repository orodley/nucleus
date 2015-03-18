#!/bin/sh

script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"
llvm_libs='core analysis bitwriter'
link_flags="$(llvm-config --ldflags --libs $llvm_libs --system-libs)"

echo Building the runtime...
make -C runtime
if [ $? -ne 0 ]; then
	exit $?
fi
echo

compile()
{
	echo "Compiling $2 with $1"
	"./$1" -link "$link_flags" compiler/main.nuc "$2"
	if [ $? -ne 0 ]; then
		exit $?
	fi
}

compile_ir()
{
	echo "Compiling $2 with $1"
	"./$1" -ir compiler/main.nuc "$2"

	if [ $? -ne 0 ]; then
		exit $?
	fi

	# The module name will be different as we've specified a different filename
	# for the output. This is fine, and we don't want to fail the consistency
	# check because of this.
	sed -i "/^; ModuleID = '.*'$/d" "$2"
}

echo "Using $(realpath snapshots/latest.sh) as stage1"
compile snapshots/latest.sh stage2
compile_ir nucc stage2.ll
compile_ir stage2 stage3.ll

echo Checking stage2.ll against stage3.ll for consistency
diff stage2.ll stage3.ll
echo
if [ $? -ne 0 ]; then
	echo "stage2 and stage3 were different!"
	echo "leaving stage2, stage2.ll and stage3.ll around for you to look at"
	exit 1
fi

# Consistency check passed, get rid of the IR and just leave the stage2 compiler
rm stage2.ll stage3.ll
mv stage2 nucc
echo Compilation finished, all is well
echo New compiler at $(realpath nucc)

# If we don't have a snapshot for this commit, save one
# TODO: This should check that the new snapshot is different to the old one, as
# not all commits change the compiler.
snapshots_dir="$script_dir/snapshots"
snapshot_name="$snapshots_dir/nucc_$(git rev-parse --short=12 HEAD)"
if [ ! -f "$snapshot_name" ]; then
	echo "Saving a shapshot to $snapshot_name"
	cp nucc "$snapshot_name"
fi
