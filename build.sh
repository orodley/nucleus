#!/bin/sh

just_stage1=false
save_snapshot=true
script_dir=$(dirname "$(realpath "$0")")

OPTIND=1
while getopts '1d' opt; do
	case "$opt" in
		1)
			just_stage1=true
			;;
		d)
			save_snapshot=false
			;;
		'?')
			echo "Unknown option"
			exit 1
			;;
	esac
done
shift "$((OPTIND-1))"

if [ $# -gt 0 ]; then
	stage0="$(realpath "$1")"
else
	stage0="$(realpath "$script_dir/snapshots/latest.sh")"
fi

cd "$script_dir"
llvm_libs='core analysis bitwriter target'
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
	"$1" -link "$link_flags" compiler/main.nuc "$2"
	if [ $? -ne 0 ]; then
		exit $?
	fi
}

compile_ir()
{
	echo "Compiling $2 with $1"
	"$1" -ir compiler/main.nuc "$2"

	if [ $? -ne 0 ]; then
		exit $?
	fi

	# The module name will be different as we've specified a different filename
	# for the output. This is fine, and we don't want to fail the consistency
	# check because of this.
	sed -i "/^; ModuleID = '.*'$/d" "$2"
}

echo "Using $stage0 as stage0"
compile "$stage0" ./stage1

if [ "$just_stage1" = true ]; then
	echo Compilation finished, all is well
	exit 0
fi

compile ./stage1 stage2
compile_ir ./stage1 stage2.ll
compile_ir ./stage2 stage3.ll

echo Checking stage2.ll against stage3.ll for consistency
diff stage2.ll stage3.ll
if [ $? -ne 0 ]; then
	echo
	echo "stage2 and stage3 were different!"
	echo "leaving stage2, stage2.ll and stage3.ll around for you to look at"
	exit 1
fi

echo

# Consistency check passed, get rid of the everything but the stage2 compiler
rm stage1 stage2.ll stage3.ll
mv stage2 nucc
echo Compilation finished, all is well
echo New compiler at $(realpath nucc)

# If there are unsaved changes since the last commit, then we don't want to
# save a snapshot because it won't actually correspond to the version of the
# code at the current commit.
clean="$(git status --porcelain --untracked-files=no)"

if [ "$save_snapshot" = true ] && [ -z "$clean" ]; then
	# If we don't have a snapshot for this commit, save one
	# TODO: This should check that the new snapshot is different to the old one, as
	# not all commits change the compiler.
	snapshots_dir="$script_dir/snapshots"
	snapshot_name="$snapshots_dir/nucc_$(git rev-parse --short=12 HEAD)"
	if [ ! -f "$snapshot_name" ]; then
		echo "Saving a shapshot to $snapshot_name"
		cp nucc "$snapshot_name"
	fi
fi
