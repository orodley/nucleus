#!/bin/bash

script_dir=$(dirname "$(realpath "$0")")
for nucc in "$script_dir"/*nucc*; do
	[[ "$nucc" -nt "$latest" ]] && latest="$nucc"
done

"$latest" "$@"
