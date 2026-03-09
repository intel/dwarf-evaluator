#!/bin/bash

# Copyright (C) 2025-2026 Advanced Micro Devices, Inc.
# SPDX-License-Identifier: MIT

-() { exit 1; }
usage() { printf 'usage: %s target profile dep_html dep_js\n' "$0"; -; }

cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" ||-

(($# >= 4)) || usage

TARGET=$1; shift
PROFILE=$1; shift
DEP_HTML=$1; shift
DEP_JS=$1; shift

SUPPORTED_OPS=$(awk '
/^type dwarf_op =/ { printing = 1; next; }
printing==1 && /^$/ { next; }
printing==1 && /^ *\|/ { print $0; next; }
{ printing = 0; }' \
../dwarf_evaluator.ml)

awk_script=''
if [[ "$PROFILE" = release ]]; then
    awk_script+="
        /^<script defer src/ {
            print \"<script type=\\\"module\\\">\"
            while (getline line < \"$DEP_JS\") {
                print line
            }
            print \"</script>\"
            next
        }
    "
fi

awk_script+="
/SUPPORTED_OPS/ {
    print supported_ops
    next
}
{ print }"

awk -v supported_ops="$SUPPORTED_OPS" "$awk_script" "$DEP_HTML" \
    | sed \
        -e "s/GIT_REVISION/$(git describe --match='' --always --dirty)/" \
        -e "s/CURRENT_TIME/$(date -u -Iminutes)/" \
    >"$TARGET" ||-
