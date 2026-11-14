#!/usr/bin/env bash

case "$(uname -s)" in
CYGWIN* | MSYS* | MINGW*)
  melange_root=$(cygpath -m "$INSIDE_DUNE")
  melange_sep=';'
  ;;
*)
  melange_root=$INSIDE_DUNE
  melange_sep=:
  ;;
esac

export MELANGELIB="$melange_root/jscomp/stdlib/.stdlib.objs/melange$melange_sep$melange_root/jscomp/runtime/.js.objs/melange"

unset melange_root melange_sep
