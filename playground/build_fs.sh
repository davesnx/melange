#!/usr/bin/env bash
set -euo pipefail

# The package dependency puts the complete Melange installation on OCAMLPATH.
melange_lib=$(ocamlfind query melange)

(
  cd "$melange_lib"
  find \
    __private__/melange_mini_stdlib/melange \
    js/melange \
    belt/melange \
    melange \
    dom/melange \
    \( -name '*.cmi' -o -name '*.cmj' \) -print0
) |
  LC_ALL=C sort -z |
  xargs -0 js_of_ocaml build-fs -I "$melange_lib" -o "$1"
