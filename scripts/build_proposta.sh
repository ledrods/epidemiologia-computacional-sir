#!/usr/bin/env bash
# Compila a proposta. O bibtex roda depois da primeira passagem, e o pdflatex
# repete enquanto o LaTeX pedir para resolver referências cruzadas.
set -e

MIKTEX="${MIKTEX:-}"
PDFLATEX="${PDFLATEX:-${MIKTEX:+$MIKTEX/}pdflatex}"
BIBTEX="${BIBTEX:-${MIKTEX:+$MIKTEX/}bibtex}"
cd "$(dirname "$0")/../proposta"

"$PDFLATEX" -interaction=nonstopmode main.tex > /dev/null
"$BIBTEX" main > /dev/null
for passagem in 1 2 3 4; do
  "$PDFLATEX" -interaction=nonstopmode main.tex > /dev/null
  grep -q "Rerun to get" main.log || break
done

if grep -q "undefined" main.log; then
  grep "undefined" main.log
  echo "Há citações ou referências indefinidas" >&2
  exit 1
fi
echo "proposta/main.pdf gerado"
