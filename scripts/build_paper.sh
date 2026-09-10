#!/usr/bin/env bash
# Compila o mini paper. Duas passagens resolvem as referências cruzadas.
set -e

PDFLATEX="${PDFLATEX:-pdflatex}"
cd "$(dirname "$0")/../paper"

"$PDFLATEX" -interaction=nonstopmode mini_paper.tex > /dev/null
"$PDFLATEX" -interaction=nonstopmode mini_paper.tex > /dev/null

echo "paper/mini_paper.pdf gerado"
