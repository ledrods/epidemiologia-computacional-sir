#!/usr/bin/env bash
# Copia para overleaf/ só os arquivos que o projeto do Overleaf precisa: os .tex,
# a bibliografia, o logo e as figuras citadas na proposta. Gera também
# overleaf.zip, que o Overleaf aceita em "New Project > Upload Project".
set -euo pipefail
cd "$(dirname "$0")/.."

destino=overleaf
rm -rf "$destino" overleaf.zip
mkdir -p "$destino/sections" "$destino/figures"

cp proposta/main.tex proposta/capa.tex proposta/abstract.tex proposta/anexos.tex \
   proposta/referencias.bib "$destino/"
cp proposta/sections/*.tex "$destino/sections/"
cp proposta/figures/* "$destino/figures/"

# Figuras do estudo citadas em algum \includegraphics da proposta.
grep -ho 'includegraphics\[[^]]*\]{fig[0-9][^}]*}' proposta/sections/*.tex proposta/*.tex |
  sed 's/.*{//; s/}$//' | sort -u | while read -r figura; do
    cp "figures/$figura" "$destino/figures/"
  done

# No Overleaf as figuras ficam todas em figures/.
sed -i 's|\\graphicspath{{figures/}{../figures/}}|\\graphicspath{{figures/}}|' "$destino/main.tex"

if command -v zip > /dev/null; then
  (cd "$destino" && zip -qr ../overleaf.zip .)
  echo "overleaf/ e overleaf.zip gerados"
else
  echo "overleaf/ gerado; zip não encontrado, compacte a pasta manualmente"
fi
