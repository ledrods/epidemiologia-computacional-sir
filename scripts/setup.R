# Instala as dependências do projeto. Idempotente.

pacotes <- c("deSolve", "reshape2", "ggplot2", "scales", "patchwork")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltando)) {
  install.packages(faltando, repos = "https://cloud.r-project.org", type = "binary")
}

for (p in pacotes) {
  cat(sprintf("%-10s %s\n", p, if (requireNamespace(p, quietly = TRUE)) "ok" else "FALHOU"))
}
