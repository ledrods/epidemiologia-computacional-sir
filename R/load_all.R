# Ponto único de entrada. Notebook e scripts carregam apenas este arquivo.

.raiz_projeto <- function() {
  if (file.exists("R/load_all.R")) return(".")
  if (file.exists("../R/load_all.R")) return("..")
  stop("Execute a partir da raiz do repositório ou de notebooks/.")
}

local({
  raiz <- .raiz_projeto()
  for (arquivo in c("config.R", "sir_model.R", "seir_sei_model.R", "solver.R",
                    "metrics.R", "scenarios.R", "dados.R", "plotting.R", "figures.R")) {
    source(file.path(raiz, "R", arquivo), encoding = "UTF-8")
  }
  DIRS <<- lapply(DIRS, function(d) file.path(raiz, d))
})

for (d in DIRS) dir.create(d, showWarnings = FALSE, recursive = TRUE)
