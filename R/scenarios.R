# Catálogo dos cenários do item 5. Os cenários são dados, não código repetido,
# e a mesma lista alimenta a tabela comparativa e as figuras.

scenario_catalog <- function(beta0 = CHIKV$beta, gamma0 = CHIKV$gamma) {
  cenarios <- list(
    list(id = 1, rotulo = "Linha de base",
         beta = beta0, gamma = gamma0, duracao = 180,
         familia = "Referência"),
    list(id = 2, rotulo = "Taxas iguais",
         beta = gamma0, gamma = gamma0, duracao = 365,
         familia = "Taxas iguais"),
    list(id = 3, rotulo = "Contágio pela metade",
         beta = beta0 / 2, gamma = gamma0, duracao = 400,
         familia = "Recuperação fixa"),
    list(id = 4, rotulo = "Contágio 50 por cento maior",
         beta = beta0 * 1.5, gamma = gamma0, duracao = 180,
         familia = "Recuperação fixa"),
    list(id = 5, rotulo = "Viremia de 14 dias",
         beta = beta0, gamma = 1 / 14, duracao = 180,
         familia = "Contágio fixo"),
    list(id = 6, rotulo = "Recuperação em 1,7 dia",
         beta = beta0, gamma = 0.6, duracao = 365,
         familia = "Contágio fixo"),
    list(id = 7, rotulo = "Mesmo R0, ritmo pela metade",
         beta = beta0 / 2, gamma = gamma0 / 2, duracao = 365,
         familia = "R0 preservado"),
    list(id = 8, rotulo = "Controle vetorial do Aedes",
         beta = beta0 * 0.4, gamma = gamma0, duracao = 730,
         familia = "Intervenção")
  )

  for (i in seq_along(cenarios)) {
    cenarios[[i]]$R0 <- cenarios[[i]]$beta / cenarios[[i]]$gamma
  }
  cenarios
}

# Roda o catálogo inteiro e devolve as curvas em formato longo e a tabela resumo.
run_scenarios <- function(cenarios = scenario_catalog()) {
  curvas <- list()
  resumos <- list()

  for (cen in cenarios) {
    sim <- run_sir_model(beta = cen$beta, gamma = cen$gamma, duracao = cen$duracao)

    curva <- sim$long
    curva$cenario <- cen$rotulo
    curva$id <- cen$id
    curva$R0 <- cen$R0
    curva$familia <- cen$familia
    curvas[[length(curvas) + 1]] <- curva

    resumo <- epidemic_summary(sim, rotulo = cen$rotulo)
    resumo$id <- cen$id
    resumo$familia <- cen$familia
    resumo$duracao <- cen$duracao
    resumos[[length(resumos) + 1]] <- resumo
  }

  curvas <- do.call(rbind, curvas)
  resumos <- do.call(rbind, resumos)

  ordem <- vapply(cenarios, function(c) c$rotulo, character(1))
  curvas$cenario <- factor(curvas$cenario, levels = ordem)
  resumos$cenario <- factor(resumos$cenario, levels = ordem)

  list(curvas = curvas, resumo = resumos)
}
