# Regenera todas as figuras e tabelas do estudo e confere os resultados contra
# as expressões analíticas do modelo SIR. Falha se algum valor divergir.

source("R/load_all.R")

confere <- function(descricao, obtido, esperado, tolerancia = 0.01) {
  desvio <- abs(obtido - esperado) / max(abs(esperado), 1e-12)
  status <- if (desvio <= tolerancia) "ok" else "FALHOU"
  cat(sprintf("  [%s] %-42s obtido %.6g, esperado %.6g\n",
              status, descricao, obtido, esperado))
  if (desvio > tolerancia) {
    stop(sprintf("Conferência falhou: %s (desvio de %.2f por cento)", descricao, 100 * desvio))
  }
}

cat("Simulando a linha de base\n")
sim <- run_sir_model(beta = CHIKV$beta, gamma = CHIKV$gamma)
resumo_base <- epidemic_summary(sim, "Linha de base")
N <- RECIFE$populacao
pico_fino <- refina_pico(sim)

cat("\nConferência da linha de base contra a teoria\n")
confere("prevalência no pico", pico_fino$I, pico_teorico(CHIKV$R0) * N)
confere("suscetíveis no pico, N / R0", pico_fino$S, N / CHIKV$R0)
confere("taxa de ataque final", resumo_base$taxa_ataque, tamanho_final_teorico(CHIKV$R0))

cat("\nSimulando os oito cenários\n")
cenarios <- run_scenarios()

cat("\nConferência de cada cenário\n")
for (i in seq_len(nrow(cenarios$resumo))) {
  linha <- cenarios$resumo[i, ]
  if (linha$R0 > 1) {
    confere(sprintf("cenário %d, taxa de ataque", linha$id),
            linha$taxa_ataque, linha$taxa_ataque_teorica)
  } else {
    stopifnot(linha$I_pico <= BASELINE$I0)
    cat(sprintf("  [ok] cenário %d sem surto, I nunca supera o caso inicial\n", linha$id))
  }
}

cat("\nConferência da invariância de R0 entre os cenários 1 e 7\n")
base <- cenarios$resumo[cenarios$resumo$id == 1, ]
metade <- cenarios$resumo[cenarios$resumo$id == 7, ]
confere("mesma taxa de ataque", metade$taxa_ataque, base$taxa_ataque)
confere("mesma prevalência no pico", metade$I_pico, base$I_pico)
confere("pico no dobro do tempo", metade$dia_pico, 2 * base$dia_pico, tolerancia = 0.03)

cat("\nGravando figuras\n")
figuras <- list(
  list("fig1_diagrama_sir.png", figura_diagrama(), 9, 4.2),
  list("fig2_linha_base_sir.png", figura_linha_base(sim), 9, 5),
  list("fig3_pico_epidemico.png", figura_pico(sim, resumo_base), 9, 5),
  list("fig4_cenarios_painel.png", figura_cenarios_painel(cenarios$curvas, cenarios$resumo), 13, 7),
  list("fig5_cenarios_infectados.png", figura_cenarios_infectados(cenarios$curvas, cenarios$resumo), 10, 5.6),
  list("fig6_limiar_r0.png", figura_limiar(cenarios$resumo), 12, 4.6)
)

for (f in figuras) {
  caminho <- save_figure(f[[2]], f[[1]], largura = f[[3]], altura = f[[4]])
  cat("  ", caminho, "\n")
}

cat("\nGravando tabelas\n")
colunas_resumo <- c("id", "cenario", "familia", "beta", "gamma", "R0",
                    "periodo_infeccioso", "dia_pico", "S_pico", "I_pico", "R_pico",
                    "prevalencia_pico", "infectados_total", "taxa_ataque",
                    "taxa_ataque_teorica", "duracao")

tabelas <- list(
  cenarios.csv = cenarios$resumo[, colunas_resumo],
  linha_base_diaria.csv = sim$wide,
  linha_base_resumo.csv = resumo_base
)

for (nome in names(tabelas)) {
  caminho <- file.path(DIRS$results, nome)
  write.csv(tabelas[[nome]], caminho, row.names = FALSE, fileEncoding = "UTF-8")
  cat("  ", caminho, "\n")
}

cat("\nConcluído sem divergências.\n")
