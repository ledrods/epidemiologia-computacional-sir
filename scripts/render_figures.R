# Regenera todas as figuras e tabelas do estudo e confere os resultados contra
# as expressões analíticas. Falha se algum valor divergir.

source("R/load_all.R")

confere <- function(descricao, obtido, esperado, tolerancia = 0.01) {
  desvio <- abs(obtido - esperado) / max(abs(esperado), 1e-12)
  status <- if (desvio <= tolerancia) "ok" else "FALHOU"
  cat(sprintf("  [%s] %-48s obtido %.7g, esperado %.7g\n",
              status, descricao, obtido, esperado))
  if (desvio > tolerancia) {
    stop(sprintf("Conferência falhou: %s (desvio de %.3f por cento)", descricao, 100 * desvio))
  }
}

confere_verdade <- function(descricao, condicao) {
  cat(sprintf("  [%s] %s\n", if (condicao) "ok" else "FALHOU", descricao))
  if (!condicao) stop(sprintf("Conferência falhou: %s", descricao))
}

N <- RECIFE$populacao
processados <- file.path(DIRS$processed, c("chik_recife_semanal.csv", "chik_recife_distrito_ano.csv",
                                           "chik_recife_ano.csv", "chik_recife_boletim_2021.csv"))
faltando <- processados[!file.exists(processados)]
if (length(faltando)) stop("Rode antes scripts/baixar_dados.R para gerar ", paste(faltando, collapse = ", "))

cat("Simulando a linha de base\n")
sim <- run_sir_model(beta = CHIKV$beta, gamma = CHIKV$gamma)
resumo_base <- epidemic_summary(sim, "Linha de base")
validacao <- validacao_sir(sim)

cat("\nConferência da linha de base contra a teoria\n")
for (i in seq_len(nrow(validacao))) {
  confere(validacao$grandeza[i], validacao$simulado[i], validacao$analitico[i])
}
inicio <- sim$wide[sim$wide$time >= 1 & sim$wide$time <= 12, ]
confere("crescimento inicial, beta - gamma",
        unname(coef(lm(log(I) ~ time, data = inicio))[2]), CHIKV$beta - CHIKV$gamma, 0.02)

cat("\nSimulando os dez cenários\n")
cenarios <- run_scenarios()

cat("\nConferência de cada cenário\n")
for (i in seq_len(nrow(cenarios$resumo))) {
  linha <- cenarios$resumo[i, ]
  if (linha$R0 > 1 + 1e-9) {
    confere(sprintf("cenário %d, taxa de ataque", linha$id),
            linha$taxa_ataque, linha$taxa_ataque_teorica)
  } else if (linha$R0 < 1 - 1e-9) {
    confere(sprintf("cenário %d, total I0 / (1 - R0)", linha$id),
            linha$infectados_total, linha$total_subcritico_teorico)
  } else {
    confere(sprintf("cenário %d, R0 = 1, total I0 (1 + beta t)", linha$id),
            linha$infectados_total, BASELINE$I0 * (1 + linha$beta * linha$duracao))
  }
}

cat("\nConferência da invariância de R0 entre os cenários 1 e 10\n")
base <- cenarios$resumo[cenarios$resumo$id == 1, ]
metade <- cenarios$resumo[cenarios$resumo$id == 10, ]
confere("mesma taxa de ataque", metade$taxa_ataque, base$taxa_ataque)
confere("mesma prevalência no pico", metade$I_pico, base$I_pico)
confere("pico no dobro do tempo", metade$dia_pico, 2 * base$dia_pico, tolerancia = 0.03)

cat("\nConferência das extensões do SIR\n")
sirs_zero <- run_sir_model(CHIKV$beta, CHIKV$gamma, omega = 0)$wide
confere("SIRS com omega = 0 igual ao SIR", max(sirs_zero$I), max(sim$wide$I), 1e-9)
sem_controle <- run_sir_model(CHIKV$beta, CHIKV$gamma,
                              modelo = make_sir_model(foi_com_controle(0, 0)))$wide
confere("controle com eficácia 0 igual à linha de base", max(sem_controle$I), max(sim$wide$I), 1e-9)
critica <- eficacia_critica(CHIKV$R0)
acima <- run_sir_model(CHIKV$beta, CHIKV$gamma, duracao = 730,
                       modelo = make_sir_model(foi_com_controle(critica + 0.02, 0)))$wide
confere_verdade(sprintf("eficácia acima da crítica (%.1f%%) desde o dia 0 impede o surto", 100 * critica),
                max(acima$I) <= BASELINE$I0 + 1e-9)
for (e in c(0.3, 0.6)) {
  sim_e <- run_sir_model(CHIKV$beta, CHIKV$gamma, duracao = 3000,
                         modelo = make_sir_model(foi_com_controle(e, 0)))$wide
  confere(sprintf("controle de %.0f%% desde o dia 0, tamanho final", 100 * e),
          1 - sim_e$S[nrow(sim_e)] / N, tamanho_final_teorico((1 - e) * CHIKV$R0))
}

cat("\nConferência do modelo SEIR-SEI\n")
p_vet <- parametros_vetor()
confere("R0 de humano para humano calibrado", r_humano_humano(p_vet), CHIKV$R0, 1e-12)
comp <- comparacao_sir_vetorial(duracao = 400)
vw <- comp$vetorial$wide
confere("população humana conservada", max(rowSums(vw[, c("Sh", "Eh", "Ih", "Rh")])), N, 1e-9)
confere("população de mosquitos conservada", max(rowSums(vw[, c("Sv", "Ev", "Iv")])),
        p_vet[["m"]] * N, 1e-9)
fase <- vw[vw$time >= 20 & vw$time <= 50, ]
r_vet <- taxa_crescimento_vetorial(p_vet)
confere("crescimento inicial contra autovalor do Jacobiano",
        unname(coef(lm(log(Ih) ~ time, data = fase))[2]), r_vet, 0.02)
ataque_vet <- 1 - vw$Sh[nrow(vw)] / N
aproximado <- tamanho_final_teorico(r_humano_humano(p_vet))
# A relação 1 - a = exp(-R0 a) supõe que quase todo mosquito siga suscetível.
# Como até um décimo deles se infecta no pico, o surto real fica um pouco menor.
confere("tamanho final perto de 1 - a = exp(-R0 a), aproximação", ataque_vet, aproximado, 0.02)
confere_verdade("tamanho final simulado não excede a aproximação", ataque_vet <= aproximado)
abaixo <- run_seir_sei_model(parametros_vetor(R0_humano = 0.9), duracao = 730)$wide
confere_verdade("R0 de humano para humano 0,9 não gera surto", max(abaixo$Ih) <= BASELINE$I0 + 1e-9)

cat("\nAnálises complementares\n")
grade <- grade_fases(betas = seq(0.01, 1.2, length.out = 160),
                     gammas = seq(0.01, 1.2, length.out = 160))
efic <- varredura_eficacia()
inicio_grade <- varredura_inicio(duracao = 5000)
sirs <- cenarios_sirs()
ctrl <- controle_vetorial()
serie <- le_serie_semanal()
cresc <- crescimento_observado(serie)
r0s <- r0_pela_taxa(cresc$r)
ondas <- comparacao_ondas(serie)
distritos <- comparacao_distritos(le_distrito_ano())
anual <- read.csv(file.path(DIRS$processed, "chik_recife_ano.csv"))
boletim <- read.csv(file.path(DIRS$processed, "chik_recife_boletim_2021.csv"))
primeira_onda <- serie$semana < as.Date("2017-01-01")
pico_2016 <- serie$semana[primeira_onda][which.max(serie$provaveis[primeira_onda])]

cat("\nConferência dos dados contra a época e entre si\n")
confere("notificados de 2021 até 26/10 contra o boletim da época", boletim$notificados,
        BOLETIM_2021$notificados, 0.05)
confere("prováveis de 2021 até 26/10 contra o boletim da época", boletim$provaveis,
        BOLETIM_2021$provaveis, 0.05)
confere("prováveis de 2015 e 2016, tabela anual contra série semanal",
        sum(anual$provaveis[anual$ano %in% 2015:2016]), sum(serie$provaveis[primeira_onda]), 1e-12)
confere_verdade("a segunda onda cresce mais rápido que a primeira, sem sobreposição dos intervalos",
                ondas$r_inf[2] > ondas$r_sup[1])
confere_verdade("o distrito com mais casos na primeira onda tem menos casos em 2021",
                distritos$razao[which.max(distritos$primeira_onda)] < 1)

# O que cada modelo homogêneo prevê para os pontos observáveis da epidemia.
suscetiveis_2021 <- 1 - SOROLOGIA$prevalencia[1]
ataque_vet_base <- resumo_vetorial(comp$vetorial)$taxa_ataque
antes_pico <- serie[serie$semana >= as.Date("2015-01-01") & serie$semana <= pico_2016, ]
casos_continuos <- max(antes_pico$semana[antes_pico$provaveis == 0]) + 7
razao_cidade <- sum(distritos$segunda_onda) / sum(distritos$primeira_onda)
infeccoes_2015_2016 <- SOROLOGIA$prevalencia[1] * N
modelo_contra_dados <- data.frame(
  ponto = c("taxa de ataque", "crescimento da primeira onda por dia", "dias até o pico",
            "R efetivo ao fim da primeira onda", "crescimento da segunda onda sobre o da primeira",
            "razão 2021 sobre a primeira onda, menor distrito",
            "razão 2021 sobre a primeira onda, maior distrito",
            "infecções por caso provável", "óbitos pelo agravo na primeira onda"),
  SIR = c(resumo_base$taxa_ataque, CHIKV$beta - CHIKV$gamma, resumo_base$dia_pico,
          CHIKV$R0 * (1 - resumo_base$taxa_ataque),
          (CHIKV$beta * suscetiveis_2021 - CHIKV$gamma) / (CHIKV$beta - CHIKV$gamma),
          razao_cidade, razao_cidade, NA, NA),
  SEIR_SEI = c(ataque_vet_base, r_vet, resumo_vetorial(comp$vetorial)$dia_pico,
               CHIKV$R0 * (1 - ataque_vet_base),
               taxa_crescimento_vetorial(parametros_vetor(R0_humano = CHIKV$R0 * suscetiveis_2021)) / r_vet,
               razao_cidade, razao_cidade, NA, NA),
  observado = c(SOROLOGIA$prevalencia[1], cresc$r, as.numeric(pico_2016 - casos_continuos),
                NA, ondas$r[2] / ondas$r[1], min(distritos$razao), max(distritos$razao),
                infeccoes_2015_2016 / sum(serie$provaveis[primeira_onda]),
                sum(anual$obitos[anual$ano %in% 2015:2016]))
)

sorologia <- rbind(
  data.frame(estrato = sprintf("SIR com R0 = %s", num_br(CHIKV$R0, 1)),
             prevalencia = resumo_base$taxa_ataque, ic_inf = NA, ic_sup = NA, fonte = "Modelo"),
  data.frame(SOROLOGIA, fonte = "Braga et al. (2023)")
)
sorologia$R0_implicito <- r0_implicito(sorologia$prevalencia)
confere("R0 implícito do próprio SIR devolve 3,4", sorologia$R0_implicito[1], CHIKV$R0, 1e-6)

cat("\nGravando figuras\n")
figuras <- list(
  list("fig01_diagrama_sir.png", figura_diagrama(), 9, 4.2),
  list("fig02_linha_base_sir.png", figura_linha_base(sim), 9, 5),
  list("fig03_pico_epidemico.png", figura_pico(sim, resumo_base), 9, 5),
  list("fig04_cenarios_painel.png", figura_cenarios_painel(cenarios$curvas, cenarios$resumo), 15, 7.4),
  list("fig05_cenarios_infectados.png", figura_cenarios_infectados(cenarios$curvas, cenarios$resumo), 10, 5.6),
  list("fig06_regime_subcritico.png", figura_regime_subcritico(cenarios$curvas, cenarios$resumo), 10, 5.6),
  list("fig07_mapa_fases.png", figura_mapa_fases(grade, cenarios$resumo), 9, 7.4),
  list("fig08_limiar_r0.png", figura_limiar(cenarios$resumo), 12, 4.6),
  list("fig09_controle_eficacia.png", figura_controle_eficacia(efic), 11, 5),
  list("fig10_controle_inicio.png", figura_controle_inicio(inicio_grade, resumo_base$dia_pico), 11, 6.4),
  list("fig11_imunidade_sirs.png", figura_imunidade(sirs), 10, 5.2),
  list("fig12_dados_recife.png", figura_dados_recife(serie), 11, 8),
  list("fig13_crescimento_observado.png", figura_crescimento(serie, cresc, r0s), 10, 5.6),
  list("fig14_sorologia.png", figura_sorologia(sorologia), 10, 4.8),
  list("fig15_diagrama_seir_sei.png", figura_diagrama_seir_sei(p_vet), 11, 5.6),
  list("fig16_sir_vs_seir_sei.png", figura_sir_vs_vetorial(comp), 10, 5.4),
  list("fig17_controle_vetorial.png", figura_controle_vetorial(ctrl), 12, 5.4),
  list("fig18_duas_ondas.png", figura_duas_ondas(serie, ondas), 10, 8),
  list("fig19_distritos.png", figura_distritos(distritos), 9, 5.2)
)

for (f in figuras) {
  caminho <- save_figure(f[[2]], f[[1]], largura = f[[3]], altura = f[[4]])
  cat("  ", caminho, "\n")
}

cat("\nGravando tabelas\n")
colunas_resumo <- c("id", "cenario", "familia", "regime", "beta", "gamma", "R0",
                    "periodo_infeccioso", "dia_pico", "S_pico", "I_pico", "R_pico",
                    "prevalencia_pico", "infectados_total", "taxa_ataque",
                    "taxa_ataque_teorica", "total_subcritico_teorico", "duracao")

segunda_onda <- do.call(rbind, lapply(split(sirs, sirs$cenario), function(d) {
  depois <- d[d$time > 365, ]
  data.frame(cenario = d$cenario[1], omega = d$omega[1],
             dia_maior_pico_apos_ano_1 = depois$time[which.max(depois$I)],
             I_maior_pico_apos_ano_1 = max(depois$I))
}))

parametros_tabela <- data.frame(
  parametro = c("a", "b", "c", "1/sigma_h", "1/gamma", "1/sigma_v", "1/mu_v", "m"),
  descricao = c("picadas por mosquito por dia", "prob. mosquito infectar humano",
                "prob. humano infectar mosquito", "latência humana (dias)",
                "período infeccioso humano (dias)", "incubação extrínseca (dias)",
                "vida média do mosquito (dias)", "mosquitos por humano"),
  valor = c(p_vet[["a"]], p_vet[["b"]], p_vet[["c"]], 1 / p_vet[["sigma_h"]],
            1 / p_vet[["gamma"]], 1 / p_vet[["sigma_v"]], 1 / p_vet[["mu_v"]], p_vet[["m"]]),
  fonte = c("Manore et al. 2014", "Manore et al. 2014", "Manore et al. 2014",
            "Manore et al. 2014; Rudolph et al. 2014", "Manore et al. 2014",
            "Christofferson et al. 2014", "Manore et al. 2014",
            "escolhido para R0 de humano para humano = 3,4")
)

modelos <- rbind(
  data.frame(modelo = "SIR", R0_humano = CHIKV$R0, R0_proxima_geracao = NA,
             crescimento_inicial = CHIKV$beta - CHIKV$gamma, dia_pico = resumo_base$dia_pico,
             I_pico = resumo_base$I_pico, prevalencia_pico = resumo_base$prevalencia_pico,
             taxa_ataque = resumo_base$taxa_ataque),
  cbind(resumo_vetorial(comp$vetorial, "SEIR-SEI")[, c("modelo", "R0_humano", "R0_proxima_geracao")],
        crescimento_inicial = r_vet,
        resumo_vetorial(comp$vetorial)[, c("dia_pico", "I_pico", "prevalencia_pico", "taxa_ataque")])
)

crescimento <- data.frame(
  cresc[, c("r", "r_inf", "r_sup", "inicio", "fim", "semanas")],
  duplicacao_dias = log(2) / cresc$r,
  R0_SIR = r0s[["SIR"]], ataque_SIR = tamanho_final_teorico(r0s[["SIR"]]),
  R0_SEIR_SEI = r0s[["SEIR-SEI"]], ataque_SEIR_SEI = tamanho_final_teorico(r0s[["SEIR-SEI"]]),
  provaveis_2015_2016 = sum(serie$provaveis[primeira_onda]),
  notificados_2015_fev2019 = sum(serie$notificados[serie$semana < as.Date("2019-03-01")]),
  semana_pico_2016 = pico_2016
)

tabelas <- list(
  validacao_sir.csv = validacao,
  cenarios.csv = cenarios$resumo[, colunas_resumo],
  linha_base_diaria.csv = sim$wide,
  linha_base_resumo.csv = resumo_base,
  controle_eficacia_critica.csv = data.frame(R0 = c(CHIKV$R0_faixa[1], CHIKV$R0, CHIKV$R0_faixa[2]),
                                             eficacia_critica = eficacia_critica(
                                               c(CHIKV$R0_faixa[1], CHIKV$R0, CHIKV$R0_faixa[2]))),
  controle_inicio.csv = inicio_grade,
  sirs_segunda_onda.csv = segunda_onda,
  sorologia_comparacao.csv = sorologia,
  crescimento_observado.csv = crescimento,
  seir_sei_parametros.csv = parametros_tabela,
  modelos_resumo.csv = modelos,
  controle_vetorial.csv = ctrl$resumo,
  ondas.csv = ondas,
  distritos_ondas.csv = distritos,
  modelo_contra_dados.csv = modelo_contra_dados
)

for (nome in names(tabelas)) {
  caminho <- file.path(DIRS$results, nome)
  write.csv(tabelas[[nome]], caminho, row.names = FALSE, fileEncoding = "UTF-8")
  cat("  ", caminho, "\n")
}

cat("\nConcluído sem divergências.\n")
