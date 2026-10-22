# Catálogo dos cenários do item 5 e das análises complementares. Os cenários são
# dados, não código repetido, e a mesma lista alimenta a tabela e as figuras.

regime_de <- function(beta, gamma) {
  if (abs(beta - gamma) < 1e-12) return("β = γ")
  if (beta > gamma) "β > γ" else "β < γ"
}

# Três famílias: gamma fixo com beta variando, beta fixo com gamma variando, e as
# duas taxas divididas pelo mesmo fator. Cada família atravessa os três regimes.
scenario_catalog <- function(beta0 = CHIKV$beta, gamma0 = CHIKV$gamma) {
  cenarios <- list(
    list(id = 1, rotulo = "Linha de base",
         beta = beta0, gamma = gamma0, duracao = 180, familia = "Referência"),
    list(id = 2, rotulo = "Contágio 50 por cento maior",
         beta = 1.5 * beta0, gamma = gamma0, duracao = 180, familia = "γ fixo, β varia"),
    list(id = 3, rotulo = "Contágio pela metade",
         beta = 0.5 * beta0, gamma = gamma0, duracao = 400, familia = "γ fixo, β varia"),
    list(id = 4, rotulo = "Contágio igual à recuperação",
         beta = gamma0, gamma = gamma0, duracao = 365, familia = "γ fixo, β varia"),
    list(id = 5, rotulo = "Contágio na metade da recuperação",
         beta = 0.5 * gamma0, gamma = gamma0, duracao = 365, familia = "γ fixo, β varia"),
    list(id = 6, rotulo = "Período infeccioso dobrado",
         beta = beta0, gamma = 0.5 * gamma0, duracao = 180, familia = "β fixo, γ varia"),
    list(id = 7, rotulo = "Recuperação igual ao contágio",
         beta = beta0, gamma = beta0, duracao = 365, familia = "β fixo, γ varia"),
    list(id = 8, rotulo = "Recuperação acima do contágio",
         beta = beta0, gamma = 1.25 * beta0, duracao = 365, familia = "β fixo, γ varia"),
    list(id = 9, rotulo = "Recuperação no dobro do contágio",
         beta = beta0, gamma = 2 * beta0, duracao = 365, familia = "β fixo, γ varia"),
    list(id = 10, rotulo = "Mesmo R0, ritmo pela metade",
         beta = 0.5 * beta0, gamma = 0.5 * gamma0, duracao = 365, familia = "Mesmo R0")
  )

  for (i in seq_along(cenarios)) {
    cenarios[[i]]$R0 <- cenarios[[i]]$beta / cenarios[[i]]$gamma
    cenarios[[i]]$regime <- regime_de(cenarios[[i]]$beta, cenarios[[i]]$gamma)
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
    curva$regime <- cen$regime
    curvas[[length(curvas) + 1]] <- curva

    resumo <- epidemic_summary(sim, rotulo = cen$rotulo)
    resumo$id <- cen$id
    resumo$familia <- cen$familia
    resumo$regime <- cen$regime
    resumo$duracao <- cen$duracao
    resumo$total_subcritico_teorico <- total_subcritico_teorico(cen$R0)
    resumos[[length(resumos) + 1]] <- resumo
  }

  curvas <- do.call(rbind, curvas)
  resumos <- do.call(rbind, resumos)

  ordem <- vapply(cenarios, function(c) c$rotulo, character(1))
  curvas$cenario <- factor(curvas$cenario, levels = ordem)
  resumos$cenario <- factor(resumos$cenario, levels = ordem)

  list(curvas = curvas, resumo = resumos)
}

# Mapa de fases no plano (gamma, beta). A taxa de ataque vem da equação do
# tamanho final, que só depende da razão beta / gamma.
grade_fases <- function(betas = seq(0.02, 1, length.out = 120),
                        gammas = seq(0.02, 1, length.out = 120)) {
  grade <- expand.grid(gamma = gammas, beta = betas)
  grade$R0 <- grade$beta / grade$gamma
  grade$taxa_ataque <- vapply(grade$R0, tamanho_final_teorico, numeric(1))
  grade
}

# Controle desde o primeiro dia, varrendo a eficácia para três valores de R0.
varredura_eficacia <- function(eficacias = seq(0, 0.95, by = 0.01),
                               R0s = c(CHIKV$R0_faixa[1], CHIKV$R0, CHIKV$R0_faixa[2]),
                               gamma = CHIKV$gamma) {
  linhas <- expand.grid(eficacia = eficacias, R0 = R0s)
  linhas$R0_efetivo <- (1 - linhas$eficacia) * linhas$R0
  linhas$taxa_ataque <- vapply(linhas$R0_efetivo, tamanho_final_teorico, numeric(1))
  linhas$prevalencia_pico <- vapply(linhas$R0_efetivo, function(r) {
    if (r <= 1) 0 else pico_teorico(r)
  }, numeric(1))
  linhas
}

# Controle iniciado em dias diferentes. Simula beta(t) com o mesmo modelo SIR.
varredura_inicio <- function(eficacias = seq(0, 0.9, by = 0.05),
                             inicios = seq(0, 70, by = 5),
                             beta = CHIKV$beta, gamma = CHIKV$gamma,
                             duracao = 1500) {
  grade <- expand.grid(eficacia = eficacias, inicio = inicios)
  N <- RECIFE$populacao
  resultado <- t(vapply(seq_len(nrow(grade)), function(i) {
    modelo <- make_sir_model(foi_com_controle(grade$eficacia[i], grade$inicio[i]))
    sim <- run_sir_model(beta = beta, gamma = gamma, duracao = duracao, modelo = modelo,
                         limiar_extincao = 1e-6)$wide
    c(taxa_ataque = (N - sim$S[nrow(sim)]) / N,
      I_pico = max(sim$I),
      dia_pico = sim$time[which.max(sim$I)])
  }, numeric(3)))
  cbind(grade, resultado)
}

# Perda de imunidade: SIRS com duração média da imunidade de 2 e 10 anos,
# contra a imunidade permanente do SIR, num horizonte de 10 anos.
cenarios_sirs <- function(duracoes_anos = c(Inf, 10, 2), horizonte_anos = 10) {
  do.call(rbind, lapply(duracoes_anos, function(anos) {
    omega <- if (is.infinite(anos)) 0 else 1 / (365 * anos)
    sim <- run_sir_model(beta = CHIKV$beta, gamma = CHIKV$gamma,
                         duracao = 365 * horizonte_anos, omega = omega)
    rotulo <- if (is.infinite(anos)) "Imunidade permanente (SIR)" else
      sprintf("Imunidade média de %d anos (SIRS)", anos)
    data.frame(time = sim$wide$time, I = sim$wide$I, S = sim$wide$S,
               cenario = rotulo, omega = omega)
  }))
}

# SIR e SEIR-SEI com o mesmo número de reprodução de humano para humano.
comparacao_sir_vetorial <- function(duracao = 365) {
  sir <- run_sir_model(beta = CHIKV$beta, gamma = CHIKV$gamma, duracao = duracao)
  vet <- run_seir_sei_model(parametros_vetor(), duracao = duracao)
  list(
    curvas = rbind(
      data.frame(time = sir$wide$time, I = sir$wide$I, modelo = "SIR"),
      data.frame(time = vet$wide$time, I = vet$wide$Ih, modelo = "SEIR-SEI")
    ),
    sir = sir,
    vetorial = vet
  )
}

# Controle sobre criadouros no SEIR-SEI, acima da eficácia crítica, começando em
# dias diferentes da fase de crescimento.
controle_vetorial <- function(eficacia = 0.8, inicios = c(120, 200, 250), duracao = 1460) {
  N <- RECIFE$populacao
  rodadas <- c(list(list(inicio = Inf, rotulo = "Sem controle")),
               lapply(inicios, function(d) list(inicio = d, rotulo = sprintf("Início no dia %d", d))))

  curvas <- list()
  mosquitos <- list()
  resumo <- list()
  for (r in rodadas) {
    sim <- run_seir_sei_model(parametros_vetor(eficacia = eficacia, inicio = r$inicio),
                              duracao = duracao)$wide
    Nv0 <- parametros_vetor()[["m"]] * N
    curvas[[length(curvas) + 1]] <- data.frame(time = sim$time, I = sim$Ih, serie = r$rotulo)
    mosquitos[[length(mosquitos) + 1]] <- data.frame(
      time = sim$time, fracao = (sim$Sv + sim$Ev + sim$Iv) / Nv0, serie = r$rotulo)
    resumo[[length(resumo) + 1]] <- data.frame(
      serie = r$rotulo, inicio = r$inicio, eficacia = eficacia,
      dia_pico = sim$time[which.max(sim$Ih)], I_pico = max(sim$Ih),
      taxa_ataque = 1 - sim$Sh[nrow(sim)] / N,
      crescimento_14_dias = if (is.finite(r$inicio))
        sim$Ih[sim$time == r$inicio + 14] / sim$Ih[sim$time == r$inicio] else NA_real_)
  }
  ordem <- vapply(rodadas, function(r) r$rotulo, character(1))
  curvas <- do.call(rbind, curvas)
  mosquitos <- do.call(rbind, mosquitos)
  curvas$serie <- factor(curvas$serie, levels = ordem)
  mosquitos$serie <- factor(mosquitos$serie, levels = ordem)
  list(curvas = curvas, mosquitos = mosquitos, resumo = do.call(rbind, resumo),
       eficacia = eficacia)
}

# Taxa de crescimento observada na fase inicial da primeira onda, por regressão
# log linear dos casos prováveis semanais.
crescimento_observado <- function(serie, inicio = ONDAS$inicio[1], fim = ONDAS$fim[1]) {
  fase <- serie[serie$semana >= inicio & serie$semana <= fim, ]
  ajuste <- lm(log(provaveis) ~ as.numeric(semana), data = fase)
  ic <- confint(ajuste)[2, ]
  data.frame(r = unname(coef(ajuste)[2]), r_inf = ic[[1]], r_sup = ic[[2]],
             intercepto = unname(coef(ajuste)[1]),
             inicio = inicio, fim = fim, semanas = nrow(fase))
}

# Número de reprodução de humano para humano que cada modelo precisa para gerar
# a taxa de crescimento r. No SIR, r = gamma (R0 - 1).
r0_pela_taxa <- function(r) {
  sir <- 1 + r / CHIKV$gamma
  vetorial <- uniroot(function(T) taxa_crescimento_vetorial(parametros_vetor(R0_humano = T)) - r,
                      interval = c(1 + 1e-6, 20), tol = 1e-10)$root
  c(SIR = sir, `SEIR-SEI` = vetorial)
}

# Crescimento de cada onda e o número de reprodução que ele exige. Em 2021 no
# máximo 64,3 por cento dos moradores eram suscetíveis (o inquérito é anterior à
# onda), então o R0 implícito é pelo menos o R efetivo dividido por essa fração.
comparacao_ondas <- function(serie, suscetiveis = c(1, 1 - SOROLOGIA$prevalencia[1])) {
  do.call(rbind, lapply(seq_len(nrow(ONDAS)), function(i) {
    cresc <- crescimento_observado(serie, ONDAS$inicio[i], ONDAS$fim[i])
    efetivo <- r0_pela_taxa(cresc$r)
    data.frame(onda = ONDAS$onda[i], cresc, duplicacao_dias = log(2) / cresc$r,
               suscetiveis_max = suscetiveis[i],
               R_efetivo_SIR = efetivo[["SIR"]], R_efetivo_SEIR_SEI = efetivo[["SEIR-SEI"]],
               R0_SIR = efetivo[["SIR"]] / suscetiveis[i],
               R0_SEIR_SEI = efetivo[["SEIR-SEI"]] / suscetiveis[i])
  }))
}

# Casos prováveis por distrito na primeira onda e em 2021. Se a segunda onda
# repetisse a geografia da primeira, a razão seria a mesma em todos.
comparacao_distritos <- function(distrito_ano) {
  d <- distrito_ano[distrito_ano$distrito != "", ]
  primeira <- tapply(d$casos[d$ano %in% 2015:2016], d$distrito[d$ano %in% 2015:2016], sum)
  segunda <- tapply(d$casos[d$ano == 2021], d$distrito[d$ano == 2021], sum)[names(primeira)]
  codigos <- names(primeira)
  data.frame(distrito = codigos,
             nome = ifelse(codigos %in% names(DISTRITOS), unname(DISTRITOS[codigos]),
                           paste("Código", codigos)),
             primeira_onda = as.integer(primeira), segunda_onda = as.integer(segunda),
             razao = as.numeric(segunda / primeira))
}
