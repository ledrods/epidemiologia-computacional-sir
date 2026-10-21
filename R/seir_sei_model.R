# Modelo SEIR-SEI da proposta: humanos com período de latência e o mosquito
# Aedes aegypti como compartimentos explícitos.
#
# Humanos: Sh -> Eh -> Ih -> Rh, população fechada.
# Mosquitos: Sv -> Ev -> Iv, com nascimento e morte na taxa mu_v. O mosquito
# infectado permanece infectante até morrer.
#
# Notação de Ross e Macdonald (Smith et al., 2012): a picadas por mosquito por
# dia, b a probabilidade de o mosquito infectar o humano na picada, c a de o
# humano infectar o mosquito, m mosquitos por humano.

ROTULOS_SEIR <- c(Sh = "Suscetíveis", Eh = "Expostos", Ih = "Infectados", Rh = "Recuperados")

# Humanos infectados por um mosquito recém infectado: ele sobrevive à incubação
# com probabilidade sigma_v / (sigma_v + mu_v) e depois pica por 1 / mu_v dias.
r_vetor_para_humano <- function(p) {
  p[["a"]] * p[["b"]] * p[["sigma_v"]] / ((p[["sigma_v"]] + p[["mu_v"]]) * p[["mu_v"]])
}

# Mosquitos infectados por um humano durante o período infeccioso de 1 / gamma dias.
r_humano_para_vetor <- function(p) {
  p[["a"]] * p[["c"]] * p[["m"]] / p[["gamma"]]
}

# Número de reprodução de humano para humano, uma volta completa pelo mosquito.
# É ele que governa o tamanho final do surto e que corresponde ao R0 do SIR.
r_humano_humano <- function(p) r_vetor_para_humano(p) * r_humano_para_vetor(p)

# R0 pela matriz de próxima geração, média geométrica das duas meias voltas.
r0_vetorial <- function(p) sqrt(r_humano_humano(p))

parametros_vetor <- function(R0_humano = CHIKV$R0, gamma = CHIKV$gamma, v = VETOR,
                             eficacia = 0, inicio = Inf) {
  p <- c(
    a = v$picadas_por_dia,
    b = v$prob_mosquito_para_humano,
    c = v$prob_humano_para_mosquito,
    sigma_h = 1 / v$latencia_humana_dias,
    gamma = gamma,
    sigma_v = 1 / v$incubacao_extrinseca_dias,
    mu_v = 1 / v$vida_mosquito_dias,
    m = 1,
    eficacia = eficacia,
    inicio = inicio
  )
  p[["m"]] <- R0_humano / r_humano_humano(p)
  p
}

# Controle sobre criadouros: a partir do dia `inicio`, o recrutamento de
# mosquitos cai pela fração `eficacia`, e a população adulta decai ao longo da
# vida média do mosquito.
seir_sei_model <- function(time, state, parameters) {
  p <- parameters
  Nh <- sum(state[c("Sh", "Eh", "Ih", "Rh")])
  controle <- if (time >= p[["inicio"]]) p[["eficacia"]] else 0

  lambda_h <- p[["a"]] * p[["b"]] * state[["Iv"]] / Nh
  lambda_v <- p[["a"]] * p[["c"]] * state[["Ih"]] / Nh
  recrutamento <- p[["mu_v"]] * p[["m"]] * Nh * (1 - controle)

  dSh <- -lambda_h * state[["Sh"]]
  dEh <-  lambda_h * state[["Sh"]] - p[["sigma_h"]] * state[["Eh"]]
  dIh <-  p[["sigma_h"]] * state[["Eh"]] - p[["gamma"]] * state[["Ih"]]
  dRh <-  p[["gamma"]] * state[["Ih"]]

  dSv <- recrutamento - lambda_v * state[["Sv"]] - p[["mu_v"]] * state[["Sv"]]
  dEv <- lambda_v * state[["Sv"]] - (p[["sigma_v"]] + p[["mu_v"]]) * state[["Ev"]]
  dIv <- p[["sigma_v"]] * state[["Ev"]] - p[["mu_v"]] * state[["Iv"]]

  list(c(dSh, dEh, dIh, dRh, dSv, dEv, dIv))
}

run_seir_sei_model <- function(parameters = parametros_vetor(),
                               Nh = RECIFE$populacao,
                               Ih0 = BASELINE$I0,
                               duracao = 365,
                               passo = BASELINE$passo_dias) {
  Nv <- parameters[["m"]] * Nh
  initial_state_values <- c(Sh = Nh - Ih0, Eh = 0, Ih = Ih0, Rh = 0, Sv = Nv, Ev = 0, Iv = 0)
  times <- seq(from = 0, to = duracao, by = passo)

  saida <- run_model(seir_sei_model, initial_state_values, parameters, times)

  list(
    wide = saida,
    long = to_long(saida, ROTULOS_SEIR),
    parameters = parameters,
    initial_state_values = initial_state_values
  )
}

# Taxa de crescimento inicial: autovalor dominante do sistema linearizado dos
# compartimentos infectados (Eh, Ih, Ev, Iv) no equilíbrio livre de doença.
taxa_crescimento_vetorial <- function(p) {
  J <- matrix(c(
    -p[["sigma_h"]], 0, 0, p[["a"]] * p[["b"]],
    p[["sigma_h"]], -p[["gamma"]], 0, 0,
    0, p[["a"]] * p[["c"]] * p[["m"]], -(p[["sigma_v"]] + p[["mu_v"]]), 0,
    0, 0, p[["sigma_v"]], -p[["mu_v"]]
  ), nrow = 4, byrow = TRUE)
  max(Re(eigen(J, only.values = TRUE)$values))
}

resumo_vetorial <- function(simulacao, rotulo = NA_character_) {
  wide <- simulacao$wide
  Nh <- sum(simulacao$initial_state_values[c("Sh", "Eh", "Ih", "Rh")])
  i_pico <- which.max(wide$Ih)
  data.frame(
    modelo = rotulo,
    R0_humano = r_humano_humano(simulacao$parameters),
    R0_proxima_geracao = r0_vetorial(simulacao$parameters),
    dia_pico = wide$time[i_pico],
    I_pico = wide$Ih[i_pico],
    prevalencia_pico = wide$Ih[i_pico] / Nh,
    taxa_ataque = 1 - wide$Sh[nrow(wide)] / Nh,
    stringsAsFactors = FALSE
  )
}
