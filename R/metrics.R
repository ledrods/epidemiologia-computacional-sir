# Indicadores extraídos de uma simulação: limiar, pico e tamanho final do surto.

# Fração da população infectada ao final do surto, raiz de 1 - a = exp(-R0 * a).
tamanho_final_teorico <- function(R0) {
  if (R0 <= 1) return(0)
  raiz <- uniroot(function(a) 1 - a - exp(-R0 * a),
                  interval = c(1e-12, 1 - 1e-12), tol = 1e-12)
  raiz$root
}

# Prevalência máxima teórica, em fração da população.
pico_teorico <- function(R0) {
  if (R0 <= 1) return(NA_real_)
  1 - (1 + log(R0)) / R0
}

epidemic_summary <- function(simulacao, rotulo = NA_character_) {
  wide <- simulacao$wide
  beta <- unname(simulacao$parameters[["beta"]])
  gamma <- unname(simulacao$parameters[["gamma"]])
  N <- sum(simulacao$initial_state_values)
  R0 <- beta / gamma

  i_pico <- which.max(wide$I)
  fino <- refina_pico(simulacao)

  data.frame(
    cenario = rotulo,
    beta = beta,
    gamma = gamma,
    R0 = R0,
    periodo_infeccioso = 1 / gamma,
    dia_pico = wide$time[i_pico],
    dia_pico_refinado = fino$time,
    S_pico = wide$S[i_pico],
    I_pico = wide$I[i_pico],
    R_pico = wide$R[i_pico],
    I_pico_refinado = fino$I,
    prevalencia_pico = wide$I[i_pico] / N,
    infectados_total = N - wide$S[nrow(wide)],
    taxa_ataque = (N - wide$S[nrow(wide)]) / N,
    taxa_ataque_teorica = tamanho_final_teorico(R0),
    prevalencia_pico_teorica = pico_teorico(R0),
    S_pico_teorico = if (R0 > 1) N / R0 else NA_real_,
    stringsAsFactors = FALSE
  )
}

# O pico diário é a resposta pedida pela atividade. Esta malha fina de 0,05 dia
# serve apenas para conferir que o pico não caiu entre dois passos inteiros.
refina_pico <- function(simulacao) {
  fino <- run_sir_model(
    beta = unname(simulacao$parameters[["beta"]]),
    gamma = unname(simulacao$parameters[["gamma"]]),
    S0 = unname(simulacao$initial_state_values[["S"]]),
    I0 = unname(simulacao$initial_state_values[["I"]]),
    R0 = unname(simulacao$initial_state_values[["R"]]),
    duracao = max(simulacao$wide$time),
    passo = 0.05
  )$wide

  fino[which.max(fino$I), c("time", "S", "I", "R")]
}
