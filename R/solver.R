# Fachada sobre deSolve::ode. Esconde a montagem dos vetores de entrada e
# devolve o resultado nos dois formatos usados pelas figuras.

suppressPackageStartupMessages(suppressWarnings(library(deSolve)))
suppressPackageStartupMessages(suppressWarnings(library(reshape2)))

run_model <- function(modelo, initial_state_values, parameters, times, ...) {
  as.data.frame(ode(
    y = initial_state_values,
    times = times,
    func = modelo,
    parms = parameters,
    ...
  ))
}

run_sir_model <- function(beta,
                          gamma,
                          S0 = BASELINE$S0,
                          I0 = BASELINE$I0,
                          R0 = BASELINE$R0_compartimento,
                          duracao = BASELINE$duracao_dias,
                          passo = BASELINE$passo_dias,
                          omega = 0,
                          modelo = sir_model,
                          limiar_extincao = NULL) {

  initial_state_values <- c(S = S0, I = I0, R = R0)
  parameters <- c(beta = beta, gamma = gamma, omega = omega)
  times <- seq(from = 0, to = duracao, by = passo)

  # Com limiar_extincao, a integração para quando I cai abaixo dele: o surto
  # acabou e seguir integrando valores da ordem de 1e-200 só gera erro numérico.
  saida <- if (is.null(limiar_extincao)) {
    run_model(modelo, initial_state_values, parameters, times)
  } else {
    run_model(modelo, initial_state_values, parameters, times,
              rootfunc = function(t, y, p) y[["I"]] - limiar_extincao)
  }

  list(
    wide = saida,
    long = to_long(saida),
    parameters = parameters,
    initial_state_values = initial_state_values
  )
}

# Formato longo com o compartimento já como fator rotulado, para as figuras.
to_long <- function(saida, rotulos = VISUAL$rotulos) {
  longo <- melt(saida[, c("time", names(rotulos))], id.vars = "time",
                variable.name = "compartimento", value.name = "pessoas")
  longo$compartimento <- factor(longo$compartimento,
                                levels = names(rotulos),
                                labels = rotulos)
  longo
}
