# Fachada sobre deSolve::ode. Esconde a montagem dos vetores de entrada e
# devolve o resultado nos dois formatos usados pelas figuras.

suppressPackageStartupMessages(suppressWarnings(library(deSolve)))
suppressPackageStartupMessages(suppressWarnings(library(reshape2)))

run_sir_model <- function(beta,
                          gamma,
                          S0 = BASELINE$S0,
                          I0 = BASELINE$I0,
                          R0 = BASELINE$R0_compartimento,
                          duracao = BASELINE$duracao_dias,
                          passo = BASELINE$passo_dias,
                          modelo = sir_model) {

  initial_state_values <- c(S = S0, I = I0, R = R0)
  parameters <- c(beta = beta, gamma = gamma)
  times <- seq(from = 0, to = duracao, by = passo)

  saida <- as.data.frame(ode(
    y = initial_state_values,
    times = times,
    func = modelo,
    parms = parameters
  ))

  list(
    wide = saida,
    long = to_long(saida),
    parameters = parameters,
    initial_state_values = initial_state_values
  )
}

# Formato longo com o compartimento já como fator rotulado, para as figuras.
to_long <- function(saida) {
  longo <- melt(saida, id.vars = "time",
                variable.name = "compartimento", value.name = "pessoas")
  longo$compartimento <- factor(longo$compartimento,
                                levels = names(VISUAL$rotulos),
                                labels = VISUAL$rotulos)
  longo
}
