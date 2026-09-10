# Modelo SIR do Módulo I do curso, com a força de infecção injetada como estratégia.
#
# O notebook 3 usa lambda constante e o notebook 4 usa lambda = beta * I / N.
# As duas versões diferem apenas nesse ponto, então a força de infecção entra
# como função e o restante do modelo permanece único.

# Estratégia do notebook 3: lambda fixo, independente da prevalência.
foi_constante <- function(state, parameters) {
  parameters[["lambda"]]
}

# Estratégia do notebook 4: lambda proporcional à fração infectada.
foi_dinamica <- function(state, parameters) {
  N <- sum(state[c("S", "I", "R")])
  parameters[["beta"]] * state[["I"]] / N
}

# Devolve a função no formato exigido por deSolve::ode.
make_sir_model <- function(forca_de_infeccao = foi_dinamica) {
  function(time, state, parameters) {
    lambda <- forca_de_infeccao(state, parameters)

    dS <- -lambda * state[["S"]]
    dI <-  lambda * state[["S"]] - parameters[["gamma"]] * state[["I"]]
    dR <-  parameters[["gamma"]] * state[["I"]]

    list(c(dS, dI, dR))
  }
}

sir_model <- make_sir_model(foi_dinamica)
