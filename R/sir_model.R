# Modelo SIR do Módulo I do curso, com a força de infecção injetada como estratégia.
#
# O notebook 3 usa lambda constante e o notebook 4 usa lambda = beta * I / N.
# As duas versões diferem apenas nesse ponto, então a força de infecção entra
# como função e o restante do modelo permanece único.

# Estratégia do notebook 3: lambda fixo, independente da prevalência.
foi_constante <- function(time, state, parameters) {
  parameters[["lambda"]]
}

# Estratégia do notebook 4: lambda proporcional à fração infectada.
foi_dinamica <- function(time, state, parameters) {
  N <- sum(state[c("S", "I", "R")])
  parameters[["beta"]] * state[["I"]] / N
}

# Controle vetorial implícito: a partir do dia `inicio`, o contágio cai pela
# fração `eficacia`. Em Ross e Macdonald, beta é proporcional à densidade de
# mosquitos, então reduzir beta equivale a reduzir essa densidade.
foi_com_controle <- function(eficacia, inicio = 0) {
  function(time, state, parameters) {
    reducao <- if (time >= inicio) eficacia else 0
    (1 - reducao) * foi_dinamica(time, state, parameters)
  }
}

# Devolve a função no formato exigido por deSolve::ode. O parâmetro opcional
# omega é a taxa de perda de imunidade. Com omega igual a zero, o padrão, o
# modelo é o SIR clássico. Com omega positivo, vira SIRS.
make_sir_model <- function(forca_de_infeccao = foi_dinamica) {
  function(time, state, parameters) {
    lambda <- forca_de_infeccao(time, state, parameters)
    omega <- if ("omega" %in% names(parameters)) parameters[["omega"]] else 0

    dS <- -lambda * state[["S"]] + omega * state[["R"]]
    dI <-  lambda * state[["S"]] - parameters[["gamma"]] * state[["I"]]
    dR <-  parameters[["gamma"]] * state[["I"]] - omega * state[["R"]]

    list(c(dS, dI, dR))
  }
}

sir_model <- make_sir_model(foi_dinamica)
