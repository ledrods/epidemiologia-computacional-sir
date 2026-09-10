# Constantes do estudo: população, parâmetros da doença e identidade visual.

RECIFE <- list(
  nome = "Recife, PE",
  populacao = 1488920,
  fonte_populacao = "IBGE, Censo 2022"
)

CHIKV <- list(
  nome = "Chikungunya",
  periodo_infeccioso_dias = 7,
  R0 = 3.4
)

CHIKV$gamma <- 1 / CHIKV$periodo_infeccioso_dias
CHIKV$beta  <- CHIKV$R0 * CHIKV$gamma

BASELINE <- list(
  S0 = RECIFE$populacao - 1,
  I0 = 1,
  R0_compartimento = 0,
  duracao_dias = 180,
  passo_dias = 1
)

VISUAL <- list(
  assinatura = "Leandro Cesar",
  disciplina = "Elementos de Epidemiologia Computacional",
  cores = c(S = "#2a78d6", I = "#eb6834", R = "#1baf7a"),
  rotulos = c(S = "Suscetíveis", I = "Infectados", R = "Recuperados"),
  cinza_texto = "#52514e",
  cinza_fraco = "#8a8880",
  cinza_grade = "#e6e5e1",
  superficie = "#fcfcfb",
  largura_pol = 9,
  altura_pol = 5,
  dpi = 200
)

DIRS <- list(figures = "figures", results = "results")
