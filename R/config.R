# Constantes do estudo: população, parâmetros da doença e identidade visual.

RECIFE <- list(
  nome = "Recife, PE",
  populacao = 1488920,
  fonte_populacao = "IBGE, Censo 2022",
  codigo_ibge = "261160"
)

# Período infeccioso de 6 dias, valor base de Manore et al. (2014), faixa de 3 a 7.
# R0 de 3,4 é a média ponderada de Haider et al. (2020), IC 95 por cento de 2,4 a 4,2.
CHIKV <- list(
  nome = "Chikungunya",
  periodo_infeccioso_dias = 6,
  R0 = 3.4,
  R0_faixa = c(2.4, 4.2)
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

# Parâmetros do modelo com vetor. Valores base de Manore et al. (2014) para
# Aedes aegypti, exceto a incubação extrínseca, que vem de Christofferson et al.
# (2014) para a linhagem ECSA, e a latência humana, que coincide com a mediana
# de Rudolph et al. (2014). A razão m entre mosquitos e humanos não é medida:
# ela é escolhida para que o número de reprodução de humano para humano seja o
# mesmo R0 usado no SIR.
VETOR <- list(
  latencia_humana_dias = 3,
  incubacao_extrinseca_dias = 8,
  vida_mosquito_dias = 14,
  picadas_por_dia = 0.5,
  prob_mosquito_para_humano = 0.24,
  prob_humano_para_mosquito = 0.24
)

# Inquérito domiciliar de Braga et al. (2023), Recife, agosto de 2018 a fevereiro
# de 2019, IgG anti CHIKV por estrato socioeconômico.
SOROLOGIA <- data.frame(
  estrato = c("Recife", "Estrato alto", "Estrato intermediário", "Estrato baixo"),
  prevalencia = c(0.357, 0.223, 0.398, 0.386),
  ic_inf = c(0.326, 0.158, 0.345, 0.336),
  ic_sup = c(0.389, 0.288, 0.450, 0.436)
)

VISUAL <- list(
  assinatura = "Leandro Cesar",
  disciplina = "Elementos de Epidemiologia Computacional",
  cores = c(S = "#2a78d6", I = "#eb6834", R = "#1baf7a"),
  rotulos = c(S = "Suscetíveis", I = "Infectados", R = "Recuperados"),
  cor_expostos = "#c2417a",
  cor_neutra = "#a3a29c",
  cor_dados = "#3d3c38",
  sequencial = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
  cinza_texto = "#52514e",
  cinza_fraco = "#8a8880",
  cinza_grade = "#e6e5e1",
  superficie = "#fcfcfb",
  largura_pol = 9,
  altura_pol = 5,
  dpi = 200
)

DIRS <- list(
  figures = "figures",
  results = "results",
  raw = "data/raw",
  processed = "data/processed"
)
