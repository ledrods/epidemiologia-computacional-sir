# Leitura e agregação das notificações de chikungunya do portal de dados abertos
# do Recife. Os arquivos mudam de layout em 2021 (nomes do SINAN em maiúsculas) e
# de formato de data em 2022, então cada campo é procurado pelos dois nomes.

CAMPOS_SINAN <- list(
  numero = c("nu_notificacao", "nu_notific"),
  dt_notificacao = c("dt_notificacao", "dt_notific"),
  dt_sintoma = c("dt_diagnostico_sintoma", "dt_sin_pri"),
  municipio = c("co_municipio_residencia", "id_mn_resi"),
  distrito = c("co_distrito_residencia", "id_distrit"),
  classificacao = c("tp_classificacao_final", "classi_fin"),
  criterio = c("tp_criterio_confirmacao", "criterio")
)

# Classificação final do SINAN: 13 é chikungunya, 5 é descartado e 8 é
# inconclusivo. Os códigos 1 e 2 aparecem só até meados de 2016 e não constam no
# dicionário do portal, por isso entram como prováveis, sem confirmação.
CLASSE_CHIKUNGUNYA <- "13"
CLASSE_DESCARTADO <- "5"

le_data <- function(x) {
  x <- trimws(x)
  data <- as.Date(rep(NA_character_, length(x)))
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  br <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)
  data[iso] <- as.Date(substr(x[iso], 1, 10))
  data[br] <- as.Date(x[br], format = "%d/%m/%Y")
  data
}

# Códigos lidos como texto podem vir com ".0" no arquivo de 2017.
le_codigo <- function(x) sub("\\.0$", "", trimws(x))

# Semana epidemiológica identificada pelo domingo em que começa.
inicio_semana <- function(data) data - as.POSIXlt(data)$wday

le_arquivo_sinan <- function(caminho) {
  bruto <- read.csv(caminho, sep = ";", encoding = "UTF-8",
                    colClasses = "character", check.names = FALSE)
  names(bruto) <- tolower(names(bruto))
  campo <- function(nomes) {
    nome <- intersect(nomes, names(bruto))[1]
    if (is.na(nome)) rep(NA_character_, nrow(bruto)) else bruto[[nome]]
  }
  data.frame(
    numero = le_codigo(campo(CAMPOS_SINAN$numero)),
    dt_notificacao = le_data(campo(CAMPOS_SINAN$dt_notificacao)),
    dt_sintoma = le_data(campo(CAMPOS_SINAN$dt_sintoma)),
    municipio = le_codigo(campo(CAMPOS_SINAN$municipio)),
    distrito = le_codigo(campo(CAMPOS_SINAN$distrito)),
    classificacao = le_codigo(campo(CAMPOS_SINAN$classificacao)),
    criterio = le_codigo(campo(CAMPOS_SINAN$criterio)),
    stringsAsFactors = FALSE
  )
}

le_casos_chikungunya <- function(pasta = DIRS$raw) {
  arquivos <- list.files(pasta, pattern = "^chikungunya_[0-9]{4}\\.csv$", full.names = TRUE)
  casos <- do.call(rbind, lapply(arquivos, le_arquivo_sinan))

  casos <- casos[casos$municipio == RECIFE$codigo_ibge, ]
  casos <- casos[!duplicated(casos[, c("numero", "dt_notificacao")]), ]

  # Sem data de sintomas válida, vale a data de notificação.
  invalida <- is.na(casos$dt_sintoma) | casos$dt_sintoma > casos$dt_notificacao |
    casos$dt_sintoma < casos$dt_notificacao - 180
  casos$dt_sintoma[invalida] <- casos$dt_notificacao[invalida]
  casos <- casos[!is.na(casos$dt_sintoma) & casos$dt_sintoma >= as.Date("2015-01-01"), ]

  casos$semana <- inicio_semana(casos$dt_sintoma)
  casos$ano <- as.integer(format(casos$dt_sintoma, "%Y"))
  casos$provavel <- casos$classificacao != CLASSE_DESCARTADO
  casos$confirmado <- casos$classificacao == CLASSE_CHIKUNGUNYA
  casos
}

agrega_semanal <- function(casos) {
  semanas <- seq(min(casos$semana), max(casos$semana), by = 7)
  conta <- function(filtro) as.integer(table(factor(casos$semana[filtro], levels = semanas)))
  data.frame(
    semana = semanas,
    notificados = conta(rep(TRUE, nrow(casos))),
    provaveis = conta(casos$provavel),
    confirmados = conta(casos$confirmado)
  )
}

agrega_distrito_ano <- function(casos) {
  provaveis <- casos[casos$provavel, ]
  tabela <- as.data.frame(table(ano = provaveis$ano, distrito = provaveis$distrito),
                          stringsAsFactors = FALSE)
  names(tabela)[3] <- "casos"
  tabela$ano <- as.integer(tabela$ano)
  tabela[order(tabela$ano, tabela$distrito), ]
}

le_serie_semanal <- function(caminho = file.path(DIRS$processed, "chik_recife_semanal.csv")) {
  serie <- read.csv(caminho, stringsAsFactors = FALSE)
  serie$semana <- as.Date(serie$semana)
  serie
}
