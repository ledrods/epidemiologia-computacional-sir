# Baixa do portal de dados abertos do Recife os casos confirmados de chikungunya
# e as tabelas auxiliares. Os arquivos brutos trazem endereço e ficam em
# data/raw/, fora do controle de versão. Só o agregado vai para data/processed/.

source("R/load_all.R")

PORTAL <- "https://dados.recife.pe.gov.br/api/3/action/package_show?id=casos-de-dengue-zika-e-chikungunya"

pacote <- jsonlite::fromJSON(PORTAL)$result
recursos <- pacote$resources

alvos <- grepl("chikungunya", recursos$name, ignore.case = TRUE) &
  grepl("confirmados", recursos$name, ignore.case = TRUE) &
  toupper(recursos$format) == "CSV"
alvos <- alvos | recursos$name %in% c("Tabela de Bairros", "Tabela Distrito")

dir.create(DIRS$raw, showWarnings = FALSE, recursive = TRUE)

for (i in which(alvos)) {
  ano <- regmatches(recursos$name[i], regexpr("[0-9]{4}", recursos$name[i]))
  nome <- if (length(ano)) sprintf("chikungunya_%s.csv", ano) else
    paste0(gsub("[^a-z]+", "_", tolower(recursos$name[i])), ".csv")
  destino <- file.path(DIRS$raw, nome)
  if (!file.exists(destino)) {
    download.file(recursos$url[i], destino, mode = "wb", quiet = TRUE)
  }
  cat(sprintf("  %-28s %8.1f kB\n", nome, file.size(destino) / 1024))
}

cat("\nAgregando\n")
casos <- le_casos_chikungunya()
semanal <- agrega_semanal(casos)
anual <- agrega_distrito_ano(casos)

dir.create(DIRS$processed, showWarnings = FALSE, recursive = TRUE)
write.csv(semanal, file.path(DIRS$processed, "chik_recife_semanal.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(anual, file.path(DIRS$processed, "chik_recife_distrito_ano.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nCasos prováveis (notificados menos descartados) por ano de início de sintomas\n")
print(aggregate(casos ~ ano, data = anual, FUN = sum), row.names = FALSE)
