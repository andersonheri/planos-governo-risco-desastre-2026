# 02_base_candidatos.R -- monta a base de governadores (cadastro + situacao)
# e o indice dos PDFs de propostas por candidato.
#
# Saidas (data/processed/):
#   candidatos_gov.csv   -- 1 linha por candidato a governador (todos), com
#                           flags no_universo / tem_pdf
#   indice_pdfs.csv      -- 1 linha por PDF (zip, arquivo interno, versao)
#
# Universo = GOVERNADOR + deferido (DS_SITUACAO_JULGAMENTO em "DEFERIDO" ou
# "DEFERIDO EM PRAZO RECURSAL OU COM RECURSO") + plano registrado.
# A situacao vem do CSV complementar: no CSV principal ela e "#NE".

source(here::here("R", "00_setup.R"))

DEFERIDOS <- c("DEFERIDO", "DEFERIDO EM PRAZO RECURSAL OU COM RECURSO")

ler_csvs_uf <- function(pasta) {
  f <- list.files(pasta, pattern = "_[A-Z]{2}[.]csv$", full.names = TRUE)
  f <- f[!grepl("_BR[.]csv$", f)]
  d <- rbindlist(lapply(f, fread, sep = ";", encoding = "Latin-1",
                        colClasses = "character"), fill = TRUE)
  # o TSE publica em Latin-1: converte tudo para UTF-8 antes de gravar
  d[, (names(d)) := lapply(.SD, enc2utf8)]
  d
}

# descompacta os CSVs se ainda nao estiverem extraidos
if (!dir.exists(file.path(DIR_RAW, "cand")))
  unzip(file.path(DIR_RAW, "consulta_cand_2026.zip"), exdir = file.path(DIR_RAW, "cand"))
if (!dir.exists(file.path(DIR_RAW, "comp")))
  unzip(file.path(DIR_RAW, "consulta_cand_complementar_2026.zip"), exdir = file.path(DIR_RAW, "comp"))

cand <- ler_csvs_uf(file.path(DIR_RAW, "cand"))
comp <- ler_csvs_uf(file.path(DIR_RAW, "comp"))

gov <- merge(
  cand[DS_CARGO == CARGO_ALVO,
       .(SQ_CANDIDATO, SG_UF, NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO,
         NM_PARTIDO, SG_FEDERACAO, DS_GENERO, DS_COR_RACA, DS_GRAU_INSTRUCAO,
         DS_OCUPACAO, DT_NASCIMENTO, DT_GERACAO, HH_GERACAO)],
  comp[, .(SQ_CANDIDATO, DS_SITUACAO_JULGAMENTO, DS_SITUACAO_CANDIDATO_TOT,
           ST_SUBSTITUIDO, NR_IDADE_DATA_POSSE)],
  by = "SQ_CANDIDATO", all.x = TRUE
)
gov[, regiao := REGIAO_UF[SG_UF]]
gov[, deferido := DS_SITUACAO_JULGAMENTO %in% DEFERIDOS]

# ---- indice de PDFs ---------------------------------------------------------
zips <- list.files(DIR_ZIPS, pattern = "[.]zip$", full.names = TRUE)
pdfs <- rbindlist(lapply(zips, function(z) {
  u <- unzip(z, list = TRUE)
  data.table(zip = basename(z), arquivo = u$Name, bytes = u$Length)
}))
pdfs <- pdfs[grepl("[.]pdf$", arquivo, ignore.case = TRUE) & !grepl("leiame", arquivo, ignore.case = TRUE)]
pdfs[, SQ_CANDIDATO := sub("^.*/2026[A-Z]{2}([0-9]+)_[0-9]+[.]pdf$", "\\1", arquivo)]
pdfs[, versao := as.integer(sub("^.*_([0-9]+)[.]pdf$", "\\1", arquivo))]

gov[, n_pdfs := pdfs[, .N, by = SQ_CANDIDATO][match(gov$SQ_CANDIDATO, SQ_CANDIDATO), N]]
gov[is.na(n_pdfs), n_pdfs := 0L]
gov[, tem_pdf := n_pdfs > 0]
gov[, no_universo := deferido & tem_pdf]

# PDFs de outros cargos (vice, deputado) ficam de fora do indice final
pdfs <- pdfs[SQ_CANDIDATO %in% gov$SQ_CANDIDATO]

fwrite(gov,  file.path(DIR_PROC, "candidatos_gov.csv"))
fwrite(pdfs, file.path(DIR_PROC, "indice_pdfs.csv"))

cat("Governadores:", nrow(gov), "\n")
print(gov[, .N, by = DS_SITUACAO_JULGAMENTO][order(-N)])
cat("\nUniverso (deferido + plano):", gov[no_universo == TRUE, .N], "\n")
cat("Deferidos sem plano:\n"); print(gov[deferido & !tem_pdf, .(SG_UF, NM_URNA_CANDIDATO, SG_PARTIDO)])
cat("\nUniverso por UF:\n"); print(gov[no_universo == TRUE, .N, by = SG_UF][order(SG_UF)][, paste(SG_UF, N, collapse = "  ")])
cat("\nUniverso por regiao:\n"); print(gov[no_universo == TRUE, .N, by = regiao])
