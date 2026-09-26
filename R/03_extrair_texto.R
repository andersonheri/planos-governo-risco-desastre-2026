# 03_extrair_texto.R -- extrai o texto dos PDFs do universo com pdftotext
# (poppler) e consolida 1 texto por candidato.
#
# Regras para candidatos com varios PDFs:
#   1. remove copias identicas (mesmo hash do texto normalizado);
#   2. descarta arquivos com quase nenhum texto (< MIN_PALAVRAS_ARQ) quando o
#      candidato tem outro arquivo com texto (capas/imagens);
#   3. concatena os restantes em ordem de versao.
# Planos com mais de PALAVRAS_ANEXOS palavras sao marcados plano_com_anexos e
# analisados a parte (ex.: plano + anexos tecnicos).
# PDFs sem texto extraivel (imagem) ficam com precisa_ocr = TRUE para o OCR
# com qwen3-vl (etapa 03b).
#
# Saidas: data/processed/texto/<SQ_CANDIDATO>.txt (paginas separadas por \f)
#         data/processed/planos_meta.csv

source(here::here("R", "00_setup.R"))
library(digest)

MIN_PALAVRAS_ARQ  <- 300
PALAVRAS_ANEXOS   <- 100000
MIN_PALAVRAS_PAG  <- 25   # abaixo disso o PDF e tratado como imagem

stopifnot(nzchar(Sys.which("pdftotext")))
DIR_TXT <- file.path(DIR_PROC, "texto"); dir.create(DIR_TXT, showWarnings = FALSE)
DIR_TMP <- file.path(DIR_PROC, "pdfs_tmp"); dir.create(DIR_TMP, showWarnings = FALSE)

gov  <- fread(file.path(DIR_PROC, "candidatos_gov.csv"), colClasses = c(SQ_CANDIDATO = "character"))
pdfs <- fread(file.path(DIR_PROC, "indice_pdfs.csv"),    colClasses = c(SQ_CANDIDATO = "character"))
pdfs <- pdfs[SQ_CANDIDATO %in% gov[no_universo == TRUE, SQ_CANDIDATO]][order(SQ_CANDIDATO, versao)]

contar_palavras <- function(x) length(strsplit(trimws(x), "\\s+")[[1]][nzchar(strsplit(trimws(x), "\\s+")[[1]])])

extrair <- function(i) {
  r <- pdfs[i]
  unzip(file.path(DIR_ZIPS, r$zip), files = r$arquivo, exdir = DIR_TMP, junkpaths = TRUE)
  p <- file.path(DIR_TMP, basename(r$arquivo))
  on.exit(unlink(p))
  tx <- suppressWarnings(system2("pdftotext", c("-enc", "UTF-8", shQuote(p), "-"), stdout = TRUE))
  tx <- paste(tx, collapse = "\n"); Encoding(tx) <- "UTF-8"
  info <- suppressWarnings(system2("pdfinfo", shQuote(p), stdout = TRUE))
  pag <- as.integer(sub("^Pages:\\s+", "", grep("^Pages:", info, value = TRUE)))
  if (length(pag) == 0) pag <- NA_integer_
  data.table(SQ_CANDIDATO = r$SQ_CANDIDATO, versao = r$versao, arquivo = r$arquivo,
             paginas = pag, palavras = contar_palavras(tx),
             hash = digest(gsub("\\s+", " ", tx)), texto = tx)
}

message("Extraindo ", nrow(pdfs), " PDFs...")
ext <- rbindlist(lapply(seq_len(nrow(pdfs)), function(i) { if (i %% 25 == 0) message(i); extrair(i) }))
ext[, palavras_pag := palavras / pmax(paginas, 1)]

# regra 1: copias identicas
ext[, dup := duplicated(hash), by = SQ_CANDIDATO]
# regra 2: arquivos quase sem texto quando ha outro com texto
ext[, tem_outro_texto := any(palavras >= MIN_PALAVRAS_ARQ), by = SQ_CANDIDATO]
ext[, descartado := dup | (palavras < MIN_PALAVRAS_ARQ & tem_outro_texto)]
ext[, motivo := fcase(dup, "copia_identica",
                      descartado, "quase_sem_texto", default = "usado")]

usados <- ext[descartado == FALSE]

# planos com anexos: o documento principal e a menor versao; as demais
# versoes viram anexos, gravados a parte (<SQ>_anexos.txt) e fora dos
# indicadores principais.
sq_anexos <- usados[, .(pal = sum(palavras)), by = SQ_CANDIDATO][pal > PALAVRAS_ANEXOS, SQ_CANDIDATO]
usados[, anexo := SQ_CANDIDATO %in% sq_anexos & versao != min(versao), by = SQ_CANDIDATO]

for (s in unique(usados$SQ_CANDIDATO)) {
  writeLines(paste(usados[SQ_CANDIDATO == s & anexo == FALSE]$texto, collapse = "\n\f"),
             file.path(DIR_TXT, paste0(s, ".txt")), useBytes = TRUE)
  if (s %in% sq_anexos) {
    writeLines(paste(usados[SQ_CANDIDATO == s & anexo == TRUE]$texto, collapse = "\n\f"),
               file.path(DIR_TXT, paste0(s, "_anexos.txt")), useBytes = TRUE)
  }
}
unlink(DIR_TMP, recursive = TRUE)

meta <- usados[anexo == FALSE, .(n_pdfs_usados = .N, paginas = sum(paginas, na.rm = TRUE),
                                 palavras = sum(palavras)), by = SQ_CANDIDATO]
meta <- merge(meta, usados[, .(palavras_anexos = sum(palavras[anexo == TRUE])), by = SQ_CANDIDATO], by = "SQ_CANDIDATO")
meta <- merge(meta, ext[, .(n_pdfs_total = .N, n_descartados = sum(descartado)), by = SQ_CANDIDATO], by = "SQ_CANDIDATO")
meta[, palavras_pag := palavras / pmax(paginas, 1)]
meta[, precisa_ocr := palavras_pag < MIN_PALAVRAS_PAG]
meta[, plano_curto := palavras < LIMIAR_PLANO_CURTO]
meta[, plano_com_anexos := palavras_anexos > 0]
meta <- merge(gov[, .(SQ_CANDIDATO, SG_UF, regiao, NM_URNA_CANDIDATO, SG_PARTIDO)], meta, by = "SQ_CANDIDATO")

fwrite(meta, file.path(DIR_PROC, "planos_meta.csv"))
fwrite(ext[, !"texto"], file.path(DIR_TAB, "log_extracao_pdfs.csv"))

cat("Planos extraidos:", nrow(meta), "\n")
print(summary(meta$palavras))
cat("\nPlanos curtos (<", LIMIAR_PLANO_CURTO, "palavras):", meta[plano_curto == TRUE, .N], "\n")
cat("Precisam de OCR:", meta[precisa_ocr == TRUE, .N], "\n")
cat("Plano com anexos:", meta[plano_com_anexos == TRUE, .N], "\n")
cat("Arquivos descartados:\n"); print(ext[, .N, by = motivo])
print(meta[precisa_ocr == TRUE | plano_curto == TRUE, .(SG_UF, NM_URNA_CANDIDATO, palavras, paginas, precisa_ocr)][order(palavras)])
