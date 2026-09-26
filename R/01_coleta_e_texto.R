# 01_coleta_e_texto.R -- coleta (TSE), base de candidatos, extracao de texto dos PDFs e OCR dos escaneados.
# (script unido: cada parte corresponde a um script da versao anterior e roda em ambiente proprio, sem misturar variaveis)

source(here::here("R", "00_setup.R"))

# O download (parte 1) so roda com RODAR_DOWNLOAD=1 (baixa ~400 MB); as demais partes usam o que ja esta em data/raw.
# ==== Parte 1. Download dos dados do TSE ==========================================================
if (identical(Sys.getenv("RODAR_DOWNLOAD"), "1")) local({
# 01_baixar_dados.R -- baixa o CSV de candidatos e os 27 ZIPs de propostas
# de governo do TSE e grava um manifesto (data, tamanho, sha256) para
# documentar a coleta congelada.
#
# ATENCAO: o CDN do TSE (Akamai) devolve 403 para clientes sem cabecalho de
# navegador. Este script envia um User-Agent de navegador. Se ainda assim
# falhar, baixe os arquivos manualmente no navegador e salve-os em
#   data/raw/consulta_cand_2026.zip
#   data/raw/zips_propostas/proposta_governo_2026_<UF>.zip
# O script pula tudo o que ja existir.


library(httr2)

UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
            "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")

baixar <- function(url, destino) {
  if (file.exists(destino) && file.size(destino) > 0) {
    return(list(status = "ja_existia", http = NA_integer_))
  }
  resp <- tryCatch(
    request(url) |>
      req_user_agent(UA) |>
      req_headers(Accept = "*/*", Referer = "https://dadosabertos.tse.jus.br/") |>
      req_retry(max_tries = 3) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(path = destino),
    error = function(e) NULL
  )
  if (is.null(resp)) return(list(status = "erro_conexao", http = NA_integer_))
  if (resp_status(resp) != 200) {
    unlink(destino)
    return(list(status = "falha_http", http = resp_status(resp)))
  }
  list(status = "baixado", http = 200L)
}

alvos <- rbindlist(list(
  data.table(id = "candidatos", url = URL_CAND,
             destino = file.path(DIR_RAW, "consulta_cand_2026.zip")),
  data.table(id = paste0("proposta_", UFS), url = vapply(UFS, url_proposta, ""),
             destino = file.path(DIR_ZIPS, sprintf("proposta_governo_2026_%s.zip", UFS)))
))

res <- lapply(seq_len(nrow(alvos)), function(i) {
  message(sprintf("[%02d/%02d] %s", i, nrow(alvos), alvos$id[i]))
  r <- baixar(alvos$url[i], alvos$destino[i])
  Sys.sleep(1)  # cortesia com o servidor
  r
})

manifesto <- alvos[, `:=`(
  status = vapply(res, `[[`, "", "status"),
  http   = vapply(res, `[[`, 0L, "http"),
  bytes  = ifelse(file.exists(destino), file.size(destino), NA_real_),
  sha256 = vapply(destino, \(f) if (file.exists(f)) digest::digest(f, "sha256", file = TRUE) else NA_character_, ""),
  data_coleta = DATA_CONGELAMENTO
)]

fwrite(manifesto, file.path(DIR_TAB, "manifesto_coleta.csv"))
print(manifesto[, .N, by = status])
if (any(manifesto$status %in% c("falha_http", "erro_conexao"))) {
  warning("Ha arquivos nao baixados; veja outputs/tables/manifesto_coleta.csv")
}
})

# ==== Parte 2. Base de candidatos e indice dos PDFs ===============================================
local({
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
})

# ==== Parte 3. Extracao de texto dos PDFs =========================================================
local({
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
})

# ==== Parte 4. OCR dos planos escaneados ==========================================================
local({
# 03b_ocr_escaneados.R -- OCR (tesseract, idioma "por") dos planos cujo PDF
# nao tem camada de texto (planos_meta$precisa_ocr == TRUE).
#
# Observacao: ac_import() do acR 0.3.3 so aciona OCR para imagens (png/jpg/
# tiff); PDFs escaneados vao para o readtext e voltam vazios. Por isso as
# paginas sao renderizadas em PNG (pdftools) e passadas ao mesmo motor
# (tesseract), que e o que o acR usaria.
#
# Uso de teste: Sys.setenv(OCR_MAX_PAGES = "2") antes de rodar.
# Saidas: data/processed/texto/<SQ>.txt (sobrescreve o vazio)
#         data/processed/planos_meta.csv (atualizado)
#         outputs/tables/log_ocr.csv


suppressPackageStartupMessages({library(pdftools); library(tesseract)})

DPI       <- 200
MAX_PAGES <- as.integer(Sys.getenv("OCR_MAX_PAGES", unset = "0"))  # 0 = todas
stopifnot("por" %in% tesseract_info()$available)

DIR_TXT <- file.path(DIR_PROC, "texto")
DIR_TMP <- file.path(DIR_PROC, "ocr_tmp"); dir.create(DIR_TMP, showWarnings = FALSE)
meta <- fread(file.path(DIR_PROC, "planos_meta.csv"), colClasses = c(SQ_CANDIDATO = "character"))
log_ext <- fread(file.path(DIR_TAB, "log_extracao_pdfs.csv"), colClasses = c(SQ_CANDIDATO = "character"))
idx <- fread(file.path(DIR_PROC, "indice_pdfs.csv"), colClasses = c(SQ_CANDIDATO = "character"))

alvos <- meta[precisa_ocr == TRUE, SQ_CANDIDATO]
engine <- tesseract("por")
log_ocr <- list()

for (s in alvos) {
  arqs <- idx[SQ_CANDIDATO == s & arquivo %in% log_ext[SQ_CANDIDATO == s & motivo == "usado", arquivo]][order(versao)]
  paginas_txt <- character(0)
  t0 <- Sys.time()
  for (k in seq_len(nrow(arqs))) {
    unzip(file.path(DIR_ZIPS, arqs$zip[k]), files = arqs$arquivo[k], exdir = DIR_TMP, junkpaths = TRUE)
    pdf <- file.path(DIR_TMP, basename(arqs$arquivo[k]))
    n <- pdf_info(pdf)$pages
    if (MAX_PAGES > 0) n <- min(n, MAX_PAGES)
    for (p in seq_len(n)) {
      # pdftoppm (poppler) em vez de pdftools::pdf_convert, que trava em
      # alguns PDFs do Illustrator (testado: pagina 1 sem terminar em 150 s)
      prefixo <- file.path(DIR_TMP, sprintf("pg_%s_%d_%d", s, k, p))
      png <- paste0(prefixo, ".png")
      system2("pdftoppm", c("-r", DPI, "-f", p, "-l", p, "-png", "-singlefile",
                            shQuote(pdf), shQuote(prefixo)))
      txt <- if (file.exists(png)) paste(ocr(png, engine = engine), collapse = "\n") else ""
      paginas_txt <- c(paginas_txt, txt)
      unlink(png)
      if (p %% 10 == 0) message(sprintf("%s: %d/%d", s, p, n))
    }
    unlink(pdf)
  }
  writeLines(paste(paginas_txt, collapse = "\n\f"), file.path(DIR_TXT, paste0(s, ".txt")), useBytes = TRUE)
  palavras <- sum(lengths(strsplit(trimws(paginas_txt), "\\s+")))
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  log_ocr[[s]] <- data.table(SQ_CANDIDATO = s, paginas = length(paginas_txt), palavras = palavras, segundos = round(seg))
  message(sprintf("OK %s: %d paginas, %d palavras, %.0f s", s, length(paginas_txt), palavras, seg))
}
unlink(DIR_TMP, recursive = TRUE)

log_ocr <- rbindlist(log_ocr)
if (MAX_PAGES == 0) {
  meta <- merge(meta, log_ocr[, .(SQ_CANDIDATO, palavras_ocr = palavras)], by = "SQ_CANDIDATO", all.x = TRUE)
  meta[!is.na(palavras_ocr), `:=`(palavras = palavras_ocr, ocr_aplicado = TRUE, precisa_ocr = FALSE,
                                   palavras_pag = palavras_ocr / pmax(paginas, 1))]
  meta[, plano_curto := palavras < LIMIAR_PLANO_CURTO]
  meta[, palavras_ocr := NULL]
  fwrite(meta, file.path(DIR_PROC, "planos_meta.csv"))
  fwrite(log_ocr, file.path(DIR_TAB, "log_ocr.csv"))
}
print(log_ocr)
})

