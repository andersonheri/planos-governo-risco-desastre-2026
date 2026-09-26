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

source(here::here("R", "00_setup.R"))
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
