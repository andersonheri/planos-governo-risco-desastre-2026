# 04_trechos_dicionario.R -- divide cada plano em trechos (150-400 palavras,
# com pagina de origem) e aplica o dicionario de risco e desastre
# (etapa 0 do codebook). Gera presenca e densidade por candidato.
#
# Saidas: data/processed/trechos.csv       (1 linha por trecho)
#         data/processed/dic_por_plano.csv (indicadores quantitativos)

source(here::here("R", "00_setup.R"))

MIN_PAL_TRECHO <- 150
MAX_PAL_TRECHO <- 400

# ---- Dicionario (aplicado a texto minusculo e sem acentos) ------------------
# Alta cobertura de proposito: o LLM decide a relevancia depois.
source(here::here("R", "dicionario.R"))


# ---- Divisao em trechos ------------------------------------------------------
n_pal <- function(x) lengths(strsplit(trimws(x), "\\s+"))

dividir_paragrafo_longo <- function(p) {
  sent <- unlist(strsplit(p, "(?<=[.!?])\\s+", perl = TRUE))
  out <- character(0); atual <- character(0)
  for (s in sent) {
    if (length(atual) > 0 && n_pal(paste(c(atual, s), collapse = " ")) > MAX_PAL_TRECHO) {
      out <- c(out, paste(atual, collapse = " ")); atual <- character(0)
    }
    atual <- c(atual, s)
  }
  if (length(atual)) out <- c(out, paste(atual, collapse = " "))
  out
}

trechos_do_plano <- function(sq) {
  txt <- paste(readLines(file.path(DIR_PROC, "texto", paste0(sq, ".txt")), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  paginas <- strsplit(txt, "\f", fixed = TRUE)[[1]]
  pars <- rbindlist(lapply(seq_along(paginas), function(i) {
    p <- trimws(strsplit(paginas[i], "\n\\s*\n")[[1]])
    p <- gsub("\\s*\n\\s*", " ", p)
    p <- p[n_pal(p) > 0 & nzchar(p)]
    if (!length(p)) return(NULL)
    data.table(pagina = i, par = p)
  }))
  if (is.null(pars) || !nrow(pars)) return(NULL)
  # quebra paragrafos muito longos
  pars <- pars[, .(par = if (n_pal(par) > MAX_PAL_TRECHO) dividir_paragrafo_longo(par) else par), by = .(pagina, i = seq_len(nrow(pars)))]
  # junta paragrafos ate atingir o minimo
  out <- list(); buf <- character(0); pg0 <- NA_integer_
  for (k in seq_len(nrow(pars))) {
    if (!length(buf)) pg0 <- pars$pagina[k]
    if (length(buf) && n_pal(paste(c(buf, pars$par[k]), collapse = " ")) > MAX_PAL_TRECHO) {
      out[[length(out) + 1]] <- data.table(pagina = pg0, texto = paste(buf, collapse = "\n\n")); buf <- character(0); pg0 <- pars$pagina[k]
    }
    buf <- c(buf, pars$par[k])
    if (n_pal(paste(buf, collapse = " ")) >= MIN_PAL_TRECHO) {
      out[[length(out) + 1]] <- data.table(pagina = pg0, texto = paste(buf, collapse = "\n\n")); buf <- character(0)
    }
  }
  if (length(buf)) out[[length(out) + 1]] <- data.table(pagina = pg0, texto = paste(buf, collapse = "\n\n"))
  r <- rbindlist(out)
  r[, `:=`(SQ_CANDIDATO = sq, trecho_id = sprintf("%s_%04d", sq, seq_len(.N)), palavras = n_pal(texto))]
  r
}

meta <- fread(file.path(DIR_PROC, "planos_meta.csv"), colClasses = c(SQ_CANDIDATO = "character"))
message("Dividindo ", nrow(meta), " planos em trechos...")
trechos <- rbindlist(lapply(meta$SQ_CANDIDATO, trechos_do_plano))

# ---- Dicionario ---------------------------------------------------------------
norm <- normalizar(trechos$texto)
hits <- sapply(REGEX_DIC, function(rx) grepl(rx, norm, perl = TRUE))
trechos[, (paste0("dic_", names(DICIONARIO))) := as.data.table(hits)]
trechos[, n_temas_dic := rowSums(hits)]
trechos[, n_fracos := vapply(regmatches(norm, gregexpr(REGEX_FRACO, norm, perl = TRUE)),
                             function(m) length(unique(m)), 0L)]
trechos[, hit_forte := n_temas_dic > 0]
trechos[, hit_dic := hit_forte | n_fracos >= 2]
# termos que casaram (para auditoria)
trechos[, termos := vapply(seq_len(.N), function(i) {
  m <- unlist(regmatches(norm[i], gregexpr(paste(c(REGEX_DIC, REGEX_FRACO), collapse = "|"), norm[i], perl = TRUE)))
  paste(head(unique(m), 6), collapse = "; ")
}, "")]

fwrite(trechos, file.path(DIR_PROC, "trechos.csv"))

# ---- Indicadores por plano ----------------------------------------------------
dic_plano <- trechos[, .(n_trechos = .N, palavras_trechos = sum(palavras),
                         n_trechos_dic = sum(hit_dic),
                         n_ocorrencias_forte = sum(dic_desastre | dic_defesa_civil | dic_hidro | dic_movimento_massa |
                                                    dic_seca | dic_fogo | dic_barragem | dic_alerta_prep)),
                     by = SQ_CANDIDATO]
dic_plano <- merge(meta, dic_plano, by = "SQ_CANDIDATO", all.x = TRUE)
dic_plano[is.na(n_trechos_dic), `:=`(n_trechos_dic = 0L, n_ocorrencias_forte = 0L)]
dic_plano[, presenca_dic := n_trechos_dic > 0]
dic_plano[, presenca_forte := n_ocorrencias_forte > 0]
dic_plano[, dens_dic_1000 := 1000 * n_trechos_dic / pmax(palavras, 1)]
fwrite(dic_plano, file.path(DIR_PROC, "dic_por_plano.csv"))

cat("Trechos:", nrow(trechos), " | com dicionario:", trechos[hit_dic == TRUE, .N],
    sprintf("(%.1f%%)\n", 100 * mean(trechos$hit_dic)))
cat("Palavras por trecho:\n"); print(summary(trechos$palavras))
cat("\nPlanos com >=1 trecho de dicionario:", dic_plano[presenca_dic == TRUE, .N], "de", nrow(dic_plano), "\n")
cat("Planos com termo 'forte' (desastre/defesa civil/enchente/...):", dic_plano[presenca_forte == TRUE, .N], "\n")
cat("\nTrechos de dicionario a classificar por UF (piloto RS PE AM MG):\n")
print(merge(trechos[hit_dic == TRUE, .N, by = SQ_CANDIDATO], meta[, .(SQ_CANDIDATO, SG_UF)], by = "SQ_CANDIDATO")[SG_UF %in% c("RS", "PE", "AM", "MG"), .(trechos = sum(N), candidatos = .N), by = SG_UF])
cat("\nPor tema (n de trechos):\n"); print(sort(colSums(trechos[, grep("^dic_", names(trechos), value = TRUE), with = FALSE]), decreasing = TRUE))
