# 04b_janelas.R -- unidade de classificacao do LLM: JANELAS em torno dos
# termos do dicionario (frase com o termo +/- 1 frase, fundindo janelas
# sobrepostas). Substitui os trechos de 150-400 palavras como unidade de
# classificacao (o piloto mostrou que a mencao de passagem no meio de trechos
# longos gerava discordancia entre modelos); os trechos continuam a base do
# denominador e do contexto de leitura.
#
# Saida: data/processed/janelas.csv  (1 linha por janela)

source(here::here("R", "00_setup.R"))
source(here::here("R", "dicionario.R"))

RAIO          <- 1     # frases de contexto de cada lado
MAX_PAL_FRASE <- 80    # frases maiores (listas sem pontuacao) sao fatiadas
FATIA         <- 60
MIN_PAL_JANELA <- 12

n_pal <- function(x) lengths(strsplit(trimws(x), "\\s+"))

fatiar <- function(s) {
  w <- strsplit(trimws(s), "\\s+")[[1]]
  if (length(w) <= MAX_PAL_FRASE) return(s)
  g <- ceiling(seq_along(w) / FATIA)
  vapply(split(w, g), paste, "", collapse = " ")
}

unidades_do_plano <- function(sq) {
  txt <- paste(readLines(file.path(DIR_PROC, "texto", paste0(sq, ".txt")), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  paginas <- strsplit(txt, "\f", fixed = TRUE)[[1]]
  rbindlist(lapply(seq_along(paginas), function(i) {
    pars <- trimws(strsplit(paginas[i], "\n\\s*\n")[[1]])
    pars <- gsub("\\s*\n\\s*", " ", pars)
    fr <- unlist(lapply(pars[nzchar(pars)], function(p) {
      s <- unlist(strsplit(p, "(?<=[.!?;])\\s+|\\s*[\u2022\u25AA\u25CF\u25E6]\\s*", perl = TRUE))
      unlist(lapply(s[nzchar(trimws(s))], fatiar))
    }))
    if (!length(fr)) return(NULL)
    data.table(pagina = i, frase = trimws(fr))
  }))
}

janelas_do_plano <- function(sq) {
  u <- unidades_do_plano(sq)
  if (is.null(u) || !nrow(u)) return(NULL)
  norm <- normalizar(u$frase)
  forte <- Reduce(`|`, lapply(REGEX_DIC, function(rx) grepl(rx, norm, perl = TRUE)))
  fracos <- vapply(regmatches(norm, gregexpr(REGEX_FRACO, norm, perl = TRUE)), function(m) length(unique(m)), 0L)
  hit <- forte | fracos >= 2
  if (!any(hit)) return(NULL)
  ini <- pmax(which(hit) - RAIO, 1L); fim <- pmin(which(hit) + RAIO, nrow(u))
  # funde intervalos sobrepostos ou adjacentes
  o <- order(ini); ini <- ini[o]; fim <- fim[o]
  g <- cumsum(c(TRUE, ini[-1] > cummax(fim)[-length(fim)] + 1L))
  jan <- data.table(ini = as.integer(tapply(ini, g, min)), fim = as.integer(tapply(fim, g, max)))
  jan[, `:=`(pagina = u$pagina[as.integer(vapply(seq_len(.N), function(k) which(hit[ini[k]:fim[k]])[1] + ini[k] - 1L, 0))],
             texto  = vapply(seq_len(.N), function(k) paste(u$frase[ini[k]:fim[k]], collapse = " "), ""),
             # frase focal: a(s) frase(s) com termo do dicionario, marcadas com » « dentro da janela.
             # Usada na especificidade (o modelo julga so o foco; o resto e contexto).
             texto_foco = vapply(seq_len(.N), function(k) { i <- ini[k]:fim[k]
               paste(ifelse(hit[i], paste0("»", u$frase[i], "«"), u$frase[i]), collapse = " ") }, ""),
             frases_foco = vapply(seq_len(.N), function(k) { i <- ini[k]:fim[k]; paste(u$frase[i][hit[i]], collapse = " ") }, ""))]
  jan[, `:=`(SQ_CANDIDATO = sq, janela_id = sprintf("%s_J%04d", sq, seq_len(.N)), palavras = n_pal(texto))]
  jan[, c("ini", "fim") := NULL]
  jan[palavras >= MIN_PAL_JANELA]
}

meta <- fread(file.path(DIR_PROC, "planos_meta.csv"), colClasses = c(SQ_CANDIDATO = "character"))
message("Montando janelas de ", nrow(meta), " planos...")
jan <- rbindlist(lapply(meta$SQ_CANDIDATO, janelas_do_plano))

norm <- normalizar(jan$texto)
hits <- sapply(REGEX_DIC, function(rx) grepl(rx, norm, perl = TRUE))
jan[, (paste0("dic_", names(DICIONARIO))) := as.data.table(hits)]
jan[, termos := vapply(seq_len(.N), function(i) {
  m <- unlist(regmatches(norm[i], gregexpr(paste(c(REGEX_DIC, REGEX_FRACO), collapse = "|"), norm[i], perl = TRUE)))
  paste(head(unique(m), 6), collapse = "; ")
}, "")]
# alias para reutilizar o script de classificacao
jan[, trecho_id := janela_id]
jan[, hit_dic := TRUE]

fwrite(jan, file.path(DIR_PROC, "janelas.csv"))

cat("Janelas:", nrow(jan), " | planos com >=1 janela:", uniqueN(jan$SQ_CANDIDATO), "de", nrow(meta), "\n")
cat("Palavras por janela:\n"); print(summary(jan$palavras))
j2 <- merge(jan[, .N, by = SQ_CANDIDATO], meta[, .(SQ_CANDIDATO, SG_UF)], by = "SQ_CANDIDATO")
cat("\nJanelas por UF do piloto:\n"); print(j2[SG_UF %in% c("RS", "PE", "AM", "MG"), .(janelas = sum(N), candidatos = .N), by = SG_UF])
