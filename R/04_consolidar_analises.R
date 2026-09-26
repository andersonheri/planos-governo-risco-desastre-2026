# 04_consolidar_analises.R -- consolida os rotulos dos modelos, mede a concordancia e gera a base analitica e as tabelas do relatorio.
# (script unido: cada parte corresponde a um script da versao anterior e roda em ambiente proprio, sem misturar variaveis)

source(here::here("R", "00_setup.R"))
source(here::here("R", "dicionario.R"))
source(here::here("R", "tema_graficos.R"))

# ==== Parte 1. Consolidacao dos rotulos e concordancia entre modelos ==============================
local({
# 08_consolidar.R -- consolida os rotulos dos modelos locais (le os lotes RDS,
# entao funciona mesmo com a execucao em andamento), normaliza as variacoes de
# rotulo, mede a concordancia entre modelos e calcula os indicadores por
# candidato.
#
# Variavel de ambiente: ESCOPO (padrao "completo"; use "piloto5" para testar).
#
# Saidas (outputs/tables/):
#   janelas_classificadas.csv   -- 1 linha por janela, rotulos dos 2 modelos
#   rotulos_<modelo>.csv        -- rotulos + raciocinio (auditoria)
#   concordancia_modelos.csv    -- % acordo, kappa, AC1 de Gwet, alpha de Krippendorff
#   indicadores_candidato.csv   -- indicadores por candidato (3 cenarios)



suppressPackageStartupMessages(library(irr))

ESCOPO <- Sys.getenv("ESCOPO", unset = "completo")
MODELOS <- c(a = "openai-gpt-oss-20b", b = "google-gemma-4-26b-a4b-qat")

FASES   <- c("prevencao_preparacao", "resposta", "recuperacao", "adaptacao")
AMEACAS <- c("hidro", "movimento_massa", "seca", "fogo", "calor_extremo", "tempestade",
             "costeira", "barragem_mineracao", "tecnologico", "generica")
ESPEC_ROTULOS <- c("n0_generica", "n1_diagnostico", "n2_diretriz", "n3_acao", "n4_acao_meta")

# ---- leitura e normalizacao -----------------------------------------------------
ler_dim <- function(tag, dim) {
  d <- file.path(DIR_LLM, sprintf("%s__%s__%s", ESCOPO, tag, dim))
  f <- list.files(d, pattern = "[.]rds$", full.names = TRUE)
  if (!length(f)) return(NULL)
  x <- rbindlist(lapply(f, function(a) as.data.table(readRDS(a))), fill = TRUE)
  unique(x, by = "doc_id")
}
limpar <- function(x) trimws(tolower(iconv(x, to = "ASCII//TRANSLIT")))
SINONIMOS <- c(prevencao = "prevencao_preparacao", preparacao = "prevencao_preparacao",
               prevencao_e_preparacao = "prevencao_preparacao",
               none = NA, null = NA, nenhum = NA, nenhuma = NA, na = NA, fora_do_escopo = NA)
rotulos <- function(x) {   # multi-rotulo: "a|b", "a, b", "a; b"
  lapply(strsplit(limpar(x), "[|,;]"), function(v) {
    v <- trimws(v); v <- ifelse(v %in% names(SINONIMOS), SINONIMOS[v], v)
    unique(v[!is.na(v) & nzchar(v)])
  })
}
flags <- function(lista, cols) as.data.table(sapply(cols, function(l) vapply(lista, function(v) l %in% v, NA)))

montar_modelo <- function(tag) {
  rel <- ler_dim(tag, "relevancia")
  if (is.null(rel)) return(NULL)
  out <- data.table(janela_id = rel$doc_id, rel = limpar(rel$categoria), racio_rel = rel$raciocinio)
  out[rel == "nao relevante", rel := "nao_relevante"]
  out[rel == "clima geral", rel := "clima_geral"]
  fs <- ler_dim(tag, "fase");  am <- ler_dim(tag, "ameaca"); es <- ler_dim(tag, "especificidade")
  if (!is.null(fs)) { f <- flags(rotulos(fs$categoria), FASES); setnames(f, paste0("fase_", FASES)); f[, janela_id := fs$doc_id]
                      out <- merge(out, f, by = "janela_id", all.x = TRUE) }
  if (!is.null(am)) { f <- flags(rotulos(am$categoria), AMEACAS); setnames(f, paste0("ameaca_", AMEACAS)); f[, janela_id := am$doc_id]
                      out <- merge(out, f, by = "janela_id", all.x = TRUE) }
  if (!is.null(es)) { e <- data.table(janela_id = es$doc_id, espec = suppressWarnings(as.integer(sub("^n([0-9]).*", "\\1", limpar(es$categoria)))),
                                      racio_espec = es$raciocinio)
                      out <- merge(out, e, by = "janela_id", all.x = TRUE) }
  out
}

M <- lapply(MODELOS, montar_modelo)
M <- M[!vapply(M, is.null, NA)]
# SO_PRINCIPAL=1: ignora o segundo modelo (uso na consolidacao intermediaria, com o
# gemma ainda rodando e com resultados parciais)
if (Sys.getenv("SO_PRINCIPAL") == "1") M <- M["a"]
stopifnot(length(M) >= 1)
for (n in names(M)) fwrite(M[[n]], file.path(DIR_TAB, sprintf("rotulos_%s.csv", MODELOS[[n]])))
message("Modelos com resultados: ", paste(MODELOS[names(M)], collapse = ", "),
        " | janelas: ", paste(vapply(M, nrow, 0L), collapse = " / "))

# ---- tabela de janelas ------------------------------------------------------------
jan  <- fread(file.path(DIR_PROC, "janelas.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")
meta <- fread(file.path(DIR_PROC, "planos_meta.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")
anc  <- fread(file.path(DIR_PROC, "janelas_ancoragem.csv"), colClasses = c(SQ_CANDIDATO = "character"))
suf <- function(dt, s) { setnames(dt, setdiff(names(dt), "janela_id"), paste0(setdiff(names(dt), "janela_id"), "_", s)); dt }
J <- jan[, .(janela_id, SQ_CANDIDATO, pagina, palavras_janela = palavras, termos)]
for (n in names(M)) J <- merge(J, suf(copy(M[[n]])[, c("racio_rel", "racio_espec") := NULL], n), by = "janela_id", all.x = TRUE)
J <- merge(J, anc[, !"SQ_CANDIDATO"], by = "janela_id", all.x = TRUE)
J <- J[!is.na(rel_a)]                       # so as janelas ja classificadas pelo modelo principal
tem_b <- "rel_b" %in% names(J) && any(!is.na(J$rel_b))
J[, rel_a_bin := rel_a == "relevante"]
if (tem_b) J[, `:=`(rel_b_bin = rel_b == "relevante", rel_acordo = rel_a == rel_b)]
# (janelas_classificadas.csv e gravado depois da revisao manual, mais abaixo)

# ---- concordancia entre modelos ---------------------------------------------------
gwet_ac1 <- function(x, y) { po <- mean(x == y); pi_ <- (mean(x) + mean(y)) / 2; pe <- 2 * pi_ * (1 - pi_); if (pe == 1) NA_real_ else (po - pe) / (1 - pe) }
metricas <- function(nome, x, y, ordinal = FALSE) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 5) return(NULL)
  k <- tryCatch(if (ordinal) irr::kappa2(cbind(x, y), weight = "squared")$value else irr::kappa2(cbind(as.character(x), as.character(y)))$value, error = function(e) NA_real_)
  ka <- tryCatch(irr::kripp.alpha(if (ordinal) rbind(as.numeric(x), as.numeric(y)) else rbind(as.character(x), as.character(y)), method = if (ordinal) "ordinal" else "nominal")$value, error = function(e) NA_real_)
  data.table(item = nome, n = length(x), pct_acordo = round(100 * mean(x == y), 1), kappa_cohen = round(k, 2),
             alpha_krippendorff = round(ka, 2),
             gwet_ac1 = if (is.logical(x)) round(gwet_ac1(x, y), 2) else NA_real_,
             pos_a = if (is.logical(x)) sum(x) else NA_integer_, pos_b = if (is.logical(y)) sum(y) else NA_integer_)
}
conc <- NULL
if (tem_b) {
  JB <- J[!is.na(rel_b)]
  conc <- rbindlist(c(
    list(metricas("relevancia (3 categorias)", JB$rel_a, JB$rel_b),
         metricas("relevancia (relevante x resto)", JB$rel_a_bin, JB$rel_b_bin)),
    lapply(c(paste0("fase_", FASES), paste0("ameaca_", AMEACAS)), function(v) {
      a <- JB[[paste0(v, "_a")]]; b <- JB[[paste0(v, "_b")]]
      if (is.null(a) || is.null(b)) return(NULL); metricas(v, a, b) }),
    list(metricas("especificidade (0-4, ordinal)", JB$espec_a, JB$espec_b, ordinal = TRUE))
  ), fill = TRUE)
  fwrite(conc, file.path(DIR_TAB, "concordancia_modelos.csv"))
  print(conc)
}

# ---- revisao manual das passagens de 'acao com meta' (nivel 4) -----------------------------
# As 14 passagens de nivel 4 foram lidas uma a uma (config/revisao_manual.csv). A concordancia
# entre modelos, acima, usa os rotulos originais; os indicadores usam os rotulos revisados.
rev <- fread(here::here('config', 'revisao_manual.csv'), encoding = 'UTF-8')
if (!'espec_a_original' %in% names(J)) J[, espec_a_original := espec_a]
J[rev, on = 'janela_id', espec_a := i.espec_corrigida]
J[, espec_revisada := janela_id %in% rev[decisao == 'rebaixada', janela_id]]
message('Revisao manual aplicada: ', sum(J$espec_revisada), ' passagens rebaixadas')
fwrite(J, file.path(DIR_TAB, 'janelas_classificadas.csv'))

# ---- indicadores por candidato ----------------------------------------------------
indicadores <- function(J, meta, sufx, rel_col, rotulo) {
  d <- J[get(rel_col) == TRUE]
  fase_cols <- paste0("fase_", FASES[1:3], "_", sufx)
  ame_cols  <- paste0("ameaca_", setdiff(AMEACAS, "generica"), "_", sufx)
  fase_cols <- intersect(fase_cols, names(d)); ame_cols <- intersect(ame_cols, names(d))
  esp <- paste0("espec_", sufx)
  agg <- d[, .(
    n_janelas_relevantes = .N,
    paginas_com_risco = uniqueN(pagina),
    fases_ciclo = if (length(fase_cols)) sum(sapply(fase_cols, function(cc) any(get(cc), na.rm = TRUE))) else NA_integer_,
    ciclo_completo = if (length(fase_cols)) all(sapply(fase_cols, function(cc) any(get(cc), na.rm = TRUE))) else NA,
    adaptacao_explicita = any(get(paste0("fase_adaptacao_", sufx)), na.rm = TRUE),
    n_tipos_ameaca = if (length(ame_cols)) sum(sapply(ame_cols, function(cc) any(get(cc), na.rm = TRUE))) else NA_integer_,
    espec_max = if (esp %in% names(d)) suppressWarnings(as.numeric(max(get(esp), na.rm = TRUE))) else NA_real_,
    espec_media = if (esp %in% names(d)) mean(get(esp), na.rm = TRUE) else NA_real_,
    pct_acao_concreta = if (esp %in% names(d)) 100 * mean(get(esp) >= 3, na.rm = TRUE) else NA_real_,
    anc_orcamento = any(anc_orcamento), anc_meta = any(anc_meta_quantificada), anc_prazo = any(anc_prazo),
    anc_indicador = any(anc_indicador), anc_orgao = any(anc_orgao)
  ), by = SQ_CANDIDATO]
  cg <- J[rel_a == "clima_geral", .(n_janelas_clima_geral = .N), by = SQ_CANDIDATO]
  out <- merge(meta, agg, by = "SQ_CANDIDATO", all.x = TRUE)
  out <- merge(out, cg, by = "SQ_CANDIDATO", all.x = TRUE)
  zeros <- c("n_janelas_relevantes", "paginas_com_risco", "n_janelas_clima_geral", "fases_ciclo", "n_tipos_ameaca")
  for (z in zeros) out[is.na(get(z)), (z) := 0L]
  out[is.na(ciclo_completo), ciclo_completo := FALSE]
  out[is.na(adaptacao_explicita), adaptacao_explicita := FALSE]
  out[, `:=`(presenca = n_janelas_relevantes > 0, cenario = rotulo,
             dens_1000 = 1000 * n_janelas_relevantes / pmax(palavras, 1))]
  out[is.infinite(espec_max), espec_max := NA_real_]
  out
}
ind <- list(indicadores(J, meta, "a", "rel_a_bin", "principal (gpt-oss)"))
if (tem_b) {
  J[, `:=`(rel_conservador = rel_a_bin & rel_b_bin, rel_amplo = rel_a_bin | rel_b_bin)]
  ind <- c(ind, list(indicadores(J, meta, "a", "rel_conservador", "conservador (ambos concordam)"),
                     indicadores(J, meta, "a", "rel_amplo", "amplo (algum modelo)")))
}
IND <- rbindlist(ind)
fwrite(IND, file.path(DIR_TAB, "indicadores_candidato.csv"))
cat("\nIndicadores:", uniqueN(IND$SQ_CANDIDATO), "candidatos x", uniqueN(IND$cenario), "cenario(s)\n")
print(IND[, .(candidatos = .N, com_presenca = sum(presenca), pct = round(100 * mean(presenca), 1),
              janelas_relev = sum(n_janelas_relevantes)), by = cenario])
})

# ==== Parte 2. Base analitica por candidato e tabelas do relatorio ================================
local({
# 09_analises.R -- base analitica por candidato e tabelas do relatorio.
# Le: outputs/tables/indicadores_candidato.csv, janelas_classificadas.csv,
#     data/processed/candidatos_gov.csv, config/partidos_campos.csv.
# Escreve: outputs/tables/base_analitica.csv e t01..t12 (csv).
#
# Perfil do candidato (5 degraus, mutuamente exclusivos, do mais alto ao mais baixo):
#   5 Acao com meta, prazo ou orcamento (especificidade maxima = 4)
#   4 Propoe acao concreta              (especificidade maxima = 3)
#   3 Menciona risco e desastre         (>= 1 janela relevante, sem acao concreta)
#   2 So agenda climatica geral         (so janelas clima_geral)
#   1 Sem mencao




ind  <- fread(file.path(DIR_TAB, "indicadores_candidato.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")
J    <- fread(file.path(DIR_TAB, "janelas_classificadas.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")
cand <- fread(file.path(DIR_PROC, "candidatos_gov.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")
campos <- fread(here::here("config", "partidos_campos.csv"), encoding = "UTF-8")

# "Acao concreta" exige que METADE OU MAIS das passagens relevantes do candidato tragam acao
# concreta (especificidade >= 3). O maximo entre janelas foi descartado: ele cresce com o tamanho
# do plano (mais janelas, mais chances de uma passagem concreta) e confundiria as comparacoes.
# O degrau 5 e um subconjunto do 4: alem da predominancia de acao, ao menos uma passagem de nivel 4
# (meta, prazo, indicador ou orcamento referidos a propria acao de risco).
LIMIAR_ACAO_PCT <- 50
perfil_nivel <- function(presenca, clima, espec_max, pct_acao) {
  acao <- presenca & !is.na(pct_acao) & pct_acao >= LIMIAR_ACAO_PCT
  fcase(acao & !is.na(espec_max) & espec_max >= 4, 5L,
        acao, 4L,
        presenca, 3L,
        clima > 0, 2L,
        default = 1L)
}
ind[, perfil_n := perfil_nivel(presenca, n_janelas_clima_geral, espec_max, pct_acao_concreta)]
ind[, poucas_janelas := presenca & n_janelas_relevantes <= 2]   # com 1 ou 2 passagens, o % de acao e instavel
ind[, perfil := factor(NIVEIS_PERFIL[perfil_n], levels = NIVEIS_PERFIL)]

info <- cand[, .(SQ_CANDIDATO, DS_GENERO, DS_OCUPACAO, SG_FEDERACAO, NM_PARTIDO)]
info[, incumbente := DS_OCUPACAO == "GOVERNADOR"]
base <- merge(ind, info, by = "SQ_CANDIDATO", all.x = TRUE)
base <- merge(base, campos[, .(SG_PARTIDO = partido, categoria_artigo, campo, tipo_classificacao = tipo)], by = "SG_PARTIDO", all.x = TRUE)
base[, plano_curto := palavras < LIMIAR_PLANO_CURTO]
fwrite(base, file.path(DIR_TAB, "base_analitica.csv"))

P <- base[grepl("^principal", cenario)]           # cenario principal
res <- function(d, by) d[, .(
  n = .N,
  com_mencao = sum(presenca), pct_mencao = round(100 * mean(presenca), 1),
  pct_so_clima = round(100 * mean(perfil_n == 2L), 1),
  pct_acao = round(100 * mean(perfil_n >= 4L), 1),
  pct_meta = round(100 * mean(perfil_n == 5L), 1),
  pct_ciclo_completo = round(100 * mean(ciclo_completo), 1),
  pct_adaptacao = round(100 * mean(adaptacao_explicita), 1),
  dens_mediana = round(median(dens_1000), 2),
  palavras_mediana = round(median(palavras))
), by = by]

# ---- t01 panorama nos 3 cenarios ---------------------------------------------------
t01 <- base[, .(n = .N, com_mencao = sum(presenca), pct_mencao = round(100 * mean(presenca), 1),
                pct_acao = round(100 * mean(perfil_n >= 4L), 1), janelas_relevantes = sum(n_janelas_relevantes)), by = cenario]
fwrite(t01, file.path(DIR_TAB, "t01_panorama_cenarios.csv"))
fwrite(P[, .(n = .N), by = .(perfil_n, perfil)][order(perfil_n)][, pct := round(100 * n / sum(n), 1)], file.path(DIR_TAB, "t01b_perfil_nacional.csv"))

# ---- recortes ------------------------------------------------------------------------
fwrite(res(P, "regiao"),              file.path(DIR_TAB, "t02_por_regiao.csv"))
fwrite(res(P, c("SG_UF", "regiao"))[order(-pct_acao, -pct_mencao)], file.path(DIR_TAB, "t03_por_uf.csv"))
fwrite(res(P, c("SG_PARTIDO", "categoria_artigo", "campo"))[order(-n)],        file.path(DIR_TAB, "t04_por_partido.csv"))
fwrite(res(P, "campo"),               file.path(DIR_TAB, "t05_por_campo.csv"))
fwrite(res(P, "incumbente"),          file.path(DIR_TAB, "t06_incumbente.csv"))
fwrite(res(P, "DS_GENERO"),           file.path(DIR_TAB, "t06b_genero.csv"))
fwrite(P[, .(n = .N), by = .(regiao, perfil_n, perfil)][order(regiao, perfil_n)], file.path(DIR_TAB, "t02b_perfil_por_regiao.csv"))
fwrite(P[, .(n = .N), by = .(campo, perfil_n, perfil)][order(campo, perfil_n)],   file.path(DIR_TAB, "t05b_perfil_por_campo.csv"))

# ---- fases, ameacas, especificidade, ancoragem (nivel candidato e janela) ---------
JR <- J[rel_a_bin == TRUE]
fase_cols <- grep("^fase_.*_a$", names(JR), value = TRUE)
ame_cols  <- grep("^ameaca_.*_a$", names(JR), value = TRUE)
por_cand <- function(cols, nome) {
  rbindlist(lapply(cols, function(cc) {
    q <- JR[, .(usa = any(get(cc), na.rm = TRUE)), by = SQ_CANDIDATO]
    data.table(item = sub("_a$", "", sub(paste0("^", nome, "_"), "", cc)),
               candidatos = sum(q$usa), pct_candidatos = round(100 * sum(q$usa) / nrow(P), 1),
               janelas = sum(JR[[cc]], na.rm = TRUE))
  }))[order(-pct_candidatos)]
}
fwrite(por_cand(fase_cols, "fase"),  file.path(DIR_TAB, "t07_fases.csv"))
fwrite(por_cand(ame_cols, "ameaca"), file.path(DIR_TAB, "t08_ameacas.csv"))
if ("espec_a" %in% names(JR)) fwrite(JR[!is.na(espec_a), .(janelas = .N), by = espec_a][order(espec_a)][, pct := round(100 * janelas / sum(janelas), 1)],
                                     file.path(DIR_TAB, "t09_especificidade_janelas.csv"))
anc_cols <- grep("^anc_", names(P), value = TRUE)
anc_cols <- setdiff(anc_cols, "anc_n_elementos")
fwrite(rbindlist(lapply(anc_cols, function(cc) data.table(elemento = sub("^anc_", "", cc),
        pct_candidatos_com_mencao = round(100 * mean(P[presenca == TRUE][[cc]], na.rm = TRUE), 1)))),
       file.path(DIR_TAB, "t10_ancoragem.csv"))

# ---- ameaca x UF (matriz) e rankings -------------------------------------------------
am <- JR[, lapply(.SD, function(x) any(x, na.rm = TRUE)), by = SQ_CANDIDATO, .SDcols = ame_cols]
am <- merge(am, P[, .(SQ_CANDIDATO, SG_UF, regiao)], by = "SQ_CANDIDATO")
fwrite(melt(am[, lapply(.SD, mean), by = SG_UF, .SDcols = ame_cols], id.vars = "SG_UF", variable.name = "ameaca", value.name = "prop")[
  , ameaca := sub("^ameaca_", "", sub("_a$", "", ameaca))], file.path(DIR_TAB, "t08b_ameaca_por_uf.csv"))
fwrite(P[order(-perfil_n, -n_tipos_ameaca, -dens_1000),
         .(SG_UF, candidato = NM_URNA_CANDIDATO, SG_PARTIDO, campo, perfil, n_janelas_relevantes, fases_ciclo, n_tipos_ameaca,
           espec_max, dens_1000 = round(dens_1000, 2), palavras, plano_curto)], file.path(DIR_TAB, "t11_ranking_candidatos.csv"))
fwrite(P[plano_curto == TRUE, .(SG_UF, candidato = NM_URNA_CANDIDATO, SG_PARTIDO, palavras, paginas, perfil)][order(palavras)],
       file.path(DIR_TAB, "t12_planos_curtos.csv"))

cat("Base analitica:", nrow(P), "candidatos (cenario principal)\n")
print(P[, .N, by = perfil][order(perfil)])
cat("\nPor regiao:\n"); print(res(P, "regiao"))
})

