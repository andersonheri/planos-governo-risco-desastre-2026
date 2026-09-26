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

source(here::here("R", "00_setup.R"))
source(here::here("R", "tema_graficos.R"))   # NIVEIS_PERFIL

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
