# 15_extras.R -- "impressao digital" dos planos, quem nao cita o desastre principal do proprio estado
# e expressoes de interesse por campo politico.
# Saidas: outputs/tables/t21_nao_citam_principal.csv, t22_impressao_digital.csv;
#         outputs/figures/17_termos_por_campo.png, 18a e 18b_impressao_digital.png

source(here::here("R", "00_setup.R"))
source(here::here("R", "tema_graficos.R")); source(here::here("R", "dicionario.R"))
suppressPackageStartupMessages({library(ggplot2); library(acR)})

tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))
base <- tab("base_analitica.csv"); P <- base[grepl("^principal", cenario)]
jc   <- tab("janelas_classificadas.csv")
al   <- tab("t15_alinhamento_uf.csv")
jw   <- fread(file.path(DIR_PROC, "janelas.csv"), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))

ROT_T <- c(hidro = "Enchente e alagamento", seca = "Seca e estiagem", fogo = "Queimadas e incêndios", movimento_massa = "Deslizamento",
           tempestade = "Tempestades", calor_extremo = "Calor extremo", costeira = "Erosão costeira", barragem_mineracao = "Barragens e mineração",
           tecnologico = "Acidentes tecnológicos", generica = "Desastres em geral")
ROT_F <- c(prevencao_preparacao = "Prevenção e preparação", resposta = "Resposta", recuperacao = "Recuperação", adaptacao = "Adaptação climática")

rel <- jc[rel_a == "relevante"]
anyf <- function(v) rel[, .(x = any(get(v) %in% TRUE)), by = SQ_CANDIDATO]$x
cand <- data.table(SQ_CANDIDATO = sort(unique(rel$SQ_CANDIDATO)))
for (k in names(ROT_T)) cand[, (k) := rel[, any(get(paste0("ameaca_", k, "_a")) %in% TRUE), by = SQ_CANDIDATO][match(cand$SQ_CANDIDATO, SQ_CANDIDATO), V1]]
for (k in names(ROT_F)) cand[, (k) := rel[, any(get(paste0("fase_", k, "_a")) %in% TRUE), by = SQ_CANDIDATO][match(cand$SQ_CANDIDATO, SQ_CANDIDATO), V1]]
cand <- merge(P[, .(SQ_CANDIDATO, candidato = NM_URNA_CANDIDATO, partido = SG_PARTIDO, uf = SG_UF, campo, passagens = n_janelas_relevantes)], cand, by = "SQ_CANDIDATO", all.x = TRUE)
for (k in c(names(ROT_T), names(ROT_F))) cand[is.na(get(k)), (k) := FALSE]
cand[, n_cat := rowSums(.SD), .SDcols = c(names(ROT_T), names(ROT_F))]

# ---- quem nao cita o desastre principal do proprio estado -----------------------------------------------------
cand <- merge(cand, al[, .(uf, tipo_principal)], by = "uf", all.x = TRUE)
cand[, cita_principal := mapply(function(t, i) if (is.na(t)) NA else isTRUE(cand[i, get(t)]), tipo_principal, seq_len(.N))]
cand[, tipos_citados := apply(.SD, 1, function(r) { k <- names(ROT_T)[as.logical(r)]; if (length(k)) paste(ROT_T[k], collapse = "; ") else "nenhum" }), .SDcols = names(ROT_T)]
nc <- cand[cita_principal == FALSE, .(uf, candidato, partido, campo, tipo_principal = ROT_T[tipo_principal], tipos_citados, passagens)][order(uf, candidato)]
fwrite(nc, file.path(DIR_TAB, "t21_nao_citam_principal.csv"))
cat("Nao citam o principal do estado:", nrow(nc), "de", cand[!is.na(tipo_principal), .N], "\n")

# ---- impressao digital: candidatos x fases e tipos ----------------------------------------------------------------
d <- cand[passagens > 0][order(uf, candidato)]
fwrite(d[, c("uf", "candidato", "partido", "campo", "passagens", "n_cat", names(ROT_F), names(ROT_T)), with = FALSE], file.path(DIR_TAB, "t22_impressao_digital.csv"))
long <- melt(d, id.vars = c("SQ_CANDIDATO", "candidato", "uf", "n_cat"), measure.vars = c(names(ROT_F), names(ROT_T)), variable.name = "cat", value.name = "cita")
long[, grupo := fifelse(cat %in% names(ROT_F), "Fases do ciclo", "Tipos de desastre")]
long[, rot_cat := factor(c(ROT_F, ROT_T)[as.character(cat)], levels = c(ROT_F, ROT_T))]
long[, rot_cand := factor(sprintf("%s (%s)", candidato, uf), levels = rev(sprintf("%s (%s)", d$candidato, d$uf)))]
long[, cel := fifelse(!cita, "não", fifelse(grupo == "Fases do ciclo", "fase", "tipo"))]
terco <- ceiling(nrow(d) / 3)
mapa_digital <- function(cands, arquivo) {
  l <- long[SQ_CANDIDATO %in% cands]
  l[, rot_cand := droplevels(rot_cand)]
  p <- ggplot(l, aes(x = rot_cat, y = rot_cand, fill = cel)) + geom_tile(colour = COR$surface, linewidth = 0.3) +
    scale_fill_manual(values = c(não = "#ecebe5", fase = COR$s1, tipo = COR$s3), guide = "none") +
    scale_x_discrete(position = "top", guide = guide_axis(angle = 45)) + labs(x = NULL, y = NULL) +
    theme_risco(13) + theme(panel.grid = element_blank(), axis.line = element_blank(), axis.text.y = element_text(size = 12),
                            axis.text.x = element_text(size = 13, hjust = 0), plot.margin = margin(8, 100, 5, 5))
  salvar_fig(p, arquivo, w = 8.6, h = 8.6)
}
mapa_digital(d$SQ_CANDIDATO[seq_len(terco)], "18a_impressao_digital.png")
mapa_digital(d$SQ_CANDIDATO[(terco + 1):(2 * terco)], "18b_impressao_digital.png")
mapa_digital(d$SQ_CANDIDATO[(2 * terco + 1):nrow(d)], "18c_impressao_digital.png")
cat("Impressao digital:", nrow(d), "candidatos; terços de", terco, "\n")

# ---- nuvem de palavras comparativa por campo (acR) --------------------------------------------------------------------
# A nuvem TF-IDF do acR foi testada e descartada (termos distintivos dominados por ruido em textos curtos).
# No lugar, % das passagens relevantes de cada campo que trazem cada expressao de interesse.
txt <- merge(jc[rel_a == "relevante", .(janela_id, SQ_CANDIDATO)], jw[, .(janela_id, frases_foco)], by = "janela_id")
txt <- merge(txt, P[, .(SQ_CANDIDATO, campo)], by = "SQ_CANDIDATO")
txt <- txt[!is.na(campo) & campo != "centro e centro-direita"]
txt[, n := normalizar(frases_foco)]
TERMOS <- c("Defesa Civil" = "defesa civil", "Resiliência" = "resilienc", "Adaptação climática" = "adaptacao",
            "Mudança ou emergência climática" = "mudanca[s]? climatica|emergencia climatica",
            "Enchente, alagamento, inundação" = "enchent|alagament|inundac", "Seca ou estiagem" = "\\bsecas?\\b|estiagem",
            "Queimadas e incêndios" = "queimad|incendi", "Deslizamento e encostas" = "deslizament|encosta",
            "Alerta ou sirene" = "alerta|sirene", "Bombeiros" = "bombeir", "Áreas de risco" = "areas? de risco", "Drenagem" = "drenagem")
res <- rbindlist(lapply(names(TERMOS), function(t) txt[, .(termo = t, pct = 100 * mean(grepl(TERMOS[[t]], n, perl = TRUE)), n = .N), by = campo]))
tot <- txt[, .(termo = names(TERMOS), pct_total = sapply(TERMOS, function(r) 100 * mean(grepl(r, n, perl = TRUE))))]
res <- merge(res, tot, by = "termo")
fwrite(res, file.path(DIR_TAB, "t23_termos_por_campo.csv"))
res[, termo := factor(termo, levels = tot[order(pct_total), termo])]
res[, campo := factor(campo, levels = c("extrema-esquerda", "esquerda e centro-esquerda", "direita e extrema-direita"),
                      labels = c("Extrema-esquerda", "Esquerda e centro-esquerda", "Direita e extrema-direita"))]
p <- ggplot(res, aes(x = pct, y = termo, fill = campo)) + geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c(COR$s1, COR$s4, COR$s7), name = NULL) + guides(fill = guide_legend(nrow = 2, reverse = TRUE)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "% das passagens do campo que trazem a expressão", y = NULL) + theme_risco_h(15)
salvar_fig(p, "17_termos_por_campo.png", w = 9, h = 9)
cat("extras ok\n")
