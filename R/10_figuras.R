# 10_figuras.R -- figuras do relatorio (PNG em outputs/figures/), a partir de
# outputs/tables/ (gerado por 08 e 09). O mapa (F11) usa geobr, que baixa as
# malhas do IPEA: so roda com MAPA=1.
#
# Convencoes: nenhum grafico empilhado (barras agrupadas ou paineis); titulo e
# fonte NAO ficam dentro da imagem (entram como legenda no documento: titulo em
# cima, fonte embaixo).

source(here::here("R", "00_setup.R"))
source(here::here("R", "tema_graficos.R"))
suppressPackageStartupMessages({library(ggplot2); library(tidyr)})

tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8")
base <- tab("base_analitica.csv"); base[, SQ_CANDIDATO := as.character(SQ_CANDIDATO)]
P <- base[grepl("^principal", cenario)]
P[, perfil := factor(perfil, levels = NIVEIS_PERFIL)]
N <- nrow(P)
ROT <- c(prevencao_preparacao = "Prevenção e preparação", resposta = "Resposta", recuperacao = "Recuperação",
         adaptacao = "Adaptação climática",
         hidro = "Enchente, inundação, alagamento", movimento_massa = "Deslizamento", seca = "Seca e estiagem",
         fogo = "Queimadas e incêndios", calor_extremo = "Calor extremo", tempestade = "Tempestades",
         costeira = "Erosão costeira", barragem_mineracao = "Barragens e mineração", tecnologico = "Acidentes tecnológicos",
         generica = "Desastres em geral (sem tipo)")
pct <- function(x) paste0(format(round(x, 1), decimal.mark = ","), "%")

# barras agrupadas de perfil por grupo (regiao ou campo): grupos no eixo x, degraus lado a lado
perfil_agrupado <- function(P, grupo, ordem, arquivo, w = 9.5, h = 4.8) {
  d <- P[!is.na(get(grupo)), .N, by = c(grupo, "perfil")]
  d <- merge(CJ(g = ordem, perfil = factor(NIVEIS_PERFIL, levels = NIVEIS_PERFIL)), setnames(d, grupo, "g"), by = c("g", "perfil"), all.x = TRUE)
  d[is.na(N), N := 0L]
  d[, pct := 100 * N / sum(N), by = g]
  n_g <- P[!is.na(get(grupo)), .N, by = grupo]; setnames(n_g, grupo, "g")
  d[, rot := factor(g, levels = ordem, labels = sprintf("%s\n(n = %d)", ordem, n_g$N[match(ordem, n_g$g)]))]
  p <- ggplot(d, aes(x = rot, y = pct, fill = perfil)) +
    geom_col(position = position_dodge(width = 0.86), width = 0.8) +
    geom_text(aes(label = ifelse(pct > 0, round(pct), "")), position = position_dodge(width = 0.86), vjust = -0.4, size = 4.2, colour = COR$ink2) +
    scale_fill_manual(values = COR_PERFIL, breaks = NIVEIS_PERFIL) + guides(fill = guide_legend(nrow = 2)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = "% dos candidatos do grupo") + theme_risco(15)
  salvar_fig(p, arquivo, w = w, h = h)
}

# ---- F01 perfil nacional ----------------------------------------------------------
d <- P[, .N, by = perfil][, pct := 100 * N / sum(N)]
d <- merge(data.table(perfil = factor(NIVEIS_PERFIL, levels = NIVEIS_PERFIL)), d, by = "perfil", all.x = TRUE)[is.na(N), `:=`(N = 0L, pct = 0)]
p <- ggplot(d, aes(x = pct, y = factor(perfil, levels = rev(NIVEIS_PERFIL)), fill = perfil)) +
  geom_col(width = 0.6) + geom_text(aes(label = paste0(N, " (", pct(pct), ")")), hjust = -0.1, colour = COR$ink2, size = 5) +
  scale_fill_manual(values = COR_PERFIL, guide = "none") + scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(x = NULL, y = NULL) +
  theme_risco_h(15) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p, "01_perfil_nacional.png", h = 3.6)

# ---- F02 perfil por regiao (barras agrupadas) ------------------------------------------
perfil_agrupado(P, "regiao", c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"), "02_perfil_por_regiao.png")

# ---- F03 fases do ciclo ---------------------------------------------------------------
f <- tab("t07_fases.csv"); f <- f[item != "nenhuma"]
f[, rot := ROT[item]]
p <- ggplot(f, aes(x = pct_candidatos, y = reorder(rot, pct_candidatos))) +
  geom_col(fill = COR$s1, width = 0.6) + geom_text(aes(label = paste0(candidatos, " (", pct(pct_candidatos), ")")), hjust = -0.1, colour = COR$ink2, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) + labs(x = NULL, y = NULL) +
  theme_risco_h(15) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p, "03_fases_ciclo.png", h = 3)

# ---- F04 ameacas -----------------------------------------------------------------------
a <- tab("t08_ameacas.csv"); a[, rot := ROT[item]]
p <- ggplot(a, aes(x = pct_candidatos, y = reorder(rot, pct_candidatos))) +
  geom_col(fill = COR$s1, width = 0.6) + geom_text(aes(label = paste0(candidatos, " (", pct(pct_candidatos), ")")), hjust = -0.1, colour = COR$ink2, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) + labs(x = NULL, y = NULL) +
  theme_risco_h(15) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p, "04_tipos_ameaca.png", h = 4.4)

# ---- F05 concretude (especificidade das janelas) ---------------------------------------
if (file.exists(file.path(DIR_TAB, "t09_especificidade_janelas.csv"))) {
  e <- tab("t09_especificidade_janelas.csv")
  rot_e <- c("0 Menção genérica", "1 Diagnóstico", "2 Diretriz", "3 Ação concreta", "4 Ação com meta")
  e[, rot := factor(rot_e[espec_a + 1], levels = rot_e)]
  p <- ggplot(e, aes(x = rot, y = pct)) + geom_col(fill = COR$s1, width = 0.6) +
    geom_text(aes(label = paste0(janelas, " (", pct(pct), ")")), vjust = -0.5, colour = COR$ink2, size = 5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = "% das passagens") + theme_risco(15)
  salvar_fig(p, "05_concretude.png", h = 4)
}

# ---- F06 ancoragem ---------------------------------------------------------------------
an <- tab("t10_ancoragem.csv")
rot_a <- c(orgao = "Nomeia órgão responsável", prazo = "Cita prazo ou ano", indicador = "Cita indicador ou monitoramento",
           meta = "Traz meta quantificada", orcamento = "Traz valor ou orçamento")
an[, rot := rot_a[elemento]]
p <- ggplot(an, aes(x = pct_candidatos_com_mencao, y = reorder(rot, pct_candidatos_com_mencao))) +
  geom_col(fill = COR$s1, width = 0.6) + geom_text(aes(label = pct(pct_candidatos_com_mencao)), hjust = -0.1, colour = COR$ink2, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.4))) + labs(x = NULL, y = NULL) +
  theme_risco_h(15) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p, "06_ancoragem.png", h = 3)

# ---- F07 perfil por campo politico (barras agrupadas) ---------------------------------------
if (any(!is.na(P$campo))) perfil_agrupado(P, "campo", CAMPOS_ORDEM, "07_perfil_por_campo.png", w = 10, h = 5)

# ---- F08 densidade x tamanho do plano ----------------------------------------------------
d <- copy(P); d[, grupo := fifelse(plano_curto, "Plano curto (menos de 2.000 palavras)", "Demais planos")]
p <- ggplot(d, aes(x = palavras, y = dens_1000, colour = grupo)) +
  geom_point(size = 2.4, alpha = 0.85) +
  scale_colour_manual(values = c("Plano curto (menos de 2.000 palavras)" = COR$s2, "Demais planos" = COR$s1)) +
  scale_x_log10(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  labs(x = "Palavras no plano (escala logarítmica)", y = "Passagens relevantes por mil palavras") + theme_risco(15)
salvar_fig(p, "08_densidade_x_tamanho.png")

# ---- F09 clima geral x risco e desastre (outlier nomeado) -------------------------------------
out <- P[NM_URNA_CANDIDATO == "MARCELO MARANATA"]
set.seed(7)
p <- ggplot(P, aes(x = n_janelas_clima_geral, y = n_janelas_relevantes)) +
  geom_jitter(width = 0.15, height = 0.15, colour = COR$s1, size = 2.2, alpha = 0.8) +
  geom_point(data = out, colour = COR$s2, size = 5) +
  geom_text(data = out, aes(label = paste0("Marcelo Maranata (", SG_PARTIDO, "-", SG_UF, ")")), hjust = 1.08, vjust = 0.4, size = 5, colour = COR$ink) +
  labs(x = "Passagens de agenda climática geral", y = "Passagens de risco e desastre") +
  theme_risco(15) + theme(panel.grid.major.x = element_line(colour = COR$grid, linewidth = 0.3))
salvar_fig(p, "09_clima_x_desastre.png")

# ---- F10 concordancia entre modelos --------------------------------------------------------
if (file.exists(file.path(DIR_TAB, "concordancia_modelos.csv"))) {
  cc <- tab("concordancia_modelos.csv")
  cc[, rot := fifelse(grepl("^fase_|^ameaca_", item), ROT[sub("^(fase|ameaca)_", "", item)],
                       fcase(item == "relevancia (3 categorias)", "Relevância (3 categorias)", item == "relevancia (relevante x resto)", "Relevância (relevante x resto)",
                             item == "especificidade (0-4, ordinal)", "Especificidade (0 a 4, ordinal)", default = item))]
  n_raros <- cc[!is.na(pos_a) & (pos_a + pos_b) < 6, .N]
  cc <- cc[is.na(pos_a) | (pos_a + pos_b) >= 6]
  cc <- cc[!is.na(alpha_krippendorff) & is.finite(alpha_krippendorff)]
  cc[, faixa := fcase(alpha_krippendorff >= 0.80, "≥ 0,80 quase perfeita", alpha_krippendorff >= 0.67, "0,67 a 0,80 aceitável", default = "< 0,67 a interpretar com cautela")]
  cc[, faixa := factor(faixa, levels = c("≥ 0,80 quase perfeita", "0,67 a 0,80 aceitável", "< 0,67 a interpretar com cautela"))]
  p <- ggplot(cc, aes(x = alpha_krippendorff, y = reorder(rot, alpha_krippendorff), colour = faixa)) +
    geom_vline(xintercept = c(0.67, 0.80), colour = COR$axis, linetype = "dashed", linewidth = 0.4) +
    geom_point(size = 3) + scale_colour_manual(values = c(COR$s3, COR$s4, COR$s2)) +
    labs(x = "Alfa de Krippendorff", y = NULL) + theme_risco_h(15) + guides(colour = guide_legend(nrow = 3))
  salvar_fig(p, "10_concordancia_modelos.png", h = 5)
}

# ---- F11 mapa por UF (opcional; baixa malhas do IPEA via geobr) ---------------------------
if (Sys.getenv("MAPA") == "1") {
  suppressPackageStartupMessages({library(geobr); library(sf)})
  uf <- read_state(year = 2020, simplified = TRUE, showProgress = FALSE)
  m <- P[, .(pct_mencao = 100 * mean(presenca), pct_acao = 100 * mean(perfil_n >= 4L)), by = SG_UF]
  g <- merge(uf, m, by.x = "abbrev_state", by.y = "SG_UF")
  p <- ggplot(g) + geom_sf(aes(fill = pct_acao), colour = COR$surface, linewidth = 0.3) +
    scale_fill_gradientn(colours = SEQ_AZUL, name = "% de candidatos", limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    theme_void(base_size = 15) + theme(plot.background = element_rect(fill = COR$surface, colour = NA), legend.position = "right")
  salvar_fig(p, "11_mapa_uf.png", h = 4.6)
}
cat("Figuras em", DIR_FIG, ":\n"); print(list.files(DIR_FIG))
