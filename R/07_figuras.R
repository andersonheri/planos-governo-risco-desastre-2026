# 07_figuras.R -- todas as figuras do relatorio (perfil, exposicao, mapas, impressao digital e termos por campo).
# (script unido: cada parte corresponde a um script da versao anterior e roda em ambiente proprio, sem misturar variaveis)

source(here::here("R", "00_setup.R"))
source(here::here("R", "dicionario.R"))
source(here::here("R", "tema_graficos.R"))

# O mapa por UF da parte 1 usa o geobr (malhas do IPEA) e so roda com MAPA=1.
# ==== Parte 1. Figuras de perfil, fases, ameacas, concretude, ancoragem, clima e concordancia =====
local({
# 10_figuras.R -- figuras do relatorio (PNG em outputs/figures/), a partir de
# outputs/tables/ (gerado por 08 e 09). O mapa (F11) usa geobr, que baixa as
# malhas do IPEA: so roda com MAPA=1.
#
# Convencoes: nenhum grafico empilhado (barras agrupadas ou paineis); titulo e
# fonte NAO ficam dentro da imagem (entram como legenda no documento: titulo em
# cima, fonte embaixo).



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
         costeira = "Erosão costeira e fluvial", barragem_mineracao = "Barragens e mineração", tecnologico = "Acidentes tecnológicos",
         generica = "Desastres em geral (sem tipo)")
pct <- function(x) paste0(format(round(x, 1), decimal.mark = ","), "%")

# barras agrupadas de perfil por grupo (regiao ou campo): grupos no eixo x, degraus lado a lado
perfil_agrupado <- function(P, grupo, ordem, arquivo, w = 9.5, h = 4.8) {
  d <- P[!is.na(get(grupo)), .N, by = c(grupo, "perfil")]
  d <- merge(CJ(g = ordem, perfil = factor(NIVEIS_PERFIL, levels = NIVEIS_PERFIL)), setnames(d, grupo, "g"), by = c("g", "perfil"), all.x = TRUE)
  d[is.na(N), N := 0L]
  d[, pct := 100 * N / sum(N), by = g]
  n_g <- P[!is.na(get(grupo)), .N, by = grupo]; setnames(n_g, grupo, "g")
  d[, rot := factor(g, levels = ordem, labels = sprintf("%s\n(n = %d)", stringr::str_wrap(ordem, 13), n_g$N[match(ordem, n_g$g)]))]
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
# Dois mapas lado a lado: presenca do tema (mencao) e acao concreta predominante,
# para nao confundir as duas variaveis (comentario C17 da revisao externa).
if (Sys.getenv("MAPA") == "1") {
  suppressPackageStartupMessages({library(geobr); library(sf); library(patchwork)})
  uf <- read_state(year = 2020, simplified = TRUE, showProgress = FALSE)
  m <- P[, .(pct_mencao = 100 * mean(presenca), pct_acao = 100 * mean(perfil_n >= 4L), n = .N), by = SG_UF]
  g <- merge(uf, m, by.x = "abbrev_state", by.y = "SG_UF")
  mapa1 <- function(var, titulo) {
    ggplot(g) + geom_sf(aes(fill = get(var)), colour = COR$surface, linewidth = 0.3) +
      scale_fill_gradientn(colours = SEQ_AZUL, name = "% de candidatos", limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(title = titulo) +
      theme_void(base_size = 14) + theme(plot.background = element_rect(fill = COR$surface, colour = NA), legend.position = "right",
                                          plot.title = element_text(hjust = 0.5, size = 13))
  }
  p <- mapa1("pct_mencao", "Presença do tema") + mapa1("pct_acao", "Ação concreta predominante") +
    plot_layout(guides = "collect") & theme(legend.position = "right")
  salvar_fig(p, "11_mapa_uf.png", h = 4.6, w = 9.5)
}
cat("Figuras em", DIR_FIG, ":\n"); print(list.files(DIR_FIG))
})

# ==== Parte 2. Figuras de exposicao a desastres ===================================================
local({
# 12_figuras_exposicao.R -- figuras da secao de exposicao a desastres (usa as tabelas t13-t16
# geradas por 11_exposicao.R). Sem graficos empilhados; titulo e fonte ficam na legenda do documento.



suppressPackageStartupMessages({library(ggplot2); library(patchwork)})

tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8")
ex  <- tab("t13_exposicao_uf_tipo.csv"); ci <- tab("t14_citacao_uf_tipo.csv")
al  <- tab("t15_alinhamento_uf.csv"); an <- tab("t16_exposicao_anual.csv")
ROT_T <- c(hidro = "Enchente e alagamento", movimento_massa = "Deslizamento e erosão", seca = "Seca e estiagem", fogo = "Queimadas e incêndios",
           calor_extremo = "Calor extremo", tempestade = "Tempestades", costeira = "Erosão costeira e fluvial", barragem_mineracao = "Barragens e mineração",
           tecnologico = "Acidentes tecnológicos")
ORD_T <- c("hidro", "seca", "fogo", "movimento_massa", "tempestade", "calor_extremo", "costeira", "barragem_mineracao", "tecnologico")

# ordem das UFs: regiao e, dentro dela, sigla
ord_uf <- unique(data.table(uf = names(REGIAO_UF), regiao = unname(REGIAO_UF))[order(match(regiao, c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")), uf)]$uf)
ord_uf <- ord_uf[ord_uf %in% ex$uf]   # so UFs com base de exposicao (>= 30 registros)

# ---- F12 mapa de calor pareado: exposicao x citacao ---------------------------------------------
mk <- function(d, val, titulo, cor_txt_limite = 55) {
  d[, `:=`(uf = factor(uf, levels = rev(ord_uf)), tipo = factor(tipo, levels = ORD_T, labels = ROT_T[ORD_T]))]
  ggplot(d, aes(x = tipo, y = uf, fill = get(val))) +
    geom_tile(colour = COR$surface, linewidth = 0.5) +
    geom_text(aes(label = ifelse(get(val) >= 1, round(get(val)), ""), colour = get(val) >= cor_txt_limite), size = 4.2, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COR$ink2)) +
    scale_fill_gradientn(colours = SEQ_AZUL, limits = c(0, 100), guide = "none") +
    scale_x_discrete(position = "top", guide = guide_axis(angle = 45)) +
    labs(title = titulo, x = NULL, y = NULL) +
    theme_risco(16) + theme(panel.grid = element_blank(), axis.line = element_blank(), axis.text.x = element_text(size = 14, hjust = 0),
                          plot.title = element_text(size = 15, face = "bold"))
}
e <- copy(ex)[, .(uf, tipo, v = share_registros)]
c_ <- copy(ci)[tipo != "generica", .(uf, tipo, v = pct_candidatos)]
p1 <- mk(e, "v", "Exposição:
% dos registros do estado")
p2 <- mk(c_, "v", "Citação:
% dos candidatos que citam") + theme(axis.text.y = element_blank(), plot.margin = margin(5, 110, 5, 5))
p <- (p1 | p2) + plot_annotation(theme = theme(plot.background = element_rect(fill = COR$surface, colour = NA)))
salvar_fig(p, "12_exposicao_x_citacao.png", w = 9, h = 12.5)

# ---- F13 quem cita o principal desastre do seu estado ---------------------------------------------
d <- copy(al)[, rot := sprintf("%s · %s (n=%d)", ifelse(uf == "DF", "DF*", uf), ROT_T[tipo_principal], candidatos)]
d[, regiao := factor(regiao, levels = names(COR_REGIAO))]
p <- ggplot(d, aes(x = pct_cita_principal, y = reorder(rot, pct_cita_principal), colour = regiao)) +
  geom_segment(aes(x = 0, xend = pct_cita_principal, yend = reorder(rot, pct_cita_principal)), colour = COR$grid, linewidth = 0.6) +
  geom_point(size = 4.5) +
  scale_colour_manual(values = COR_REGIAO) + scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.03))) +
  labs(x = "% dos candidatos que citam o tipo principal do estado", y = NULL) +
  theme_risco_h(16) + theme(legend.position = "top")
salvar_fig(p, "13_cita_principal_desastre.png", w = 9, h = 10)

# ---- F14 registros por ano: um painel por tipo, anos lado a lado (sem empilhar) ---------------------
an[, grupo := factor(grupo, levels = c("Seca e estiagem", "Enchente e alagamento", "Queimadas e incêndios", "Tempestades e vendavais",
                                        "Deslizamento e erosão", "Outros (calor, costeira e fluvial, barragem, tecnológico)"))]
cores <- setNames(c(COR$s2, COR$s1, COR$s3, COR$s4, COR$s7, COR$muted), levels(an$grupo))
p <- ggplot(an, aes(x = factor(ano), y = N, fill = grupo)) +
  geom_col(width = 0.75) +
  facet_wrap(~ grupo, ncol = 2, scales = "fixed", labeller = labeller(grupo = function(x) ifelse(grepl("^Outros", x), "Outros tipos", x))) +
  scale_fill_manual(values = cores, guide = "none") +
  scale_x_discrete(breaks = c("2013", "2016", "2019", "2022", "2025")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16)), labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Registros") + theme_risco(16) +
  theme(strip.text = element_text(face = "bold", hjust = 0, colour = COR$ink), panel.spacing = unit(1.1, "lines"))
salvar_fig(p, "14_registros_por_ano.png", w = 9, h = 10)
cat("figuras de exposicao ok\n")
})

# ==== Parte 3. Mapa do descompasso e cruzamento exposicao x concretude ============================
local({
# 14_figuras_mapas.R -- mapa do descompasso (exposicao x citacao) e cruzamento exposicao x concretude.
# Saidas: outputs/figures/15_mapa_descompasso.png, 16_exposicao_x_concretude.png e
# outputs/tables/t20_exposicao_x_concretude.csv. Requer as tabelas de 09/11 e as malhas do geobr (IPEA), em cache.



suppressPackageStartupMessages({library(ggplot2); library(patchwork); library(geobr); library(sf)})

tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8")
al  <- tab("t15_alinhamento_uf.csv"); ex <- tab("t13_exposicao_uf_tipo.csv"); uf_t <- tab("t03_por_uf.csv")
mun <- fread(here::here("config", "municipios_por_uf.csv"), encoding = "UTF-8")
ROT_T <- c(hidro = "Enchente e alagamento", seca = "Seca e estiagem", fogo = "Queimadas e incêndios", movimento_massa = "Deslizamento e erosão")
COR_T <- setNames(c(COR$s1, COR$s2, COR$s3, COR$s7), ROT_T)

g <- read_state(year = 2020, simplified = TRUE, showProgress = FALSE)
g <- merge(g, al[, .(uf, tipo_principal, pct_cita_principal)], by.x = "abbrev_state", by.y = "uf", all.x = TRUE)
xy <- st_coordinates(suppressWarnings(st_centroid(st_geometry(g))))
d <- data.frame(uf = g$abbrev_state, x = xy[, 1], y = xy[, 2], tipo = factor(ROT_T[g$tipo_principal], levels = ROT_T), pct = g$pct_cita_principal, stringsAsFactors = FALSE)
d <- d[!is.na(d$pct), ]
# UFs pequenas: rotulo deslocado para o mar, com linha ate o centroide
DESL <- data.frame(uf = c("RN", "PB", "PE", "AL", "SE", "ES", "RJ", "SC", "PR", "AC", "DF"), dx = c(4.5, 5, 5, 4.5, 4, 3.5, 3.5, 4.5, 5.5, -3.5, 13), dy = c(1.6, 0.2, -1.2, -2.3, -3.4, -0.4, -2, -1, 0.4, -1.5, -0.6))
d <- merge(d, DESL, by = "uf", all.x = TRUE)
d$desloc <- !is.na(d$dx); d$dx[!d$desloc] <- 0; d$dy[!d$desloc] <- 0
d$lx <- d$x + d$dx; d$ly <- d$y + d$dy
d$rot_B <- ifelse(d$desloc, paste0(ifelse(d$uf == "DF", "DF*", d$uf), " ", round(d$pct), "%"), paste0(d$uf, "\n", round(d$pct), "%"))

tema_mapa <- function() theme_void(base_size = 16) + theme(plot.background = element_rect(fill = COR$surface, colour = NA),
  legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(face = "bold", size = 16, hjust = 0),
  legend.text = element_text(size = 14), plot.margin = margin(5, 50, 5, 5))

pA <- ggplot() + geom_sf(data = g, aes(fill = factor(ROT_T[tipo_principal], levels = ROT_T)), colour = COR$surface, linewidth = 0.4) +
  geom_segment(data = d[d$desloc, ], aes(x = x, y = y, xend = lx - 0.8, yend = ly), colour = COR$ink2, linewidth = 0.3) +
  geom_text(data = d, aes(x = lx, y = ly, label = ifelse(uf == "DF", "DF*", uf), colour = desloc), size = 4.6, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = COR$ink, `FALSE` = "white")) +
  scale_fill_manual(values = COR_T, na.value = "#d9d8d1", na.translate = FALSE) +
  coord_sf(clip = "off") + guides(fill = guide_legend(nrow = 2)) + labs(title = "O que o estado enfrenta:\ntipo com mais registros") + tema_mapa()

pB <- ggplot() + geom_sf(data = g, aes(fill = pct_cita_principal), colour = COR$surface, linewidth = 0.4) +
  geom_segment(data = d[d$desloc, ], aes(x = x, y = y, xend = lx - 1.0, yend = ly), colour = COR$ink2, linewidth = 0.3) +
  geom_text(data = d, aes(x = lx, y = ly, label = rot_B, colour = pct >= 55 & !desloc), size = 4.4, lineheight = 0.9, fontface = "bold", hjust = ifelse(d$desloc, ifelse(d$dx > 0, 0, 1), 0.5), show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COR$ink)) +
  scale_fill_gradientn(colours = SEQ_AZUL, limits = c(0, 100), na.value = "#d9d8d1", labels = function(x) paste0(x, "%"), guide = guide_colourbar(barwidth = 12, barheight = 0.7)) +
  coord_sf(clip = "off") +
  labs(title = "O que os candidatos dizem:\n% que citam esse tipo") + tema_mapa()
salvar_fig(pA + pB + plot_layout(ncol = 2), "15_mapa_descompasso.png", w = 11, h = 6.4)

# ---- cruzamento: registros por municipio x % de candidatos com acao concreta ----------------------
reg <- ex[, .(registros = sum(registros)), by = uf]
e <- merge(merge(reg, mun, by = "uf"), uf_t[, .(uf = SG_UF, pct_acao, n)], by = "uf")
e[, reg_mun := registros / municipios]
e[, regiao := unname(REGIAO_UF[uf])]
# quadrantes: cortes na mediana de registros por municipio e no % nacional de candidatos com acao concreta
base <- tab("base_analitica.csv"); Pn <- base[grepl("^principal", cenario)]
CORTE_X <- median(e$reg_mun); CORTE_Y <- 100 * mean(Pn$perfil_n >= 4L)
e[, quadrante := fcase(reg_mun >= CORTE_X & pct_acao >= CORTE_Y, "Mais exposto, mais ação",
                       reg_mun < CORTE_X & pct_acao >= CORTE_Y, "Menos exposto, mais ação",
                       reg_mun >= CORTE_X & pct_acao < CORTE_Y, "Mais exposto, menos ação", default = "Menos exposto, menos ação")]
fwrite(e, file.path(DIR_TAB, "t20_exposicao_x_concretude.csv"))
cat("Cortes: x =", round(CORTE_X, 1), " y =", round(CORTE_Y, 1), "\n"); print(e[, .N, by = quadrante])
rho <- suppressWarnings(cor.test(e$reg_mun, e$pct_acao, method = "spearman"))
cat("Spearman:", round(rho$estimate, 2), " p =", round(rho$p.value, 3), "\n")
# posicao dos rotulos (evita sobreposicao): dx em unidades do eixo x, dy em pontos percentuais
POS <- data.table(uf = c("PB", "RS", "BA", "CE", "AP", "PA", "RJ", "RO", "RR", "TO", "SE"),
                  dx = c(-0.3, 0.3, -0.3, 0, -0.3, 0.3, -0.3, -0.3, 0.3, -0.3, 0.3), dy = c(0, 0, 0, -4.5, 3, 3, -3, 0, 0, 0, 3),
                  hj = c(1, 0, 1, 0.5, 1, 0, 1, 1, 0, 1, 0))
e <- merge(e, POS, by = "uf", all.x = TRUE)
e[is.na(dx), `:=`(dx = 0, dy = 4.5, hj = 0.5)]
p <- ggplot(e, aes(x = reg_mun, y = pct_acao, colour = regiao)) +
  geom_vline(xintercept = CORTE_X, linetype = "dashed", colour = COR$muted) + geom_hline(yintercept = CORTE_Y, linetype = "dashed", colour = COR$muted) +
  annotate("text", x = Inf, y = Inf, label = "Mais exposto,\nmais ação", hjust = 1.05, vjust = 1.2, size = 4.6, colour = COR$muted, fontface = "italic") +
  annotate("text", x = -Inf, y = Inf, label = "Menos exposto,\nmais ação", hjust = -0.05, vjust = 1.2, size = 4.6, colour = COR$muted, fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, label = "Mais exposto,\nmenos ação", hjust = 1.05, vjust = -0.3, size = 4.6, colour = COR$muted, fontface = "italic") +
  annotate("text", x = -Inf, y = -Inf, label = "Menos exposto,\nmenos ação", hjust = -0.05, vjust = -0.3, size = 4.6, colour = COR$muted, fontface = "italic") +
  geom_point(aes(size = n), alpha = 0.85) +
  geom_text(aes(x = reg_mun + dx, y = pct_acao + dy, label = ifelse(uf == "DF", "DF*", uf), hjust = hj), size = 4.8, colour = COR$ink, show.legend = FALSE) +
  scale_colour_manual(values = COR_REGIAO, name = NULL) + scale_size_continuous(range = c(3, 8), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.14, 0.1))) +
  labs(x = "Registros de desastre por município, 2013 a 2025", y = "% de candidatos com ação concreta predominante") +
  theme_risco(16) + guides(colour = guide_legend(override.aes = list(size = 4)))
salvar_fig(p, "16_exposicao_x_concretude.png", w = 9.5, h = 7)
cat("mapas ok\n")
})

# ==== Parte 4. Impressao digital, quem nao cita o desastre do estado e termos por campo ===========
local({
# 15_extras.R -- "impressao digital" dos planos, quem nao cita o desastre principal do proprio estado
# e expressoes de interesse por campo politico.
# Saidas: outputs/tables/t21_nao_citam_principal.csv, t22_impressao_digital.csv;
#         outputs/figures/17_termos_por_campo.png, 18a e 18b_impressao_digital.png



suppressPackageStartupMessages({library(ggplot2); library(acR)})

tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))
base <- tab("base_analitica.csv"); P <- base[grepl("^principal", cenario)]
jc   <- tab("janelas_classificadas.csv")
al   <- tab("t15_alinhamento_uf.csv")
jw   <- fread(file.path(DIR_PROC, "janelas.csv"), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))

ROT_T <- c(generica = "Desastres em geral", hidro = "Enchente e alagamento", seca = "Seca e estiagem", fogo = "Queimadas e incêndios", movimento_massa = "Deslizamento",
           tempestade = "Tempestades", calor_extremo = "Calor extremo", costeira = "Erosão costeira e fluvial", barragem_mineracao = "Barragens e mineração",
           tecnologico = "Acidentes tecnológicos")
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
# ordem por regiao e, dentro dela, UF e candidato em ordem alfabetica (comentario C16 da revisao externa)
d <- cand[passagens > 0]
d[, regiao := factor(REGIAO_UF[uf], levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))]
d <- d[order(regiao, uf, candidato)]
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
    scale_fill_manual(values = c(não = COR$nada, fase = COR$s1, tipo = COR$s3), guide = "none") +
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
res[, campo := factor(campo, levels = c("extrema-esquerda", "esquerda e centro-esquerda", "direita", "extrema-direita"),
                      labels = c("Extrema-esquerda", "Esquerda e centro-esquerda", "Direita", "Extrema-direita"))]
p <- ggplot(res, aes(x = pct, y = termo, fill = campo)) + geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = setNames(COR_CAMPO[c("extrema-esquerda", "esquerda e centro-esquerda", "direita", "extrema-direita")], levels(res$campo)), name = NULL) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "% das passagens do campo que trazem a expressão", y = NULL) + theme_risco_h(15)
salvar_fig(p, "17_termos_por_campo.png", w = 9, h = 9)
cat("extras ok\n")
})

