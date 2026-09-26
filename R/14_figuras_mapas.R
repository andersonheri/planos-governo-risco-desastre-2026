# 14_figuras_mapas.R -- mapa do descompasso (exposicao x citacao) e cruzamento exposicao x concretude.
# Saidas: outputs/figures/15_mapa_descompasso.png, 16_exposicao_x_concretude.png e
# outputs/tables/t20_exposicao_x_concretude.csv. Requer as tabelas de 09/11 e as malhas do geobr (IPEA), em cache.

source(here::here("R", "00_setup.R"))
source(here::here("R", "tema_graficos.R"))
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
