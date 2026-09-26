# 12_figuras_exposicao.R -- figuras da secao de exposicao a desastres (usa as tabelas t13-t16
# geradas por 11_exposicao.R). Sem graficos empilhados; titulo e fonte ficam na legenda do documento.

source(here::here("R", "00_setup.R"))
source(here::here("R", "tema_graficos.R"))
suppressPackageStartupMessages({library(ggplot2); library(patchwork)})

tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8")
ex  <- tab("t13_exposicao_uf_tipo.csv"); ci <- tab("t14_citacao_uf_tipo.csv")
al  <- tab("t15_alinhamento_uf.csv"); an <- tab("t16_exposicao_anual.csv")
ROT_T <- c(hidro = "Enchente e alagamento", movimento_massa = "Deslizamento e erosão", seca = "Seca e estiagem", fogo = "Queimadas e incêndios",
           calor_extremo = "Calor extremo", tempestade = "Tempestades", costeira = "Erosão costeira", barragem_mineracao = "Barragens e mineração",
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
d <- copy(al)[, rot := sprintf("%s · %s (n=%d)", uf, ROT_T[tipo_principal], candidatos)]
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
                                        "Deslizamento e erosão", "Outros (calor, costeira, barragem, tecnológico)"))]
cores <- setNames(c(COR$s2, COR$s1, COR$s3, COR$s4, COR$s7, COR$muted), levels(an$grupo))
p <- ggplot(an, aes(x = factor(ano), y = N, fill = grupo)) +
  geom_col(width = 0.75) +
  facet_wrap(~ grupo, ncol = 2, scales = "free_y", labeller = labeller(grupo = function(x) ifelse(grepl("^Outros", x), "Outros tipos", x))) +
  scale_fill_manual(values = cores, guide = "none") +
  scale_x_discrete(breaks = c("2013", "2016", "2019", "2022", "2025")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16)), labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Registros") + theme_risco(16) +
  theme(strip.text = element_text(face = "bold", hjust = 0, colour = COR$ink), panel.spacing = unit(1.1, "lines"))
salvar_fig(p, "14_registros_por_ano.png", w = 9, h = 10)
cat("figuras de exposicao ok\n")
