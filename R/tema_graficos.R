# tema_graficos.R -- tema e paletas dos graficos do relatorio (ggplot2).
# Paleta categorica validada (ordem fixa, modo claro), rampa sequencial azul e
# cinzas de tinta. Regras: cor so na marca (texto em cinza), grade discreta,
# um eixo, cores neutras para campos politicos (sem vermelho/azul partidarios).

suppressPackageStartupMessages({library(ggplot2); library(scales)})

COR <- list(
  # categoricas, ordem fixa (slots 1-8)
  s1 = "#2a78d6", s2 = "#eb6834", s3 = "#1baf7a", s4 = "#eda100",
  s5 = "#e87ba4", s6 = "#008300", s7 = "#4a3aa7", s8 = "#e34948",
  # tinta e superficie
  ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781",
  grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb", nada = "#c3c2b7"
)
SEQ_AZUL <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
CAT <- unname(unlist(COR[paste0("s", 1:8)]))

# Perfil do candidato em 5 degraus ordenados (da ausencia a acao com meta)
NIVEIS_PERFIL <- c("Sem menção", "Só agenda climática geral", "Menciona risco e desastre",
                   "Propõe ação concreta", "Ação com meta, prazo ou orçamento")
COR_PERFIL <- setNames(c(COR$nada, "#eda100", "#9ec5f4", "#3987e5", "#0d366b"), NIVEIS_PERFIL)
COR_REGIAO <- setNames(CAT[1:5], c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))
CAMPOS_ORDEM <- c("extrema-esquerda", "esquerda e centro-esquerda", "centro e centro-direita", "direita e extrema-direita")
COR_CAMPO  <- setNames(CAT[1:4], CAMPOS_ORDEM)   # neutras, sem carga partidaria

theme_risco <- function(base = 11) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = COR$surface, colour = NA),
      panel.background = element_rect(fill = COR$surface, colour = NA),
      panel.grid.major.y = element_line(colour = COR$grid, linewidth = 0.3),
      panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = COR$axis, linewidth = 0.4),
      axis.ticks = element_blank(),
      axis.text  = element_text(colour = COR$ink2), axis.title = element_text(colour = COR$ink2),
      plot.title = element_text(colour = COR$ink, face = "bold", size = base + 2, hjust = 0),
      plot.subtitle = element_text(colour = COR$ink2, size = base, hjust = 0),
      plot.caption = element_text(colour = COR$muted, size = base - 2, hjust = 0),
      legend.position = "top", legend.justification = "left",
      legend.text = element_text(colour = COR$ink2), legend.title = element_blank(),
      strip.text = element_text(colour = COR$ink, face = "bold", hjust = 0),
      plot.margin = margin(10, 14, 8, 10)
    )
}
# variante com grade no eixo x (barras horizontais)
theme_risco_h <- function(base = 11) {
  theme_risco(base) + theme(panel.grid.major.y = element_blank(),
                            panel.grid.major.x = element_line(colour = COR$grid, linewidth = 0.3),
                            axis.line.x = element_blank(), axis.line.y = element_line(colour = COR$axis, linewidth = 0.4))
}
.modelos <- if (exists("DIR_TAB") && file.exists(file.path(DIR_TAB, "concordancia_modelos.csv"))) "gpt-oss-20b e gemma-4-26b" else "gpt-oss-20b"
FONTE_CAP <- stringr::str_wrap(paste0("Fonte: planos de governo dos candidatos a governador (TSE, coleta de 23/09/2026); classificação por LLM local (", .modelos, ") e dicionário (acR)."), width = 100)
salvar_fig <- function(p, nome, w = 8, h = 4.5) {
  ggsave(file.path(DIR_FIG, nome), p, width = w, height = h, dpi = 150, bg = COR$surface)
  invisible(file.path(DIR_FIG, nome))
}
