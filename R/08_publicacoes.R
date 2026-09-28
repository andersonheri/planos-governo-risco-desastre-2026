# 08_publicacoes.R -- one-page (PT e EN) em HTML + PDF.
# Preenche os templates de publicacoes/ com os numeros das tabelas e converte para PDF com o Edge (headless).
# O sumario executivo (publicacoes/sumario_executivo_narrativo.qmd) e renderizado a parte, via Quarto.
# Saidas: publicacoes/one_page_pt.pdf, one_page_en.pdf (+ HTML) e figuras *_en.png.

source(here::here("R", "00_setup.R"))
source(here::here("R", "tema_graficos.R"))
suppressPackageStartupMessages({library(ggplot2); library(patchwork); library(geobr); library(sf)})

DIR_PUB <- here::here("publicacoes")
REPO <- "https://github.com/andersonheri/planos-governo-risco-desastre-2026"
tab <- function(n) fread(file.path(DIR_TAB, n), encoding = "UTF-8")
vir <- function(x, d = 1) format(round(x, d), decimal.mark = ",", nsmall = d)
vd  <- function(x) format(round(x, 2), decimal.mark = ",", nsmall = 2)
vir_en <- function(x, d = 1) format(round(x, d), nsmall = d)

base <- tab("base_analitica.csv"); P <- base[grepl("^principal", cenario)]
cand <- fread(file.path(DIR_PROC, "candidatos_gov.csv"), encoding = "UTF-8")
fases <- tab("t07_fases.csv"); amea <- tab("t08_ameacas.csv"); esp <- tab("t09_especificidade_janelas.csv"); anc <- tab("t10_ancoragem.csv")
inc <- tab("t06_incumbente.csv"); campo <- tab("t05_por_campo.csv"); al <- tab("t15_alinhamento_uf.csv"); e20 <- tab("t20_exposicao_x_concretude.csv")
nc <- tab("t21_nao_citam_principal.csv"); tv <- tab("t18_tempo_verbal.csv"); conc <- tab("concordancia_modelos.csv")
rev <- fread(here::here("config", "revisao_manual.csv"), encoding = "UTF-8")
N <- nrow(P)
rho <- suppressWarnings(cor.test(e20$reg_mun, e20$pct_acao, method = "spearman"))$estimate
P[, faixa := cut(palavras, c(0, 5000, 10000, 20000, Inf), labels = c("a", "b", "c", "d"))]
me <- P[, .(m = 100 * mean(presenca)), by = faixa]
pf <- function(x) vir(fases[item == x, pct_candidatos]); pa <- function(x) vir(amea[item == x, pct_candidatos]); pe <- function(k) vir(esp[espec_a == k, pct])
pn <- function(x) vir(anc[elemento == x, pct_candidatos_com_mencao])
cg <- function(g, v) campo[campo == g][[v]]

V <- list(
  N = N, N_GOV = nrow(cand), N_DEF = cand[deferido == TRUE, .N], DATA_COLETA = format(DATA_CONGELAMENTO, "%d/%m/%Y"), ANO = format(Sys.Date(), "%Y"),
  N_SO_CLIMA = sum(P$perfil_n == 2L), P_ADAPT = pf("adaptacao"), P_GENERICA = pa("generica"), P_ORGAO = pn("orgao"),
  N_MENCAO = sum(P$presenca), P_MENCAO = vir(100 * mean(P$presenca)), N_NADA = sum(P$perfil_n == 1L), N_MENCIONA = sum(P$perfil_n == 3L),
  N_ACAO = sum(P$perfil_n >= 4L), P_ACAO = vir(100 * mean(P$perfil_n >= 4L)), N_META = sum(P$perfil_n == 5L),
  N_CICLO = sum(P$ciclo_completo), P_CICLO = vir(100 * mean(P$ciclo_completo)),
  P_PREV = pf("prevencao_preparacao"), P_RESP = pf("resposta"), P_REC = pf("recuperacao"),
  P_HIDRO = pa("hidro"), P_SECA = pa("seca"), P_FOGO = pa("fogo"),
  N_JANELAS = format(sum(P$n_janelas_relevantes), big.mark = "."), PE1 = pe(1), PE2 = pe(2), PE3 = pe(3), PE4 = pe(4),
  P_METAQ = pn("meta"), P_PRAZO = pn("prazo"), P_ORC = pn("orcamento"), N_PROM = 3,
  N_REB = rev[decisao == "rebaixada", .N], N_REV = nrow(rev),
  P_TV_INC = vir(tv[incumbente == TRUE, pct]), P_TV_NAO = vir(tv[incumbente == FALSE, pct]),
  P_CITA_PRINC = vir(100 * sum(al$citam_principal) / sum(al$candidatos)), PI_SHARE = vir(al[uf == "PI", share_principal], 0), PI_CITA = vir(al[uf == "PI", pct_cita_principal], 0),
  N_NC = nrow(nc), RHO = vd(rho),
  P_ME_5MIL = vir(me[faixa == "a", m], 0), P_ME_20MIL = vir(me[faixa == "d", m], 0), N_CURTOS = sum(P$plano_curto),
  P_ME_EE = vir(cg("extrema-esquerda", "pct_mencao"), 0), P_ME_DIR = vir(cg("direita", "pct_mencao"), 0), P_ME_XD = vir(cg("extrema-direita", "pct_mencao"), 0),
  N_INC = inc[incumbente == TRUE, n], P_INC_ACAO = vir(inc[incumbente == TRUE, pct_acao], 0), P_NAOINC_ACAO = vir(inc[incumbente == FALSE, pct_acao], 0),
  ALFA_REL = vd(conc[item == "relevancia (3 categorias)", alpha_krippendorff]), ALFA_ESP = vd(conc[item == "especificidade (0-4, ordinal)", alpha_krippendorff]),
  REPO = REPO, REPO_CURTO = sub("https://", "", REPO))

# ---- figuras em ingles (perfil, fases e mapa) --------------------------------------------------------------------------
NIV_EN <- c("No mention", "Climate agenda only", "Mentions risk and disaster", "Proposes concrete action", "Action with target, deadline or budget")
d <- P[, .N, by = perfil_n][order(perfil_n)]; d[, `:=`(pct = 100 * N / sum(N), rot = factor(NIV_EN[perfil_n], levels = rev(NIV_EN)))]
p1 <- ggplot(d, aes(x = pct, y = rot, fill = factor(perfil_n))) + geom_col(width = 0.6) +
  geom_text(aes(label = paste0(N, " (", vir_en(pct), "%)")), hjust = -0.1, colour = COR$ink2, size = 5) +
  scale_fill_manual(values = unname(COR_PERFIL), guide = "none") + scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(x = NULL, y = NULL) + theme_risco_h(15) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p1, "01_perfil_nacional_en.png", h = 3.6)
# versoes compactas (fonte grande) para o one-page, em PT e EN
perfil_op <- function(niveis, arquivo, fm = vir) {
  dd <- copy(d); dd[, rot := factor(niveis[perfil_n], levels = rev(niveis))]
  ggplot(dd, aes(x = pct, y = rot, fill = factor(perfil_n))) + geom_col(width = 0.62) +
    geom_text(aes(label = paste0(N, " (", fm(pct), "%)")), hjust = -0.08, colour = COR$ink2, size = 6.6) +
    scale_fill_manual(values = unname(COR_PERFIL), guide = "none") + scale_x_continuous(expand = expansion(mult = c(0, 0.6))) +
    labs(x = NULL, y = NULL) + theme_risco_h(20) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()) -> pp
  salvar_fig(pp, arquivo, w = 7.2, h = 4.4)
}
perfil_op(c("Sem menção", "Só clima geral", "Menciona o tema", "Ação concreta", "Ação com meta"), "01_perfil_nacional_op.png")
perfil_op(c("No mention", "Climate only", "Mentions topic", "Concrete action", "Action + target"), "01_perfil_nacional_op_en.png", fm = vir_en)
FEN <- c(prevencao_preparacao = "Prevention and preparedness", resposta = "Response", recuperacao = "Recovery", adaptacao = "Climate adaptation")
f <- fases[item != "nenhuma"]; f[, rot := FEN[item]]
p2 <- ggplot(f, aes(x = pct_candidatos, y = reorder(rot, pct_candidatos))) + geom_col(fill = COR$s1, width = 0.6) +
  geom_text(aes(label = paste0(candidatos, " (", vir_en(pct_candidatos), "%)")), hjust = -0.1, colour = COR$ink2, size = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) + labs(x = NULL, y = NULL) + theme_risco_h(15) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p2, "03_fases_ciclo_en.png", h = 3)

ROT_EN <- c(hidro = "Floods", seca = "Droughts", fogo = "Wildfires", movimento_massa = "Landslides"); COR_EN <- setNames(c(COR$s1, COR$s2, COR$s3, COR$s7), ROT_EN)
g <- read_state(year = 2020, simplified = TRUE, showProgress = FALSE)
g <- merge(g, al[, .(uf, tipo_principal, pct_cita_principal)], by.x = "abbrev_state", by.y = "uf", all.x = TRUE)
xy <- st_coordinates(suppressWarnings(st_centroid(st_geometry(g))))
dm <- data.frame(uf = g$abbrev_state, x = xy[, 1], y = xy[, 2], pct = g$pct_cita_principal, stringsAsFactors = FALSE); dm <- dm[!is.na(dm$pct), ]
DESL <- data.frame(uf = c("RN", "PB", "PE", "AL", "SE", "ES", "RJ", "SC", "PR", "AC", "DF"), dx = c(4.5, 5, 5, 4.5, 4, 3.5, 3.5, 4.5, 5.5, -3.5, 13), dy = c(1.6, 0.2, -1.2, -2.3, -3.4, -0.4, -2, -1, 0.4, -1.5, -0.6))
dm <- merge(dm, DESL, by = "uf", all.x = TRUE); dm$desloc <- !is.na(dm$dx); dm$dx[!dm$desloc] <- 0; dm$dy[!dm$desloc] <- 0
dm$lx <- dm$x + dm$dx; dm$ly <- dm$y + dm$dy; dm$uf2 <- ifelse(dm$uf == "DF", "DF*", dm$uf)
dm$rot_B <- ifelse(dm$desloc, paste0(dm$uf2, " ", round(dm$pct), "%"), paste0(dm$uf2, "\n", round(dm$pct), "%"))
tema_mapa <- function() theme_void(base_size = 16) + theme(plot.background = element_rect(fill = COR$surface, colour = NA), legend.position = "bottom", legend.title = element_blank(),
  plot.title = element_text(face = "bold", size = 16, hjust = 0), legend.text = element_text(size = 14), plot.margin = margin(5, 50, 5, 5))
pA <- ggplot() + geom_sf(data = g, aes(fill = factor(ROT_EN[tipo_principal], levels = ROT_EN)), colour = COR$surface, linewidth = 0.4) +
  geom_segment(data = dm[dm$desloc, ], aes(x = x, y = y, xend = lx - 0.8, yend = ly), colour = COR$ink2, linewidth = 0.3) +
  geom_text(data = dm, aes(x = lx, y = ly, label = uf2, colour = desloc), size = 4.6, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = COR$ink, `FALSE` = "white")) + scale_fill_manual(values = COR_EN, na.translate = FALSE) +
  coord_sf(clip = "off") + guides(fill = guide_legend(nrow = 2)) + labs(title = "What the state faces:\nmost frequent disaster type") + tema_mapa()
pB <- ggplot() + geom_sf(data = g, aes(fill = pct_cita_principal), colour = COR$surface, linewidth = 0.4) +
  geom_segment(data = dm[dm$desloc, ], aes(x = x, y = y, xend = lx - 1.0, yend = ly), colour = COR$ink2, linewidth = 0.3) +
  geom_text(data = dm, aes(x = lx, y = ly, label = rot_B, colour = pct >= 55 & !desloc), size = 4.4, lineheight = 0.9, fontface = "bold", hjust = ifelse(dm$desloc, ifelse(dm$dx > 0, 0, 1), 0.5), show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COR$ink)) +
  scale_fill_gradientn(colours = SEQ_AZUL, limits = c(0, 100), na.value = "#d9d8d1", labels = function(x) paste0(x, "%"), guide = guide_colourbar(barwidth = 12, barheight = 0.7)) +
  coord_sf(clip = "off") + labs(title = "What candidates say:\n% citing that type") + tema_mapa()
salvar_fig(pA + pB + plot_layout(ncol = 2), "15_mapa_descompasso_en.png", w = 11, h = 6.4)

# ---- versoes compactas em PT (fonte grande) para o sumario ----------------------------------------------------------------
barras_op <- function(dd, x, y, rotulo, arquivo, cor = COR$s1, h = 3.2) {
  p <- ggplot(dd, aes(x = .data[[x]], y = reorder(.data[[y]], .data[[x]]))) + geom_col(fill = cor, width = 0.62) +
    geom_text(aes(label = .data[[rotulo]]), hjust = -0.08, colour = COR$ink2, size = 6.6) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.55))) + labs(x = NULL, y = NULL) +
    theme_risco_h(20) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
  salvar_fig(p, arquivo, w = 7.2, h = h)
}
ff <- fases[item != "nenhuma"]; ff[, `:=`(rot = c(prevencao_preparacao = "Prevenção e preparação", resposta = "Resposta", recuperacao = "Recuperação", adaptacao = "Adaptação climática")[item], lab = paste0(candidatos, " (", vir(pct_candidatos), "%)"))]
barras_op(ff, "pct_candidatos", "rot", "lab", "03_fases_op.png", h = 3.4)
ee <- copy(esp); ee[, `:=`(rot = c("Menção genérica", "Diagnóstico", "Diretriz", "Ação concreta", "Ação com meta")[espec_a + 1], lab = paste0(vir(pct), "%"))]
p5 <- ggplot(ee, aes(x = pct, y = reorder(rot, pct))) + geom_col(fill = COR$s1, width = 0.62) + geom_text(aes(label = lab), hjust = -0.08, colour = COR$ink2, size = 6.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) + labs(x = NULL, y = NULL) + theme_risco_h(20) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
salvar_fig(p5, "05_concretude_op.png", w = 7.2, h = 3.6)
aa <- copy(anc); aa[, `:=`(rot = c(orgao = "Órgão responsável", prazo = "Prazo ou ano", indicador = "Indicador ou monitoramento", meta = "Meta quantificada", orcamento = "Valor ou orçamento")[elemento], lab = paste0(vir(pct_candidatos_com_mencao), "%"))]
barras_op(aa, "pct_candidatos_com_mencao", "rot", "lab", "06_ancoragem_op.png", h = 3.6)

# ---- preenche os templates e gera os PDFs --------------------------------------------------------------------------------
preenche <- function(tpl, saida, vals = NULL) {
  if (is.null(vals)) vals <- get("V", envir = globalenv())
  x <- paste(readLines(file.path(DIR_PUB, tpl), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (k in names(vals)) x <- gsub(paste0("{{", k, "}}"), as.character(vals[[k]]), x, fixed = TRUE)
  sobra <- regmatches(x, gregexpr("\\{\\{[A-Z0-9_]+\\}\\}", x))[[1]]
  if (length(sobra)) stop("placeholders sem valor em ", tpl, ": ", paste(unique(sobra), collapse = ", "))
  writeLines(x, file.path(DIR_PUB, saida), useBytes = TRUE)
}
EDGE <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
pdf_de <- function(html, pdf) {
  url <- paste0("file:///", gsub(" ", "%20", normalizePath(file.path(DIR_PUB, html), winslash = "/")))
  system2(EDGE, c("--headless=new", "--disable-gpu", "--no-pdf-header-footer", paste0("--print-to-pdf=", shQuote(normalizePath(file.path(DIR_PUB, pdf), winslash = "/", mustWork = FALSE))), shQuote(url)), stdout = FALSE, stderr = FALSE)
}
preenche("template_onepager_pt.html", "one_page_pt.html");         pdf_de("one_page_pt.html", "one_page_pt.pdf")
V_en <- lapply(V, function(v) if (is.character(v)) sub("^(-?[0-9]+),([0-9]+)$", "\\1.\\2", v) else v)
preenche("template_onepager_en.html", "one_page_en.html", V_en);         pdf_de("one_page_en.html", "one_page_en.pdf")
cat("publicacoes ok:", paste(list.files(DIR_PUB, pattern = "pdf$"), collapse = ", "), "\n")
