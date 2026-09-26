# 02_janelas_e_codebook.R -- dicionario, janelas, codebooks do acR e ancoragem por regras.
# (script unido: cada parte corresponde a um script da versao anterior e roda em ambiente proprio, sem misturar variaveis)

source(here::here("R", "00_setup.R"))
source(here::here("R", "dicionario.R"))

# ==== Parte 1. Trechos e dicionario (presenca e densidade) ========================================
local({
# 04_trechos_dicionario.R -- divide cada plano em trechos (150-400 palavras,
# com pagina de origem) e aplica o dicionario de risco e desastre
# (etapa 0 do codebook). Gera presenca e densidade por candidato.
#
# Saidas: data/processed/trechos.csv       (1 linha por trecho)
#         data/processed/dic_por_plano.csv (indicadores quantitativos)



MIN_PAL_TRECHO <- 150
MAX_PAL_TRECHO <- 400

# ---- Dicionario (aplicado a texto minusculo e sem acentos) ------------------
# Alta cobertura de proposito: o LLM decide a relevancia depois.



# ---- Divisao em trechos ------------------------------------------------------
n_pal <- function(x) lengths(strsplit(trimws(x), "\\s+"))

dividir_paragrafo_longo <- function(p) {
  sent <- unlist(strsplit(p, "(?<=[.!?])\\s+", perl = TRUE))
  out <- character(0); atual <- character(0)
  for (s in sent) {
    if (length(atual) > 0 && n_pal(paste(c(atual, s), collapse = " ")) > MAX_PAL_TRECHO) {
      out <- c(out, paste(atual, collapse = " ")); atual <- character(0)
    }
    atual <- c(atual, s)
  }
  if (length(atual)) out <- c(out, paste(atual, collapse = " "))
  out
}

trechos_do_plano <- function(sq) {
  txt <- paste(readLines(file.path(DIR_PROC, "texto", paste0(sq, ".txt")), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  paginas <- strsplit(txt, "\f", fixed = TRUE)[[1]]
  pars <- rbindlist(lapply(seq_along(paginas), function(i) {
    p <- trimws(strsplit(paginas[i], "\n\\s*\n")[[1]])
    p <- gsub("\\s*\n\\s*", " ", p)
    p <- p[n_pal(p) > 0 & nzchar(p)]
    if (!length(p)) return(NULL)
    data.table(pagina = i, par = p)
  }))
  if (is.null(pars) || !nrow(pars)) return(NULL)
  # quebra paragrafos muito longos
  pars <- pars[, .(par = if (n_pal(par) > MAX_PAL_TRECHO) dividir_paragrafo_longo(par) else par), by = .(pagina, i = seq_len(nrow(pars)))]
  # junta paragrafos ate atingir o minimo
  out <- list(); buf <- character(0); pg0 <- NA_integer_
  for (k in seq_len(nrow(pars))) {
    if (!length(buf)) pg0 <- pars$pagina[k]
    if (length(buf) && n_pal(paste(c(buf, pars$par[k]), collapse = " ")) > MAX_PAL_TRECHO) {
      out[[length(out) + 1]] <- data.table(pagina = pg0, texto = paste(buf, collapse = "\n\n")); buf <- character(0); pg0 <- pars$pagina[k]
    }
    buf <- c(buf, pars$par[k])
    if (n_pal(paste(buf, collapse = " ")) >= MIN_PAL_TRECHO) {
      out[[length(out) + 1]] <- data.table(pagina = pg0, texto = paste(buf, collapse = "\n\n")); buf <- character(0)
    }
  }
  if (length(buf)) out[[length(out) + 1]] <- data.table(pagina = pg0, texto = paste(buf, collapse = "\n\n"))
  r <- rbindlist(out)
  r[, `:=`(SQ_CANDIDATO = sq, trecho_id = sprintf("%s_%04d", sq, seq_len(.N)), palavras = n_pal(texto))]
  r
}

meta <- fread(file.path(DIR_PROC, "planos_meta.csv"), colClasses = c(SQ_CANDIDATO = "character"))
message("Dividindo ", nrow(meta), " planos em trechos...")
trechos <- rbindlist(lapply(meta$SQ_CANDIDATO, trechos_do_plano))

# ---- Dicionario ---------------------------------------------------------------
norm <- normalizar(trechos$texto)
hits <- sapply(REGEX_DIC, function(rx) grepl(rx, norm, perl = TRUE))
trechos[, (paste0("dic_", names(DICIONARIO))) := as.data.table(hits)]
trechos[, n_temas_dic := rowSums(hits)]
trechos[, n_fracos := vapply(regmatches(norm, gregexpr(REGEX_FRACO, norm, perl = TRUE)),
                             function(m) length(unique(m)), 0L)]
trechos[, hit_forte := n_temas_dic > 0]
trechos[, hit_dic := hit_forte | n_fracos >= 2]
# termos que casaram (para auditoria)
trechos[, termos := vapply(seq_len(.N), function(i) {
  m <- unlist(regmatches(norm[i], gregexpr(paste(c(REGEX_DIC, REGEX_FRACO), collapse = "|"), norm[i], perl = TRUE)))
  paste(head(unique(m), 6), collapse = "; ")
}, "")]

fwrite(trechos, file.path(DIR_PROC, "trechos.csv"))

# ---- Indicadores por plano ----------------------------------------------------
dic_plano <- trechos[, .(n_trechos = .N, palavras_trechos = sum(palavras),
                         n_trechos_dic = sum(hit_dic),
                         n_ocorrencias_forte = sum(dic_desastre | dic_defesa_civil | dic_hidro | dic_movimento_massa |
                                                    dic_seca | dic_fogo | dic_barragem | dic_alerta_prep)),
                     by = SQ_CANDIDATO]
dic_plano <- merge(meta, dic_plano, by = "SQ_CANDIDATO", all.x = TRUE)
dic_plano[is.na(n_trechos_dic), `:=`(n_trechos_dic = 0L, n_ocorrencias_forte = 0L)]
dic_plano[, presenca_dic := n_trechos_dic > 0]
dic_plano[, presenca_forte := n_ocorrencias_forte > 0]
dic_plano[, dens_dic_1000 := 1000 * n_trechos_dic / pmax(palavras, 1)]
fwrite(dic_plano, file.path(DIR_PROC, "dic_por_plano.csv"))

cat("Trechos:", nrow(trechos), " | com dicionario:", trechos[hit_dic == TRUE, .N],
    sprintf("(%.1f%%)\n", 100 * mean(trechos$hit_dic)))
cat("Palavras por trecho:\n"); print(summary(trechos$palavras))
cat("\nPlanos com >=1 trecho de dicionario:", dic_plano[presenca_dic == TRUE, .N], "de", nrow(dic_plano), "\n")
cat("Planos com termo 'forte' (desastre/defesa civil/enchente/...):", dic_plano[presenca_forte == TRUE, .N], "\n")
cat("\nTrechos de dicionario a classificar por UF (piloto RS PE AM MG):\n")
print(merge(trechos[hit_dic == TRUE, .N, by = SQ_CANDIDATO], meta[, .(SQ_CANDIDATO, SG_UF)], by = "SQ_CANDIDATO")[SG_UF %in% c("RS", "PE", "AM", "MG"), .(trechos = sum(N), candidatos = .N), by = SG_UF])
cat("\nPor tema (n de trechos):\n"); print(sort(colSums(trechos[, grep("^dic_", names(trechos), value = TRUE), with = FALSE]), decreasing = TRUE))
})

# ==== Parte 2. Janelas (frase do termo +/- 1 frase) e frase focal =================================
local({
# 04b_janelas.R -- unidade de classificacao do LLM: JANELAS em torno dos
# termos do dicionario (frase com o termo +/- 1 frase, fundindo janelas
# sobrepostas). Substitui os trechos de 150-400 palavras como unidade de
# classificacao (o piloto mostrou que a mencao de passagem no meio de trechos
# longos gerava discordancia entre modelos); os trechos continuam a base do
# denominador e do contexto de leitura.
#
# Saida: data/processed/janelas.csv  (1 linha por janela)




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
})

# ==== Parte 3. Codebooks do acR (v0.8) ============================================================
local({
# 05_codebooks.R -- codebooks do acR (versao v0.8). Documentacao completa no Anexo E do relatorio.
# Um codebook por dimensao: o acR codifica uma variavel por codebook.
# A relevancia roda primeiro; as demais dimensoes so nos trechos relevantes.
#
# Saida: data/processed/codebooks/*.rds (+ yaml via ac_qual_save_codebook)


suppressPackageStartupMessages(library(acR))

VERSAO_CODEBOOK <- "v0.8"
DIR_CB <- file.path(DIR_PROC, "codebooks"); dir.create(DIR_CB, showWarnings = FALSE)

INSTR_BASE <- paste(
  "Voce e um analista de conteudo de politicas publicas. O texto e um trecho do plano de governo",
  "de um candidato a governador de um estado brasileiro (eleicoes 2026).",
  "Classifique APENAS com base no que o trecho diz, sem inferir intencoes nem usar conhecimento externo.",
  "Definicao de risco e desastre: eventos naturais, climaticos ou tecnologicos (ou a probabilidade de",
  "ocorrerem) que causam danos a pessoas, bens ou ao ambiente (conceito da Defesa Civil brasileira,",
  "Lei 12.608/2012). Fora do escopo: mitigacao de emissoes pura, pandemias e saude publica,",
  "violencia e seguranca publica, risco fiscal, e 'meio ambiente' generico sem ligacao a risco.")

# ---- 1. Relevancia ------------------------------------------------------------
cb_relevancia <- ac_qual_codebook(
  name = paste0("relevancia_", VERSAO_CODEBOOK),
  instructions = paste(INSTR_BASE,
    "Tarefa: classifique o trecho em relevante, clima_geral ou nao_relevante.",
    "REGRA DE DECISAO (siga nesta ordem, sem exigir evento especifico ou acao concreta):",
    "1) Se o trecho contiver QUALQUER destas expressoes ou ideias, mesmo de forma breve, em lista ou generica, classifique como relevante:",
    "risco(s) climatico(s); evento(s) extremo(s) ou climatico(s); adaptacao climatica; vulnerabilidade a eventos climaticos;",
    "Defesa Civil; desastre(s), calamidade, emergencia (de origem natural); enchente, inundacao, alagamento, cheia, seca, estiagem,",
    "deslizamento, queimada, incendio florestal; area(s) de risco; risco de rompimento de barragem; gestao de riscos; alerta, sirene, contingencia.",
    "2) Somente se NENHUMA dessas ideias aparecer e o trecho falar de mudanca climatica, emergencia climatica, transicao ecologica,",
    "economia verde ou resiliencia climatica de forma geral, classifique como clima_geral.",
    "3) Caso contrario, nao_relevante. Nao rebaixe um trecho de relevante para clima_geral por ele ser breve, generico ou sem acao concreta."),
  categories = list(
    relevante = list(
      label = "Relevante",
      definition = paste("O trecho contem MENCAO EXPLICITA, mesmo breve, a desastres, riscos ou eventos extremos naturais,",
                         "climaticos ou tecnologicos (enchentes, deslizamentos, seca, cheia, queimadas, rompimento de barragens,",
                         "riscos climaticos, areas de risco, eventos extremos), a Defesa Civil no contexto de risco e emergencia,",
                         "a acoes de prevencao, preparacao, resposta ou recuperacao, OU a adaptacao climatica explicita",
                         "(reduzir vulnerabilidade a eventos climaticos). Basta a mencao estar no trecho: a profundidade",
                         "(generica, diretriz, acao concreta) e medida a parte, na dimensao de especificidade. Uma mencao em lista",
                         "ou diretriz e relevante. Desastres naturais que afetam saude, transporte ou servicos tambem sao",
                         "relevantes (ex.: cheia e seca que isolam comunidades)."),
      examples_pos = c(
        "Vamos instalar sirenes de alerta nas encostas e criar um plano de contingencia com a Defesa Civil.",
        "As politicas estaduais deverao considerar os riscos climaticos e a prevencao de eventos extremos.",
        "Financiaremos projetos de adaptacao climatica e drenagem para reduzir enchentes.",
        "Criaremos um pacto metropolitano para saneamento, drenagem e adaptacao climatica.",
        "Nos periodos de cheia, comunidades ribeirinhas ficam isoladas e sem acesso a saude.",
        "Populacao em situacao de rua atingida por enchentes e deslizamentos tera resposta rapida de protecao social."),
      examples_neg = c(
        "Vamos ampliar as vagas em creches e aumentar o salario dos professores.",
        "Reduziremos emissoes de CO2 com incentivo a energia solar.",
        "Combateremos a violencia com mais policiamento nas ruas.")),
    clima_geral = list(
      label = "Agenda climatica geral",
      definition = paste("O trecho menciona mudanca climatica, emergencia climatica, transicao ecologica, economia verde ou",
                         "resiliencia climatica de forma GERAL, sem referir evento extremo, desastre, risco ou vulnerabilidade",
                         "e sem adaptacao explicita. E agenda climatica ou ambiental, nao gestao de risco e desastre.",
                         "Se o trecho tambem cita evento extremo, desastre ou adaptacao explicita, e relevante, nao clima_geral."),
      examples_pos = c(
        "A transformacao demografica, as mudancas climaticas e a inteligencia artificial ja alteram a forma como vivemos.",
        "Precisamos de uma transicao justa que enfrente a emergencia climatica e a desigualdade.",
        "Eixo: responsabilidade fiscal, resiliencia climatica, inovacao e sustentabilidade.",
        "Ampliaremos o mercado de carbono e a economia verde para enfrentar as mudancas climaticas."),
      examples_neg = c(
        "Criaremos o Plano de Adaptacao Climatica para reduzir a vulnerabilidade a secas e enchentes.",
        "Vamos combater as queimadas e os incendios florestais.")),
    nao_relevante = list(
      label = "Nao relevante",
      definition = paste("O trecho nao contem mencao explicita a risco, desastre ou agenda climatica, ou so usa termos como 'risco' em",
                         "outro sentido (risco fiscal, risco social, seguranca), ou trata de emergencias de saude publica",
                         "(epidemias, surtos, preparacao do SUS), ou faz mencao apenas ao tema 'meio ambiente' ou 'sustentabilidade'",
                         "sem ligacao a eventos perigosos nem a agenda climatica."),
      examples_pos = c(
        "Garantiremos responsabilidade fiscal e reduzir o risco de endividamento do estado.",
        "Protegeremos criancas em situacao de risco social com o fortalecimento dos CRAS.",
        "Ampliaremos o saneamento basico e a coleta seletiva de lixo.",
        "Prepararemos o SUS com protocolos de resposta a emergencias de saude e surtos."),
      examples_neg = c(
        "Construiremos contencao de encostas nas areas sujeitas a deslizamento."))
  ),
  multilabel = FALSE, lang = "pt"
)

# ---- 2. Fase do ciclo de gestao de risco ---------------------------------------
cb_fase <- ac_qual_codebook(
  name = paste0("fase_", VERSAO_CODEBOOK),
  instructions = paste(INSTR_BASE, "O trecho ja foi considerado relevante.",
                       "Tarefa: identifique TODAS as fases do ciclo de gestao de risco de desastres que o trecho aborda.",
                       "Marque so as fases sustentadas pelo texto; nunca invente.",
                       "REGRAS: (a) marque adaptacao SOMENTE se o trecho falar explicitamente em adaptacao climatica ou em reduzir",
                       "a vulnerabilidade / preparar o territorio para eventos climaticos; mera mencao a mudanca climatica, economia",
                       "verde, preservacao ambiental ou reducao de emissoes NAO e adaptacao. (b) Prevencao e preparacao formam UMA so",
                       "categoria (prevencao_preparacao): tudo o que e feito ANTES do desastre, seja obra e medida fisica, seja gestao de",
                       "riscos, mapeamento, monitoramento, alerta, planejamento, Defesa Civil ou capacitacao. (c) Se nada disso se aplicar",
                       "e o trecho so cita o tema, marque nenhuma."),
  categories = list(
    prevencao_preparacao = list(
      label = "Prevencao e preparacao",
      definition = paste("Tudo o que e feito ANTES do desastre para reduzir sua chance, seu impacto ou aumentar a capacidade de",
                         "responder: obras de drenagem contra alagamento, contencao de encostas, reflorestamento de areas de risco,",
                         "remocao de familias de areas de risco, zoneamento e regulacao do uso do solo, protecao de nascentes e",
                         "matas ciliares (prevencao e mitigacao); e sistemas de alerta e sirenes, planos de contingencia, mapeamento",
                         "de areas de risco, monitoramento, treinamento e simulados, equipar e fortalecer a Defesa Civil (preparacao)."),
      examples_pos = c("Faremos obras de contencao de encostas e drenagem para evitar deslizamentos.",
                       "Vamos reassentar familias que vivem em areas de risco de inundacao.",
                       "Instalaremos sirenes e um sistema de alerta por SMS.",
                       "Fortaleceremos a Defesa Civil estadual com equipamentos e treinamento."),
      examples_neg = c("Enviaremos equipes de resgate durante as enchentes.",
                       "Construiremos moradias para quem perdeu tudo na enchente.")),
    resposta = list(
      label = "Resposta",
      definition = paste("Acao DURANTE ou logo APOS o desastre: resgate, abrigos, ajuda humanitaria, mobilizacao de bombeiros",
                         "e forcas de emergencia, assistencia imediata as vitimas."),
      examples_pos = c("Garantiremos abrigos e ajuda humanitaria imediata para desalojados.",
                       "O Corpo de Bombeiros fara o resgate de vitimas em enchentes."),
      examples_neg = c("Investiremos em prevencao para evitar enchentes.")),
    recuperacao = list(
      label = "Recuperacao e reconstrucao",
      definition = paste("Voltar a normalidade depois do desastre: reconstrucao de casas e infraestrutura, moradia definitiva,",
                         "auxilio economico e retomada da atividade das vitimas."),
      examples_pos = c("Reconstruiremos escolas e pontes destruidas pelas chuvas.",
                       "Daremos auxilio financeiro aos produtores atingidos pela estiagem para retomar a producao."),
      examples_neg = c("Instalaremos sirenes para alertar a populacao.")),
    adaptacao = list(
      label = "Adaptacao climatica",
      definition = paste("Acoes que citam explicitamente a mudanca do clima ou eventos extremos como motivo para reduzir a",
                         "vulnerabilidade: planos de adaptacao, infraestrutura resiliente ao clima, agricultura adaptada, cidades",
                         "resilientes. NAO e adaptacao: corte de emissoes (mitigacao) nem 'agenda verde' generica."),
      examples_pos = c("Criaremos o Plano de Adaptacao Climatica, com infraestrutura resiliente a eventos extremos.",
                       "Apoiaremos culturas resistentes a seca diante das mudancas climaticas."),
      examples_neg = c("Reduziremos as emissoes de gases de efeito estufa com energia limpa.")),
    nenhuma = list(
      label = "Nenhuma fase identificavel",
      definition = "O trecho so faz mencao ao tema, sem descrever qualquer acao ou fase do ciclo de gestao de risco.",
      examples_pos = c("Nosso estado sofre com enchentes todos os anos."),
      examples_neg = c("Construiremos reservatorios para conter enchentes."))
  ),
  multilabel = TRUE, lang = "pt"
)

# ---- 3. Tipo de ameaca ---------------------------------------------------------
cb_ameaca <- ac_qual_codebook(
  name = paste0("ameaca_", VERSAO_CODEBOOK),
  instructions = paste(INSTR_BASE, "O trecho ja foi considerado relevante.",
                       "Tarefa: identifique TODOS os tipos de ameaca mencionados EXPLICITAMENTE no trecho.",
                       "REGRA: marque generica SOMENTE quando o trecho fala de desastres, riscos ou eventos extremos sem citar nenhum tipo",
                       "especifico. Se citar qualquer tipo especifico (enchente, seca, queimada, deslizamento etc.), marque apenas os tipos",
                       "especificos e NAO marque generica. Mencao a mudanca climatica sem evento nao e ameaca: nao marque nada."),
  categories = list(
    hidro = list(label = "Enchente, inundacao, alagamento",
                 definition = "Chuvas intensas, cheias de rios, alagamentos urbanos, inundacoes.",
                 examples_pos = c("Reduziremos alagamentos e enchentes com drenagem urbana."), examples_neg = c("Ampliaremos o abastecimento de agua.")),
    movimento_massa = list(label = "Deslizamento e erosao de encosta",
                 definition = "Deslizamentos, escorregamentos, queda de barreiras, movimentos de massa em encostas.",
                 examples_pos = c("Contencao de encostas para evitar deslizamentos."), examples_neg = c("Recuperaremos estradas vicinais.")),
    seca = list(label = "Seca e estiagem",
                definition = "Estiagem, seca prolongada, crise hidrica, escassez de agua por eventos climaticos.",
                examples_pos = c("Convivencia com a seca com cisternas e reservatorios."), examples_neg = c("Melhoraremos o saneamento basico.")),
    fogo = list(label = "Queimadas e incendios florestais",
                definition = "Queimadas, incendios florestais e em vegetacao.",
                examples_pos = c("Combate as queimadas e brigadas de incendio florestal."), examples_neg = c("Reforcaremos o Corpo de Bombeiros para incendios urbanos em predios.")),
    calor_extremo = list(label = "Calor extremo", definition = "Ondas de calor e temperaturas extremas.",
                examples_pos = c("Plano de contingencia para ondas de calor."), examples_neg = c("Ampliaremos a arborizacao urbana por qualidade de vida.")),
    tempestade = list(label = "Tempestades e vendavais", definition = "Tempestades, vendavais, granizo, ciclones, tornados.",
                examples_pos = c("Apoio a familias atingidas por vendavais e granizo."), examples_neg = c("Melhoraremos a iluminacao publica.")),
    costeira = list(label = "Erosao costeira e mar", definition = "Erosao da costa, avanco e elevacao do nivel do mar, ressacas.",
                examples_pos = c("Contencao da erosao costeira e protecao da orla."), examples_neg = c("Promoveremos o turismo na orla.")),
    barragem_mineracao = list(label = "Barragens e mineracao", definition = "Rompimento ou risco de barragens (mineracao, rejeitos) e desastres associados a mineracao.",
                examples_pos = c("Fiscalizacao de barragens de rejeito para evitar rompimentos."), examples_neg = c("Construiremos barragens de agua para irrigacao.")),
    tecnologico = list(label = "Acidentes tecnologicos", definition = "Acidentes quimicos, radiologicos, industriais, vazamentos, produtos perigosos.",
                examples_pos = c("Plano de emergencia para acidentes com produtos perigosos."), examples_neg = c("Atrairemos industrias para o estado.")),
    generica = list(label = "Desastres em geral", definition = "Fala de desastres, calamidades ou eventos extremos sem especificar o tipo.",
                examples_pos = c("Fortaleceremos a Defesa Civil para responder a desastres."), examples_neg = c("Investiremos em drenagem para conter enchentes."))
  ),
  multilabel = TRUE, lang = "pt"
)

# ---- 4. Especificidade ---------------------------------------------------------
cb_especificidade <- ac_qual_codebook(
  name = paste0("especificidade_", VERSAO_CODEBOOK),
  instructions = paste(INSTR_BASE, "O trecho ja foi considerado relevante.",
                       "Tarefa: atribua UM nivel de especificidade ao tratamento de RISCO E DESASTRE no trecho.",
                       "FOCO: as frases entre » « contem o termo de risco ou desastre e sao o FOCO da analise; as demais frases",
                       "sao apenas contexto. Julgue SOMENTE o que as frases marcadas propoem ou afirmam sobre risco e desastre.",
                       "IGNORE acoes, metas, prazos, indicadores e valores que se refiram a outros temas (pavimentacao, saude,",
                       "agua potavel, educacao, seguranca, inclusao digital etc.), mesmo que apareçam junto ou logo depois.",
                       "Meta, prazo, indicador ou orcamento so contam para o nivel 4 se se referirem A PROPRIA acao de risco e desastre.",
                       "Criterios de priorizacao (ex.: priorizar municipios com maior risco climatico), listas genericas de investimento",
                       "(ex.: investimentos preventivos) e a simples citacao de um orgao ou da Defesa Civil sem acao sao no maximo nivel 2.",
                       "Na duvida entre dois niveis, escolha o MENOR."),
  categories = list(
    n0_generica = list(label = "0. Mencao generica",
        definition = "Cita o tema sem dizer nada concreto (slogan ou intencao vaga).",
        examples_pos = c("Vamos combater os desastres naturais."), examples_neg = c("Criaremos o Centro de Monitoramento de Desastres.")),
    n1_diagnostico = list(label = "1. Diagnostico",
        definition = "Descreve o problema (dados, historico, situacao) sem propor solucao.",
        examples_pos = c("O estado sofreu tres enchentes graves nos ultimos cinco anos, com milhares de desabrigados."), examples_neg = c("Vamos construir diques.")),
    n2_diretriz = list(label = "2. Diretriz ou compromisso",
        definition = paste("Propoe um compromisso, criterio de priorizacao ou direcao de politica sobre risco e desastre, sem acao",
                           "especifica identificavel. Inclui listas genericas de investimento e a mera citacao da Defesa Civil ou de um orgao."),
        examples_pos = c("Investiremos em prevencao de desastres.",
                         "Priorizar municipios e comunidades com maior risco climatico.",
                         "A Defesa Civil integrara o planejamento estadual."),
        examples_neg = c("Instalaremos 200 sirenes ate 2028.")),
    n3_acao = list(label = "3. Acao concreta",
        definition = paste("Descreve uma acao, programa, obra ou instrumento identificavel (o que sera feito) cujo objeto e prevenir, se preparar,",
                           "responder, recuperar ou se adaptar a riscos e desastres. A acao tem de ser SOBRE risco e desastre, e nao sobre outro tema",
                           "citado na mesma passagem."),
        examples_pos = c("Criaremos um sistema de alerta por SMS e um centro estadual de monitoramento.",
                         "Implantar obras de drenagem e contencao de encostas nas areas sujeitas a deslizamento."),
        examples_neg = c("Melhoraremos a gestao de riscos.", "Construir 500 km de pavimentacao urbana (a acao e pavimentacao, nao risco).")),
    n4_acao_meta = list(label = "4. Acao com meta, prazo, indicador ou orcamento",
        definition = paste("Acao concreta de risco e desastre acompanhada de meta quantificada, prazo, indicador ou orcamento que se referem A PROPRIA acao",
                           "de risco e desastre. Meta ou valor de outro tema nao conta."),
        examples_pos = c("Instalaremos 200 sirenes ate 2028, com investimento de R$ 50 milhoes.",
                         "Orcamento de R$ 1,5 bilhao para a Defesa Civil e o Corpo de Bombeiros."),
        examples_neg = c("Instalaremos sirenes nas areas de risco.", "Inventario dos sistemas de agua em 100 dias (meta de outro tema)."))
  ),
  multilabel = FALSE, lang = "pt"
)

# ---- 5. Ancoragem (elementos que dao concretude) --------------------------------
cb_ancoragem <- ac_qual_codebook(
  name = paste0("ancoragem_", VERSAO_CODEBOOK),
  instructions = paste(INSTR_BASE, "O trecho ja foi considerado relevante.",
                       "Tarefa: identifique TODOS os elementos de ancoragem presentes NO TRECHO, ligados a risco e desastre."),
  categories = list(
    orgao_responsavel = list(label = "Orgao responsavel", definition = "Nomeia secretaria, orgao ou instituicao responsavel pela acao.",
        examples_pos = c("A Defesa Civil estadual coordenara o plano."), examples_neg = c("Sera criado um plano.")),
    orcamento = list(label = "Orcamento ou fonte de recursos", definition = "Cita valor, fundo, orcamento ou fonte de financiamento.",
        examples_pos = c("Investimento de R$ 100 milhoes via Fundo Estadual de Prevencao."), examples_neg = c("Sera feito com prioridade.")),
    meta_quantificada = list(label = "Meta quantificada", definition = "Cita numero, quantidade ou percentual a ser alcancado.",
        examples_pos = c("Reduzir em 30% as areas de risco."), examples_neg = c("Reduziremos as areas de risco.")),
    prazo = list(label = "Prazo", definition = "Cita data, ano ou periodo para a acao.",
        examples_pos = c("Ate 2028."), examples_neg = c("No futuro.")),
    indicador = list(label = "Indicador de monitoramento", definition = "Cita indicador, monitoramento ou avaliacao da acao.",
        examples_pos = c("Monitoraremos o numero de familias em areas de risco."), examples_neg = c("Faremos a obra.")),
    nenhum = list(label = "Nenhum elemento", definition = "Nenhum dos elementos acima aparece no trecho.",
        examples_pos = c("Fortaleceremos a prevencao."), examples_neg = c("Ate 2028, a Defesa Civil recebera R$ 50 milhoes."))
  ),
  multilabel = TRUE, lang = "pt"
)

CODEBOOKS <- list(relevancia = cb_relevancia, fase = cb_fase, ameaca = cb_ameaca,
                  especificidade = cb_especificidade, ancoragem = cb_ancoragem)
for (n in names(CODEBOOKS)) saveRDS(CODEBOOKS[[n]], file.path(DIR_CB, paste0(n, ".rds")))
cat("Codebooks criados:", paste(names(CODEBOOKS), collapse = ", "), "\n")
print(CODEBOOKS$fase)
})

# ==== Parte 4. Ancoragem por regras (funcoes em dicionario.R) ==============================
local({
  jan <- fread(file.path(DIR_PROC, "janelas.csv"), colClasses = c(SQ_CANDIDATO = "character"))
  anc <- cbind(jan[, .(janela_id, SQ_CANDIDATO)], ancoragem_regras(jan$frases_foco))  # so a frase focal (evita meta de frase vizinha)
  fwrite(anc, file.path(DIR_PROC, "janelas_ancoragem.csv"))
  cat("Janelas:", nrow(anc), "\n")
  print(round(100 * colMeans(anc[, grep("^anc_", names(anc)), with = FALSE]), 1))
  cat("\nExemplos com orcamento:\n")
  print(head(jan[anc$anc_orcamento == TRUE, .(texto = substr(gsub("\\s+", " ", texto), 1, 160))], 3))
})


# ==== Parte 5. Documentacao legivel do codebook (codebook/codebook_v0.8.md) ==================================
local({
# gera codebook/codebook_v0.8.md, a documentacao legivel do codebook
# (definicoes e exemplos reais vem de config/codebook_exemplos.csv; as regras de decisao sao as dos prompts em
# R/02_janelas_e_codebook.R, parte 3). Rodar depois de alterar o codebook ou os exemplos.

cb <- data.table::fread(here::here("config", "codebook_exemplos.csv"), encoding = "UTF-8")
dir.create(here::here("codebook"), showWarnings = FALSE)

quadro <- function(dim) {
  d <- cb[dimensao == dim]
  c("| Categoria | Definição | Exemplo real |", "|---|---|---|",
    sprintf("| %s | %s | “%s” (%s) |", d$categoria, d$definicao, gsub("\\|", "/", d$exemplo), d$fonte))
}
md <- c(
  "# Codebook v0.8: risco e desastre nos planos de governo (governadores, 2026)", "",
  "**Unidade de análise:** *janela* (frase que contém um termo do dicionário, mais a frase anterior e a seguinte). A frase que contém o termo é a *frase focal* e vem marcada com » « no texto enviado aos modelos.",
  "**Definição-base:** risco e desastre são eventos naturais, climáticos ou tecnológicos, ou a probabilidade de ocorrerem, que causam danos a pessoas, bens ou ao ambiente (conceito da Defesa Civil, Lei 12.608/2012). Ficam de fora mitigação de emissões pura, emergências de saúde pública, violência e segurança pública, risco fiscal e \"meio ambiente\" genérico sem ligação a risco.",
  "**Recorte:** candidatos a governador com candidatura deferida e plano registrado. Coleta congelada em 23/09/2026.",
  "**Classificadores:** `gpt-oss-20b` (todas as janelas) e `gemma-4-26b` (relevância em todas; demais dimensões em 30% das relevantes), executados localmente pelo pacote `acR` (`ac_qual_codebook()` e `ac_qual_code()`).", "",
  "## 1. Relevância (uma categoria)", "",
  "Regra de decisão, em ordem: (1) qualquer menção a risco climático, evento extremo, adaptação climática, vulnerabilidade a eventos climáticos, Defesa Civil, desastre, calamidade ou emergência de origem natural, enchente, inundação, alagamento, cheia, seca, estiagem, deslizamento, queimada, incêndio florestal, área de risco, risco de rompimento de barragem, gestão de riscos, alerta, sirene ou contingência torna a janela *relevante*, mesmo que breve, em lista ou genérica; (2) só se nenhuma dessas ideias aparecer e o trecho falar de mudança ou emergência climática, transição ecológica, economia verde ou resiliência climática de forma geral, é *agenda climática geral*; (3) caso contrário, *não relevante*.", "",
  quadro("Relevância"), "",
  "## 2. Fase do ciclo de gestão de risco (várias categorias)", "",
  "Marcam-se todas as fases sustentadas pelo texto, sem inventar fases ausentes. Prevenção e preparação formam uma só categoria. A adaptação só vale com menção explícita à mudança do clima ou à vulnerabilidade a eventos climáticos.", "",
  quadro("Fase do ciclo"), "",
  "## 3. Tipo de ameaça (várias categorias)", "",
  "Só vale a citação explícita. \"Desastres em geral\" só se marca quando nenhum tipo específico é citado, e mudança climática sem evento não é ameaça.", "",
  quadro("Tipo de ameaça"), "",
  "## 4. Especificidade (uma nota, de 0 a 4)", "",
  "Julga-se apenas o que a frase focal propõe ou afirma sobre risco e desastre. Ações, metas, prazos e valores de outros temas são ignorados, e meta, prazo, indicador ou orçamento só contam para o nível 4 se disserem respeito à própria ação de risco. Na dúvida entre dois níveis, escolhe-se o menor. As passagens de nível 4 foram lidas uma a uma (`config/revisao_manual.csv`).", "",
  quadro("Especificidade"), "",
  "## 5. Ancoragem (detecção por regras textuais na frase focal, sem modelo)", "",
  "As expressões regulares estão em `R/dicionario.R` (`REGRAS_ANC`).", "",
  quadro("Ancoragem (regras)"), "",
  "## Indicadores derivados por candidato", "",
  "1. **Presença** do tema (ao menos uma janela relevante). 2. **Densidade** (janelas relevantes por mil palavras). 3. **Ciclo completo** (prevenção e preparação, resposta e recuperação). 4. **Tipos de ameaça** citados. 5. **Perfil em cinco degraus**, com \"ação concreta\" definida como metade ou mais das passagens com nível 3 ou 4. 6. **Ancoragem** (órgão, orçamento, meta, prazo, indicador).", "",
  "Todo indicador é apresentado junto do tamanho do plano; planos com menos de 2.000 palavras recebem alerta.")
writeLines(md, here::here("codebook", "codebook_v0.8.md"), useBytes = TRUE)
cat("codebook/codebook_v0.8.md gerado (", length(md), "linhas )\n")
})
