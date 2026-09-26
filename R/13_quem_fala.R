# 13_quem_fala.R -- tabelas da secao "Quem fala e como fala" e dos anexos do relatorio.
# Saidas (outputs/tables/): t17_destaques.csv, t18_tempo_verbal.csv, t19_anexo_candidatos.csv.
#
# Tempo verbal: indicador APROXIMADO por regra textual. Marca a frase focal que traz verbo de
# acao no preterito (o que "ja foi feito"): 1a pessoa do plural ("executamos") ou 3a pessoa
# ("o estado ampliou") e "foram + participio de acao". Nao marca "foi/foram/houve" sozinhos,
# que aparecem em diagnosticos, nem expressoes como "nos ultimos anos". O codebook nao
# distingue proposta, balanco e critica; este indicador so aproxima o balanco.

source(here::here("R", "00_setup.R"))
source(here::here("R", "dicionario.R"))

jc <- fread(file.path(DIR_TAB, "janelas_classificadas.csv"), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))
jw <- fread(file.path(DIR_PROC, "janelas.csv"), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))
base <- fread(file.path(DIR_TAB, "base_analitica.csv"), encoding = "UTF-8", colClasses = c(SQ_CANDIDATO = "character"))
P <- base[grepl("^principal", cenario)]

# ---- destaques: maiores e menores % de acao concreta (5 ou mais passagens) ------------
R <- P[n_janelas_relevantes >= 5, .(candidato = NM_URNA_CANDIDATO, partido = SG_PARTIDO, uf = SG_UF, campo,
                                    passagens = n_janelas_relevantes, pct_acao = round(pct_acao_concreta, 1), palavras, incumbente)]
top <- R[order(-pct_acao, -passagens, candidato)][1:5][, grupo := "maiores"]
bot <- R[order(pct_acao, -passagens, candidato)][1:5][, grupo := "menores"]
fwrite(rbind(top, bot), file.path(DIR_TAB, "t17_destaques.csv"))

# ---- tempo verbal nas passagens com acao concreta ------------------------------------
V1 <- "fizemos|executamos|entregamos|implantamos|investimos|realizamos|constru[ií]mos|ampliamos|criamos|reduzimos|capacitamos|distribu[ií]mos|modernizamos|instalamos|contratamos|adquirimos|conclu[ií]mos|consolidamos|estruturamos|fortalecemos|entregamos|recuperamos|reformamos|inauguramos|ampliou|fortaleceu|executou|entregou|criou|investiu|reduziu|construiu|realizou|implantou|instalou|concluiu|modernizou|consolidou|estruturou|distribuiu|capacitou|contratou|recuperou|reformou|inaugurou|ampliaram|entregaram|fortaleceram|executaram|implantaram|instalaram|reduziram|construíram|distribu[ií]ram|capacitaram"
V2 <- "foram (distribu[ií]d|capacitad|entregue|executad|constru[ií]d|implantad|instalad|conclu[ií]d|ampliad|realizad|contratad|reformad|inaugurad)"
rx <- paste0("\\b(", V1, ")\\b|\\b", V2)
j <- merge(jc, jw[, .(janela_id, ff = frases_foco)], by = "janela_id")
j <- merge(j, P[, .(SQ_CANDIDATO, incumbente)], by = "SQ_CANDIDATO")
j[, passado := grepl(rx, ff, ignore.case = TRUE, perl = TRUE)]
tv <- j[espec_a >= 3, .(passagens_concretas = .N, com_passado = sum(passado), pct = round(100 * mean(passado), 1)), by = incumbente]
fwrite(tv, file.path(DIR_TAB, "t18_tempo_verbal.csv"))
cat("Tempo verbal (passagens com espec >= 3):\n"); print(tv)
cat("\nExemplos incumbentes:\n"); print(j[espec_a >= 3 & incumbente & passado, .(janela_id, ff = substr(gsub("\\s+", " ", ff), 1, 200))][1:10])
cat("\nExemplos demais:\n"); print(j[espec_a >= 3 & !incumbente & passado, .(janela_id, ff = substr(gsub("\\s+", " ", ff), 1, 200))][1:10])

# ---- anexo: os 172 candidatos --------------------------------------------------------------
A <- P[order(SG_UF, SG_PARTIDO, NM_URNA_CANDIDATO), .(uf = SG_UF, candidato = NM_URNA_CANDIDATO, partido = SG_PARTIDO, campo,
        palavras, passagens = n_janelas_relevantes, clima_geral = n_janelas_clima_geral, pct_acao = round(pct_acao_concreta, 0), perfil_n, incumbente)]
fwrite(A, file.path(DIR_TAB, "t19_anexo_candidatos.csv"))
cat("\nAnexo:", nrow(A), "candidatos\n")
