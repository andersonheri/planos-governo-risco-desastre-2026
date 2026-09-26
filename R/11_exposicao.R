# 11_exposicao.R -- exposicao historica a desastres por UF (Atlas Digital de Desastres /
# S2iD, MIDR) x tipos de desastre citados nos planos.
#
# Fonte: data/raw/atlas_desastres/BD_Atlas_1991_2025_v1.1_2026.08.06_Consolidado.csv
# (registros municipais e reconhecimentos federais; seca/estiagem ja deduplicadas pelo Atlas).
# Periodo de exposicao: 2013-2025 (ano do evento).
#
# Exposicao por UF e tipo: composicao dos registros 2013-2025 (peso = parcela dos registros da UF
# que sao do tipo; soma 1). O tipo principal da UF e o de mais registros. A amplitude (% dos
# municipios com registro) fica na tabela t13 como informacao complementar; ela satura em 100%
# em varios estados e nao serve para o DF (um unico municipio).
# Cobertura da exposicao de um candidato: soma, sobre os tipos que ele cita, dos pesos da sua UF.
#
# Saidas (outputs/tables/): t13_exposicao_uf_tipo.csv, t14_citacao_uf_tipo.csv,
#   t15_alinhamento_uf.csv, t16_exposicao_anual.csv, cobertura_candidato.csv

source(here::here("R", "00_setup.R"))

PERIODO <- c(2013, 2025)
TIPOS <- c("hidro", "movimento_massa", "seca", "fogo", "calor_extremo", "tempestade",
           "costeira", "barragem_mineracao", "tecnologico")

# ---- mapeamento COBRADE -> tipos do codebook -----------------------------------------
# 12xxx inundacoes/enxurradas/alagamentos e 13214 chuvas intensas -> hidro; 113xx movimento de
# massa e 1142x/1143x erosao fluvial/continental -> movimento_massa; 11410 erosao costeira;
# 14110/14120 estiagem e seca; 1413x incendio florestal; 13310/14140 calor e baixa umidade;
# 1311x/1321x (exceto 13214) vendaval, ciclone, tornado, granizo, tempestade; 24200 barragens;
# demais 2xxxx tecnologicos. Fora do escopo: frio, doencas (15xxx) e outros.
tipo_cobrade <- function(cod) {
  fcase(
    grepl("^12", cod) | cod == "13214", "hidro",
    grepl("^113", cod) | grepl("^114[23]", cod), "movimento_massa",
    cod == "11410", "costeira",
    cod %in% c("14110", "14120"), "seca",
    grepl("^1413", cod), "fogo",
    cod %in% c("13310", "14140"), "calor_extremo",
    (grepl("^1311", cod) | grepl("^1321", cod)) & cod != "13214", "tempestade",
    cod == "24200", "barragem_mineracao",
    grepl("^2", cod), "tecnologico",
    default = NA_character_
  )
}

atlas <- fread(file.path(DIR_RAW, "atlas_desastres", "BD_Atlas_1991_2025_v1.1_2026.08.06_Consolidado.csv"),
               sep = ";", encoding = "Latin-1", colClasses = "character", showProgress = FALSE,
               select = c("Protocolo_S2iD", "Sigla_UF", "Data_Evento", "Data_Registro", "Cod_Cobrade", "Cod_IBGE_Mun", "Status"))
atlas[, (names(atlas)) := lapply(.SD, enc2utf8)]
atlas[, Sigla_UF := toupper(trimws(Sigla_UF))]   # a base traz "pa" em minusculo
ano_de <- function(x) as.integer(substr(x, nchar(x) - 3, nchar(x)))
atlas[, ano := ano_de(Data_Evento)]
atlas[is.na(ano) | ano < 1900, ano := ano_de(Data_Registro)]
atlas[, tipo := tipo_cobrade(Cod_Cobrade)]
mun <- fread(here::here("config", "municipios_por_uf.csv"))

cat("Registros no Atlas:", nrow(atlas), "| UFs distintas:", uniqueN(atlas$Sigla_UF), "\n")
cat("UFs fora da lista (ignoradas):", paste(setdiff(unique(atlas$Sigla_UF), UFS), collapse = ","), "\n")
# validacao da tabela de municipios: nenhuma UF pode ter mais municipios com registro do que o total
chk <- merge(atlas[Sigla_UF %in% UFS, .(com_registro = uniqueN(Cod_IBGE_Mun)), by = Sigla_UF], mun, by.x = "Sigla_UF", by.y = "uf")
stopifnot(all(chk$com_registro <= chk$municipios))
cat("Validacao dos totais de municipios: ok (max de municipios com registro por UF <= total)\n")

A <- atlas[Sigla_UF %in% UFS & ano >= PERIODO[1] & ano <= PERIODO[2]]
cat(sprintf("Periodo %d-%d: %d registros; %d (%.1f%%) mapeados aos tipos do estudo\n", PERIODO[1], PERIODO[2], nrow(A),
            A[!is.na(tipo), .N], 100 * A[!is.na(tipo), .N] / nrow(A)))
print(A[is.na(tipo), .N, by = Cod_Cobrade][order(-N)][1:6])

# ---- exposicao por UF e tipo -----------------------------------------------------------
expo <- A[!is.na(tipo), .(registros = .N, reconhecidos = sum(Status == "Reconhecido"), municipios_atingidos = uniqueN(Cod_IBGE_Mun)), by = .(uf = Sigla_UF, tipo)]
grade <- CJ(uf = UFS, tipo = TIPOS)
expo <- merge(grade, expo, by = c("uf", "tipo"), all.x = TRUE)
expo[is.na(registros), `:=`(registros = 0L, reconhecidos = 0L, municipios_atingidos = 0L)]
expo <- merge(expo, setNames(mun, c("uf", "municipios_uf")), by = "uf")
expo[, `:=`(pct_municipios = round(100 * municipios_atingidos / municipios_uf, 1), share_registros = NA_real_)]
expo[, share_registros := round(100 * registros / pmax(sum(registros), 1), 1), by = uf]
expo[, peso := registros / pmax(sum(registros), 1), by = uf]   # composicao dos registros da UF (soma 1)
# UFs com poucos registros nao permitem um perfil de exposicao (ex.: DF, um unico municipio)
MIN_REGISTROS <- 10   # DF entra com 16 registros (1 municipio): incluido com ressalva no texto
ufs_ok <- expo[, .(r = sum(registros)), by = uf][r >= MIN_REGISTROS, uf]
cat("UFs sem base para perfil de exposicao (< ", MIN_REGISTROS, " registros em ", PERIODO[1], "-", PERIODO[2], "): ", paste(setdiff(UFS, ufs_ok), collapse = ","), "\n", sep = "")
expo <- expo[uf %in% ufs_ok]
fwrite(expo, file.path(DIR_TAB, "t13_exposicao_uf_tipo.csv"))

# serie anual nacional por grupo de tipo
A[, grupo := fcase(tipo == "hidro", "Enchente e alagamento", tipo == "seca", "Seca e estiagem", tipo == "fogo", "Queimadas e incêndios",
                   tipo == "movimento_massa", "Deslizamento e erosão", tipo == "tempestade", "Tempestades e vendavais",
                   !is.na(tipo), "Outros (calor, costeira, barragem, tecnológico)", default = NA_character_)]
fwrite(A[!is.na(grupo), .N, by = .(ano, grupo)][order(ano, grupo)], file.path(DIR_TAB, "t16_exposicao_anual.csv"))

# ---- citacao nos planos --------------------------------------------------------------------
J    <- fread(file.path(DIR_TAB, "janelas_classificadas.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")
base <- fread(file.path(DIR_TAB, "base_analitica.csv"), colClasses = c(SQ_CANDIDATO = "character"), encoding = "UTF-8")[grepl("^principal", cenario)]
JR <- J[rel_a_bin == TRUE]
cols <- paste0("ameaca_", c(TIPOS, "generica"), "_a")
cita <- JR[, lapply(.SD, function(x) any(x, na.rm = TRUE)), by = SQ_CANDIDATO, .SDcols = cols]
setnames(cita, cols, c(TIPOS, "generica"))
cita <- merge(base[SG_UF %in% ufs_ok, .(SQ_CANDIDATO, uf = SG_UF, NM_URNA_CANDIDATO, SG_PARTIDO, campo, palavras)], cita, by = "SQ_CANDIDATO", all.x = TRUE)
for (v in c(TIPOS, "generica")) cita[is.na(get(v)), (v) := FALSE]

cit_long <- melt(cita, id.vars = c("SQ_CANDIDATO", "uf"), measure.vars = c(TIPOS, "generica"), variable.name = "tipo", value.name = "cita")
n_uf <- base[SG_UF %in% ufs_ok, .(candidatos = .N), by = .(uf = SG_UF)]
t14 <- cit_long[, .(candidatos_citam = sum(cita)), by = .(uf, tipo)]
t14 <- merge(t14, n_uf, by = "uf")[, pct_candidatos := round(100 * candidatos_citam / candidatos, 1)]
fwrite(t14, file.path(DIR_TAB, "t14_citacao_uf_tipo.csv"))

# ---- cobertura da exposicao por candidato e alinhamento por UF -----------------------------
W <- dcast(expo, uf ~ tipo, value.var = "peso")
cob <- merge(cita, W, by = "uf", suffixes = c("", "_w"))
cob[, cobertura := 100 * Reduce(`+`, lapply(TIPOS, function(t) get(t) * get(paste0(t, "_w"))))]
cob[, cita_tipo_especifico := Reduce(`|`, lapply(TIPOS, function(t) get(t)))]
fwrite(cob[, .(SQ_CANDIDATO, uf, candidato = NM_URNA_CANDIDATO, SG_PARTIDO, campo, palavras, cita_tipo_especifico, cobertura = round(cobertura, 1))],
       file.path(DIR_TAB, "cobertura_candidato.csv"))

top <- expo[order(uf, -registros)][, .(tipo_principal = tipo[1], pct_mun_principal = pct_municipios[1], share_principal = share_registros[1],
                                              tipo_2 = tipo[2], pct_mun_2 = pct_municipios[2]), by = uf]
cit_principal <- merge(top, cit_long, by.x = c("uf", "tipo_principal"), by.y = c("uf", "tipo"))[, .(citam_principal = sum(cita), .N), by = .(uf, tipo_principal)]
cit_top2 <- merge(top, cob[, c("uf", "SQ_CANDIDATO", TIPOS), with = FALSE], by = "uf")
cit_top2[, cita_top2 := mapply(function(a, b, i) isTRUE(get(a)[i]) || isTRUE(get(b)[i]), tipo_principal, tipo_2, seq_len(.N))]
cit_top2 <- cit_top2[, .(citam_top2 = sum(cita_top2)), by = uf]
t15 <- merge(merge(top, cit_principal[, .(uf, citam_principal)], by = "uf"), cit_top2, by = "uf")
t15 <- merge(t15, cob[, .(candidatos = .N, pct_cita_algum_tipo = round(100 * mean(cita_tipo_especifico), 1),
                          cobertura_media = round(mean(cobertura), 1), cobertura_mediana = round(median(cobertura), 1)), by = uf], by = "uf")
t15[, `:=`(pct_cita_principal = round(100 * citam_principal / candidatos, 1), pct_cita_top2 = round(100 * citam_top2 / candidatos, 1))]
t15 <- merge(t15, base[, .(regiao = regiao[1]), by = .(uf = SG_UF)], by = "uf")
fwrite(t15[order(-cobertura_media)], file.path(DIR_TAB, "t15_alinhamento_uf.csv"))

cat("\n== Alinhamento por UF (ordenado por cobertura media):\n")
print(t15[order(-cobertura_media), .(uf, candidatos, tipo_principal, share_principal, pct_cita_principal, pct_cita_top2, pct_cita_algum_tipo, cobertura_media)])
cat("\nNacional: cobertura media", round(mean(cob$cobertura), 1), "% | candidatos que citam o tipo principal do seu estado:",
    round(100 * sum(t15$citam_principal) / sum(t15$candidatos), 1), "%\n")
