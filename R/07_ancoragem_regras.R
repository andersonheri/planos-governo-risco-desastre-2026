# 07_ancoragem_regras.R -- ancoragem das propostas de risco e desastre por
# REGRAS (deterministico, reprodutivel), no lugar do LLM: no piloto os dois
# modelos concordaram pouco (kappa 0,0 a 0,66) em detectar elementos objetivos
# como valor, percentual e ano.
#
# Elementos detectados em cada janela: orcamento (R$, milhoes/bilhoes),
# meta_quantificada (numero + unidade de acao, percentual), prazo (ano 20xx,
# "ate", "em N anos"), indicador (indicador, monitoramento, taxa de),
# orgao (secretaria, defesa civil, cemaden, corpo de bombeiros, fundo, instituto).
#
# Uso: source() e chamar ancoragem_regras(textos); tambem roda standalone
# sobre data/processed/janelas.csv.

source(here::here("R", "00_setup.R"))
source(here::here("R", "dicionario.R"))

REGRAS_ANC <- list(
  orcamento = "r\\$|[0-9][0-9.,]* (milhoes?|bilhoes?|mil) de reais|[0-9][0-9.,]* (milhoes?|bilhoes?)\\b|orcamento (proprio|de|do|estadual)|fundo estadual de",
  meta_quantificada = "\\b[0-9]+([.,][0-9]+)?\\s?%|\\b[0-9]{1,3}([.][0-9]{3})*\\s+(sirenes?|pluviometros?|estacoes|municipios|familias|unidades|obras|barragens|km|hectares|casas|moradias|reservatorios|cisternas|equipes)|percentual|por cento",
  prazo = "\\b20[2-4][0-9]\\b|ate o? ?ano|em [0-9]+ (anos|meses)|primeiro ano|primeiros? [0-9]+ (dias|meses)|curto prazo|medio prazo|longo prazo|ate (o fim|final) do (mandato|governo)",
  indicador = "indicador(es)?|monitorament|taxa de|metas? de|painel|avaliacao (anual|periodica)|linha de base",
  orgao = "secretaria|defesa civil|cemaden|corpo de bombeiros|bombeiros|instituto|agencia|superintendencia|companhia|autarquia|fundacao|conselho|comite|centro (estadual|de monitoramento|de gerenciamento)"
)

ancoragem_regras <- function(textos) {
  n <- normalizar(textos)
  out <- as.data.table(sapply(REGRAS_ANC, function(rx) grepl(rx, n, perl = TRUE)))
  setnames(out, paste0("anc_", names(REGRAS_ANC)))
  out[, n_elementos_anc := rowSums(.SD)]
  out
}

if (sys.nframe() == 0) {
  jan <- fread(file.path(DIR_PROC, "janelas.csv"), colClasses = c(SQ_CANDIDATO = "character"))
  anc <- cbind(jan[, .(janela_id, SQ_CANDIDATO)], ancoragem_regras(jan$frases_foco))  # so a frase focal (evita meta de frase vizinha)
  fwrite(anc, file.path(DIR_PROC, "janelas_ancoragem.csv"))
  cat("Janelas:", nrow(anc), "\n")
  print(round(100 * colMeans(anc[, grep("^anc_", names(anc)), with = FALSE]), 1))
  cat("\nExemplos com orcamento:\n")
  print(head(jan[anc$anc_orcamento == TRUE, .(texto = substr(gsub("\\s+", " ", texto), 1, 160))], 3))
}
