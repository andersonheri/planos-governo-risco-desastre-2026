# dicionario.R -- dicionario de risco e desastre (etapa 0 do codebook).
# Aplicado a texto em minusculas e sem acentos. Alta cobertura de proposito:
# o LLM decide a relevancia depois. Usado por 04_trechos_dicionario.R e
# 04b_janelas.R.

normalizar <- function(x) tolower(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Termos FORTES: um so ja leva o trecho ao LLM.
DICIONARIO <- list(
  desastre       = "desastre|calamidade|situacao de emergencia|estado de emergencia|catastrofe",
  defesa_civil   = "defesa civil|cemaden|s2id|cenad",
  hidro          = "enchente|inundac|alagament|enxurrada|\\bcheias?\\b|transbordament",
  movimento_massa= "deslizament|movimento de massa|encosta|desmoronament",
  seca           = "estiagem|\\bsecas?\\b|crise hidrica|escassez hidrica",
  fogo           = "queimada|incendio florestal|incendios florestais|fogo florestal",
  calor_tempest  = "onda de calor|ondas de calor|calor extremo|tempestade|vendaval|granizo|ciclone",
  costeira       = "erosao costeira|elevacao do nivel do mar",
  barragem       = "rompimento de barragem|rompimento de barragens|rompimento da barragem",
  risco          = "area de risco|areas de risco|gestao de risco|gestao de riscos|reducao de risco|mapeamento de risco|risco de desastre|riscos de desastre|risco climatico|riscos climaticos",
  clima          = "mudanca climatica|mudancas climaticas|crise climatica|emergencia climatica|evento extremo|eventos extremos|eventos climaticos|adaptacao climatica|resiliencia climatica",
  alerta_prep    = "sirene|sistema de alerta|alerta precoce|alerta antecipado|plano de contingencia|plano de emergencia|abrigo temporario"
)
REGEX_DIC <- setNames(sprintf("(%s)", DICIONARIO), names(DICIONARIO))

# Termos FRACOS: ambiguos (drenagem, barragem de agua, resiliencia...). Sozinhos
# nao levam o trecho ao LLM; dois ou mais termos fracos distintos levam.
FRACOS <- c("vulnerabilidade", "resiliencia", "drenagem", "galeria pluvial", "erosao",
            "barragem", "barragens", "rejeito", "rompimento", "orla maritima",
            "nivel do mar", "incendios", "desertificac", "contingencia", "alerta",
            "calamitos", "risco social", "riscos ambientais", "risco ambiental")
REGEX_FRACO <- paste(FRACOS, collapse = "|")

# ---- ancoragem por regras (antigo 07_ancoragem_regras.R) ------------------------------------------------
# Elementos detectados na frase focal: orcamento, meta_quantificada, prazo, indicador e orgao.
# No piloto os dois modelos concordaram pouco em detecta-los; regras sao deterministicas e reprodutiveis.
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

