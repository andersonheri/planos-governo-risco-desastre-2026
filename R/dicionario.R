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
