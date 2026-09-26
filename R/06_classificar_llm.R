# 06_classificar_llm.R -- classifica trechos com ac_qual_code() (acR) usando
# um modelo local do LM Studio. Retomavel: grava um RDS por lote e pula o que
# ja foi feito.
#
# Variaveis de ambiente:
#   MODELO_ID  id do modelo no LM Studio (padrao: MODELO_A)
#   ESCOPO     "piloto" (amostra estratificada) ou "completo"
#   K          rodadas de autoconsistencia (padrao 1)
#   N_PILOTO   trechos por UF no piloto (padrao 15)
#
# Fluxo: relevancia em todos os trechos selecionados; fase, ameaca,
# especificidade e ancoragem so nos trechos que o modelo marcou relevantes.
# Saidas: outputs/llm/<escopo>__<modelo>__<dimensao>/lote_XXX.rds
#         outputs/llm/<escopo>__<modelo>__<dimensao>.csv

source(here::here("R", "00_setup.R"))
suppressPackageStartupMessages({library(acR); library(ellmer)})

MODELO_ID <- Sys.getenv("MODELO_ID", unset = MODELO_A)
ESCOPO    <- Sys.getenv("ESCOPO",    unset = "piloto")
K         <- as.integer(Sys.getenv("K", unset = "1"))
N_PILOTO  <- as.integer(Sys.getenv("N_PILOTO", unset = "15"))
UFS_PILOTO <- c("RS", "PE", "AM", "MG")
TAM_LOTE  <- 10
MIN_PALAVRAS_CLASSIFICAR <- 12

tag_modelo <- gsub("[^A-Za-z0-9]+", "-", MODELO_ID)

# Falha rapida se o LM Studio travar: limite de 150 s por requisicao (o padrao do
# ellmer e 300 s com 3 tentativas, o que esconde um servidor pendurado por ~15 min).
options(ellmer_timeout_s = 150)
falhas_seguidas <- 0L

chat <- chat_openai_compatible(
  base_url = LLM_BASE_URL, model = MODELO_ID,
  credentials = function() "lm-studio",
  params = params(temperature = 0)
)

trechos <- fread(file.path(DIR_PROC, Sys.getenv("FONTE", unset = "janelas.csv")), colClasses = c(SQ_CANDIDATO = "character"))
meta    <- fread(file.path(DIR_PROC, "planos_meta.csv"), colClasses = c(SQ_CANDIDATO = "character"))
trechos <- merge(trechos, meta[, .(SQ_CANDIDATO, SG_UF)], by = "SQ_CANDIDATO")
sel <- trechos[hit_dic == TRUE & palavras >= MIN_PALAVRAS_CLASSIFICAR]
if (startsWith(ESCOPO, "piloto")) {
  set.seed(2026)  # mesma amostra para todos os modelos
  sel <- sel[SG_UF %in% UFS_PILOTO][, .SD[sample(.N, min(.N, N_PILOTO))], by = SG_UF]
}
cat(sprintf("Modelo: %s | escopo: %s | k=%d | trechos: %d\n", MODELO_ID, ESCOPO, K, nrow(sel)))

classificar_dim <- function(dim, dt) {
  dir_out <- file.path(DIR_LLM, sprintf("%s__%s__%s", ESCOPO, tag_modelo, dim))
  dir.create(dir_out, showWarnings = FALSE)
  cb <- readRDS(file.path(DIR_PROC, "codebooks", paste0(dim, ".rds")))
  lotes <- split(dt, ceiling(seq_len(nrow(dt)) / TAM_LOTE))
  for (i in seq_along(lotes)) {
    f <- file.path(dir_out, sprintf("lote_%03d.rds", i))
    if (file.exists(f)) next
    b <- lotes[[i]]
    co <- ac_corpus(data.frame(doc_id = b$trecho_id, text = if (dim == "especificidade" && "texto_foco" %in% names(b)) b$texto_foco else b$texto), text = text, docid = doc_id)
    t0 <- Sys.time()
    r <- NULL
    for (tentativa in 1:2) {
      r <- tryCatch(
        ac_qual_code(co, cb, chat = chat, k_consistency = K,
                     confidence = if (K > 1) "total" else "none",
                     temperature = 0, reasoning = TRUE, reasoning_length = "short", live = "off"),
        error = function(e) { message("ERRO lote ", i, " (tentativa ", tentativa, "): ", conditionMessage(e)); NULL })
      if (!is.null(r)) break
      Sys.sleep(20)
    }
    if (is.null(r)) {
      falhas_seguidas <<- falhas_seguidas + 1L
      if (falhas_seguidas >= 3L) stop("LM Studio nao responde: 3 lotes seguidos falharam. Reinicie o modelo e rode de novo (retoma do ultimo lote gravado).")
      next
    }
    falhas_seguidas <<- 0L
    saveRDS(r, f)
    message(sprintf("[%s/%s] lote %d/%d (%.0f s)", dim, tag_modelo, i, length(lotes),
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  arqs <- list.files(dir_out, pattern = "[.]rds$", full.names = TRUE)
  res <- rbindlist(lapply(arqs, function(a) as.data.table(readRDS(a))), fill = TRUE)
  fwrite(res, paste0(dir_out, ".csv"))
  res
}

rel <- classificar_dim("relevancia", sel)
ids_rel <- rel[categoria == "relevante", doc_id]
cat(sprintf("\nRelevantes: %d de %d (%.0f%%)\n", length(ids_rel), nrow(rel), 100 * length(ids_rel) / nrow(rel)))
print(table(rel$categoria))
if (Sys.getenv("SO_RELEVANCIA") == "1") { cat("Parado apos a relevancia (SO_RELEVANCIA=1).
"); quit(save = "no") }
# Modo "enxuto" do segundo modelo (gemma) na execucao completa: a relevancia roda em
# todas as janelas; fase, ameaca e especificidade rodam so numa amostra aleatoria
# (30%, semente fixa) das janelas que o modelo principal (gpt-oss) marcou como
# relevantes, para medir a concordancia nas mesmas janelas. GEMMA_COMPLETO=1 desliga.
MODO_ENXUTO <- grepl("gemma", MODELO_ID) && ESCOPO == "completo" && Sys.getenv("GEMMA_COMPLETO") != "1"
if (MODO_ENXUTO) {
  dir_ref <- file.path(DIR_LLM, sprintf("completo__%s__relevancia", gsub("[^A-Za-z0-9]+", "-", MODELO_A)))
  ref <- rbindlist(lapply(list.files(dir_ref, pattern = "[.]rds$", full.names = TRUE), function(a) as.data.table(readRDS(a))), fill = TRUE)
  stopifnot(nrow(ref) > 0)
  ids_ref <- ref[categoria == "relevante", doc_id]
  set.seed(2026)
  ids_amostra <- sample(ids_ref, round(0.30 * length(ids_ref)))
  cat(sprintf("Modo enxuto: %d janelas relevantes para o gpt-oss; amostra de %d para as demais dimensoes.\n", length(ids_ref), length(ids_amostra)))
  sel_rel <- sel[trecho_id %in% ids_amostra]
} else {
  sel_rel <- sel[trecho_id %in% ids_rel]
}
for (d in c("fase", "ameaca", "especificidade")) classificar_dim(d, sel_rel)  # ancoragem: regras (07), fora do LLM
cat("\nConcluido.\n")
