# 00_setup.R -- configuracao comum a todo o pipeline.
# Uso: source(here::here("R", "00_setup.R")) no inicio de cada script.

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(dplyr)
  library(stringr)
  library(purrr)
})

try(Sys.setlocale("LC_ALL", "Portuguese_Brazil.utf8"), silent = TRUE)
options(encoding = "UTF-8", timeout = 600)

# ---- Caminhos (sempre via here::here, nunca relativos) ----------------------
DIR_RAW   <- here("data", "raw")
DIR_ZIPS  <- here("data", "raw", "zips_propostas")
DIR_PROC  <- here("data", "processed")
DIR_TAB   <- here("outputs", "tables")
DIR_FIG   <- here("outputs", "figures")
DIR_LLM   <- here("outputs", "llm")
for (d in c(DIR_RAW, DIR_ZIPS, DIR_PROC, DIR_TAB, DIR_FIG, DIR_LLM)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- Parametros do estudo ---------------------------------------------------
ANO_ELEICAO      <- 2026
DATA_CONGELAMENTO <- as.Date("2026-09-23")   # data de coleta congelada
CARGO_ALVO       <- "GOVERNADOR"
SITUACAO_ALVO    <- "DEFERIDO"                # candidaturas deferidas
LIMIAR_PLANO_CURTO <- 2000                    # palavras; abaixo disso, alerta

UFS <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT",
         "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

REGIAO_UF <- c(
  AC="Norte", AM="Norte", AP="Norte", PA="Norte", RO="Norte", RR="Norte", TO="Norte",
  AL="Nordeste", BA="Nordeste", CE="Nordeste", MA="Nordeste", PB="Nordeste",
  PE="Nordeste", PI="Nordeste", RN="Nordeste", SE="Nordeste",
  DF="Centro-Oeste", GO="Centro-Oeste", MS="Centro-Oeste", MT="Centro-Oeste",
  ES="Sudeste", MG="Sudeste", RJ="Sudeste", SP="Sudeste",
  PR="Sul", RS="Sul", SC="Sul"
)

# ---- Fontes (TSE, portal de dados abertos) ----------------------------------
URL_CDN <- "https://cdn.tse.jus.br/estatistica/sead/odsele"
URL_CAND <- file.path(URL_CDN, "consulta_cand", "consulta_cand_2026.zip")
url_proposta <- function(uf) {
  file.path(URL_CDN, "proposta_governo", sprintf("proposta_governo_2026_%s.zip", uf))
}

# ---- LLM local (LM Studio, API compativel com OpenAI) -----------------------
LLM_BASE_URL <- "http://localhost:1234/v1"
MODELO_A   <- "openai/gpt-oss-20b"            # classificador 1
MODELO_B   <- "google/gemma-4-26b-a4b-qat"    # classificador 2
MODELO_OCR <- "qwen/qwen3-vl-8b"              # OCR / PDFs escaneados

# ---- Formatadores (mesma convencao do relatorio de educacao) ----------------
fmt_n   <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x, digits = 1) format(round(x, digits), decimal.mark = ",", nsmall = digits)
