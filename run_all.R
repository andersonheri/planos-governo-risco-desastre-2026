# run_all.R -- roda o pipeline completo, na ordem. Abrir Candidatos_gov.Rproj no RStudio antes.
#
# Por padrao roda so as etapas leves, a partir dos arquivos ja versionados em outputs/ e data/processed/.
# As etapas pesadas ficam desligadas e sao ligadas por variaveis de ambiente:
#   RODAR_COLETA=1    roda 01 (coleta, texto e OCR) e 02 (janelas e codebook); precisa de data/raw e data/texto
#   RODAR_DOWNLOAD=1  dentro da coleta, baixa ~400 MB do TSE e do Atlas (pedir aval antes)
#   RODAR_LLM=1       roda 03 (classificacao); exige o LM Studio em localhost:1234 com os dois modelos; leva horas
#   RENDERIZAR=1      no fim, gera HTML e PDF do relatorio com o Quarto
# Ex.: Sys.setenv(RODAR_COLETA = "1"); source("run_all.R")

usa <- function(v) identical(Sys.getenv(v), "1")

passos <- c(
  "00_setup.R",
  if (usa("RODAR_COLETA")) c("01_coleta_e_texto.R", "02_janelas_e_codebook.R"),
  if (usa("RODAR_LLM")) "03_classificar_llm.R",
  "04_consolidar_analises.R",   # rotulos, concordancia entre modelos, base analitica e tabelas
  "05_exposicao.R",             # exposicao a desastres (Atlas/S2iD) x tipos citados
  "06_quem_fala.R",             # destaques, verbos no passado, anexo dos candidatos
  "07_figuras.R")                # todas as figuras (o mapa por UF exige MAPA=1)

for (p in passos) {
  cat("\n=== ", p, " ===\n", sep = "")
  source(here::here("R", p), echo = FALSE)
}

if (usa("RENDERIZAR")) system2("quarto", c("render", shQuote(here::here("relatorios", "relatorio_final.qmd"))))
cat("\nPipeline concluido.\n")
