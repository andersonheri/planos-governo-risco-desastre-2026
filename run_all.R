# run_all.R -- roda o pipeline completo, na ordem. Abrir Candidatos_gov.Rproj no RStudio antes.
#
# Os passos 01 (download) e 06 (classificacao por LLM) sao pesados e ficam desligados por padrao:
#   - 01 baixa ~400 MB do TSE e do Atlas Digital de Desastres (pedir aval antes de ligar);
#   - 06 exige o LM Studio aberto em localhost:1234 com os dois modelos (gpt-oss-20b e gemma-4-26b)
#     e leva horas. As rotulagens ja consolidadas estao em outputs/tables/ (rotulos_*.csv).
# Para ligar: RODAR_DOWNLOAD <- TRUE / RODAR_LLM <- TRUE, ou defina as variaveis de ambiente.

RODAR_DOWNLOAD <- identical(Sys.getenv("RODAR_DOWNLOAD"), "1")
RODAR_LLM      <- identical(Sys.getenv("RODAR_LLM"), "1")
RENDERIZAR     <- identical(Sys.getenv("RENDERIZAR"), "1")   # gera HTML e PDF do relatorio (Quarto)

passos <- c(
  "00_setup.R", if (RODAR_DOWNLOAD) "01_baixar_dados.R",
  if (RODAR_DOWNLOAD) c("02_base_candidatos.R", "03_extrair_texto.R", "03b_ocr_escaneados.R", "04_trechos_dicionario.R", "04b_janelas.R"),
  "05_codebooks.R", if (RODAR_LLM) "06_classificar_llm.R",
  "07_ancoragem_regras.R", "08_consolidar.R", "09_analises.R", "10_figuras.R",
  "11_exposicao.R", "12_figuras_exposicao.R", "13_quem_fala.R", "14_figuras_mapas.R")

for (p in passos) {
  cat("\n=== ", p, " ===\n", sep = "")
  source(here::here("R", p), echo = FALSE)
}

if (RENDERIZAR) system2("quarto", c("render", shQuote(here::here("relatorios", "relatorio_final.qmd"))))
cat("\nPipeline concluido.\n")
