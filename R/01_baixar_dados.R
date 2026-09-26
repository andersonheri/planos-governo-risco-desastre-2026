# 01_baixar_dados.R -- baixa o CSV de candidatos e os 27 ZIPs de propostas
# de governo do TSE e grava um manifesto (data, tamanho, sha256) para
# documentar a coleta congelada.
#
# ATENCAO: o CDN do TSE (Akamai) devolve 403 para clientes sem cabecalho de
# navegador. Este script envia um User-Agent de navegador. Se ainda assim
# falhar, baixe os arquivos manualmente no navegador e salve-os em
#   data/raw/consulta_cand_2026.zip
#   data/raw/zips_propostas/proposta_governo_2026_<UF>.zip
# O script pula tudo o que ja existir.

source(here::here("R", "00_setup.R"))
library(httr2)

UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
            "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")

baixar <- function(url, destino) {
  if (file.exists(destino) && file.size(destino) > 0) {
    return(list(status = "ja_existia", http = NA_integer_))
  }
  resp <- tryCatch(
    request(url) |>
      req_user_agent(UA) |>
      req_headers(Accept = "*/*", Referer = "https://dadosabertos.tse.jus.br/") |>
      req_retry(max_tries = 3) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(path = destino),
    error = function(e) NULL
  )
  if (is.null(resp)) return(list(status = "erro_conexao", http = NA_integer_))
  if (resp_status(resp) != 200) {
    unlink(destino)
    return(list(status = "falha_http", http = resp_status(resp)))
  }
  list(status = "baixado", http = 200L)
}

alvos <- rbindlist(list(
  data.table(id = "candidatos", url = URL_CAND,
             destino = file.path(DIR_RAW, "consulta_cand_2026.zip")),
  data.table(id = paste0("proposta_", UFS), url = vapply(UFS, url_proposta, ""),
             destino = file.path(DIR_ZIPS, sprintf("proposta_governo_2026_%s.zip", UFS)))
))

res <- lapply(seq_len(nrow(alvos)), function(i) {
  message(sprintf("[%02d/%02d] %s", i, nrow(alvos), alvos$id[i]))
  r <- baixar(alvos$url[i], alvos$destino[i])
  Sys.sleep(1)  # cortesia com o servidor
  r
})

manifesto <- alvos[, `:=`(
  status = vapply(res, `[[`, "", "status"),
  http   = vapply(res, `[[`, 0L, "http"),
  bytes  = ifelse(file.exists(destino), file.size(destino), NA_real_),
  sha256 = vapply(destino, \(f) if (file.exists(f)) digest::digest(f, "sha256", file = TRUE) else NA_character_, ""),
  data_coleta = DATA_CONGELAMENTO
)]

fwrite(manifesto, file.path(DIR_TAB, "manifesto_coleta.csv"))
print(manifesto[, .N, by = status])
if (any(manifesto$status %in% c("falha_http", "erro_conexao"))) {
  warning("Ha arquivos nao baixados; veja outputs/tables/manifesto_coleta.csv")
}
