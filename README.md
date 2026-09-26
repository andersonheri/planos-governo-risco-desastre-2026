# Risco e desastre nos planos de governo dos candidatos a governador (2026)

Relatório descritivo e público sobre **o que os candidatos a governador dizem, e deixam de dizer, sobre risco e desastre** nos planos de governo registrados no TSE para as eleições de 2026. A leitura dos planos combina um **dicionário de termos** com **modelos de linguagem locais** e regras textuais, por meio do pacote [**acR**](https://github.com/andersonheri/acR).

**Autor:** Anderson Henrique (CEM/USP, com apoio FAPESP) · andersonheri@gmail.com
**Coleta congelada em:** 23/09/2026 (o TSE atualiza os arquivos várias vezes ao dia)

---

## O que o relatório responde

| Pergunta | Onde |
|---|---|
| Quantos candidatos abordam o tema e com que concretude? | Seções 3 a 5 |
| Quais fases do ciclo e quais tipos de desastre aparecem? | Seção 4 |
| Falar de clima é falar de desastre? | Seção 6 |
| **Quem fala e como fala?** (casos, trechos, balanço x proposta x crítica) | Seção 7 |
| Muda por região, estado, partido e campo político? | Seções 8 e 10 |
| O plano fala do desastre que o estado de fato sofre? | Seção 9 |
| Os modelos são confiáveis? | Seção 12 |

## Principais resultados (universo: 172 candidatos deferidos, com plano)

- **74,4%** (128) mencionam risco ou desastre; **41** não têm nenhuma menção.
- **34,9%** (60) têm ação concreta em metade ou mais das passagens sobre o tema; só **2,3%** (4) associam a ação a meta, prazo, indicador ou orçamento.
- Os planos **antecipam mais do que respondem**: prevenção e preparação em 71,5% dos candidatos, resposta em 34,9% e recuperação em 24,4%.
- **Descompasso territorial:** no Piauí, a seca é 92% dos registros de desastre, mas só 17% dos candidatos a citam. Nenhuma relação entre a intensidade de desastres do estado e a concretude dos planos (Spearman = 0,05).
- **Confiabilidade entre os dois modelos:** boa para tipos de ameaça, aceitável para relevância (alfa de Krippendorff = 0,78) e **fraca para especificidade (0,58)**, que deve ser lida como estimativa aproximada.

![Perfil dos candidatos](outputs/figures/01_perfil_nacional.png)

![Mapa do descompasso](outputs/figures/15_mapa_descompasso.png)

## Método em uma página

1. **Coleta** (TSE, dados abertos): cadastro, situação da candidatura e planos em PDF; universo = deferidos com plano. PDFs escaneados passam por OCR (Tesseract).
2. **Janelas:** o dicionário seleciona frases com termos de risco e desastre; cada uma, com a anterior e a seguinte, forma uma *janela*. A frase com o termo é a *frase focal*.
3. **Codebook (5 dimensões):** relevância, fase do ciclo, tipo de ameaça, especificidade (0 a 4) e ancoragem (por regras).
4. **Classificação:** `gpt-oss-20b` (todas as janelas) e `gemma-4-26b` (relevância em todas; demais dimensões em 30% das relevantes), rodando localmente no LM Studio.
5. **Perfil do candidato** em cinco degraus. "Ação concreta" = metade ou mais das passagens com nível ≥ 3.
6. **Revisão manual** das 14 passagens de nível 4 (`config/revisao_manual.csv`): 8 mantidas, 6 rebaixadas.
7. **Confiabilidade:** concordância entre modelos (acordo, kappa, alfa de Krippendorff, AC1 de Gwet).
8. **Cruzamentos:** região, UF, partido e campo (Bolognesi, Ribeiro e Codato, 2023), incumbência e exposição a desastres (Atlas Digital de Desastres, MIDR/S2iD, 2013 a 2025).

## Estrutura do repositório

```
R/                    scripts numerados, na ordem de execução (00 a 14)
config/               classificação de partidos, revisão manual, exemplos do codebook e casos citados
relatorios/           relatorio_final.qmd (Quarto), estilo.css, renderizar.bat
outputs/tables/       tabelas e rótulos consolidados (base_analitica.csv, t01..t20)
outputs/figures/      figuras usadas no relatório (01..16)
data/processed/       arquivos pequenos necessários para renderizar o relatório
```

| Script | Função |
|---|---|
| `00_setup.R` | caminhos, parâmetros, formatadores |
| `01`–`03b` | download, base de candidatos, extração de texto e OCR |
| `04`, `04b` | dicionário e janelas |
| `05_codebooks.R` | codebooks do acR (versão v0.8) |
| `06`, `07` | classificação pelos modelos e ancoragem por regras |
| `08`, `09` | consolidação e análises |
| `10`–`14` | figuras, exposição a desastres, "quem fala" e mapas |

## Como reproduzir

Requisitos: R 4.6, [Quarto](https://quarto.org), LaTeX (para o PDF) e, só para refazer a classificação, o [LM Studio](https://lmstudio.ai) com os dois modelos carregados.

```r
# pacotes principais
install.packages(c("here", "data.table", "dplyr", "stringr", "purrr", "ggplot2",
                   "patchwork", "geobr", "sf", "irr", "knitr", "rmarkdown"))
remotes::install_github("andersonheri/acR")
```

- **Só o relatório** (usa os arquivos já incluídos em `outputs/` e `data/processed/`):
  `quarto render relatorios/relatorio_final.qmd` (ou `relatorios/renderizar.bat`, no Windows).
- **Pipeline completo:** rode `R/00` a `R/14` em ordem. Os dados brutos (TSE e Atlas Digital de Desastres) são baixados por `R/01_baixar_dados.R` e não estão no repositório; cada arquivo tem `sha256` registrado no manifesto da coleta.

## Limitações que importam

- A classificação **não foi validada por codificação humana**; mede-se a consistência entre modelos, não o acerto.
- O codebook mede o que a frase diz sobre risco e desastre, mas **não distingue proposta, balanço e crítica**; isso pesa nos governadores em exercício.
- Ausência de menção não significa omissão deliberada, e o tamanho do plano (de centenas a dezenas de milhares de palavras) explica boa parte da presença do tema.
- Poucos candidatos por UF (4 a 10) e por partido: leia os percentuais como retrato, não como tendência.

## Uso de inteligência artificial

Dois modelos de código aberto (`gpt-oss-20b`, `gemma-4-26b`) foram usados como **instrumento de medida**, localmente. O Claude (Anthropic) apoiou a programação, a montagem do documento e versões preliminares de texto. Formulação da pergunta, codebook, revisão manual, escolha dos casos e interpretação são do autor. A declaração completa está no relatório.

## Como citar

HENRIQUE, A. *Risco e desastre nos planos de governo dos candidatos a governador (2026)*. Relatório descritivo. 2026. Repositório: <https://github.com/andersonheri/planos-governo-risco-desastre-2026>.
