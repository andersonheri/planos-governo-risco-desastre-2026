# Codebook v0.9: risco e desastre nos planos de governo (governadores, 2026)

**Alterações desde a v0.8** (revisão externa de Cristiane Capuchinho, CEM, e checagens adicionais em dados; a base já classificada não foi reprocessada com estas mudanças, ver limitações no relatório):

1. **Relevância** — a regra de decisão qualifica "gestão de riscos" como "gestão de riscos *de desastres*", para não colidir com o sentido de gestão de riscos fiscais, de corrupção ou corporativos, comum nos planos (item C10/A1 da revisão).
2. **Fase do ciclo** — as definições de *resposta* e *recuperação e reconstrução* trazem agora uma regra explícita para benefício habitacional: moradia temporária durante a emergência conta como resposta; aluguel social com horizonte de transição, moradia definitiva e reconstrução contam como recuperação, mesmo quando o benefício começa ainda na emergência (item C07).
3. **Tipo de ameaça** — a categoria "Erosão costeira e mar" foi renomeada para "Erosão costeira e fluvial" e sua definição ampliada para cobrir erosão de margem de rio, depois de identificarmos um caso de erosão fluvial (margem do rio Acre) classificado nela; uma varredura mostrou também ruído do termo genérico "erosão" nessa categoria, registrado como limitação no relatório (item C14).
4. Dois exemplos de tipo de ameaça foram trocados por não serem discriminantes (traziam mais de um tipo de ameaça na mesma passagem): "Deslizamento e erosão de encosta" e "Calor extremo" (item A5).

**Unidade de análise:** *janela* (frase que contém um termo do dicionário, mais a frase anterior e a seguinte). A frase que contém o termo é a *frase focal* e vem marcada com » « no texto enviado aos modelos.
**Definição-base:** risco e desastre são eventos naturais, climáticos ou tecnológicos, ou a probabilidade de ocorrerem, que causam danos a pessoas, bens ou ao ambiente (conceito da Defesa Civil, Lei 12.608/2012). Ficam de fora mitigação de emissões pura, emergências de saúde pública, violência e segurança pública, risco fiscal e "meio ambiente" genérico sem ligação a risco.
**Recorte:** candidatos a governador com candidatura deferida e plano registrado. Coleta congelada em 23/09/2026.
**Classificadores:** `gpt-oss-20b` (todas as janelas) e `gemma-4-26b` (relevância em todas; demais dimensões em amostra aleatória de 30% das relevantes), executados localmente pelo pacote `acR` (`ac_qual_codebook()` e `ac_qual_code()`).

## 1. Relevância (uma categoria)

Regra de decisão, em ordem: (1) qualquer menção a risco climático, evento extremo, adaptação climática, vulnerabilidade a eventos climáticos, Defesa Civil, desastre, calamidade ou emergência de origem natural, enchente, inundação, alagamento, cheia, seca, estiagem, deslizamento, queimada, incêndio florestal, área de risco, risco de rompimento de barragem, **gestão de riscos de desastres** (não conta gestão de riscos em outro sentido, como risco de corrupção ou integridade), alerta, sirene ou contingência torna a janela *relevante*, mesmo que breve, em lista ou genérica; (2) só se nenhuma dessas ideias aparecer e o trecho falar de mudança ou emergência climática, transição ecológica, economia verde ou resiliência climática de forma geral, é *agenda climática geral*; (3) caso contrário, *não relevante*.

| Categoria | Definição | Exemplo real |
|---|---|---|
| Relevante | Menção explícita, ainda que breve, a desastre, risco ou evento extremo (enchente, deslizamento, seca, queimada, rompimento de barragem), à Defesa Civil no contexto de risco, a ações de prevenção, preparação, resposta ou recuperação, ou à adaptação climática explícita. A profundidade é medida à parte, na especificidade. | "Nosso propósito: fortalecer São Paulo diante dos eventos climáticos extremos, promovendo resiliência às secas, enchentes e incêndios, com segurança hídrica, rios revitalizados, mais áreas verdes [...]" (Tarcísio (Republicanos-SP), p. 42) |
| Agenda climática geral | Menciona mudança ou emergência climática, transição ecológica, economia verde ou resiliência climática de forma geral, sem referir evento extremo, desastre, risco ou vulnerabilidade e sem adaptação explícita. | "Revisar e atualizar o Plano Estadual de Mudanças Climáticas e a Política Estadual de Enfrentamento das Mudanças Climáticas, alinhando-os ao Plano Nacional sobre Mudança do Clima e estabelecendo prioridades para sua execução." (Eduardo Braide (PSD-MA), p. 25) |
| Não relevante | Usa o termo de risco em outro sentido (fiscal, social, de corrupção), trata de emergência de saúde pública ou de meio ambiente sem ligação com evento perigoso nem com a agenda climática. | "A prevenção da corrupção, a gestão de riscos, a prestação de contas e o acesso público à informação serão deveres permanentes." (Thor Dantas (PSB-AC), p. 6) |

## 2. Fase do ciclo de gestão de risco (várias categorias)

Marcam-se todas as fases sustentadas pelo texto, sem inventar fases ausentes. Prevenção e preparação formam uma só categoria. A adaptação só vale com menção explícita à mudança do clima ou à vulnerabilidade a eventos climáticos. **Benefício habitacional:** moradia temporária durante a emergência conta como resposta; aluguel social com horizonte de transição para solução definitiva, moradia definitiva e reconstrução contam como recuperação, mesmo quando o benefício começa ainda na emergência.

| Categoria | Definição | Exemplo real |
|---|---|---|
| Prevenção e preparação | Tudo o que se faz antes do desastre. Prevenção reúne obras de drenagem e contenção, remoção de famílias de áreas de risco e zoneamento. Preparação reúne alertas e sirenes, planos de contingência, mapeamento, monitoramento, treinamento e fortalecimento da Defesa Civil. | "Padronizar e revisar periodicamente os planos estaduais, regionais e municipais, definindo responsabilidades, níveis de acionamento, rotas de evacuação, áreas de abrigo [...] com exercícios simulados regulares." (Zucco (PL-RS), p. 14) |
| Resposta | Ação durante ou logo após o desastre, como resgate, abrigos, ajuda humanitária, mobilização de bombeiros e assistência imediata às vítimas. Moradia temporária durante a emergência conta como resposta; aluguel social com horizonte de transição conta como recuperação. | "Resposta rápida e humanizada a famílias atingidas por secas, enchentes e desastres, com auxílio-moradia e ajuda humanitária;" (Alan Rick (Republicanos-AC), p. 27) |
| Recuperação e reconstrução | Volta à normalidade depois do desastre, como reconstrução de casas e infraestrutura, moradia definitiva, auxílio econômico de médio prazo e retomada da atividade das vítimas. Aluguel social com horizonte de transição conta como recuperação, mesmo quando começa na emergência. | "Instrumentos de fomento como o Fundopem-RS viabilizaram mais de R$ 6 bilhões em investimentos produtivos e contribuíram para a geração e preservação de milhares de empregos, inclusive durante o processo de reconstrução pós-enchentes." (Gabriel Souza (MDB-RS), p. 40) |
| Adaptação climática | Ações que citam a mudança do clima ou os eventos extremos como motivo para reduzir a vulnerabilidade, como planos de adaptação e infraestrutura resiliente. Corte de emissões e agenda verde genérica não contam. | "O Plano Estadual de Adaptação Climática será construído com os territórios e incorporado a todas as secretarias." (Isael Munduruku (Rede-AM), p. 18) |
| Nenhuma fase identificável | O trecho apenas cita o tema, sem descrever ação ou fase do ciclo. Nota: esta categoria e "Tempestades e vendavais" (tipo de ameaça) compartilham o mesmo exemplo real por propósito — são dimensões independentes, e a mesma passagem não descreve nenhuma fase, mas cita um tipo de ameaça específico. | "Tornados, vendavais e tempestades se tornaram frequentes." (Rejane de Oliveira (PSTU-RS), p. 30) |

## 3. Tipo de ameaça (várias categorias)

Só vale a citação explícita. "Desastres em geral" só se marca quando nenhum tipo específico é citado, e mudança climática sem evento não é ameaça.

| Categoria | Definição | Exemplo real |
|---|---|---|
| Enchente e alagamento | Chuvas intensas, cheias de rios, alagamentos urbanos e inundações. | "Ampliar os investimentos em drenagem urbana, prevenção de alagamentos e manejo das águas pluviais, priorizando as áreas mais vulneráveis." (Alan Rick (Republicanos-AC), p. 15) |
| Deslizamento e erosão de encosta | Deslizamentos, escorregamentos, queda de barreiras e movimentos de massa em encostas. | "Na prevenção de desastres, executamos em Jardim Monte Verde a maior obra de desenvolvimento urbano e proteção de encostas do Brasil, investindo mais de R$ 80 milhões para garantir a segurança das famílias e mitigar o risco de deslizamentos estruturais." (Raquel Lyra (PSD-PE), p. 89) |
| Seca e estiagem | Estiagem, seca prolongada, crise hídrica e escassez de água por eventos climáticos. | "Criar um plano estadual permanente de segurança hídrica e enfrentamento às secas, com reservação, irrigação, proteção de nascentes, uso eficiente da água e planejamento por bacia." (Marcelo Maranata (PSDB-RS), p. 11) |
| Queimadas e incêndios florestais | Queimadas e incêndios florestais ou em vegetação. | "Treinar e equipar brigadas municipais de combate a incêndios florestais, incluindo o fornecimento de kits de EPIs, veículos e equipamentos, para todos os municípios." (Rafael Fonteles (PT-PI), p. 58) |
| Calor extremo | Ondas de calor e temperaturas extremas. | "Criar espaços públicos de proteção térmica, hidratação, higiene pessoal, guarda de pertences e atendimento social, especialmente nos períodos de calor intenso e eventos climáticos extremos." (Ivan Moraes (PSOL-PE), p. 43) |
| Tempestades e vendavais | Tempestades, vendavais, granizo, ciclones e tornados. | "Tornados, vendavais e tempestades se tornaram frequentes." (Rejane de Oliveira (PSTU-RS), p. 30) |
| Erosão costeira e fluvial | Erosão da costa e de margens de rios, elevação do nível do mar, ressacas e obras de contenção e estabilização de margens. | "Adaptação costeira ao novo clima: monitoramento da erosão, da elevação do nível do mar, das ressacas, das inundações e das áreas de risco, com obras resilientes e soluções baseadas na natureza;" (Sandro Alex (PSD-PR), p. 184) |
| Barragens e mineração | Rompimento ou risco de barragens de rejeitos e desastres associados à mineração. | "A garantia do direito a Assessoria Técnica Independente para as comunidades que vivem com barragens de rejeitos da mineração;" (Indira Xavier (UP-MG), p. 9) |
| Acidentes tecnológicos | Acidentes químicos, radiológicos e industriais, vazamentos e produtos perigosos. | "[...] resposta a acidentes com produtos perigosos e grandes desastres, mediante investimentos em aeronaves, ambulâncias de resgate, equipamentos especializados [...]" (Marcos Rogério (PL-RO), p. 55) |
| Desastres em geral (sem tipo) | Fala de desastres, calamidades ou eventos extremos sem nomear o tipo. Só se marca quando nenhum tipo específico é citado. | "Quem é atingido por um desastre precisa encontrar um Estado preparado antes, durante e depois da emergência." (Alexandre Kalil (PDT-MG), p. 65) |

## 4. Especificidade (uma nota, de 0 a 4)

Julga-se apenas o que a frase focal propõe ou afirma sobre risco e desastre. Ações, metas, prazos e valores de outros temas são ignorados, e meta, prazo, indicador ou orçamento só contam para o nível 4 se disserem respeito à própria ação de risco. Na dúvida entre dois níveis, escolhe-se o menor. As passagens de nível 4 foram lidas uma a uma (`config/revisao_manual.csv`).

| Categoria | Definição | Exemplo real |
|---|---|---|
| 0. Menção genérica | Cita o tema sem dizer nada concreto, como um título, um slogan ou uma intenção vaga. | "Corpo de Bombeiros, Defesa Civil e proteção diante de desastres" (Alexandre Kalil (PDT-MG), p. 4) |
| 1. Diagnóstico | Descreve o problema (dados, histórico, situação) sem propor solução. | "[...] e a resposta a riscos climáticos é fragmentada entre órgãos que não compartilham dados." (André Luis (Missão-MA), p. 16) |
| 2. Diretriz ou compromisso | Propõe um compromisso, um critério de priorização ou uma direção de política sem ação identificável. Inclui listas genéricas de investimento e a simples citação de um órgão. | "Municípios e comunidades precisarão estar mais preparados para eventos climáticos extremos." (Marconi Perillo (PSDB-GO), p. 40) |
| 3. Ação concreta | Descreve ação, programa, obra ou instrumento identificável, cujo objeto é prevenir, preparar, responder, recuperar ou adaptar-se a risco e desastre. | "Treinar e equipar brigadas municipais de combate a incêndios florestais, incluindo o fornecimento de kits de EPIs, veículos e equipamentos, para todos os municípios." (Rafael Fonteles (PT-PI), p. 58) |
| 4. Ação com meta, prazo, indicador ou orçamento | Ação concreta acompanhada de meta quantificada, prazo, indicador ou orçamento que se referem à própria ação de risco e desastre. Meta ou valor de outro tema não conta. | "Investimento: R$ 300-520 milhões em quatro anos para fiscalização, defesa civil, sistemas, treinamento e infraestrutura pública;" (Ben Mendes (Missão-MG), p. 70) |

## 5. Ancoragem (detecção por regras textuais na frase focal, sem modelo)

As expressões regulares estão em `R/dicionario.R` (`REGRAS_ANC`). Não há validação (precisão/revocação) dessas regras contra classificação por modelo ou leitura manual; ver limitações no relatório.

| Categoria | Definição | Exemplo real |
|---|---|---|
| Órgão responsável | Nomeia secretaria, órgão ou instituição (Defesa Civil, Corpo de Bombeiros, Cemaden, secretaria, fundo, conselho). | "A Defesa Civil será fortalecida como estrutura técnica, profissional e integrada, capaz de atuar continuamente no monitoramento, na prevenção, na preparação, na resposta e na reconstrução." (Zucco (PL-RS), p. 13) |
| Orçamento | Cita valor em reais, milhões ou bilhões, ou fundo estadual. | "Investimento: R$ 300-520 milhões em quatro anos para fiscalização, defesa civil, sistemas, treinamento e infraestrutura pública;" (Ben Mendes (Missão-MG), p. 70) |
| Meta quantificada | Cita percentual ou quantidade de unidades a alcançar. | "Garantir que 100% dos municípios prioritários (aqueles com maior recorrência de desastres nos últimos 10 anos) possuam plano de contingência atualizado [...]" (Rafael Fonteles (PT-PI), p. 60) |
| Prazo | Cita ano, período ou horizonte de tempo para a ação. | "Vamos investir mais de R$ 25 bilhões, até o final de 2029, em ações de adaptação e resiliência climática [...]" (Tarcísio (Republicanos-SP), p. 42) |
| Indicador ou monitoramento | Cita indicador, monitoramento ou avaliação da ação. | "Meta / indicador (KPI): Área queimada/ano;" (Renato Gomes (DC-MS), p. 49) |

## Indicadores derivados por candidato

1. **Presença** do tema (ao menos uma janela relevante). 2. **Densidade** (janelas relevantes por mil palavras). 3. **Ciclo completo** (prevenção e preparação, resposta e recuperação). 4. **Tipos de ameaça** citados. 5. **Perfil em cinco degraus**, com "ação concreta" definida como metade ou mais das passagens com nível 3 ou 4 (instável para candidatos com uma ou duas passagens relevantes). 6. **Ancoragem** (órgão, orçamento, meta, prazo, indicador).

Todo indicador é apresentado junto do tamanho do plano; planos com menos de 2.000 palavras recebem alerta. O limiar de 2.000 palavras é uma convenção testada por sensibilidade em 1.000 e 3.000 palavras (ver relatório).
