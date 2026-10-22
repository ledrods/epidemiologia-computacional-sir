# Chikungunya no Recife: do SIR ao modelo com vetor

Trabalho da disciplina Elementos de Epidemiologia Computacional, BSI UFRPE. Começou como a atividade do
Módulo I, uma simulação do modelo SIR clássico para a introdução do vírus chikungunya no Recife, e virou
a base de uma proposta de projeto de pesquisa com um modelo SEIR-SEI, que tem períodos de latência no
humano e no mosquito e o *Aedes aegypti* como população explícita. Tudo é implementado em R com
`deSolve`, seguindo a estrutura dos cadernos do curso.

## Resultados principais

| Grandeza | SIR | SEIR-SEI |
| --- | --- | --- |
| Número de reprodução de humano para humano | 3,4 | 3,4 |
| Crescimento inicial | 0,40 por dia | 0,053 por dia |
| Dia do pico | 38 | 271 |
| Infectados no pico | 515.090 | 115.189 |
| Taxa de ataque final | 96,2 por cento | 94,9 por cento |

Diante dos dados do Recife:

- Os casos prováveis da primeira onda cresceram 0,021 por dia entre setembro de 2015 e janeiro de 2016,
  com pico na semana de 14 de fevereiro de 2016. Para crescer nesse ritmo, o SIR precisaria de R0 = 1,12 e
  o SEIR-SEI de 1,71.
- O inquérito sorológico de 2018 e 2019 (Braga et al., 2023) encontrou 35,7 por cento da população com
  anticorpos, o que corresponde a R0 = 1,24 sob mistura homogênea.
- A onda de 2021 cresceu 0,046 por dia, 2,2 vezes mais rápido que a primeira, no início da estação
  chuvosa. Lida pelo SEIR-SEI, com no máximo 64,3 por cento de suscetíveis, ela exige R0 de pelo menos
  4,56. A razão entre os casos de 2021 e os da primeira onda variou de 0,72 a 3,04 entre os distritos
  sanitários, contra 1,6 na cidade.
- Comparados com nove pontos observados, o SIR diverge em seis e o SEIR-SEI em cinco
  (`results/modelo_contra_dados.csv`). As divergências apontam sazonalidade, heterogeneidade entre grupos
  e fração de notificação como os componentes que faltam.
- Os números do portal batem com os divulgados pela Prefeitura em 2021: 15.058 casos prováveis no recorte
  do boletim, contra 14.926 noticiados na época.
- O SIR revisado cobre os três regimes de taxas (β > γ, β = γ, γ > β), confere cada resultado com a
  teoria, mede a eficácia crítica do controle vetorial (70,6 por cento) e o efeito da data de início.

## Como reproduzir

```bash
Rscript scripts/setup.R            # instala deSolve, reshape2, ggplot2, scales, patchwork e jsonlite
Rscript scripts/baixar_dados.R     # baixa as notificações do portal do Recife e gera data/processed/
Rscript scripts/render_figures.R   # gera figures/ e results/, conferindo tudo contra a teoria
bash scripts/build_proposta.sh     # compila proposta/main.pdf
bash scripts/montar_overleaf.sh    # copia para overleaf/ e overleaf.zip só o que o Overleaf precisa
```

O `overleaf.zip` sobe direto no Overleaf, em New Project e depois Upload Project. A pasta `overleaf/` tem o
mesmo conteúdo, para quem preferir arrastar os arquivos. As duas ficam fora do controle de versão e são
refeitas pelo script a cada mudança na proposta.

O build da proposta usa `pdflatex` e `bibtex` do PATH. Se eles não estiverem no PATH, a variável `MIKTEX`
pode apontar para a pasta `bin` da instalação, por exemplo `MIKTEX=/caminho/miktex/bin/x64 bash
scripts/build_proposta.sh`.

`data/processed/` já vem no repositório, então `render_figures.R` roda sem baixar nada. Os arquivos
brutos trazem endereço dos pacientes e ficam em `data/raw/`, fora do controle de versão. Só entram no
repositório contagens agregadas por semana, por ano e por distrito.

O `render_figures.R` confere a prevalência máxima, os suscetíveis no pico e o tamanho final do SIR, o total
de infectados abaixo do limiar, a equivalência das extensões quando desligadas, a conservação das
populações e a taxa de crescimento do SEIR-SEI contra o autovalor do Jacobiano. Também confere a série do
portal contra os números divulgados pela Prefeitura em 2021. Ele interrompe a execução se algum valor
divergir.

Para abrir os notebooks é preciso o kernel R do Jupyter:

```r
install.packages("IRkernel")
IRkernel::installspec()
```

## Estrutura

```
R/                    modelos, integração, métricas, cenários, dados e figuras
scripts/              instalação, download dos dados, renderização e build da proposta
notebooks/            os dois notebooks do estudo e os cadernos originais do curso
data/processed/       casos de chikungunya do Recife por semana, ano e distrito
figures/              figuras geradas
results/              tabelas geradas em CSV
proposta/             proposta de projeto no template BSI, em LaTeX
paper/                mini paper da entrega do Módulo I, mantido como registro
```

| Arquivo | Papel |
| --- | --- |
| `R/config.R` | População, parâmetros da doença e do vetor, sorologia e identidade visual |
| `R/sir_model.R` | SIR com a força de infecção injetada, controle vetorial e perda de imunidade opcionais |
| `R/seir_sei_model.R` | Modelo SEIR-SEI, números de reprodução e taxa de crescimento |
| `R/solver.R` | Fachada sobre `deSolve::ode` |
| `R/metrics.R` | Pico, tamanho final, R0 implícito, eficácia crítica e a tabela de validação |
| `R/scenarios.R` | Os dez cenários e as análises de controle, imunidade, crescimento e das duas ondas |
| `R/dados.R` | Leitura e agregação das notificações do portal |
| `R/plotting.R` | Tema, escalas, cores, assinatura e gravação das figuras |
| `R/figures.R` | Construção das dezenove figuras |

| Notebook | Conteúdo |
| --- | --- |
| `01_sir_chikungunya_recife.ipynb` | O SIR revisado, respondendo aos seis itens do enunciado e às anotações da professora |
| `02_dados_e_modelo_vetorial.ipynb` | Dados do Recife, taxa de crescimento, o modelo SEIR-SEI e a comparação com a onda de 2021 |

## Uso de inteligência artificial

A disciplina exige que todo uso de IA seja identificado. Este trabalho usou o assistente de programação
Claude Code, com o modelo de linguagem Claude Opus 5.5, da Anthropic, em três frentes:

- **Código.** Escrita e revisão do código em R dos modelos, dos cenários, do tratamento dos dados e das
  figuras, e do script que confere os resultados contra as expressões analíticas.
- **Referências.** Busca de literatura e conferência das referências nas fontes primárias. Nessa
  conferência foi encontrada e corrigida uma citação errada da versão anterior, que atribuía o R0 de 3,4
  a outros autores.
- **Texto.** Complementação e ajuste da redação a partir do texto base elaborado pelo autor, e revisão de
  trechos da proposta e dos notebooks.

Os resultados numéricos não foram gerados pela ferramenta: todos vêm do código do repositório, que os
confere contra as expressões analíticas sempre que as figuras são regeneradas. A exploração da estrutura
dos arquivos do portal de dados abertos, as decisões de modelagem, a escolha dos parâmetros e a
interpretação dos resultados foram realizadas pelo autor, que conferiu as referências citadas e é
responsável por todo o conteúdo. A declaração formal está na proposta, em `proposta/sections/UsoIA.tex`.

## Entrega do Módulo I

A versão entregue no Módulo I, com o mini paper e o notebook originais, está preservada na tag
`modulo-1`. O `paper/mini_paper.pdf` continua no repositório como registro. O `.tex` aponta para nomes
de figuras que mudaram, então para recompilá-lo é preciso voltar à tag.

## Material de referência

Os quatro cadernos originais do curso, em `notebooks/curso/`, são material didático de terceiros mantido
aqui apenas como referência do padrão de código seguido neste trabalho.
