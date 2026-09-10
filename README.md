# Modelo SIR para uma epidemia de chikungunya no Recife

Atividade do Módulo I da disciplina Elementos de Epidemiologia Computacional, PPGBAS UFPE e BSI UFRPE.
Simulação do modelo SIR clássico aplicado à introdução do vírus chikungunya no município do Recife,
implementada em R com `deSolve`, seguindo a estrutura dos cadernos do curso de modelagem de doenças
infecciosas.

## Resultados principais

Com população de 1.488.920 habitantes, período infeccioso de 7 dias e R0 de 3,4:

| Grandeza | Valor |
| --- | --- |
| Dia do pico | 44 |
| Infectados no pico | 514.377 pessoas, 34,5 por cento da população |
| Suscetíveis no pico | 463.375 pessoas |
| Recuperados no pico | 511.168 pessoas |
| Taxa de ataque final | 96,2 por cento |

Oito variações das taxas de contágio e de recuperação mostram que a razão entre elas determina o tamanho
da epidemia, enquanto a escala determina apenas a sua velocidade. A discussão das lacunas confronta a taxa
de ataque prevista com a soroprevalência de 11,8 por cento medida em inquérito de campo no Nordeste.

## Como reproduzir

```bash
Rscript scripts/setup.R            # instala deSolve, reshape2, ggplot2, scales e patchwork
Rscript scripts/render_figures.R   # gera figures/ e results/, conferindo tudo contra a teoria
bash scripts/build_paper.sh        # compila paper/mini_paper.pdf
```

O `render_figures.R` valida cada resultado contra as expressões analíticas do modelo SIR, a prevalência
máxima, o número de suscetíveis no pico e o tamanho final do surto, e interrompe a execução se algum
valor divergir mais de 1 por cento.

Para abrir o notebook é preciso o kernel R do Jupyter:

```r
install.packages("IRkernel")
IRkernel::installspec()
```

## Estrutura

```
R/                    modelo, integração, métricas, cenários e figuras
scripts/              instalação de dependências, renderização e build do paper
notebooks/            notebook da entrega e os cadernos originais do curso
figures/              figuras geradas
results/              tabelas geradas em CSV
paper/                mini paper em LaTeX e o PDF
```

| Arquivo | Papel |
| --- | --- |
| `R/config.R` | População, parâmetros da doença e identidade visual |
| `R/sir_model.R` | Equações do modelo, com a força de infecção injetada como estratégia |
| `R/solver.R` | Fachada sobre `deSolve::ode` |
| `R/metrics.R` | Pico, R0, tamanho final e as expressões analíticas de referência |
| `R/scenarios.R` | Catálogo dos oito cenários |
| `R/plotting.R` | Tema, escalas, assinatura e gravação das figuras |
| `R/figures.R` | Construção das seis figuras |

## Organização do código

O modelo é único e a diferença entre o caderno 3 do curso, com força de infecção constante, e o caderno 4,
com força de infecção dinâmica, é resolvida injetando a função de `lambda` em `make_sir_model`. Os oito
cenários são dados em `scenario_catalog`, não código repetido. A assinatura das figuras é aplicada por
`sign_plot`, que envolve um gráfico pronto sem que as funções de plotagem precisem saber disso, e
`epi_plot` fixa eixos, legenda e tema para que cada figura forneça apenas os dados e os textos.

## Material de referência

Os quatro cadernos originais do curso, em `notebooks/curso/`, são material didático de terceiros mantido
aqui apenas como referência do padrão de código seguido neste trabalho.
