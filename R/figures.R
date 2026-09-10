# Construtores das figuras. Cada função devolve um ggplot já assinado, de modo
# que o notebook e o script de renderização produzam exatamente o mesmo gráfico.

# Ancora o rótulo de cada série no ponto em que ele fica mais longe das demais
# curvas. A distância é medida no espaço normalizado do painel, porque é ali que
# a sobreposição realmente acontece.
posicoes_rotulos <- function(longo, margem = 0.1, altura_rotulo = 0.05) {
  faixa_x <- range(longo$time)
  faixa_y <- range(longo$pessoas)
  ux <- function(v) (v - faixa_x[1]) / diff(faixa_x)
  uy <- function(v) (v - faixa_y[1]) / diff(faixa_y)

  dentro <- ux(longo$time) >= margem & ux(longo$time) <= 1 - margem
  candidatos <- longo[dentro, ]

  do.call(rbind, lapply(split(candidatos, candidatos$compartimento), function(serie) {
    outras <- longo[longo$compartimento != serie$compartimento[1], ]
    qx <- ux(outras$time)
    qy <- uy(outras$pessoas)

    # Posições cujo rótulo cairia fora do painel são descartadas.
    avalia <- function(deslocamento) {
      px <- ux(serie$time)
      py <- uy(serie$pessoas) + deslocamento
      vapply(seq_along(px), function(i) {
        if (py[i] < 0.02 || py[i] > 0.98) return(-Inf)
        min(sqrt((qx - px[i])^2 + (qy - py[i])^2))
      }, numeric(1))
    }

    acima <- avalia(altura_rotulo)
    abaixo <- avalia(-altura_rotulo)
    usa_acima <- max(acima) >= max(abaixo)

    distancias <- if (usa_acima) acima else abaixo
    ancora <- serie[which.max(distancias), ]
    ancora$acima <- usa_acima
    ancora
  }))
}

rotulos_diretos <- function(longo) {
  ancoras <- posicoes_rotulos(longo)
  desenha <- function(dados, vjust) {
    geom_text(data = dados,
              aes(x = time, y = pessoas, label = compartimento, colour = compartimento),
              vjust = vjust, size = 3.6, fontface = "bold", show.legend = FALSE)
  }
  list(desenha(ancoras[ancoras$acima, , drop = FALSE], -0.9),
       desenha(ancoras[!ancoras$acima, , drop = FALSE], 1.7))
}

figura_diagrama <- function(beta = CHIKV$beta, gamma = CHIKV$gamma) {
  caixas <- data.frame(
    x = c(1, 3, 5),
    compartimento = factor(VISUAL$rotulos, levels = VISUAL$rotulos),
    sigla = c("S", "I", "R")
  )
  setas <- data.frame(x = c(1.6, 3.6), xend = c(2.4, 4.4))
  taxas <- data.frame(
    x = c(2, 4),
    rotulo = c(
      sprintf("λ = β I / N\nβ = %s por dia", format(round(beta, 3), decimal.mark = ",")),
      sprintf("γ = %s por dia\nperíodo infeccioso de %.0f dias",
              format(round(gamma, 3), decimal.mark = ","), 1 / gamma)
    )
  )
  paleta <- setNames(unname(VISUAL$cores), unname(VISUAL$rotulos))

  p <- ggplot() +
    geom_tile(data = caixas, aes(x = x, y = 0, fill = compartimento),
              width = 1.1, height = 0.72, alpha = 0.16) +
    geom_tile(data = caixas, aes(x = x, y = 0, colour = compartimento),
              width = 1.1, height = 0.72, fill = NA, linewidth = 0.9) +
    geom_text(data = caixas, aes(x = x, y = 0.09, label = sigla, colour = compartimento),
              size = 9, fontface = "bold", show.legend = FALSE) +
    geom_text(data = caixas, aes(x = x, y = -0.2, label = compartimento),
              size = 3.6, colour = VISUAL$cinza_texto) +
    geom_segment(data = setas, aes(x = x, xend = xend, y = 0, yend = 0),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
                 linewidth = 0.6, colour = VISUAL$cinza_texto) +
    geom_text(data = taxas, aes(x = x, y = 0.36, label = rotulo),
              size = 3.2, colour = VISUAL$cinza_texto, lineheight = 1.2) +
    scale_fill_manual(values = paleta, guide = "none") +
    scale_colour_manual(values = paleta, guide = "none") +
    coord_cartesian(xlim = c(0.35, 5.65), ylim = c(-0.5, 0.62)) +
    labs(title = "Modelo SIR aplicado ao chikungunya no Recife",
         subtitle = "Três compartimentos, população fechada e imunidade permanente após a recuperação") +
    theme_epi() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  sign_plot(p)
}

figura_linha_base <- function(sim) {
  N <- sum(sim$initial_state_values)
  R0 <- sim$parameters[["beta"]] / sim$parameters[["gamma"]]

  epi_plot(
    sim$long,
    titulo = "Surto de chikungunya no Recife pelo modelo SIR",
    subtitulo = sprintf(
      "N = %s habitantes, β = %s por dia, γ = %s por dia, R0 = %s",
      milhares(N),
      format(round(sim$parameters[["beta"]], 3), decimal.mark = ","),
      format(round(sim$parameters[["gamma"]], 3), decimal.mark = ","),
      format(round(R0, 1), decimal.mark = ",")
    )
  ) + rotulos_diretos(sim$long)
}

figura_pico <- function(sim, resumo) {
  infectados <- sim$long[sim$long$compartimento == VISUAL$rotulos[["I"]], ]
  destaque <- data.frame(time = resumo$dia_pico, pessoas = resumo$I_pico)

  legenda <- sprintf(
    "Dia %d\nS = %s\nI = %s\nR = %s",
    resumo$dia_pico, milhares(resumo$S_pico), milhares(resumo$I_pico), milhares(resumo$R_pico)
  )

  p <- ggplot(infectados, aes(x = time, y = pessoas)) +
    geom_area(fill = VISUAL$cores[["I"]], alpha = 0.12) +
    geom_line(colour = VISUAL$cores[["I"]], linewidth = 1) +
    geom_segment(data = destaque, aes(x = time, xend = time, y = 0, yend = pessoas),
                 linetype = "22", colour = VISUAL$cinza_fraco, linewidth = 0.5) +
    geom_point(data = destaque, aes(x = time, y = pessoas),
               size = 3, colour = VISUAL$cores[["I"]]) +
    geom_label(data = destaque, aes(x = time, y = pessoas, label = legenda),
               hjust = -0.3, vjust = 0.92, size = 3.3, label.size = 0,
               fill = VISUAL$superficie, colour = VISUAL$cinza_texto, lineheight = 1.3) +
    escala_dias() +
    escala_pessoas() +
    labs(
      title = "Pico da epidemia de chikungunya no Recife",
      subtitle = sprintf(
        "Prevalência máxima de %s pessoas infectadas, %s por cento da população",
        milhares(resumo$I_pico),
        format(round(100 * resumo$prevalencia_pico, 1), decimal.mark = ",")
      ),
      x = "Tempo (dias)", y = "Pessoas infectadas"
    ) +
    theme_epi()

  sign_plot(p)
}

figura_cenarios_painel <- function(curvas, resumo) {
  faixas <- setNames(
    sprintf("%d. %s\nR0 = %s", resumo$id, resumo$cenario,
            format(round(resumo$R0, 2), decimal.mark = ",")),
    as.character(resumo$cenario)
  )

  p <- ggplot(curvas, aes(x = time, y = pessoas, colour = compartimento)) +
    geom_line(linewidth = 0.75) +
    facet_wrap(~cenario, ncol = 4, scales = "free_x",
               labeller = labeller(cenario = faixas)) +
    scale_colour_sir() +
    escala_dias(n.breaks = 4) +
    escala_pessoas(expand = expansion(mult = c(0.02, 0.06))) +
    labs(title = "Oito variações das taxas de contágio e de recuperação",
         subtitle = "A escala vertical é a mesma nos oito painéis. O eixo do tempo acompanha a duração de cada surto",
         x = "Tempo (dias)", y = "Número de pessoas") +
    theme_epi(base_size = 11) +
    theme(panel.spacing = unit(1.1, "lines"))

  sign_plot(p)
}

figura_cenarios_infectados <- function(curvas, resumo, limite_dias = 400) {
  rotulo_surto <- "R0 > 1, há surto"
  rotulo_sem_surto <- "R0 <= 1, não há surto"

  infectados <- curvas[curvas$compartimento == VISUAL$rotulos[["I"]], ]
  infectados$regime <- ifelse(infectados$R0 > 1, rotulo_surto, rotulo_sem_surto)

  picos <- resumo[resumo$R0 > 1, ]
  picos$rotulo <- sprintf("%d. R0 = %s", picos$id,
                          format(round(picos$R0, 2), decimal.mark = ","))

  # Se outra curva passa acima deste pico, o rótulo vai para a direita.
  picos$encoberto <- vapply(seq_len(nrow(picos)), function(i) {
    vizinhas <- infectados[infectados$cenario != picos$cenario[i] &
                           infectados$time == picos$dia_pico[i], "pessoas"]
    length(vizinhas) > 0 && max(vizinhas) > picos$I_pico[i]
  }, logical(1))

  cores_regime <- setNames(c(VISUAL$cores[["I"]], VISUAL$cores[["S"]]),
                           c(rotulo_surto, rotulo_sem_surto))

  p <- ggplot(infectados, aes(x = time, y = pessoas, group = cenario, colour = regime)) +
    geom_line(aes(linewidth = cenario == "Linha de base")) +
    geom_point(data = picos, aes(x = dia_pico, y = I_pico),
               colour = VISUAL$cores[["I"]], size = 2, inherit.aes = FALSE) +
    geom_text(data = picos[!picos$encoberto, ], aes(x = dia_pico, y = I_pico, label = rotulo),
              colour = VISUAL$cinza_texto, vjust = -1.1, size = 3.1,
              fontface = "bold", inherit.aes = FALSE) +
    geom_text(data = picos[picos$encoberto, ], aes(x = dia_pico, y = I_pico, label = rotulo),
              colour = VISUAL$cinza_texto, hjust = -0.15, size = 3.1,
              fontface = "bold", inherit.aes = FALSE) +
    scale_colour_manual(name = "Regime", values = cores_regime) +
    scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.7), guide = "none") +
    coord_cartesian(xlim = c(0, limite_dias)) +
    escala_dias() +
    escala_pessoas(expand = expansion(mult = c(0.02, 0.12))) +
    labs(title = "Curvas de infectados nos oito cenários",
         subtitle = "A linha mais grossa é a linha de base. Cada rótulo marca o pico do cenário correspondente",
         x = "Tempo (dias)", y = "Pessoas infectadas") +
    theme_epi()

  sign_plot(p)
}

figura_limiar <- function(resumo, gamma_ref = CHIKV$gamma) {
  grade <- seq(0.5, 7, by = 0.1)

  varredura <- do.call(rbind, lapply(grade, function(R0) {
    sim <- run_sir_model(beta = R0 * gamma_ref, gamma = gamma_ref,
                         duracao = 2000, passo = 0.5)
    data.frame(R0 = R0,
               dia_pico = sim$wide$time[which.max(sim$wide$I)],
               prevalencia_pico = max(sim$wide$I) / sum(sim$initial_state_values),
               taxa_ataque = tamanho_final_teorico(R0))
  }))

  mesma_gama <- resumo[abs(resumo$gamma - gamma_ref) < 1e-9 & resumo$R0 > 1, ]
  com_surto <- resumo[resumo$R0 > 1, ]
  pct <- label_percent(accuracy = 1, decimal.mark = ",")

  painel <- function(dados, coluna, pontos, titulo, rotulo_y, formatador) {
    ggplot(dados, aes(x = R0, y = .data[[coluna]])) +
      geom_vline(xintercept = 1, linetype = "22",
                 colour = VISUAL$cinza_fraco, linewidth = 0.5) +
      geom_line(colour = VISUAL$cores[["I"]], linewidth = 1) +
      geom_point(data = pontos, aes(x = R0, y = .data[[coluna]]),
                 colour = VISUAL$cores[["S"]], size = 2.4) +
      scale_y_continuous(labels = formatador) +
      scale_x_continuous(breaks = 0:7) +
      labs(title = titulo, x = "R0", y = rotulo_y) +
      theme_epi(base_size = 11)
  }

  composta <-
    painel(varredura[varredura$R0 > 1, ], "dia_pico", mesma_gama,
           "Dia do pico", "Dias até o pico", milhares) |
    painel(varredura, "prevalencia_pico", com_surto,
           "Prevalência no pico", "Percentual da população", pct) |
    painel(varredura, "taxa_ataque", com_surto,
           "Taxa de ataque final", "Percentual da população", pct)

  composta +
    plot_annotation(
      title = "O que muda quando R0 muda",
      subtitle = sprintf(
        "Varredura com γ fixo em %s por dia. Os pontos marcam os cenários simulados e a linha tracejada marca o limiar R0 = 1",
        format(round(gamma_ref, 3), decimal.mark = ",")
      ),
      caption = paste(VISUAL$assinatura, "|", VISUAL$disciplina),
      theme = theme_epi()
    )
}
