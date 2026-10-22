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
    sprintf("%d. %s\n%s, R0 = %s", resumo$id, resumo$cenario, resumo$regime,
            format(round(resumo$R0, 2), decimal.mark = ",")),
    as.character(resumo$cenario)
  )

  p <- ggplot(curvas, aes(x = time, y = pessoas, colour = compartimento)) +
    geom_line(linewidth = 0.75) +
    facet_wrap(~cenario, ncol = 5, scales = "free_x",
               labeller = as_labeller(faixas)) +
    scale_colour_sir() +
    escala_dias(n.breaks = 4) +
    escala_pessoas(expand = expansion(mult = c(0.02, 0.06))) +
    labs(title = "Dez variações das taxas de contágio e de recuperação",
         subtitle = "Mesma escala vertical nos dez painéis. Nos cenários com β ≤ γ as curvas ficam paradas, porque o surto não acontece",
         x = "Tempo (dias)", y = "Número de pessoas") +
    theme_epi(base_size = 11) +
    theme(panel.spacing = unit(1.1, "lines"),
          strip.text = element_text(size = 8.5, face = "bold", colour = "#0b0b0b"))

  sign_plot(p)
}

figura_cenarios_infectados <- function(curvas, resumo, limite_dias = 250) {
  infectados <- curvas[curvas$compartimento == VISUAL$rotulos[["I"]] & curvas$R0 > 1, ]

  picos <- resumo[resumo$R0 > 1, ]
  picos$rotulo <- sprintf("%d. R0 = %s", picos$id,
                          format(round(picos$R0, 2), decimal.mark = ","))

  # Se outra curva passa acima deste pico, o rótulo vai para a direita.
  picos$encoberto <- vapply(seq_len(nrow(picos)), function(i) {
    vizinhas <- infectados[infectados$cenario != picos$cenario[i] &
                           infectados$time == picos$dia_pico[i], "pessoas"]
    length(vizinhas) > 0 && max(vizinhas) > picos$I_pico[i]
  }, logical(1))

  p <- ggplot(infectados, aes(x = time, y = pessoas, group = cenario)) +
    geom_line(aes(linewidth = cenario == "Linha de base"), colour = VISUAL$cores[["I"]]) +
    geom_point(data = picos, aes(x = dia_pico, y = I_pico),
               colour = VISUAL$cores[["I"]], size = 2, inherit.aes = FALSE) +
    geom_text(data = picos[!picos$encoberto, ], aes(x = dia_pico, y = I_pico, label = rotulo),
              colour = VISUAL$cinza_texto, vjust = -1.1, size = 3.1,
              fontface = "bold", inherit.aes = FALSE) +
    geom_text(data = picos[picos$encoberto, ], aes(x = dia_pico, y = I_pico, label = rotulo),
              colour = VISUAL$cinza_texto, hjust = -0.15, size = 3.1,
              fontface = "bold", inherit.aes = FALSE) +
    scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.7), guide = "none") +
    coord_cartesian(xlim = c(0, limite_dias)) +
    escala_dias() +
    escala_pessoas(expand = expansion(mult = c(0.02, 0.12))) +
    labs(title = "Curvas de infectados nos cenários em que há surto (β > γ)",
         subtitle = "A linha mais grossa é a linha de base. Cada rótulo marca o pico do cenário correspondente",
         x = "Tempo (dias)", y = "Pessoas infectadas") +
    theme_epi()

  sign_plot(p)
}

# Os cenários com beta menor ou igual a gamma, em escala logarítmica. Abaixo do
# limiar, I(t) = I0 exp((beta - gamma) t), que vira uma reta nessa escala.
figura_regime_subcritico <- function(curvas, resumo, limite_dias = 60) {
  sub <- resumo[resumo$R0 <= 1 + 1e-9, ]
  infectados <- curvas[curvas$compartimento == VISUAL$rotulos[["I"]] &
                       curvas$id %in% sub$id & curvas$time <= limite_dias, ]

  teoria <- do.call(rbind, lapply(seq_len(nrow(sub)), function(i) {
    t <- seq(0, limite_dias, by = 0.5)
    data.frame(time = t, pessoas = BASELINE$I0 * exp((sub$beta[i] - sub$gamma[i]) * t),
               id = sub$id[i])
  }))

  # Rótulos sobre a própria linha, em pontos escolhidos para não colidirem.
  # Os dois cenários com R0 = 1 coincidem e dividem um rótulo.
  abaixo <- sub[sub$R0 < 1 - 1e-9, ]
  abaixo <- abaixo[order(abaixo$gamma - abaixo$beta, decreasing = TRUE), ]
  abaixo$x <- c(8, 30, 48)[seq_len(nrow(abaixo))]
  abaixo$texto <- sprintf("%d. R0 = %s (β = %s, γ = %s)", abaixo$id, num_br(abaixo$R0, 2),
                          num_br(abaixo$beta, 3), num_br(abaixo$gamma, 3))
  iguais <- sub[abs(sub$R0 - 1) < 1e-9, ]
  limiar <- data.frame(
    x = limite_dias / 2, regime = "β = γ",
    texto = sprintf("%s. R0 = 1 (β = γ = %s)", paste(iguais$id, collapse = " e "),
                    paste(num_br(iguais$beta, 3), collapse = " e ")))
  rotulos <- rbind(abaixo[, c("x", "regime", "texto")], limiar)
  rotulos$y <- c(BASELINE$I0 * exp((abaixo$beta - abaixo$gamma) * abaixo$x), BASELINE$I0)

  totais <- paste(sprintf("%s para R0 = %s", num_br(abaixo$total_subcritico_teorico, 0),
                          num_br(abaixo$R0, 2))[!duplicated(abaixo$R0)], collapse = " e ")

  p <- ggplot(infectados, aes(x = time, y = pessoas, group = id, colour = regime)) +
    geom_line(data = teoria, aes(x = time, y = pessoas, group = id), inherit.aes = FALSE,
              colour = VISUAL$cinza_fraco, linewidth = 2.4, alpha = 0.35) +
    geom_line(linewidth = 0.9) +
    geom_label(data = rotulos, aes(x = x, y = y, label = texto, colour = regime),
               inherit.aes = FALSE, size = 3, label.size = 0, fill = VISUAL$superficie,
               show.legend = FALSE) +
    scale_colour_regime() +
    escala_log_pessoas(breaks = c(1, 0.1, 0.01, 0.001)) +
    coord_cartesian(ylim = c(0.001, 2.5), xlim = c(0, limite_dias)) +
    escala_dias() +
    labs(title = "Quando γ ≥ β, a infecção não se espalha",
         subtitle = sprintf("Escala log. A faixa cinza é a solução analítica I0 exp((β − γ) t).\nTotal de infectados até a extinção, I0 / (1 − R0): %s",
                            totais),
         x = "Tempo (dias)", y = "Pessoas infectadas (escala log)") +
    theme_epi()

  sign_plot(p)
}

# Taxa de ataque no plano (gamma, beta). A diagonal separa surto de ausência de surto.
figura_mapa_fases <- function(grade, resumo) {
  pontos <- resumo
  pct <- label_percent(accuracy = 1, decimal.mark = ",")

  p <- ggplot(grade, aes(x = gamma, y = beta)) +
    geom_raster(aes(fill = taxa_ataque), interpolate = TRUE) +
    geom_abline(slope = 1, intercept = 0, colour = "#0b0b0b", linewidth = 0.6) +
    geom_abline(slope = CHIKV$R0, intercept = 0, colour = VISUAL$superficie,
                linewidth = 0.4, linetype = "22") +
    geom_point(data = pontos, aes(x = gamma, y = beta), shape = 21, size = 3.2,
               fill = VISUAL$superficie, colour = "#0b0b0b", stroke = 0.7) +
    geom_text(data = pontos, aes(x = gamma, y = beta, label = id), size = 2.6,
              nudge_x = 0.035, nudge_y = 0.035, colour = "#0b0b0b", fontface = "bold") +
    annotate("text", x = 0.86, y = 0.18, label = "γ > β\nnão há surto", size = 3.6,
             colour = VISUAL$cinza_texto, lineheight = 1) +
    annotate("text", x = 0.22, y = 1.05, label = "β > γ\nsurto", size = 3.6,
             colour = VISUAL$superficie, lineheight = 1, fontface = "bold") +
    annotate("text", x = 1.08, y = 1.16, label = "β = γ", size = 3.2, angle = 45,
             colour = "#0b0b0b") +
    scale_fill_sequencial("Taxa de ataque", rotulos = pct, limits = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0), labels = label_number(decimal.mark = ",")) +
    scale_y_continuous(expand = c(0, 0), labels = label_number(decimal.mark = ",")) +
    coord_fixed(xlim = c(0.02, 1.2), ylim = c(0.02, 1.2)) +
    labs(title = "Taxa de ataque final em cada combinação de β e γ",
         subtitle = "A diagonal β = γ é o limiar R0 = 1. A linha tracejada clara é R0 = 3,4. Os números são os cenários da tabela",
         x = "γ, taxa de recuperação (por dia)", y = "β, taxa de contágio (por dia)") +
    theme_epi() +
    theme(legend.position = "right", legend.justification = "center",
          panel.grid.major = element_blank())

  sign_plot(p)
}

figura_controle_eficacia <- function(varredura) {
  valores <- sort(unique(varredura$R0))
  niveis <- sprintf("R0 = %s, eficácia crítica de %s%%", num_br(valores, 1),
                    num_br(100 * eficacia_critica(valores), 1))
  varredura$serie <- factor(niveis[match(varredura$R0, valores)], levels = niveis)
  cores <- setNames(VISUAL$sequencial[c(2, 4, 5)], niveis)
  criticas <- data.frame(serie = factor(niveis, levels = niveis),
                         eficacia = eficacia_critica(valores))

  painel <- function(coluna, titulo) {
    ggplot(varredura, aes(x = eficacia, y = .data[[coluna]], colour = serie)) +
      geom_line(linewidth = 1) +
      geom_point(data = criticas, aes(x = eficacia, y = 0), size = 2.6) +
      scale_colour_manual(name = NULL, values = cores) +
      scale_x_continuous(labels = label_percent(accuracy = 1), breaks = seq(0, 1, 0.2)) +
      escala_percentual() +
      labs(title = titulo, x = "Redução do contágio (eficácia do controle)",
           y = "Percentual da população") +
      theme_epi(base_size = 11)
  }

  (painel("taxa_ataque", "Taxa de ataque final") |
     painel("prevalencia_pico", "Prevalência no pico")) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Quanto o controle precisa reduzir o contágio",
      subtitle = "Controle desde o primeiro caso. Os pontos marcam a eficácia crítica 1 − 1/R0, a partir da qual o surto não acontece",
      caption = paste(VISUAL$assinatura, "|", VISUAL$disciplina),
      theme = theme_epi()
    ) & theme(legend.position = "bottom")
}

figura_controle_inicio <- function(grade, dia_pico_base) {
  pct <- label_percent(accuracy = 1, decimal.mark = ",")
  p <- ggplot(grade, aes(x = inicio, y = eficacia, fill = taxa_ataque)) +
    geom_tile(colour = VISUAL$superficie, linewidth = 0.4) +
    geom_text(aes(label = round(100 * taxa_ataque),
                  colour = taxa_ataque > 0.55), size = 2.4, show.legend = FALSE) +
    geom_vline(xintercept = dia_pico_base, colour = "#0b0b0b", linewidth = 0.5) +
    annotate("label", x = dia_pico_base, y = 0.96, label = sprintf("pico sem controle, dia %d", dia_pico_base),
             size = 3, label.size = 0, fill = VISUAL$superficie, colour = VISUAL$cinza_texto) +
    scale_fill_sequencial("Taxa de ataque", rotulos = pct, limits = c(0, 1)) +
    scale_colour_manual(values = c(`TRUE` = VISUAL$superficie, `FALSE` = "#0b0b0b")) +
    scale_x_continuous(breaks = unique(grade$inicio), expand = c(0, 0)) +
    scale_y_continuous(labels = label_percent(accuracy = 1), breaks = unique(grade$eficacia),
                       expand = expansion(add = c(0, 0.07))) +
    labs(title = "O mesmo controle vale menos quanto mais tarde começa",
         subtitle = "Taxa de ataque final, em percentual, para cada eficácia e cada dia de início do controle. Linha de base com R0 = 3,4",
         x = "Dia de início do controle", y = "Redução do contágio") +
    theme_epi() +
    theme(panel.grid.major = element_blank(), legend.position = "right",
          legend.justification = "center")

  sign_plot(p)
}

figura_imunidade <- function(sirs) {
  sirs$anos <- sirs$time / 365
  ordem <- unique(sirs$cenario)
  sirs$cenario <- factor(sirs$cenario, levels = ordem)
  cores <- setNames(c(VISUAL$cinza_fraco, VISUAL$sequencial[3], VISUAL$sequencial[5]), ordem)

  p <- ggplot(sirs, aes(x = anos, y = I, colour = cenario)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(name = NULL, values = cores) +
    escala_log_pessoas(breaks = c(1, 100, 1e4, 1e6)) +
    coord_cartesian(ylim = c(0.5, 1e6)) +
    scale_x_continuous(breaks = 0:10) +
    labs(title = "E se a imunidade não fosse permanente?",
         subtitle = "Infectados em escala logarítmica. Com perda de imunidade (SIRS), novas ondas surgem quando os suscetíveis voltam a passar de N/R0",
         x = "Tempo (anos)", y = "Pessoas infectadas (escala log)") +
    theme_epi()

  sign_plot(p)
}

figura_dados_recife <- function(serie, inicio_zoom = as.Date("2015-06-01"),
                                fim_zoom = as.Date("2016-12-31")) {
  inquerito <- data.frame(xmin = as.Date("2018-08-01"), xmax = as.Date("2019-02-28"))
  picos <- do.call(rbind, lapply(c(2016, 2021), function(a) {
    z <- serie[format(serie$semana, "%Y") == a, ]
    z[which.max(z$provaveis), ]
  }))
  picos$rotulo <- sprintf("semana de %s\n%s casos", format(picos$semana, "%d/%m/%Y"),
                          milhares(picos$provaveis))

  completa <- ggplot(serie, aes(x = semana, y = provaveis)) +
    geom_rect(data = inquerito, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = VISUAL$sequencial[1], alpha = 0.7) +
    annotate("text", x = as.Date("2018-11-15"), y = 1150, label = "inquérito\nsorológico",
             size = 2.9, colour = VISUAL$cinza_texto, lineheight = 1) +
    geom_col(fill = VISUAL$cor_dados, width = 6) +
    geom_text(data = picos, aes(label = rotulo), hjust = -0.08, vjust = 0.9, size = 2.9,
              colour = VISUAL$cinza_texto, lineheight = 1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = 0.01)) +
    escala_pessoas(expand = expansion(mult = c(0, 0.08))) +
    labs(title = "2015 a 2025", x = NULL, y = "Casos prováveis por semana") +
    theme_epi(base_size = 11)

  zoom <- serie[serie$semana >= inicio_zoom & serie$semana <= fim_zoom, ]
  detalhe <- ggplot(zoom, aes(x = semana, y = provaveis)) +
    geom_col(fill = VISUAL$cor_dados, width = 6) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = expansion(mult = 0.01)) +
    escala_pessoas(expand = expansion(mult = c(0, 0.08))) +
    labs(title = "Primeira onda, junho de 2015 a dezembro de 2016", x = NULL,
         y = "Casos prováveis por semana") +
    theme_epi(base_size = 11)

  (completa / detalhe) +
    plot_annotation(
      title = "Casos prováveis de chikungunya no Recife por semana de início dos sintomas",
      subtitle = "Notificações de residentes, excluídos os descartados. Fonte: portal de dados abertos da Prefeitura do Recife",
      caption = paste(VISUAL$assinatura, "|", VISUAL$disciplina),
      theme = theme_epi()
    )
}

figura_sorologia <- function(comparacao) {
  comparacao$estrato <- factor(comparacao$estrato, levels = rev(comparacao$estrato))
  comparacao$tipo <- ifelse(comparacao$fonte == "Modelo", "Modelo SIR", "Inquérito sorológico")
  cores <- c(`Modelo SIR` = CORES_MODELO[["SIR"]], `Inquérito sorológico` = VISUAL$cor_dados)
  comparacao$rotulo <- sprintf("%s%%   R0 implícito %s", num_br(100 * comparacao$prevalencia, 1),
                               num_br(comparacao$R0_implicito, 2))

  p <- ggplot(comparacao, aes(y = estrato, x = prevalencia, colour = tipo)) +
    geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.18, linewidth = 0.6, na.rm = TRUE) +
    geom_point(size = 3.2) +
    geom_text(aes(label = rotulo), vjust = -1.1, size = 3, colour = VISUAL$cinza_texto) +
    scale_colour_manual(name = NULL, values = cores) +
    scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(0, 1.05),
                       breaks = seq(0, 1, 0.2)) +
    labs(title = "O modelo prevê 96% de infectados. O Recife teve cerca de 36%",
         subtitle = "Taxa de ataque do SIR contra a soroprevalência de IgG medida em 2018 e 2019 (Braga et al., 2023), com IC de 95%.\nO R0 implícito é o valor que, sob mistura homogênea, reproduziria cada percentual",
         x = "Percentual da população já infectada", y = NULL) +
    theme_epi() +
    theme(panel.grid.major.y = element_blank())

  sign_plot(p)
}

figura_diagrama_seir_sei <- function(p = parametros_vetor()) {
  caixas <- data.frame(
    x = c(1, 3, 5, 7, 2, 4, 6),
    y = c(1, 1, 1, 1, -1, -1, -1),
    sigla = c("Sh", "Eh", "Ih", "Rh", "Sv", "Ev", "Iv"),
    nome = c("Suscetíveis", "Expostos", "Infectados", "Recuperados",
             "Suscetíveis", "Expostos", "Infectantes"),
    cor = c(VISUAL$cores[["S"]], VISUAL$cor_expostos, VISUAL$cores[["I"]], VISUAL$cores[["R"]],
            VISUAL$cores[["S"]], VISUAL$cor_expostos, VISUAL$cores[["I"]])
  )
  setas <- data.frame(
    x = c(1.55, 3.55, 5.55, 2.55, 4.55),
    xend = c(2.45, 4.45, 6.45, 3.45, 5.45),
    y = c(1, 1, 1, -1, -1),
    yend = c(1, 1, 1, -1, -1),
    rotulo = c("picada de mosquito infectante\na b Iv / Nh",
               sprintf("latência\n%s dias", num_br(1 / p[["sigma_h"]], 0)),
               sprintf("recuperação\n%s dias", num_br(1 / p[["gamma"]], 0)),
               "picada em humano infectado\na c Ih / Nh",
               sprintf("incubação extrínseca\n%s dias", num_br(1 / p[["sigma_v"]], 0)))
  )
  cruzadas <- data.frame(x = c(6, 5), xend = c(1.3, 2.3), y = c(-0.7, 0.7), yend = c(0.7, -0.7))

  p_plot <- ggplot() +
    geom_curve(data = cruzadas, aes(x = x, xend = xend, y = y, yend = yend),
               curvature = 0.15, linetype = "22", colour = VISUAL$cinza_fraco, linewidth = 0.5,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    geom_tile(data = caixas, aes(x = x, y = y), width = 1.05, height = 0.62,
              fill = caixas$cor, alpha = 0.14) +
    geom_tile(data = caixas, aes(x = x, y = y), width = 1.05, height = 0.62,
              fill = NA, colour = caixas$cor, linewidth = 0.9) +
    geom_text(data = caixas, aes(x = x, y = y + 0.08, label = sigla), colour = caixas$cor,
              size = 6.5, fontface = "bold") +
    geom_text(data = caixas, aes(x = x, y = y - 0.17, label = nome), size = 3,
              colour = VISUAL$cinza_texto) +
    geom_segment(data = setas, aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
                 colour = VISUAL$cinza_texto, linewidth = 0.6) +
    geom_text(data = setas, aes(x = (x + xend) / 2, y = y + ifelse(y > 0, 0.48, -0.5),
                                label = rotulo), size = 2.7, colour = VISUAL$cinza_texto,
              lineheight = 1) +
    annotate("text", x = 0.2, y = 1, label = "Humanos", angle = 90, size = 3.8,
             fontface = "bold", colour = "#0b0b0b") +
    annotate("text", x = 0.2, y = -1, label = "Aedes aegypti", angle = 90, size = 3.8,
             fontface = "bold.italic", colour = "#0b0b0b") +
    annotate("text", x = 4, y = -1.85, size = 2.8, colour = VISUAL$cinza_texto,
             label = sprintf("Mosquitos nascem e morrem na taxa μv, vida média de %s dias. Razão m = %s mosquitos por humano",
                             num_br(1 / p[["mu_v"]], 0), num_br(p[["m"]], 2))) +
    coord_cartesian(xlim = c(0, 7.7), ylim = c(-2, 1.75)) +
    labs(title = "Modelo SEIR-SEI proposto para a chikungunya no Recife",
         subtitle = "Latência nos humanos, mosquito explícito com incubação extrínseca. As setas tracejadas indicam de onde vem cada infecção") +
    theme_epi() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  sign_plot(p_plot)
}

figura_sir_vs_vetorial <- function(comp) {
  picos <- do.call(rbind, lapply(split(comp$curvas, comp$curvas$modelo), function(d) {
    d[which.max(d$I), ]
  }))
  picos$rotulo <- sprintf("%s: pico no dia %d\n%s infectados", picos$modelo, picos$time,
                          milhares(picos$I))

  p <- ggplot(comp$curvas, aes(x = time, y = I, colour = modelo)) +
    geom_line(linewidth = 1) +
    geom_point(data = picos, size = 2.4) +
    geom_text(data = picos, aes(label = rotulo), hjust = -0.1, vjust = 0, size = 3,
              colour = VISUAL$cinza_texto, lineheight = 1) +
    scale_colour_manual(name = "Modelo", values = CORES_MODELO) +
    escala_dias() +
    escala_pessoas(expand = expansion(mult = c(0.02, 0.1))) +
    labs(title = "Latência e vetor atrasam e achatam o pico",
         subtitle = sprintf("Mesmo número de reprodução de humano para humano, %s, e portanto a mesma taxa de ataque final, %s%%",
                            num_br(CHIKV$R0, 1),
                            num_br(100 * tamanho_final_teorico(CHIKV$R0), 1)),
         x = "Tempo (dias)", y = "Pessoas infectadas") +
    theme_epi()

  sign_plot(p)
}

figura_controle_vetorial <- function(ctrl, limite_dias = 450) {
  niveis <- levels(ctrl$curvas$serie)
  cores <- setNames(c(VISUAL$cor_neutra, VISUAL$sequencial[c(2, 4, 5)])[seq_along(niveis)], niveis)
  inicios <- ctrl$resumo[is.finite(ctrl$resumo$inicio), ]

  curvas <- ggplot(ctrl$curvas, aes(x = time, y = I, colour = serie)) +
    geom_vline(data = inicios, aes(xintercept = inicio, colour = serie), linewidth = 0.4,
               show.legend = FALSE) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(name = NULL, values = cores) +
    escala_dias() +
    escala_pessoas() +
    coord_cartesian(xlim = c(0, limite_dias)) +
    labs(title = "Humanos infectados", x = "Tempo (dias)", y = "Pessoas infectadas") +
    theme_epi(base_size = 11)

  mosquitos <- ggplot(ctrl$mosquitos, aes(x = time, y = fracao, colour = serie)) +
    geom_hline(yintercept = 1 - ctrl$eficacia, colour = VISUAL$cinza_fraco, linewidth = 0.4) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(name = NULL, values = cores) +
    escala_percentual(limits = c(0, 1.05)) +
    coord_cartesian(xlim = c(0, limite_dias)) +
    labs(title = "Mosquitos adultos", x = "Tempo (dias)",
         y = "Percentual da população inicial") +
    theme_epi(base_size = 11)

  (curvas | mosquitos) +
    plot_layout(widths = c(1.4, 1), guides = "collect") +
    plot_annotation(
      title = "Controle de criadouros no SEIR-SEI: o efeito chega com atraso",
      subtitle = sprintf("Redução de %s%% no nascimento de mosquitos, acima da eficácia crítica. Os infectados continuam crescendo nas duas semanas seguintes ao início",
                         num_br(100 * ctrl$eficacia, 0)),
      caption = paste(VISUAL$assinatura, "|", VISUAL$disciplina),
      theme = theme_epi()
    ) & theme(legend.position = "bottom")
}

# Mesma taxa de crescimento observada, dois modelos, dois R0 diferentes.
figura_crescimento <- function(serie, cresc, r0s) {
  fase <- serie[serie$semana >= cresc$inicio - 42 & serie$semana <= cresc$fim + 56, ]
  ajuste <- data.frame(semana = seq(cresc$inicio, cresc$fim, by = 7))
  ajuste$provaveis <- exp(cresc$intercepto + cresc$r * as.numeric(ajuste$semana))

  texto <- sprintf(
    "r = %s por dia, duplicação em %s dias\nSIR precisaria de R0 = %s (ataque de %s%%)\nSEIR-SEI precisaria de R0 = %s (ataque de %s%%)",
    num_br(cresc$r, 4), num_br(log(2) / cresc$r, 0),
    num_br(r0s[["SIR"]], 2), num_br(100 * tamanho_final_teorico(r0s[["SIR"]]), 0),
    num_br(r0s[["SEIR-SEI"]], 2), num_br(100 * tamanho_final_teorico(r0s[["SEIR-SEI"]]), 0))

  p <- ggplot(fase, aes(x = semana, y = provaveis)) +
    geom_point(colour = VISUAL$cor_dados, size = 1.8) +
    geom_line(data = ajuste, colour = VISUAL$cores[["I"]], linewidth = 1) +
    annotate("label", x = min(fase$semana), y = max(fase$provaveis), label = texto,
             hjust = 0, vjust = 1, size = 3, label.size = 0, fill = VISUAL$superficie,
             colour = VISUAL$cinza_texto, lineheight = 1.1) +
    escala_log_pessoas(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
    labs(title = "A taxa de crescimento observada no Recife e o R0 que cada modelo exige",
         subtitle = "Casos prováveis semanais em escala log. A reta é o ajuste log linear de setembro de 2015 a janeiro de 2016",
         x = NULL, y = "Casos prováveis por semana (escala log)") +
    theme_epi()

  sign_plot(p)
}

figura_duas_ondas <- function(serie, ondas) {
  ondas$rotulo <- sprintf("%s: crescimento de %s por dia, duplicação em %s dias",
                          ondas$onda, num_br(ondas$r, 4), num_br(ondas$duplicacao_dias, 0))
  ondas$de <- ondas$inicio - 56
  ondas$ate <- ondas$fim + 252

  pontos <- do.call(rbind, lapply(seq_len(nrow(ondas)), function(i) {
    z <- serie[serie$semana >= ondas$de[i] & serie$semana <= ondas$ate[i] & serie$provaveis > 0, ]
    data.frame(rotulo = ondas$rotulo[i], semana = z$semana, provaveis = z$provaveis)
  }))
  ajustes <- do.call(rbind, lapply(seq_len(nrow(ondas)), function(i) {
    semana <- seq(ondas$inicio[i], ondas$fim[i], by = 7)
    data.frame(rotulo = ondas$rotulo[i], semana = semana,
               provaveis = exp(ondas$intercepto[i] + ondas$r[i] * as.numeric(semana)))
  }))
  chuva <- do.call(rbind, lapply(seq_len(nrow(ondas)), function(i) {
    anos <- seq(as.integer(format(ondas$de[i], "%Y")), as.integer(format(ondas$ate[i], "%Y")))
    inicio <- as.Date(sprintf("%d-%02d-01", anos, QUADRA_CHUVOSA[["inicio"]]))
    fim <- as.Date(sprintf("%d-%02d-01", anos, QUADRA_CHUVOSA[["fim"]] + 1)) - 1
    data.frame(rotulo = ondas$rotulo[i], xmin = pmax(inicio, ondas$de[i]), xmax = pmin(fim, ondas$ate[i]))
  }))
  chuva <- chuva[chuva$xmax - chuva$xmin > 30, ]
  chuva$meio <- chuva$xmin + (chuva$xmax - chuva$xmin) / 2

  p <- ggplot(pontos, aes(x = semana, y = provaveis)) +
    # A faixa começa em 0,5, abaixo do menor valor plotado, que também é a borda do eixo.
    geom_rect(data = chuva, aes(xmin = xmin, xmax = xmax, ymin = 0.5, ymax = Inf),
              inherit.aes = FALSE, fill = VISUAL$sequencial[1], alpha = 0.7) +
    geom_text(data = chuva, aes(x = meio, y = 1.3, label = "abril a julho"), inherit.aes = FALSE,
              size = 2.9, colour = VISUAL$cinza_texto) +
    geom_point(colour = VISUAL$cor_dados, size = 1.6) +
    geom_line(data = ajustes, colour = VISUAL$cores[["I"]], linewidth = 1) +
    facet_wrap(~ rotulo, ncol = 1, scales = "free_x") +
    escala_log_pessoas(breaks = c(1, 3, 10, 30, 100, 300, 1000), expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y", expand = expansion(mult = 0.01)) +
    labs(title = "As duas ondas de chikungunya no Recife",
         subtitle = paste("Casos prováveis semanais em escala log. A reta é o ajuste log linear da fase de crescimento.",
                          "A faixa azul marca abril a julho, que concentram 59,8% da chuva anual", sep = "\n"),
         x = NULL, y = "Casos prováveis por semana (escala log)") +
    theme_epi()

  sign_plot(p, "Chuva: Ferreira (2016)")
}

figura_distritos <- function(distritos) {
  razao_cidade <- sum(distritos$segunda_onda) / sum(distritos$primeira_onda)
  distritos$nome <- factor(distritos$nome, levels = distritos$nome[order(distritos$razao)])
  distritos$casos <- sprintf("de %s para %s casos", milhares(distritos$primeira_onda),
                             milhares(distritos$segunda_onda))

  p <- ggplot(distritos, aes(x = razao, y = nome)) +
    geom_vline(xintercept = razao_cidade, colour = VISUAL$cor_neutra, linetype = "dashed",
               linewidth = 0.6) +
    annotate("text", x = razao_cidade, y = nrow(distritos) + 0.75,
             label = sprintf("razão da cidade, %s", num_br(razao_cidade, 1)),
             hjust = -0.05, size = 3, colour = VISUAL$cinza_texto) +
    geom_segment(aes(x = razao_cidade, xend = razao, yend = nome), colour = VISUAL$cor_neutra,
                 linewidth = 0.6) +
    geom_point(colour = VISUAL$cor_dados, size = 2.8) +
    geom_text(aes(x = 1.1 * max(razao), label = casos), hjust = 0, size = 3,
              colour = VISUAL$cinza_texto) +
    scale_x_continuous(labels = function(x) num_br(x, 1), limits = c(0, 1.45 * max(distritos$razao)),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
    labs(title = "Casos de 2021 sobre os da primeira onda, por distrito sanitário",
         subtitle = "Um modelo homogêneo prevê a mesma razão em todos os distritos, a da cidade",
         x = "Casos prováveis de 2021 divididos pelos de 2015 e 2016", y = NULL) +
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
