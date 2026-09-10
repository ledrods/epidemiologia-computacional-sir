# Camada visual. Tema, escalas, assinatura e gravação ficam definidos uma única
# vez aqui, de modo que nenhuma figura precise repetir decisão de layout.

suppressPackageStartupMessages(suppressWarnings(library(ggplot2)))
suppressPackageStartupMessages(suppressWarnings(library(scales)))
suppressPackageStartupMessages(suppressWarnings(library(patchwork)))

milhares <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)

theme_epi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = VISUAL$superficie, colour = NA),
      panel.background = element_rect(fill = VISUAL$superficie, colour = NA),
      panel.grid.major = element_line(colour = VISUAL$cinza_grade, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = VISUAL$cinza_texto),
      axis.title = element_text(colour = VISUAL$cinza_texto),
      plot.title = element_text(face = "bold", size = base_size * 1.2, colour = "#0b0b0b"),
      plot.subtitle = element_text(colour = VISUAL$cinza_texto, size = base_size * 0.9),
      plot.caption = element_text(colour = VISUAL$cinza_fraco, size = base_size * 0.7, hjust = 1),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.justification = "left",
      legend.key = element_blank(),
      legend.title = element_text(colour = VISUAL$cinza_texto, size = base_size * 0.85),
      legend.text = element_text(colour = VISUAL$cinza_texto, size = base_size * 0.85),
      strip.text = element_text(colour = "#0b0b0b", face = "bold", size = base_size * 0.85),
      plot.margin = margin(14, 18, 10, 14)
    )
}

scale_colour_sir <- function() {
  scale_colour_manual(
    name = "Compartimento",
    values = setNames(unname(VISUAL$cores), unname(VISUAL$rotulos))
  )
}

escala_pessoas <- function(...) scale_y_continuous(labels = milhares, ...)
escala_dias <- function(...) scale_x_continuous(expand = expansion(mult = c(0.01, 0.06)), ...)

# Decorator: recebe um ggplot pronto e devolve o mesmo gráfico assinado.
sign_plot <- function(p, nota = NULL) {
  credito <- paste(VISUAL$assinatura, "|", VISUAL$disciplina)
  if (!is.null(nota)) credito <- paste(nota, "|", credito)
  p + labs(caption = credito)
}

# Template das figuras de série temporal: eixos, escalas, tema e assinatura
# fixos; cada figura fornece apenas os dados e os textos.
epi_plot <- function(dados, titulo, subtitulo = NULL, nota = NULL,
                     mapeamento = aes(x = time, y = pessoas, colour = compartimento)) {
  p <- ggplot(dados, mapeamento) +
    geom_line(linewidth = 0.9) +
    scale_colour_sir() +
    escala_dias() +
    escala_pessoas() +
    labs(title = titulo, subtitle = subtitulo,
         x = "Tempo (dias)", y = "Número de pessoas") +
    theme_epi()
  sign_plot(p, nota)
}

# Ajusta o dispositivo gráfico do IRkernel para que a figura exibida no notebook
# tenha a mesma proporção do PNG gravado.
tamanho_figura <- function(largura = VISUAL$largura_pol, altura = VISUAL$altura_pol) {
  options(repr.plot.width = largura, repr.plot.height = altura, repr.plot.res = 140)
  invisible(NULL)
}

save_figure <- function(p, arquivo,
                        largura = VISUAL$largura_pol,
                        altura = VISUAL$altura_pol) {
  caminho <- file.path(DIRS$figures, arquivo)
  ggsave(caminho, plot = p, width = largura, height = altura,
         dpi = VISUAL$dpi, bg = VISUAL$superficie, type = "cairo")
  caminho
}
