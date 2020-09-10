# CRIANDO NOSSO TEMA BASE

tema_analise <- function() {
  theme(
    text = element_text(
      color = "#4ABDAC",
      size = 16),
    plot.title = element_text(
      colour = "#FC4A1A",
      hjust = 0.5,
      size = 25),
    panel.background = element_rect(fill = "#DFDCE3"),
    legend.position = "bottom")
}
