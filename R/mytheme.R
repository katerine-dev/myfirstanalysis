#' CRIANDO NOSSO TEMA BASE
#' Essa função devolverá um tema base para o projeto
#' @1º parâmetro: text
#' @2º parâmetro: colors
#' @retur: tema em todos os gŕaficos que incluir a função em questão
#' @export


tema_analise <- function() {
  theme(
    text = element_text(
      color = "#4ABDAC",
      size = 10),
    plot.title = element_text(
      colour = "#FC4A1A",
      hjust = 0.5,
      size = 20),
    panel.background = element_rect(fill = "#DFDCE3"),
    legend.position = "bottom")
}
