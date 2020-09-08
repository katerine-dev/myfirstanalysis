# Vamos fazer nossa primeira análise de dados, e eu katerine vou tentar auxiliar vocês nessa jornada. 
# Nosso objetivo é impressionar dois prosutores de filmes e auxliar eles a identificar qual seria o melhor filme para esse ano para obter sucesso financeiro e 
# reconhecimento da crítica
install.packages("ggthemes")
install.packages("RColorBrewer")
## O nosso primeiro passo é importar os dados, no meu caso minha base se encontrava na pasta dados no formato `.rds`. 
# Importanto base 
imdb <- readr::read_rds("dados/imdb.rds")

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(pagedown)
glimpse(imdb)
## Usamos essa função do pacote `tidyverse` para visualisar todas as informações da base, ela mostra as suas váriaveis e suas características. 
## Com essas informações será mais fácil chegar tomar uma decisão na sua analise. 
## Portanto temos que observar os seguintes pontos quais são as melhores notas, melhores atores (eles estão relacionados), melhores diretores
# e o genêro que mais teve notas altas. 

# CRIANDO NOSSO TEMA BASE 

tema_analise <- function() {
  theme(
    text = element_text(
      color = "#4ABDAC",
      size = 16),
    plot.title = element_text(
      family = "Sans Serif",
      colour = "#FC4A1A",
      hjust = 0.5,
      size = 25),
    panel.background = element_rect(fill = "#DFDCE3"),
    legend.position = "bottom")
}
#da base recebida podemos indicar aqueles que apresentaram lucro ou não 

imdb <- imdb %>% mutate(lucro = receita - orcamento)

imdb %>%
  filter(!is.na(lucro)) %>% 
  mutate(lucrou = ifelse(lucro <= 0, "Não", "Sim")) %>% 
  ggplot() +
  geom_point(aes(x = orcamento, y = receita, color = lucrou)) +
  labs(
    title = "Houve lucro ou não?",
    subtitle = "Receita vs Orçamento")+
  scale_x_continuous(labels = scales::dollar)+ 
  tema_analise()+
  scale_colour_brewer(palette = "Set2")


# Vamos fazer um gráfico de dispersão da lucro maior de 50000 em filmes com nota imdb > 8 x orçamento
## É considerado melhores notas aqueles filmes que tem nota acima de 8 
imdb %>% 
  filter(receita - orcamento > 5000000 & nota_imdb > 8) %>% 
  group_by(orcamento, nota_imdb, receita) %>% 
  ggplot() +
  geom_point(aes(x = orcamento, y = receita), colour = "#E24E42", size = 2) +
  labs(
    title = "Lucro das melhores notas",
    subtitle = "Lucro maior de 5000000 em filmes com nota imdb maiores de 8 vs Orçamento") +
  tema_analise() 
  
#fazer um gráfico com os top 5 diretores 

top_5_diretores <- imdb %>% 
  count(diretor) %>%
  filter(!is.na(diretor)) %>% 
  top_n(5, n)  

top_5_diretores %>%
  mutate(diretor = forcats::fct_reorder(diretor, n)) %>% 
  ggplot() +
  geom_col(
    aes(x = n, y = diretor, fill = diretor), 
    show.legend = FALSE) +
  geom_label(aes(x = n/2, y = diretor, label = n)) +
  labs(
    x = "numero de filmes",
    y = "Diretores ($)",
    title = "Os melhores Diretores")+
scale_fill_manual(values = c("#F781D8", "#FF8000", "#00FF00", "#819FF7", "#FE2E2E")) +
  tema_analise()

# 2. Calcule o lucro médio dos filmes com duracao 
# menor que 60 minutos. 

imdb %>% 
  mutate(
    lucro = orcamento - receita
  ) %>% 
  mutate(tipo_duracao = ifelse(duracao < 60, "menos de 1h","mais de 1h"), na.rm = TRUE) %>% 
  group_by(tipo_duracao, receita, lucro) %>% 
  summarise(
    duracao_media = mean(duracao, na.rm = TRUE),
    lucro_medio = mean(lucro, na.rm = TRUE)
  ) %>% 
  geom_vline()+
  ggplot(aes(x = duracao_media , y = tipo_duracao, color = receita))+
  geom_boxplot()+
  tema_analise()

