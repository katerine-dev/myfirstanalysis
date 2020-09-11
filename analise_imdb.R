# Carregar Pacotes


library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)



# Importanto base ----
imdb <- readr::read_rds("data_raw/imdb.rds")


## Visualizar base

glimpse(imdb)

# Manipulação e Criação de base ----


# tabela com top 10 de filmes - 1ª tabela

imdb %>%
  mutate(lucro = receita - orcamento) %>%
  top_n(10, lucro) %>%
  arrange(desc(lucro)) %>%
  mutate(
    pos = 1:10,
    lucro = scales::dollar(lucro)
  ) %>%
  select(`Posição` = pos, Filme = titulo, Lucro = lucro)

# apresentaram lucro ou não - 1ª Gráfico

imdb <- imdb %>% mutate(lucro = receita - orcamento)

imdb %>%
  filter(!is.na(lucro)) %>%
  mutate(lucrou = ifelse(lucro <= 0, "Não", "Sim")) %>%
  ggplot() +
  geom_point(aes(x = orcamento, y = receita, color = lucrou)) +
  labs(
    title = "Houve Lucro ou Não?",
    subtitle = "Receita vs Orçamento")+
  scale_x_continuous(labels = scales::dollar)+
  scale_y_continuous(labels = scales::dollar)+
  tema_analise()+
  scale_colour_brewer(palette = "Set2")


#  lucro maior de 50000 em filmes com nota imdb > 8 x orçamento - 2ª Gráfico

imdb %>%
  filter(receita - orcamento > 5000000 & nota_imdb > 8) %>%
  group_by(orcamento, nota_imdb, receita) %>%
  ggplot() +
  geom_point(aes(x = orcamento, y = receita), colour = "#E24E42", size = 2) +
  labs(
    title = "Lucro das Melhores Notas",
    subtitle = "Lucro maior de 5000000 em filmes com nota imdb maiores de 8 vs Orçamento") +
  scale_x_continuous(labels = scales::dollar)+
  scale_y_continuous(labels = scales::dollar)+
  tema_analise()



# fazer um gráfico com os top 5 diretores 3ª Gráfico

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
    x = "Numero de filmes",
    y = "Diretores",
    title = "Os Melhores Diretores")+
scale_fill_manual(values = c("#F781D8", "#FF8000", "#00FF00", "#819FF7", "#FE2E2E")) +
  tema_analise()

#LUCRO  - 4ª gráfico
imdb %>%
  filter(!is.na(diretor)) %>%
  group_by(diretor) %>%
  filter(n() >= 15) %>%
  ggplot() +
  geom_boxplot(aes(x = diretor, y = lucro), color = "#E24E42")+
  labs(
    title = "Lucro dos diretores",
    subtitle = "Lucro vs Diretor")+
  scale_y_continuous(labels = scales::dollar)+
  tema_analise()


# filmes com duracao - menor que 120 minutos. 5º gráfico

  imdb %>%
    mutate(tipo_duracao = ifelse(duracao < 120, "menos de 2h","mais de 2h")) %>%
    filter(!is.na(tipo_duracao)) %>%
    count(tipo_duracao) %>%
    ggplot()+
  geom_col(aes(x = tipo_duracao, y = n), colour = "#819FF7", fill = "#819FF7")+
      labs(
      title = "Filmes com Duração de 2 Horas")+
      tema_analise()

# TOP Atores - 3ª tabela


 top_atores <- imdb %>%
      pivot_longer(
        cols = starts_with("ator"),
        names_to = "posicao",
        values_to = "ator") %>%
      select(ator, nota_imdb) %>%
        filter(nota_imdb >= 8) %>%
   arrange(desc(nota_imdb))


# Média da nota dos atores - 3º tabela

top_atores %>%
   group_by(ator, nota_imdb) %>%
   nest() %>%
    mutate(media_das_notas = mean(nota_imdb)) %>%
    top_n(5, n)



# Generos mais badalados- 4ª tabela

    top_5_generos <- imdb %>%
      count(generos) %>%
      filter(!is.na(generos)) %>%
      top_n(5, n)


# media de receita no genero comedia - gráfico 6º


  top_comedia <- imdb %>%
      filter(generos %in% "Comedy")


 top_comedia %>%
      group_by(receita, orcamento) %>%
      ggplot()+
      geom_point(aes(x = orcamento, y = receita/2), colour = "#E24E42", size = 2)+
     labs(
     title = "Média de Lucro dos Filmes de Comédia")+
      scale_x_continuous(labels = scales::dollar)+
      scale_y_continuous(labels = scales::dollar)+
      geom_hline(yintercept = mean((top_comedia$receita - top_comedia$orcamento), na.rm = TRUE), colour = "#819FF7", size = 1)+
   tema_analise()












