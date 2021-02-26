# preparacao

library(tidyverse)
theme_set(theme_bw())
library(lubridate)

# leitura dos dados

dados <- 
  read.csv(file = "data-mfp.csv") %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= ymd("2020-02-26"))

# grafico

ggplot(dados, aes(x = Date, y = Weight)) +
  geom_line() +
  # academia
  annotate("text", x = ymd("2020-03-18"), y = 76.9, label = "Academia fecha") +
  geom_segment(aes(x = ymd("2020-03-18"), y = 74.6, xend = ymd("2020-03-18"), yend = 76.7), col = "red") +
  # corrida
  annotate("text", x = ymd("2020-10-12"), y = 73.7, label = "Corridas na rua\ncom mais de 4km") +
  geom_segment(aes(x = ymd("2020-10-12"), y = 75.9, xend = ymd("2020-10-12"), yend = 74), col = "red") +
  # nutricionista
  annotate("text", x = ymd("2021-01-15"), y = 76.9, label = "Vou à Nutricionista") +
  geom_segment(aes(x = ymd("2021-01-15"), y = 76.2, xend = ymd("2021-01-15"), yend = 76.7), col = "red")
