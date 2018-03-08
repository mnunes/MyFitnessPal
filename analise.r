library(ggplot2)
library(dplyr)
library(forecast)
library(lubridate)
library(wesanderson)
library(scales)
library(zoo)

# leitura de dados

dados <- as_data_frame(read.table(file="data.csv", sep=",", header=TRUE))
dados <- dados %>%
  select(-Calories)

dim(dados)

str(dados)

dados$Date           <- ymd(dados$Date)
dados$Year           <- year(dados$Date)
dados$Month          <- month(dados$Date)
dados$Day            <- day(dados$Date)
dados$GrupoDiaSemana <- wday(dados$Date, label=TRUE)
dados$GrupoMes       <- ymd(paste(dados$Year, dados$Month, "01", sep="-"))

# grafico

g1 <- ggplot(data=dados, aes(x=Date)) + 
  labs(x="Data", y="Peso (kg)") + 
  geom_line(aes(y=Weight, colour="Peso Real")) + 
  geom_line(aes(y=rollmean(Weight, 30, fill=NA), , colour="MM 30 Dias")) +
  geom_line(aes(y=rollmean(Weight, 90, fill=NA), , colour="MM 90 Dias")) +
  scale_colour_manual("", values = wes_palette("Zissou")[c(3, 5, 1)]) + 
  scale_y_continuous(breaks = round(seq(floor(min(dados$Weight)), ceiling(max(dados$Weight)), by=1), 1), limits=c(min(dados$Weight), max(dados$Weight))) + 
  scale_x_date(breaks=seq(min(dados$Date), max(dados$Date), by="2 month"), date_labels="%b/%Y", minor_breaks=seq(min(dados$Date), max(dados$Date), by="2 month")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g1

pdf("grafico01.pdf")
g1
dev.off()

# analise a cada mes

g2 <- ggplot(data=dados, aes(x=GrupoMes, y=Weight, group=GrupoMes, fill=as.factor(GrupoMes))) +
  labs(x="Meses", y="Peso (kg)") + 
  geom_boxplot() + 
  scale_fill_manual("Meses", values=rep(wes_palette(5, name="Zissou"), 20)[1:length(unique(dados$GrupoMes))]) +
  scale_y_continuous(breaks = round(seq(floor(min(dados$Weight)), ceiling(max(dados$Weight)), by=1), 1), limits=c(min(dados$Weight), max(dados$Weight))) + 
  scale_x_date(breaks=seq(min(dados$Date), max(dados$Date), by="1 month"), date_labels="%b/%Y", minor_breaks=seq(min(dados$Date), max(dados$Date), by="1 month")) +
  theme_bw() +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g2

pdf("grafico02.pdf")
g2
dev.off()

# analise a cada dia da semana

ggplot(data=dados, aes(x=GrupoDiaSemana, y=Weight, group=GrupoDiaSemana, fill=GrupoDiaSemana)) +
  labs(x="Dia da Semana", y="Peso (kg)") + 
  geom_boxplot() + 
  scale_fill_manual("Dia da Semana", values=rep(wes_palette(3, name="Royal1"), 20)[1:length(unique(dados$GrupoDiaSemana))]) +
  scale_y_continuous(breaks = round(seq(floor(min(dados$Weight)), ceiling(max(dados$Weight)), by=1), 1), limits=c(min(dados$Weight), max(dados$Weight))) + 
  guides(fill=FALSE) +
  theme_bw()

# descritivas

dados %>%
  group_by(Year, Month) %>%
  summarise(Mediana=median(Weight), Média=mean(Weight)) %>%
  mutate(Data=ymd(paste(Year, Month, 01, sep="-"))) %>%
  select(Data, Média, Mediana) %>%
  print(n=Inf)

dados %>%
  arrange(Weight) %>%
  select(Day, Month, Year, Weight, GrupoDiaSemana) %>%
  print(n=20)

dados %>%
  arrange(Weight) %>%
  select(Day, Month, Year, Weight, GrupoDiaSemana) %>%
  print(n=20) %>%
  filter(Weight < 71)

dados %>%
  arrange(Weight) %>%
  head(n=20) %>%
  group_by(Year) %>%
  count()
  
dados %>%
  arrange(Weight) %>%
  head(n=50) %>%
  count(GrupoDiaSemana) %>%
  arrange(desc(n))

dados %>%
  filter(Year==2017) %>%
  arrange(Weight) %>%
  head(n=30) %>%
  count(GrupoDiaSemana) %>%
  arrange(desc(n))



#######################################
### analise usando series temporais ###
#######################################

# arima - melhorar visualização

ajuste.arima <- auto.arima(dados$Weight, max.p=10, max.q=10, max.d=3, seasonal=TRUE, ic="aicc")

tsdiag(ajuste.arima)

plot(forecast(ajuste.arima, h=20))

