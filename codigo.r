library(ggplot2)
library(forecast)

# leitura de dados

dados <- read.table(file="~/Documents/Research/Miscelania/MyFitnessPal/data.csv", sep=",", header=TRUE)

# converte sequência de caracteres para data

dados$Date <- as.Date(dados$Date)
n          <- length(dados$Date)
dados$App  <- seq(dados$Weight[1], by=-.5/7, length.out=n)

# grafico

par(cex=1.5)

p <- ggplot(data=dados, aes(x=Date)) + xlab("Data") + ylab("Peso (kg)") +
geom_line(aes(y=Weight, colour="Peso Real")) +
geom_smooth(aes(y=Weight, colour="Tendência"), method=loess, se=FALSE, span=7) +
geom_line(aes(y=App, colour="App")) +
scale_colour_manual("", breaks = c("App", "Peso Real", "Tendência"), values = c("lightblue", "black", "blue"))

p

# analise usando regressao 
# (sim, eu violo as hipoteses da regressao linear ao nao ter independencia entre as minhas observacoes)

summary(ajuste.lm <- lm(Weight~Date, data=dados))

round(ajuste.lm$coefficients[2], 4)

par(mfrow=c(2,2))

plot(ajuste.lm)

# analise usando series temporais

ajuste.arima <- auto.arima(dados$Weight, max.p=10, max.q=10, max.d=3, seasonal=FALSE, ic="aicc")

tsdiag(ajuste.arima)

plot(forecast(ajuste.arima, h=20))
