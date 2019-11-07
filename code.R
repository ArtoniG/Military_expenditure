library(tidyverse)

dados <- read.csv("~/Downloads/2sem19/ME415/dados/analise2/Military Expenditure.csv", header = TRUE, stringsAsFactors = FALSE)

# ARMAZENA O NÚMERO QUE PAÍSES QUE INVESTIRAM EM CADA ANO
no.paises <- apply(dados[,5:ncol(dados)], 2, function(x) table(is.na(x))[["FALSE"]])
#plot(no.paises, type = "l")

# ARMAZENA QUAIS FORAM O PAÍSES QUE INVESTIRAM EM CADA ANO
countries <- sapply(dados[,5:ncol(dados)], function(x) dados$Name[!is.na(x)])

# ARMAZENA OS PAÍSES QUE SEMPRE INVESTIRAM
always <- countries[[1]][countries[[1]] %in% countries[[2]]]
for(i in 3:length(countries)){
  always <- always[always %in% countries[[i]]]
}

# ARMAZENA O INVESTIMENTO ACUMULADO DOS PAÍSES
acum.country <- apply(dados[,5:ncol(dados)], 1, sum, na.rm = TRUE)
acum.country <- cbind(dados$Name, acumulate)

# ARMAZENA O INVESTIMENTOS ACUMULADO POR ANO
acum.year <- apply(dados[,5:ncol(dados)], 2, sum, na.rm = TRUE)