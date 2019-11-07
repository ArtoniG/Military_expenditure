library(tidyverse)

dados <- read.csv("~/Downloads/2sem19/ME415/dados/analise2/Military Expenditure.csv", header = TRUE, stringsAsFactors = FALSE)

# ARMAZENA O NÚMERO QUE PAÍSES QUE INVESTIRAM
no.paises <- apply(dados[,5:ncol(dados)], 2, function(x) table(is.na(x))[["FALSE"]])
plot(no.paises, type = "l")

# ARMAZENA QUAIS FORAM O PAÍSES QUE INVESTIRAM
countries <- apply(dados[,5:ncol(dados)], 2, function(x) dados$Name[!is.na(x)])
