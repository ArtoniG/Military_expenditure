library(tidyverse)

dados <- read.csv("~/Downloads/2sem19/ME415/dados/analise2/Military Expenditure.csv", header = TRUE, stringsAsFactors = FALSE)

no.países <- apply(dados[,5:ncol(dados)], 2, function(x) table(is.na(x))[["FALSE"]])
plot(no.países, type = "l")