library(readr)
library(GGally)
library(ggpubr)
library(tidyverse)
source("~/Public/trabaioME613/norm_diag.R")
source("~/Public/trabaioME613/model_measures.R")
source("~/Public/trabaioME613/estimate_table.R")
source("~/Public/trabaioME613/envel_norm.R")

dados <- read_csv("~/Downloads/Books/data/a_modern_approach_to_regression_with_r/nyc.csv", col_names = TRUE)
attach(dados)

# MEDIDAS RESUMO DOS DADOS
summary(dados)
apply(dados[,3:6], 2, sd)

# MEDIDAS RESUMOS SEPARADAS POR REGIÕES DA CIDADE
by(dados, East, summary)
by(dados[,3:7], East, sd)

# GRÁFICOS DAS DISTRIBUIÇÕES AMOSTRAIS E CORRELAÇÕES
ggpairs(dados[,3:6])

# TESTES DE NORMALIDADE SHAPIRO-WILK 
shapiro.test(filter(dados, East == 1)$Price) # Normal
shapiro.test(filter(dados, East == 1)$Food) # Anormal
shapiro.test(filter(dados, East == 1)$Decor) # Anormal
shapiro.test(filter(dados, East == 1)$Service) # Anormal
shapiro.test(filter(dados, East == 0)$Price) # Normal
shapiro.test(filter(dados, East == 0)$Food) # Normal
shapiro.test(filter(dados, East == 0)$Decor) # Anormal
shapiro.test(filter(dados, East == 0)$Service) # Normal

# TESTE DE HOMOCEDÁSTICIDADE
var.test(filter(dados, East == 1)$Price, filter(dados, East == 0)$Price) # Igual
var.test(filter(dados, East == 1)$Food, filter(dados, East == 0)$Food) # Igual
var.test(filter(dados, East == 1)$Decor, filter(dados, East == 0)$Decor) # Diferente
var.test(filter(dados, East == 1)$Service, filter(dados, East == 0)$Service) # Igual

# TESTES DE COMPARAÇÕES DE MÉDIAS
t.test()

t.test(filter(dados, East == 1)$Price, filter(dados, East == 0)$Price) #dif medias !=0 (Rejeita H0)
t.test(filter(dados, East == 1)$Food, filter(dados, East == 0)$Food) #dif medias != 0 (Rejeita H0)
t.test(filter(dados, East == 1)$Decor, filter(dados, East == 0)$Decor)#dif medias = 0 (Não Rejeita H0)
t.test(filter(dados, East == 1)$Service, filter(dados, East == 0)$Service)#dif medias != 0 (Rejeita H0)
