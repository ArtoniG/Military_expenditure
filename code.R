#https://pt.overleaf.com/project/5dcdb71eb9d4d8000112bf57
library(readr)
library(GGally)
library(ggpubr)
library(tidyverse)
library(nortest)
source("~/Public/trabaioME613/norm_diag.R")
source("~/Public/trabaioME613/model_measures.R")
source("~/Public/trabaioME613/estimate_table.R")
source("~/Public/trabaioME613/cook_hat.R")

dados <- read_csv("~/Downloads/Books/data/a_modern_approach_to_regression_with_r/nyc.csv", col_names = TRUE)
attach(dados)

# MEDIDAS RESUMO DOS DADOS
summary(dados)
apply(dados[,3:6], 2, sd)
combn(c(3,4,5,6), 2, function(i) cor(dados[,i[1]], dados[,i[2]], method = "spearman"), simplify = TRUE)

Regions <- c()
Regions[East == 1] <- "Leste"
Regions[East == 0] <- "Oeste"
ggboxplot(cbind(dados, Regions), x = "Regions", y = "Price", color = "Regions", palette = c("#00AFBB", "#E7B800"))
ggboxplot(cbind(dados, Regions), x = "Regions", y = "Food", color = "Regions", palette = c("#00AFBB", "#E7B800"))
ggboxplot(cbind(dados, Regions), x = "Regions", y = "Decor", color = "Regions", palette = c("#00AFBB", "#E7B800"))
ggboxplot(cbind(dados, Regions), x = "Regions", y = "Service", color = "Regions", palette = c("#00AFBB", "#E7B800"))

# MEDIDAS RESUMOS SEPARADAS POR REGIÕES DA CIDADE
by(dados, East, summary)
apply(filter(dados[,3:6], East == 1), 2, sd)
apply(filter(dados[,3:6], East == 0), 2, sd)

# GRÁFICOS DAS DISTRIBUIÇÕES AMOSTRAIS E CORRELAÇÕES
ggpairs(dados[,3:6])
ggpairs(filter(dados[,3:6], East == 1))
ggpairs(filter(dados[,3:6], East == 0))

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

t.test(filter(dados, East == 1)$Price, filter(dados, East == 0)$Price) #dif medias !=0 (Rejeita H0)
t.test(filter(dados, East == 1)$Food, filter(dados, East == 0)$Food) #dif medias != 0 (Rejeita H0)
t.test(filter(dados, East == 1)$Decor, filter(dados, East == 0)$Decor)#dif medias = 0 (Não Rejeita H0)
t.test(filter(dados, East == 1)$Service, filter(dados, East == 0)$Service)#dif medias != 0 (Rejeita H0)

# AJUSTE DE MODELO NORMAL, INDEPENDENTE E HOMOCEDÁSTICO COM AS VARIÁVEIS CENTRALIZADAS NA MÉDIA
dados <- cbind(dados, Food.c = dados$Food - mean(dados$Food),
                Service.c = dados$Service - mean(dados$Service),
                Decor.c = dados$Decor - mean(dados$Decor))
model <- lm(Price ~ Food.c + Service.c + Decor.c + East, dados)
summary(model)

estimate_tibble(model)
normal_diag(model)
cook_hat(model)

# AJUSTE DE MODELO SEM A VARIÁVEL SERVICE
model.red <- lm(Price ~ Food.c + Decor.c + East, dados)
summary(model.red)

estimate_tibble(model.red)
normal_diag(model.red)
cook_hat(model.red)

dados <- dados[-c(56, 30, 109, 141, 83, 130, 165, 103, 48),]

# REPETINDO OS AJUSTES, SÓ QUE DESSA VEZ SEM OS VALORES INFLUENTES
# MODELO COMPLETO
model <- lm(Price ~ Food.c + Service.c + Decor.c + East, dados)
summary(model)

estimate_tibble(model)
normal_diag(model)
cook_hat(model)

# MODELO SEM A VARIÁVEL SERVICE
model.red <- lm(Price ~ Food.c + Decor.c + East, dados)
summary(model.red)

estimate_tibble(model.red)
normal_diag(model.red)
cook_hat(model.red)

# AJUSTE DE MODELO SEM A VARIÁVEL EAST
model.red <- lm(Price ~ Food.c + Decor.c, dados)
summary(model.red)

estimate_tibble(model.red)
normal_diag(model.red)
cook_hat(model.red)
