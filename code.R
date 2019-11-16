#https://pt.overleaf.com/project/5dcdb71eb9d4d8000112bf57
library(readr)
library(GGally)
library(ggpubr)
library(tidyverse)
library(nortest)
library(car)
library(MASS)
library(gvlma)
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

dados <- dados[-c(56, 30, 117, 130, 152, 168, 97, 7),] # identificados pelas funções

# REPETINDO OS AJUSTES, SÓ QUE DESSA VEZ SEM OS VALORES INFLUENTES, OUTLIERS, ETC
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

# AJUSTE DE MODELO COM INTERAÇÃO COM A VARIÁVEL EAST
model.int <- lm(Price ~ Food.c + Decor.c + Service.c + East + Food.c:East + Decor.c:East + Service.c:East, dados)
summary(model.int)

estimate_tibble(model.int)
normal_diag(model.int)
cook_hat(model.int)

# TESTA SE OS EFEITOS DAS VARIÁVEIS EXPLICATIVAS DEPENDEM
# DA VARIÁVEL DUMMY

anova(model.red, model.int)

##################################################################################
# FUNÇÕES PARA ANÁLISE DOS RESÍDUOS
# PRECISA DOS PACOTES car, MASS e gvlma

# outliers
outlierTest(model)
qqPlot(model, main = "QQ Plot")
leveragePlots(model)

# observações influentes
avPlots(model)
cutoff <- 4/(nrow(dados)-length(model$coefficients)-2)
plot(model, which = 4, cook.levels = cutoff)
influencePlot(model, id = "identify", main = "Influence plot", sub = "Tamanho dos círculos são proporcionais à distância de Cook")

# não normalidade
qqPlot(model, main = "QQ Plot")
stud.res <- studres(model)
hist(stud.res, freq = FALSE, main = "Distribution of Studentized Residuals")
xfit <- seq(min(stud.res), max(stud.res), length.out = 40)
yfit <- dnorm(xfit)
lines(xfit,yfit)

# heterocedásticidade
ncvTest(model)
spreadLevelPlot(model)

# multicolinearidade
crPlots(model)
ceresPlots(model)

# dependencia dos erros
durbinWatsonTest(model)

# ajuda adicional no diagnóstico
gvmodel <- gvlma(model)
summary(gvmodel)