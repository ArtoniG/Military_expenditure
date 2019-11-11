library(readr)
library(ggplot2)

dados <- read_csv("~/Downloads/Books/data/a_modern_approach_to_regression_with_r/nyc.csv", col_names = TRUE)

attach(dados)

summary(dados)

by(dados, East, summary)
