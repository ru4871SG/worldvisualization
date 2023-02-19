library(tidyverse)

education_vs_income_full <- read.csv("education_vs_income_full.csv")

cor.test(education_vs_income_full$per_capita_income, education_vs_income_full$bachelor_degree)
