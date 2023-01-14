library(tidyverse)
library(lavaan)
source("utils.R")


## symbolism model
sym_model = "SYM =~ GL + NV + IMP"


## Finer constructs
set.seed(54321)
load("asd_constructs_scores.RData")

pairs(df_scores %>% dplyr::select(GL, NV, IMP))

fit0 = cfa(sym_model, data=df_scores %>% dplyr::select(GL, NV, IMP))
summary(fit0, fit.measures=TRUE, rsquare=TRUE)

df_scores$SYM = c(predict(fit0))

save(df_scores, file="asd_constructs_scores.RData")


## Coarser constructs
set.seed(54321)
load("all_constructs_scores.RData")

pairs(df_scores %>% dplyr::select(GL, NV, IMP))

fit1 = cfa(sym_model, data=df_scores %>% dplyr::select(GL, NV, IMP))
summary(fit1, fit.measures=TRUE, rsquare=TRUE)

df_scores$SYM = c(predict(fit1))

save(df_scores, file="all_constructs_scores.RData")
