library(tidyverse)
library(mirt)
source("utils.R")

load("Constructs_ASD.RData")

# merge all data
out = mymerge(Constructs)
df = out$df; colidx = out$col.indx

# remove na
df_nona = na.omit(df)

# filter age ----
df_nona = df_nona %>% dplyr::filter(scq_age_at_eval_years >= 5)

# filter missing codes:
df_nona = df_nona %>% dplyr::filter(
  combined_phrases_age_mos < 100 &
    combined_words_age_mos < 100 &
    used_words_age_mos < 100 
)

# Build model ----
df_model = df_nona[-colidx$Covariates]
df_model = data.frame(lapply(df_model, as.numeric))
df_cov = df_nona[colidx$Covariates[-1]]
df_cov = data.frame(lapply(df_cov, as.numeric))
model_str = sprintf(
   "GL = %s
    Msport = %s
    Mfine = %s
    BCD = %s
    NV = %s
    Aloof = %s
    AL = %s
    RRB = %s
    SOC = %s
    COV = GL*Msport*Mfine*BCD*NV*Aloof*AL*RRB*SOC", 
  paste(colidx$GeneralLanguage[-1]-1, collapse=","),
  paste(colidx$MotorSport[-1]-1, collapse=","),
  paste(colidx$MotorFine[-1]-1, collapse=","),
  paste(colidx$BCD[-1]-1, collapse=","),
  paste(colidx$NVLanguage[-1]-1, collapse=","),
  paste(colidx$Aloofness[-1]-1, collapse=","),
  paste(colidx$ASDLanguage[-1]-1, collapse=","),
  paste(colidx$RRB[-1]-1, collapse=","),
  paste(colidx$SocialSkill[-1]-1, collapse=",")
)

# fit.model = mirt(df_model, model_str, covdata=df_cov, formula=covs, method="QMCEM")
set.seed(20211219)
fit.model = mirt(df_model, model_str, method="MHRM")
save(fit.model, file="irt_fit_asd.RData")

# load("irt_fit_asd.RData")

# Get scores ----
COVAR = coef(fit.model)$GroupPars[9:44]
names(COVAR) = colnames(coef(fit.model)$GroupPars)[9:44]
cov_fit_mat = matrix(NA, 8, 8)
for(i in 1:8){
  for(j in i:8){
    cov_fit_mat[i,j] = COVAR[paste0("COV_",j,i)]
    cov_fit_mat[j,i] = COVAR[paste0("COV_",j,i)]
  }
}
cov_fit_mat

set.seed(20211219)
scores = fscores(fit.model, QMC=TRUE)
# save(scores, file="7_construct_scores_asd.RData")


# build data-frame

df_scores = data.frame(
  subject_sp_id = df_nona$subject_sp_id,
  sex = df_nona$sex,
  asd = df_nona$asd,
  age_eval_scq = df_nona$scq_age_at_eval_years,
  age_eval_dcdq = df_nona$dcdq_age_at_eval_years,
  age_eval_rbsr = df_nona$rbsr_age_at_eval_years,
  imp_item1 = df_nona$q35_make_believe,
  imp_item2 = df_nona$q39_imaginative_games,
  imi_item1 = df_nona$q21_copy_you,
  imi_item2 = df_nona$q34_copy_actions,
  GL = scores[,"GL"],
  MS = scores[,"Msport"],
  MF = scores[,"Mfine"],
  BCD = scores[,"BCD"],
  NV = scores[,"NV"],
  Aloof = scores[,"Aloof"],
  AL = scores[,"AL"],
  RRB = scores[,"RRB"],
  SOC = scores[,"SOC"]
)

# summary imaginative play and imitation as PC1
df_scores = df_scores %>% mutate(
  IMP = princomp(cbind(imp_item1, imp_item2))$scores[,1],
  IMI = princomp(cbind(imi_item1, imi_item2))$scores[,1],
  asd = as.numeric(asd),
  age_eval_scq = as.numeric(age_eval_scq)
) %>% dplyr::select(-imp_item1, -imp_item2, -imi_item1, -imi_item2)

save(df_scores, file="asd_constructs_scores.RData")
