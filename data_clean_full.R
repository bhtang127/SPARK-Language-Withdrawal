# load packages ----
library(tidyverse)
library(corrplot)
source("utils.R")


# load data ----
load("items_full_data.RData")


# Construct specification ----

## General language -- from background history
GL_child = full_data$data[[5]] %>% dplyr::select(
  subject_sp_id,
  combined_phrases_age_mos, combined_words_age_mos, used_words_age_mos
)
GL_sb = full_data$data[[6]] %>% 
  dplyr::filter(!(subject_sp_id %in% full_data$data[[5]]$subject_sp_id)) %>%
  dplyr::select(
    subject_sp_id,
    combined_phrases_age_mos, combined_words_age_mos, used_words_age_mos
)
# GL_sb$language_age_level = NA
GeneralLanguage = rbind(GL_child, GL_sb)


# Motor -- from dcdq, scq, basic medical screen
Motor_bms = full_data$data[[3]] %>%
  dplyr::select(subject_sp_id, dev_motor) 
  # %>% dplyr::rename(age_at_eval_years_bms = age_at_eval_years)
Motor_bhc = full_data$data[[5]] %>%
  dplyr::select(subject_sp_id, sat_wo_support_age_mos, walked_age_mos)
Motor_bhs = full_data$data[[6]] %>% dplyr::filter(!(subject_sp_id %in% full_data$data[[5]]$subject_sp_id)) %>%
  dplyr::select(subject_sp_id, sat_wo_support_age_mos, walked_age_mos)
Motor_bgh = rbind(Motor_bhc, Motor_bhs)

Motor = merge(Motor_bms, Motor_bgh, by="subject_sp_id")

# SocialSkill -- from scq
SocialSkill = full_data$data[[4]] %>%
  dplyr::select(
    subject_sp_id, q30_join_enjoyment, q29_share, q20_talk_friendly, 
    q36_same_age, q40_cooperatively_games
  )

# Imitation -- from scq
Imitation = full_data$data[[4]] %>%
  dplyr::select(subject_sp_id, q34_copy_actions, q21_copy_you)

# BCD -- from scq, rbsr, bgh, bms
# BCD = full_data$data[[3]] %>%
#   dplyr::select(subject_sp_id, dev_ld, dev_id)
# remove learning disability
BCD = full_data$data[[3]] %>%
  dplyr::select(subject_sp_id, dev_id)

# NVLanguage -- from scq
NVLanguage = full_data$data[[4]] %>% dplyr::select(
  subject_sp_id, q33_range_expressions,
  q34_copy_actions, q27_smile_back, q32_help_attention, 
  q22_point_things, q21_copy_you, q25_shake_head,
  q09_expressions_appropriate
)

# Aloofness -- from scq, bgh
Aloofness = full_data$data[[4]] %>% dplyr::select(
  subject_sp_id, q30_join_enjoyment,
  q28_things_interested, q29_share, q20_talk_friendly,
  q36_same_age, q34_copy_actions, q37_respond_positively,
  q40_cooperatively_games, q27_smile_back, q31_comfort,
  q22_point_things, q19_best_friend
)

# ASDLanguage -- from scq
ASDLanguage = full_data$data[[4]] %>% dplyr::select(
  subject_sp_id, q05_pronouns_mixed,
  q03_odd_phrase, q06_invented_words, q02_conversation,
  q04_inappropriate_question, q23_gestures_wanted
)

# RRB -- from rbsr, scq
RRB = full_data$data[[4]] %>% dplyr::select(
  subject_sp_id, q08_particular_way,
  q13_interests_intensity, q07_same_over, q11_interest_preoccupy
)

# ImgPlay -- from scq
ImaginativePlay = full_data$data[[4]] %>% dplyr::select(
  subject_sp_id, q35_make_believe, q39_imaginative_games
)

# Global covariates
Covariates = full_data$data[[1]] %>% dplyr::select(
  subject_sp_id, sex, asd
)
Covariates = merge(
  Covariates, 
  full_data$data[[4]] %>% dplyr::select(subject_sp_id, age_at_eval_years, final_score), 
  by="subject_sp_id"
) %>% rename(scq_age_at_eval_years = age_at_eval_years)
Covariates = merge(
  Covariates, 
  full_data$data[[3]] %>% dplyr::select(subject_sp_id, dev_lang_dis), 
  by="subject_sp_id"
)

Covariates$sex = as.numeric(Covariates$sex == "Male")
Covariates$dev_lang_dis[is.na(Covariates$dev_lang_dis)] = 0


# Invalid data rows
Invalid = unique(na.omit(c(
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$asd_validity_flag==1],
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$asd_confound_flag==1],
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$age_validity_flag==1],
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$individual_data_validity==1],
  full_data$data[[4]]$subject_sp_id[full_data$data[[4]]$scq_measure_validity_flag==1],
  full_data$data[[6]]$subject_sp_id[full_data$data[[6]]$bghx_validity_flag==1]
)))


# clean data columns

Motor$dev_motor[is.na(Motor$dev_motor)] = 0

# BCD$dev_ld[is.na(BCD$dev_ld)] = 0
BCD$dev_id[is.na(BCD$dev_id)] = 0


# Combine data ----
Constructs = list(
  GeneralLanguage = GeneralLanguage,
  Motor = Motor,
  SocialSkill = SocialSkill,
  Imitation = Imitation,
  BCD = BCD,
  NVLanguage = NVLanguage,
  Aloofness = Aloofness,
  ASDLanguage = ASDLanguage,
  RRB = RRB,
  ImaginativePlay = ImaginativePlay,
  Covariates = Covariates
)


# filter data and prefix name ----
for(nm in names(Constructs)){
  Constructs[[nm]] = Constructs[[nm]] %>% dplyr::filter(!(subject_sp_id %in% Invalid))
}

sapply(
  Constructs,
  function(df){
    sapply(df, function(x) sum(!is.na(x)))
  }
)


# save data ----
save(Constructs, file="Constructs.RData")



### Prefixing not needed
# Prefix = list(
#   GeneralLanguage = "gl_",
#   GeneralLanguage_dev = "gldev_",
#   Motor = "mot_",
#   SocialSkill = "sos_",
#   Imitation = "imi_",
#   BCD = "bcd_",
#   NVLanguage = "nv_",
#   Aloofness = "alf_",
#   ASDLanguage = "al_",
#   RRB = "rrb_",
#   ImaginativePlay = "imp_",
#   Covariates = ""
# )