# load packages ----
library(tidyverse)
library(corrplot)
source("utils.R")


# load data ----
load("items_full_data.RData")


# Constructs specification

## General language -- from background history
GeneralLanguage = full_data$data[[5]] %>% dplyr::select(
  subject_sp_id, language_age_level,
  combined_phrases_age_mos, combined_words_age_mos, used_words_age_mos
)
GeneralLanguage$language_age_level[GeneralLanguage$language_age_level==""] = NA
GeneralLanguage$language_age_level = as.numeric(factor(
  GeneralLanguage$language_age_level, 
  levels=c("signif_below_age", "slight_below_age", "at_age", "above_age")
))


## Motor sport -- from dcdq
MotorSport = full_data$data[[9]] %>% dplyr::select(
  subject_sp_id, q01_throw_ball, q02_catch_ball, q03_hit_ball, q04_jump_obstacles,
  q05_run_fast_similar, q11_likes_sports_motors_skills, q12_learns_new_motor_tasks
)

## Motor fine -- from dcdq
MotorFine = full_data$data[[9]] %>% dplyr::select(
  subject_sp_id, q08_printing_letters_legible,
  q09_appropriate_tension_printing_writing,
  q07_printing_writing_drawing_fast,
  q10_cuts_pictures_shapes,
  q13_quick_competent_tidying_up
)

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
# remove learning disability
# BCD = full_data$data[[3]] %>%
#   dplyr::select(subject_sp_id, dev_ld, dev_id)
BCD = full_data$data[[3]] %>%
  dplyr::select(subject_sp_id, dev_id)
BCD = merge(
  BCD, full_data$data[[5]] %>% dplyr::select(subject_sp_id, cog_age_level), 
  by="subject_sp_id"
)
BCD$cog_age_level[BCD$cog_age_level==""] = NA
BCD$cog_age_level = as.numeric(factor(
  BCD$cog_age_level, 
  levels=c("signif_below_age", "slight_below_age", "at_age", "above_age")
))

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
RRB = merge(
  RRB, full_data$data[[8]] %>% dplyr::select(subject_sp_id, 
    q38_insists_routine, q39_insists_time, q29_things_same_place,
    q34_dislikes_changes, q35_insists_door, q37_resists_change,
    q15_arranging, q41_strongly_attached, q42_preoccupation,
    q40_fascination_subject, q18_checking
  ), by="subject_sp_id"
)

# ImgPlay -- from scq
ImaginativePlay = full_data$data[[4]] %>% dplyr::select(
  subject_sp_id, q35_make_believe, q39_imaginative_games
)

# Global covariates
Covariates = full_data$data[[1]] %>% dplyr::select(
  subject_sp_id, sex, asd
)
Covariates$sex = as.numeric(Covariates$sex == "Male")
Covariates = merge(
  Covariates, 
  full_data$data[[4]] %>% dplyr::select(subject_sp_id, age_at_eval_years, final_score), 
  by="subject_sp_id"
) %>% rename(scq_age_at_eval_years = age_at_eval_years)
Covariates = merge(
  Covariates, 
  full_data$data[[8]] %>% dplyr::select(subject_sp_id, age_at_eval_years), 
  by="subject_sp_id"
) %>% rename(rbsr_age_at_eval_years = age_at_eval_years)
Covariates = merge(
  Covariates, 
  full_data$data[[9]] %>% dplyr::select(subject_sp_id, age_at_eval_years), 
  by="subject_sp_id"
) %>% rename(dcdq_age_at_eval_years = age_at_eval_years)
Covariates = merge(
  Covariates, 
  full_data$data[[3]] %>% dplyr::select(subject_sp_id, dev_lang_dis), 
  by="subject_sp_id"
)

Covariates$dev_lang_dis[is.na(Covariates$dev_lang_dis)] = 0

# Invalid data rows
Invalid = unique(na.omit(c(
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$asd_validity_flag==1],
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$asd_confound_flag==1],
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$age_validity_flag==1],
  full_data$data[[1]]$subject_sp_id[full_data$data[[1]]$individual_data_validity==1],
  full_data$data[[4]]$subject_sp_id[full_data$data[[4]]$scq_measure_validity_flag==1],
  full_data$data[[8]]$subject_sp_id[full_data$data[[8]]$rbsr_validity_flag==1]
)))


# clean data columns

Motor$dev_motor[is.na(Motor$dev_motor)] = 0

# BCD$dev_ld[is.na(BCD$dev_ld)] = 0
BCD$dev_id[is.na(BCD$dev_id)] = 0


# Combine data ----
Constructs = list(
  GeneralLanguage = GeneralLanguage,
  MotorSport = MotorSport,
  MotorFine = MotorFine,
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
save(Constructs, file="Constructs_ASD.RData")
