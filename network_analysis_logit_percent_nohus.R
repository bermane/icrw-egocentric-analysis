# this code runs logit models on ego data

# load packages
library(tidyverse)
library(magrittr)
library(janitor)
library(rcompanion)
library(nnet)
library(DescTools)
library(LogisticDx)

# load data
load(
  '/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/logit_data_table_percent_nohus.RData'
)

##################################
### BASIC DATA TRANSFORMATIONS ###
##################################

# make parity a factor
# dat %<>% mutate(parity = as.factor(parity))

# create list of cont vars to center
cont <- c(
  'parity',
  'sons',
  'daughters',
  'age',
  'education',
  'husband_education',
  'children',
  'ego_husband_edu_diff',
  'ego_support_fp_num_ppl',
  'ego_support_fp_num_aaa',
  'ego_support_fp_num_non_aaa',
  'ego_advice_num_ppl',
  'ego_advice_num_aaa',
  'ego_advice_num_non_aaa',
  'ego_fam',
  'ego_fam_close',
  'ego_friends',
  'ego_neigh',
  'num_alt_not_family',
  "alt_median_age",
  "alt_mean_age",
  "alt_median_num_fam",
  "alt_mean_num_fam",
  "alt_sum_num_fam",               
  "alt_median_num_fam_close",
  "alt_mean_num_fam_close",
  "alt_sum_num_fam_close",
  "alt_median_friends",             
  "alt_mean_friends",
  "alt_sum_friends",
  "alt_median_neigh",
  "alt_mean_neigh",               
  "alt_sum_neigh",
  'homo_num_fam',
  'homo_friends',
  'homo_neigh',
  'homo_friends_neigh',
  'homo_age',
  'ego_deg_cen',
  'alt_mean_deg_cen',
  'alt_mean_years_known',
  'alt_mean_talk_subjects',
  'alt_mean_things_learned',
  'alt_mean_social_influence_index',
  'alt_total_social_influence_index',
  'alt_mean_social_support_index',
  'alt_total_social_support_index'
)

# center all continuous vars around the mean
dat %<>% mutate(across(all_of(cont), ~ scale(.x, scale = F)))

# create list of cont proportion vars to center and scale
cont_pro <- c(
  "alt_percent_in_laws",
  "alt_percent_ego_fam",
  'homo_gender',
  'homo_know_shg',
  'homo_using_mod_fp',
  'homo_live_household',
  'homo_live_village',
  'homo_live_another_village',
  'homo_live_another_district',
  'density',
  'alt_percent_talk_fp_weekly',
  'alt_percent_talk_fp_monthly',
  'alt_percent_talked_number_children',
  'alt_percent_discuss_fp_freely',
  'alt_percent_encourage_fp',
  'alt_percent_support_no_child',
  'alt_percent_not_say_bad_things_fp',
  'alt_percent_talked_fp_methods',
  'alt_percent_info_get_fp',
  'alt_percent_info_sideeff_fp',
  'alt_percent_wants_more_children',
  'alt_percent_wants_more_sons',
  'alt_percent_wants_more_daughters',
  'alt_percent_using_fp',
  'alt_percent_used_fp',
  'alt_percent_help_ego',
  'alt_percent_helped_by_ego',
  'alt_percent_easy_follow_advice',
  'alt_percent_borrow_rps_hh',
  'alt_percent_learned_fp_methods'
)

# center all continuous vars around the mean
dat %<>% mutate(across(all_of(cont_pro), ~ scale(.x, scale = 0.2)))
#dat %<>% mutate(across(all_of(cont_pro), ~ scale(.x, scale = F)))

# move neighbors_using_modern_fp column to contagion section
dat %<>% relocate(neighbors_using_modern_fp, .after = alt_percent_used_fp)

# move alt_sil and alt_mil
dat %<>% relocate(alt_sil, .after = last_col()) %>%
  relocate(alt_mil, .after = last_col())

# move other additional columns added to the end
dat %<>% relocate(all_of(
  c(
    'ego_fam',
    'ego_fam_close',
    'ego_friends',
    'ego_neigh',
    'ego_shg',
    "alt_median_age",
    "alt_mean_age",
    "alt_median_num_fam",
    "alt_mean_num_fam",
    "alt_sum_num_fam",
    "alt_median_num_fam_close",
    "alt_mean_num_fam_close",
    "alt_sum_num_fam_close",
    "alt_median_friends",
    "alt_mean_friends",
    "alt_sum_friends",
    "alt_median_neigh",
    "alt_mean_neigh",
    "alt_sum_neigh",
    "alt_percent_in_laws",
    "alt_percent_ego_fam",
    'alt_mean_things_learned',
    'alt_percent_learned_fp_methods',
    'ego_support_fp_num_ppl',
    'ego_support_fp_num_aaa',
    'ego_support_fp_num_non_aaa',
    'ego_advice_num_ppl',
    'ego_advice_num_aaa',
    'ego_advice_num_non_aaa'
  )
), .after = last_col())

################################################################################
### SEPARATE INTO TWO DATASETS BASED ON MOD FP VS. NONE and TRAD FP VS. NONE ###
################################################################################

# check one woman is pregnant and has baby under 6 months
# dat %<>% filter(preg != 2 & age_months_child < 6)

# remove pregnant
dat %<>% filter(preg == 2)

# remove children > 6 months old
# dat %<>% filter(age_months_child >= 6 | is.na(age_months_child))

# remove age_months_child from df
dat %<>% select(-age_months_child)

# modern only
dat_mod <- dat %>% filter(using_trad_fp == 0)

# trad only
dat_trad <- dat %>% filter(using_mod_fp == 0)

#########################################
### SEPARATE INTO MULTINOMIAL DATASET ###
#########################################

# create fp factor
dat_fp <- dat %>% mutate(using_fp = NA)
dat_fp$using_fp[dat_fp$using_mod_fp == 1] <- 'modern'
dat_fp$using_fp[dat_fp$using_trad_fp == 1] <- 'trad'
dat_fp$using_fp[dat_fp$using_any_fp == 0] <- 'none'
dat_fp$using_fp <- factor(dat_fp$using_fp)

###################################################
### CHECK DISTRIBUTION OF CATEGORICAL VARIABLES ###
###################################################

# seem ok

# state, caste, want_more_children, neighbors_using_modern_fp

# # modern
# dat_mod %>% tabyl(using_mod_fp, state)
# dat_mod %>% tabyl(using_mod_fp, caste)
# dat_mod %>% tabyl(using_mod_fp, want_more_children)
# dat_mod %>% tabyl(using_mod_fp, neighbors_using_modern_fp)
# dat_mod %>% tabyl(using_mod_fp, alt_hw)
# 
# # trad
# dat_trad %>% tabyl(using_trad_fp, state)
# dat_trad %>% tabyl(using_trad_fp, caste)
# dat_trad %>% tabyl(using_trad_fp, want_more_children)
# dat_trad %>% tabyl(using_trad_fp, neighbors_using_modern_fp)
# dat_trad %>% tabyl(using_trad_fp, alt_hw)

##########################
### CHECK MISSING DATA ###
##########################

lapply(dat, function(x) {
  sum(is.na(x))
})

############################################
### RUN BIVARIATE MODELS MODERN VS. NONE ###
############################################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric(),
  n = numeric()
)

# loop through all variables
for (var in colnames(dat_mod)) {
  # only run for indep vars
  if (!(
    var %in% c(
      'using_any_fp',
      'using_mod_fp',
      'using_trad_fp',
      'ego_id',
      'preg',
      'fp_method',
      'num_alt_is_hw'
    )
  )) {
    # run model
    glm <- eval(substitute(
      glm(
        using_mod_fp ~ variable,
        family = binomial(link = 'logit'),
        data = dat_mod
      ),
      list(variable = as.name(var))
    ))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))), 4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x) {
      if (x < 0.001) {
        p_sig <- '***'
      } else if (x < 0.01 & x >= 0.001) {
        p_sig <- '**'
      } else if (x < 0.05 & x >= 0.01) {
        p_sig <- '*'
      } else if (x < 0.1 & x >= 0.05) {
        p_sig <- '.'
      } else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(
      model = var,
      var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
      var_p_sig = str_c(p_sig, collapse = ', '),
      var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))), 4] %>% round(5), collapse = ', '),
      aic = glm$aic,
      n = nobs(glm)
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_modern_none_nohus.csv',
          row.names = F)

##########################################
### RUN BIVARIATE MODELS TRAD VS. NONE ###
##########################################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric(),
  n = numeric()
)

# loop through all variables
for (var in colnames(dat_trad)) {
  # only run for indep vars
  if (!(
    var %in% c(
      'using_any_fp',
      'using_mod_fp',
      'using_trad_fp',
      'ego_id',
      'preg',
      'fp_method',
      'num_alt_is_hw'
    )
  )) {
    # run model
    glm <- eval(substitute(
      glm(
        using_trad_fp ~ variable,
        family = binomial(link = 'logit'),
        data = dat_trad
      ),
      list(variable = as.name(var))
    ))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))), 4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x) {
      if (x < 0.001) {
        p_sig <- '***'
      } else if (x < 0.01 & x >= 0.001) {
        p_sig <- '**'
      } else if (x < 0.05 & x >= 0.01) {
        p_sig <- '*'
      } else if (x < 0.1 & x >= 0.05) {
        p_sig <- '.'
      } else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(
      model = var,
      var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
      var_p_sig = str_c(p_sig, collapse = ', '),
      var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))), 4] %>% round(5), collapse = ', '),
      aic = glm$aic,
      n = nobs(glm)
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_trad_none_nohus.csv',
          row.names = F)

########################################
### RUN BIVARIATE MODELS MULTINOMIAL ###
########################################

# # allocate output table
# univar <- tibble(
#   model = character(),
#   var_odds = character(),
#   var_p_sig = character(),
#   var_p = character(),
#   aic = numeric(),
#   n = numeric()
# )
# 
# # specify the level of dep var to use as baseline
# dat_fp$using_fp <- relevel(dat_fp$using_fp, ref = "none")
# 
# mlr <- multinom(using_fp ~ caste, data = dat_fp)
# 
# exp(coef(mlr))[, 2:NCOL(exp(coef(mlr)))]
# 
# x <- exp(coef(mlr))
# 
# # calculate z vals
# z <- summary(mlr)$coefficients / summary(mlr)$standard.errors
# 
# # calculate p vals
# p <- (1 - pnorm(abs(z), 0, 1)) * 2

#################################
### RUN MODERN vs. NONE MODEL ###
#################################

# check missing data
lapply(dat_mod, function(x) {
  sum(is.na(x))
})

# create final dat for mod vs. non (not including indices)
dat_mod2 <- dat_mod %>% select(-c('using_trad_fp', 'using_any_fp',
                                 'preg', 'ego_id', 'fp_method',
                                 'want_more_children', 'hetero_discuss_fp_blau',
                                 'hetero_discuss_fp_iqv',
                                 'children',
                                 'alt_percent_wants_more_sons',
                                 'alt_percent_wants_more_daughters',
                                 'alt_percent_wants_more_children',
                                 'alt_percent_talk_fp_weekly',
                                 'alt_percent_talk_fp_monthly',
                                 'alt_total_social_influence_index',
                                 'alt_total_social_support_index',
                                 'alt_mean_social_influence_index',
                                 'alt_mean_social_influence_index',
                                 'sons',
                                 'daughters',
                                 'homo_using_mod_fp',
                                 'alt_median_num_fam_close',
                                 'alt_mean_num_fam_close'))

# # create final dat for mod vs. non (including indices)
# dat_mod2 <- dat_mod %>% select(
#   -c(
#     'using_trad_fp',
#     'using_any_fp',
#     'preg',
#     'ego_id',
#     'fp_method',
#     'want_more_children',
#     'hetero_discuss_fp_blau',
#     'hetero_discuss_fp_iqv',
#     'children',
#     'alt_percent_wants_more_sons',
#     'alt_percent_wants_more_daughters',
#     'alt_percent_wants_more_children',
#     'alt_percent_talk_fp_weekly',
#     'alt_percent_talk_fp_monthly',
#     'alt_total_social_influence_index',
#     'alt_total_social_support_index',
#     'sons',
#     'daughters',
#     'alt_percent_talked_number_children',
#     'alt_percent_discuss_fp_freely',
#     'alt_percent_encourage_fp',
#     'alt_percent_support_no_child',
#     'alt_percent_not_say_bad_things_fp',
#     'alt_percent_help_ego',
#     'alt_percent_helped_by_ego',
#     'alt_percent_easy_follow_advice',
#     'alt_percent_borrow_rps_hh',
#     'homo_using_mod_fp',
#     'alt_median_num_fam_close',
#     'alt_mean_num_fam_close'
#   )
# )

# # remove vars with strange sign
# dat_mod2 %<>% select(-c('alt_hw',
#                         'neighbors_using_modern_fp'))

# # check missing data
# lapply(dat_mod, function(x){
#   sum(is.na(x))
# })

# drop missing values
dat_mod2 <- na.omit(dat_mod2)

# run full model
all_mod <- glm(formula = using_mod_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat_mod2)

# run intercept only
int_mod <- glm(formula = using_mod_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat_mod2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# create final dat for mod vs. non (final variable testing)
dat_mod2 <- dat_mod %>% select(c('using_mod_fp',
                                 'state',
                                 'parity',
                                 'age',
                                 'caste',
                                 'ego_husband_edu_diff',
                                 'alt_hw',
                                 'ego_deg_cen',
                                 'density',
                                 'alt_mean_years_known',
                                 'alt_mean_talk_subjects',
                                 'alt_percent_talked_fp_methods',
                                 'alt_percent_info_get_fp',
                                 'alt_percent_using_fp',
                                 'alt_percent_borrow_rps_hh',
                                 'ego_advice_num_ppl',
                                 'alt_mean_age'))

# drop missing values
dat_mod2 <- na.omit(dat_mod2)

# run full model
mod <- glm(formula = using_mod_fp ~ state +
             parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
               family = binomial(link = 'logit'),
               data = dat_mod2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

# # use interaction of mil and sil (no influence and support indices)
# glm <- glm(
#   using_mod_fp ~ parity + 
#     alt_percent_borrow_rps_hh + 
#     ego_husband_edu_diff +
#     homo_live_another_district +
#     homo_friends +
#     homo_age +
#     alt_sil*alt_mil,
#   family = binomial(link = 'logit'),
#   data = dat_mod2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)

# # run comb of best stepwise and INDICES
# glm <- glm(
#   using_mod_fp ~ parity +
#     homo_using_mod_fp +
#     alt_percent_info_get_fp +
#     homo_live_another_village +
#     homo_age +
#     husband_education +
#     homo_friends_neigh +
#     alt_mean_social_influence_index +
#     alt_mean_social_support_index,
#   family = binomial(link = 'logit'),
#   data = dat_mod2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)
# 
# # run comb of best stepwise and support/influence (NO INDICES)
# glm <- glm(
#   using_mod_fp ~ parity + 
#     homo_using_mod_fp +
#     alt_percent_info_get_fp +
#     homo_live_another_village +
#     homo_age +
#     alt_percent_easy_follow_advice + 
#     ego_husband_edu_diff +
#     alt_percent_borrow_rps_hh + 
#     homo_live_household + 
#     alt_sil*alt_mil +
#     alt_percent_discuss_fp_freely +
#     alt_percent_talked_number_children + 
#     alt_percent_encourage_fp +
#     alt_percent_using_fp,
#   family = binomial(link = 'logit'),
#   data = dat_mod2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)

#################################
### RUN TRAD vs. NONE MODEL ###
#################################

# create final dat for trad vs. non (not including indices)
dat_trad2 <- dat_trad %>% select(-c('using_mod_fp', 'using_any_fp',
                                 'preg', 'ego_id', 'fp_method',
                                 'want_more_children', 'hetero_discuss_fp_blau',
                                 'hetero_discuss_fp_iqv',
                                 'children',
                                 'alt_percent_wants_more_sons',
                                 'alt_percent_wants_more_daughters',
                                 'alt_percent_wants_more_children',
                                 'alt_percent_talk_fp_weekly',
                                 'alt_percent_talk_fp_monthly',
                                 'alt_total_social_influence_index',
                                 'alt_total_social_support_index',
                                 'alt_mean_social_influence_index',
                                 'alt_mean_social_support_index',
                                 'sons',
                                 'daughters',
                                 'homo_using_mod_fp'))

# # create final dat for trad vs. non (including indices)
# dat_trad2 <- dat_trad %>% select(
#   -c(
#     'using_mod_fp',
#     'using_any_fp',
#     'preg',
#     'ego_id',
#     'fp_method',
#     'want_more_children',
#     'hetero_discuss_fp_blau',
#     'hetero_discuss_fp_iqv',
#     'children',
#     'alt_percent_wants_more_sons',
#     'alt_percent_wants_more_daughters',
#     'alt_percent_wants_more_children',
#     'alt_percent_talk_fp_weekly',
#     'alt_percent_talk_fp_monthly',
#     'alt_total_social_influence_index',
#     'alt_total_social_support_index',
#     'sons',
#     'daughters',
#     'alt_percent_talked_number_children',
#     'alt_percent_discuss_fp_freely',
#     'alt_percent_encourage_fp',
#     'alt_percent_support_no_child',
#     'alt_percent_not_say_bad_things_fp',
#     'alt_percent_help_ego',
#     'alt_percent_helped_by_ego',
#     'alt_percent_easy_follow_advice',
#     'alt_percent_borrow_rps_hh',
#     'homo_using_mod_fp'
#   )
# )

# drop missing values
dat_trad2 <- na.omit(dat_trad2)

# run full model
all_mod <- glm(formula = using_trad_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat_trad2)

# run intercept only
int_mod <- glm(formula = using_trad_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat_trad2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# create final dat for trad vs. non (final variable testing)
dat_trad2 <- dat_trad %>% select(c('using_trad_fp',
                                 'state',
                                 'parity',
                                 'age',
                                 'caste',
                                 'ego_husband_edu_diff',
                                 'alt_hw',
                                 'ego_deg_cen',
                                 'density',
                                 'alt_mean_years_known',
                                 'alt_mean_talk_subjects',
                                 'alt_percent_talked_fp_methods',
                                 'alt_percent_info_get_fp',
                                 'alt_percent_using_fp',
                                 'alt_percent_borrow_rps_hh',
                                 'ego_advice_num_ppl',
                                 'alt_mean_age'))

# drop missing values
dat_trad2 <- na.omit(dat_trad2)

# run full model
mod <- glm(formula = using_trad_fp ~ state +
             parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat_trad2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

# check correlation
corr <- cor(dat_trad2 %>% select(-c('using_trad_fp',
                                    'state', 'caste',
                                    'alt_hw')))
corrplot(corr, method = 'number')

# # use interaction of mil and sil
# glm <- glm(
#   using_trad_fp ~ state +
#     alt_mean_talk_subjects + 
#     alt_mean_deg_cen + 
#     age +
#     alt_percent_info_sideeff_fp +
#     alt_sil*alt_mil,
#   family = binomial(link = 'logit'),
#   data = dat_trad2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 8)
# diag$gof$pVal
# VIF(glm)

#############################################
### RUN MODERN VS (TRAD + NONE) BIVARIATE ###
#############################################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric(),
  n = numeric()
)

# loop through all variables
for (var in colnames(dat)) {
  # only run for indep vars
  if (!(
    var %in% c(
      'using_any_fp',
      'using_mod_fp',
      'using_trad_fp',
      'ego_id',
      'preg',
      'fp_method',
      'num_alt_is_hw'
    )
  )) {
    # run model
    glm <- eval(substitute(
      glm(
        using_mod_fp ~ variable,
        family = binomial(link = 'logit'),
        data = dat
      ),
      list(variable = as.name(var))
    ))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))), 4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x) {
      if (x < 0.001) {
        p_sig <- '***'
      } else if (x < 0.01 & x >= 0.001) {
        p_sig <- '**'
      } else if (x < 0.05 & x >= 0.01) {
        p_sig <- '*'
      } else if (x < 0.1 & x >= 0.05) {
        p_sig <- '.'
      } else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(
      model = var,
      var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
      var_p_sig = str_c(p_sig, collapse = ', '),
      var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))), 4] %>% round(5), collapse = ', '),
      aic = glm$aic,
      n = nobs(glm)
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_vs_tradnone_percent.csv',
          row.names = F)

#############################################
### RUN (MODERN + TRAD) VS NONE BIVARIATE ###
#############################################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric(),
  n = numeric()
)

# loop through all variables
for (var in colnames(dat)) {
  # only run for indep vars
  if (!(
    var %in% c(
      'using_any_fp',
      'using_mod_fp',
      'using_trad_fp',
      'ego_id',
      'preg',
      'fp_method',
      'num_alt_is_hw'
    )
  )) {
    # run model
    glm <- eval(substitute(
      glm(
        using_any_fp ~ variable,
        family = binomial(link = 'logit'),
        data = dat
      ),
      list(variable = as.name(var))
    ))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))), 4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x) {
      if (x < 0.001) {
        p_sig <- '***'
      } else if (x < 0.01 & x >= 0.001) {
        p_sig <- '**'
      } else if (x < 0.05 & x >= 0.01) {
        p_sig <- '*'
      } else if (x < 0.1 & x >= 0.05) {
        p_sig <- '.'
      } else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(
      model = var,
      var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
      var_p_sig = str_c(p_sig, collapse = ', '),
      var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))), 4] %>% round(5), collapse = ', '),
      aic = glm$aic,
      n = nobs(glm)
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_modtrad_vs_none_percent.csv',
          row.names = F)

#####################################
### RUN MODERN vs. TRADNONE MODEL ###
#####################################

# check missing data
lapply(dat, function(x) {
  sum(is.na(x))
})

# create final dat for mod vs. tradnone (not including indices)
dat2 <- dat %>% select(-c('using_trad_fp', 'using_any_fp',
                          'preg', 'ego_id', 'fp_method',
                          'want_more_children', 'hetero_discuss_fp_blau',
                          'hetero_discuss_fp_iqv',
                          'children',
                          'alt_percent_wants_more_sons',
                          'alt_percent_wants_more_daughters',
                          'alt_percent_wants_more_children',
                          'alt_percent_talk_fp_weekly',
                          'alt_percent_talk_fp_monthly',
                          'alt_total_social_influence_index',
                          'alt_total_social_support_index',
                          'alt_mean_social_influence_index',
                          'alt_mean_social_support_index',
                          'sons',
                          'daughters',
                          'homo_using_mod_fp',
                          'homo_age',
                          'homo_neigh'))

# # create final dat for mod vs. non (including indices)
# dat2 <- dat %>% select(
#   -c(
#     'using_trad_fp',
#     'using_any_fp',
#     'preg',
#     'ego_id',
#     'fp_method',
#     'want_more_children',
#     'hetero_discuss_fp_blau',
#     'hetero_discuss_fp_iqv',
#     'children',
#     'alt_percent_wants_more_sons',
#     'alt_percent_wants_more_daughters',
#     'alt_percent_wants_more_children',
#     'alt_percent_talk_fp_weekly',
#     'alt_percent_talk_fp_monthly',
#     'alt_total_social_influence_index',
#     'alt_total_social_support_index',
#     'sons',
#     'daughters',
#     'alt_percent_talked_number_children',
#     'alt_percent_discuss_fp_freely',
#     'alt_percent_encourage_fp',
#     'alt_percent_support_no_child',
#     'alt_percent_not_say_bad_things_fp',
#     'alt_percent_help_ego',
#     'alt_percent_helped_by_ego',
#     'alt_percent_easy_follow_advice',
#     'alt_percent_borrow_rps_hh',
#     'homo_using_mod_fp',
#     'homo_neigh'
#   )
# )

# remove vars with strange sign
# dat_mod2 %<>% select(-c('neighbors_using_modern_fp'))

# # check missing data
# lapply(dat_mod, function(x){
#   sum(is.na(x))
# })

# drop missing values
dat2 <- na.omit(dat2)

# run full model
all_mod <- glm(formula = using_mod_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat2)

# run intercept only
int_mod <- glm(formula = using_mod_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# create final dat for mod vs. tradnone (final variable testing)
dat2 <- dat %>% select(c('using_mod_fp',
                                   'state',
                                   'parity',
                                   'age',
                                   'caste',
                                   'ego_husband_edu_diff',
                                   'alt_hw',
                                   'ego_deg_cen',
                                   'density',
                                   'alt_mean_years_known',
                                   'alt_mean_talk_subjects',
                                   'alt_percent_talked_fp_methods',
                                   'alt_percent_info_get_fp',
                                   'alt_percent_using_fp',
                                   'alt_percent_borrow_rps_hh',
                                   'ego_advice_num_ppl',
                                   'alt_mean_age'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
mod <- glm(formula = using_mod_fp ~ state +
             parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

# check correlation
corr <- cor(dat2 %>% select(-c('using_mod_fp',
                                    'state', 'caste',
                                    'alt_hw')))
corrplot(corr, method = 'number')

# # use interaction of mil and sil (no influence and support indices)
# glm <- glm(
#   using_mod_fp ~ parity +
#     alt_percent_borrow_rps_hh +
#     ego_husband_edu_diff +
#     homo_live_another_district +
#     homo_age +
#     alt_percent_easy_follow_advice +
#     homo_live_village +
#     alt_sil*alt_mil,
#   family = binomial(link = 'logit'),
#   data = dat2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)

# # run comb of best stepwise and INDICES
# glm <- glm(
#   using_mod_fp ~ parity +
#     homo_using_mod_fp +
#     alt_percent_using_fp +
#     alt_hw +
#     alt_percent_info_get_fp +
#     ego_husband_edu_diff +
#     homo_live_village +
#     homo_live_another_district +
#     alt_mean_talk_subjects +
#     alt_mean_social_influence_index +
#     alt_mean_social_support_index,
#   family = binomial(link = 'logit'),
#   data = dat2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)
# 
# # run comb of best stepwise and support/influence (NO INDICES)
# glm <- glm(
#   using_mod_fp ~ parity + 
#     homo_using_mod_fp +
#     alt_percent_using_fp +
#     alt_hw +
#     alt_percent_info_get_fp +
#     ego_husband_edu_diff +
#     alt_percent_easy_follow_advice +
#     homo_live_village +
#     alt_mean_talk_subjects +
#     homo_live_another_district +
#     alt_sil*alt_mil +
#     alt_percent_discuss_fp_freely +
#     alt_percent_talked_number_children + 
#     alt_percent_encourage_fp,
#   family = binomial(link = 'logit'),
#   data = dat2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)

#####################################
### RUN MODERNTRAD vs. NONE MODEL ###
#####################################

# check missing data
lapply(dat, function(x) {
  sum(is.na(x))
})

# create final dat for mod vs. non (not including indices)
dat2 <- dat %>% select(-c('using_trad_fp', 'using_mod_fp',
                          'preg', 'ego_id', 'fp_method',
                          'want_more_children', 'hetero_discuss_fp_blau',
                          'hetero_discuss_fp_iqv',
                          'children',
                          'alt_percent_wants_more_sons',
                          'alt_percent_wants_more_daughters',
                          'alt_percent_wants_more_children',
                          'alt_percent_talk_fp_weekly',
                          'alt_percent_talk_fp_monthly',
                          'alt_total_social_influence_index',
                          'alt_total_social_support_index',
                          'alt_mean_social_influence_index',
                          'alt_mean_social_influence_index',
                          'sons',
                          'daughters',
                          'homo_using_mod_fp',
                          'alt_mean_num_fam',
                          'alt_median_num_fam'))

# # create final dat for mod vs. non (including indices)
# dat2 <- dat %>% select(
#   -c(
#     'using_trad_fp',
#     'using_mod_fp',
#     'preg',
#     'ego_id',
#     'fp_method',
#     'want_more_children',
#     'hetero_discuss_fp_blau',
#     'hetero_discuss_fp_iqv',
#     'children',
#     'alt_percent_wants_more_sons',
#     'alt_percent_wants_more_daughters',
#     'alt_percent_wants_more_children',
#     'alt_percent_talk_fp_weekly',
#     'alt_percent_talk_fp_monthly',
#     'alt_total_social_influence_index',
#     'alt_total_social_support_index',
#     'sons',
#     'daughters',
#     'alt_percent_talked_number_children',
#     'alt_percent_discuss_fp_freely',
#     'alt_percent_encourage_fp',
#     'alt_percent_support_no_child',
#     'alt_percent_not_say_bad_things_fp',
#     'alt_percent_help_ego',
#     'alt_percent_helped_by_ego',
#     'alt_percent_easy_follow_advice',
#     'alt_percent_borrow_rps_hh',
#     'homo_friends_neigh',
#     'homo_using_mod_fp'
#   )
# )

# remove vars with strange sign
dat2 %<>% select(-c('neighbors_using_modern_fp'))

# # check missing data
# lapply(dat_mod, function(x){
#   sum(is.na(x))
# })

# drop missing values
dat2 <- na.omit(dat2)

# run full model
all_mod <- glm(formula = using_any_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat2)

# run intercept only
int_mod <- glm(formula = using_any_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# create final dat for any vs. none (final variable testing)
dat2 <- dat %>% select(c('using_any_fp',
                         'state',
                         'parity',
                         'age',
                         'caste',
                         'ego_husband_edu_diff',
                         'alt_hw',
                         'ego_deg_cen',
                         'density',
                         'alt_mean_years_known',
                         'alt_mean_talk_subjects',
                         'alt_percent_talked_fp_methods',
                         'alt_percent_info_get_fp',
                         'alt_percent_using_fp',
                         'alt_percent_borrow_rps_hh',
                         'ego_advice_num_ppl',
                         'alt_mean_age'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
mod <- glm(formula = using_any_fp ~ state +
             parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

# # use interaction of mil and sil (no influence and support indices)
# glm <- glm(using_any_fp,
#   family = binomial(link = 'logit'),
#   data = dat2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)
# 
# # run comb of best stepwise and INDICES
# glm <- glm(
#   using_any_fp ~ parity +
#     homo_using_mod_fp +
#     husband_education +
#     alt_percent_info_get_fp +
#     alt_mean_deg_cen +
#     homo_neigh +
#     homo_age +
#     homo_live_another_village +
#     alt_mean_social_influence_index +
#     alt_mean_social_support_index,
#   family = binomial(link = 'logit'),
#   data = dat2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)
# 
# # run comb of best stepwise and support/influence (NO INDICES)
# glm <- glm(
#   using_any_fp ~ parity + 
#     homo_using_mod_fp +
#     alt_mean_deg_cen +
#     husband_education +
#     alt_percent_info_get_fp +
#     alt_percent_borrow_rps_hh +
#     homo_age +
#     homo_neigh +
#     homo_live_another_village +
#     alt_sil*alt_mil +
#     alt_percent_using_fp +
#     alt_percent_discuss_fp_freely +
#     alt_percent_talked_number_children + 
#     alt_percent_encourage_fp,
#   family = binomial(link = 'logit'),
#   data = dat2
# )
# summary(glm)
# exp(coef(glm)) %>% round(3)
# confint(glm)
# 
# # diagnostics
# AIC(glm) %>% round(2)
# PseudoR2(glm,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(glm, plotROC = F, g = 9)
# diag$gof$pVal %>% round(3)
# VIF(glm)

################################################
### RUN MODERN vs. TRADNONE MODEL BIHAR ONLY ###
################################################

# check missing data
lapply(dat, function(x) {
  sum(is.na(x))
})

# subset bihar only
dat2 <- dat %>% filter(state == 'bihar')

# create final dat for mod vs. non (not including indices)
dat2 <- dat2 %>% select(-c('using_trad_fp', 'using_any_fp',
                          'preg', 'ego_id', 'fp_method',
                          'want_more_children', 'hetero_discuss_fp_blau',
                          'hetero_discuss_fp_iqv',
                          'children',
                          'alt_percent_wants_more_sons',
                          'alt_percent_wants_more_daughters',
                          'alt_percent_wants_more_children',
                          'alt_percent_talk_fp_weekly',
                          'alt_percent_talk_fp_monthly',
                          'alt_total_social_influence_index',
                          'alt_total_social_support_index',
                          'alt_mean_social_influence_index',
                          'alt_mean_social_support_index',
                          'sons',
                          'daughters',
                          'homo_using_mod_fp',
                          'state',
                          'alt_percent_not_say_bad_things_fp'))

# remove vars with strange sign
# dat_mod2 %<>% select(-c('neighbors_using_modern_fp'))

# # check missing data
# lapply(dat_mod, function(x){
#   sum(is.na(x))
# })

# drop missing values
dat2 <- na.omit(dat2)

# run full model
all_mod <- glm(formula = using_mod_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat2)

# run intercept only
int_mod <- glm(formula = using_mod_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# subset bihar only
dat2 <- dat %>% filter(state == 'bihar')

# create final dat for mod vs. tradnone (final variable testing)
dat2 <- dat2 %>% select(c('using_mod_fp',
                         'parity',
                         'age',
                         'caste',
                         'ego_husband_edu_diff',
                         'alt_hw',
                         'ego_deg_cen',
                         'density',
                         'alt_mean_years_known',
                         'alt_mean_talk_subjects',
                         'alt_percent_talked_fp_methods',
                         'alt_percent_info_get_fp',
                         'alt_percent_using_fp',
                         'alt_percent_borrow_rps_hh',
                         'ego_advice_num_ppl',
                         'alt_mean_age'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
mod <- glm(formula = using_mod_fp ~ parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

#############################################
### RUN MODERN vs. TRADNONE MODEL UP ONLY ###
#############################################

# check missing data
lapply(dat, function(x) {
  sum(is.na(x))
})

# subset up only
dat2 <- dat %>% filter(state == 'up')

# create final dat for mod vs. non (not including indices)
dat2 <- dat2 %>% select(-c('using_trad_fp', 'using_any_fp',
                           'preg', 'ego_id', 'fp_method',
                           'want_more_children', 'hetero_discuss_fp_blau',
                           'hetero_discuss_fp_iqv',
                           'children',
                           'alt_percent_wants_more_sons',
                           'alt_percent_wants_more_daughters',
                           'alt_percent_wants_more_children',
                           'alt_percent_talk_fp_weekly',
                           'alt_percent_talk_fp_monthly',
                           'alt_total_social_influence_index',
                           'alt_total_social_support_index',
                           'alt_mean_social_influence_index',
                           'alt_mean_social_support_index',
                           'sons',
                           'daughters',
                           'homo_using_mod_fp',
                           'state',
                           "alt_median_neigh",
                           "alt_mean_neigh",               
                           "alt_sum_neigh"))

# remove vars with strange sign
# dat_mod2 %<>% select(-c('neighbors_using_modern_fp'))

# # check missing data
# lapply(dat_mod, function(x){
#   sum(is.na(x))
# })

# drop missing values
dat2 <- na.omit(dat2)

# run full model
all_mod <- glm(formula = using_mod_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat2)

# run intercept only
int_mod <- glm(formula = using_mod_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 11)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# subset up only
dat2 <- dat %>% filter(state == 'up')

# create final dat for mod vs. tradnone (final variable testing)
dat2 <- dat2 %>% select(c('using_mod_fp',
                          'parity',
                          'age',
                          'caste',
                          'ego_husband_edu_diff',
                          'alt_hw',
                          'ego_deg_cen',
                          'density',
                          'alt_mean_years_known',
                          'alt_mean_talk_subjects',
                          'alt_percent_talked_fp_methods',
                          'alt_percent_info_get_fp',
                          'alt_percent_using_fp',
                          'alt_percent_borrow_rps_hh',
                          'ego_advice_num_ppl',
                          'alt_mean_age'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
mod <- glm(formula = using_mod_fp ~ parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

################################################
### RUN MODERNTRAD vs. NONE MODEL BIHAR ONLY ###
################################################

# subset bihar only
dat2 <- dat %>% filter(state == 'bihar')

# # create final dat for mod vs. non (not including indices)
dat2 <- dat2 %>% select(-c('using_trad_fp', 'using_mod_fp',
                          'preg', 'ego_id', 'fp_method',
                          'want_more_children', 'hetero_discuss_fp_blau',
                          'hetero_discuss_fp_iqv',
                          'children',
                          'alt_percent_wants_more_sons',
                          'alt_percent_wants_more_daughters',
                          'alt_percent_wants_more_children',
                          'alt_percent_talk_fp_weekly',
                          'alt_percent_talk_fp_monthly',
                          'alt_total_social_influence_index',
                          'alt_total_social_support_index',
                          'alt_mean_social_influence_index',
                          'alt_mean_social_influence_index',
                          'sons',
                          'daughters',
                          'homo_using_mod_fp',
                          'state',
                          'alt_sum_num_fam',
                          'alt_median_num_fam'))

# remove vars with strange sign
dat2 %<>% select(-c('neighbors_using_modern_fp'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
all_mod <- glm(formula = using_any_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat2)

# run intercept only
int_mod <- glm(formula = using_any_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# subset bihar only
dat2 <- dat %>% filter(state == 'bihar')

# create final dat for mod vs. tradnone (final variable testing)
dat2 <- dat2 %>% select(c('using_any_fp',
                          'parity',
                          'age',
                          'caste',
                          'ego_husband_edu_diff',
                          'alt_hw',
                          'ego_deg_cen',
                          'density',
                          'alt_mean_years_known',
                          'alt_mean_talk_subjects',
                          'alt_percent_talked_fp_methods',
                          'alt_percent_info_get_fp',
                          'alt_percent_using_fp',
                          'alt_percent_borrow_rps_hh',
                          'ego_advice_num_ppl',
                          'alt_mean_age'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
mod <- glm(formula = using_any_fp ~ parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

#############################################
### RUN MODERNTRAD vs. NONE MODEL UP ONLY ###
#############################################

# subset bihar only
dat2 <- dat %>% filter(state == 'up')

# # create final dat for mod vs. non (not including indices)
dat2 <- dat2 %>% select(-c('using_trad_fp', 'using_mod_fp',
                           'preg', 'ego_id', 'fp_method',
                           'want_more_children', 'hetero_discuss_fp_blau',
                           'hetero_discuss_fp_iqv',
                           'children',
                           'alt_percent_wants_more_sons',
                           'alt_percent_wants_more_daughters',
                           'alt_percent_wants_more_children',
                           'alt_percent_talk_fp_weekly',
                           'alt_percent_talk_fp_monthly',
                           'alt_total_social_influence_index',
                           'alt_total_social_support_index',
                           'alt_mean_social_influence_index',
                           'alt_mean_social_support_index',
                           'sons',
                           'daughters',
                           'homo_using_mod_fp',
                           'state',
                           "alt_median_age",
                           "alt_median_num_fam_close",
                           'alt_percent_support_no_child'))

# remove vars with strange sign
dat2 %<>% select(-c('neighbors_using_modern_fp'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
all_mod <- glm(formula = using_any_fp ~ .,
               family = binomial(link = 'logit'),
               data = dat2)

# run intercept only
int_mod <- glm(formula = using_any_fp ~ 1,
               family = binomial(link = 'logit'),
               data = dat2)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod) %>% round(2)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F, g = 9)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# subset up only
dat2 <- dat %>% filter(state == 'up')

# create final dat for mod vs. tradnone (final variable testing)
dat2 <- dat2 %>% select(c('using_any_fp',
                          'parity',
                          'age',
                          'caste',
                          'ego_husband_edu_diff',
                          'alt_hw',
                          'ego_deg_cen',
                          'density',
                          'alt_mean_years_known',
                          'alt_mean_talk_subjects',
                          'alt_percent_talked_fp_methods',
                          'alt_percent_info_get_fp',
                          'alt_percent_using_fp',
                          'alt_percent_borrow_rps_hh',
                          'ego_advice_num_ppl',
                          'alt_mean_age'))

# drop missing values
dat2 <- na.omit(dat2)

# run full model
mod <- glm(formula = using_any_fp ~ parity +
             age +
             caste +
             ego_husband_edu_diff +
             alt_hw +
             ego_deg_cen +
             density +
             alt_mean_years_known + 
             alt_mean_talk_subjects +
             alt_percent_talked_fp_methods +
             alt_percent_info_get_fp +
             alt_percent_using_fp +
             alt_percent_borrow_rps_hh +
             ego_advice_num_ppl +
             alt_mean_age,
           family = binomial(link = 'logit'),
           data = dat2)
summary(mod)
exp(coef(mod)) %>% round(3)
confint(mod)

# diagnostics
AIC(mod) %>% round(2)
PseudoR2(mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(mod, plotROC = F, g = 10)
diag$gof$pVal %>% round(3)
VIF(mod)

#############################
### CALCULATE CORRELATION ###
#############################

# remove independent variables
dat_cor <-
  dat %>% select(-c(
    'using_any_fp',
    'using_mod_fp',
    'using_trad_fp',
    'ego_id',
    'preg',
    'fp_method'
  ))

# change integer columns to numeric
dat_cor %<>% mutate_if(is.integer, as.numeric)

# remove factor columns
dat_cor %<>% select_if(is.numeric)

# calculate correlation matrix
cor_mat <- cor(dat_cor, use = "pairwise.complete.obs")

# melt
cor_mat <- reshape2::melt(cor_mat)

# arrange
cor_mat <- arrange(cor_mat, desc(abs(value)))

# write csv
write.csv(cor_mat,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/correlation_matrix.csv',
          row.names = F)

# calculate cramer's V for factor variables
cramerV(dat$parity, dat$caste)
cramerV(dat$parity, dat$want_more_children)
cramerV(dat$parity, dat$neighbors_using_modern_fp)
cramerV(dat$caste, dat$want_more_children)
cramerV(dat$caste, dat$neighbors_using_modern_fp)
cramerV(dat$want_more_children, dat$neighbors_using_modern_fp)

# ################################
# ### FORWARD STEPWISE MODEL C ###
# ################################
# 
# # create final dat for model c
# dat_mod_c <-
#   dat %>% select(
#     -c(
#       'using_mod_fp',
#       'using_trad_fp',
#       'ego_id',
#       'preg',
#       'fp_method',
#       'children',
#       'num_alt_not_family',
#       'homo_neigh',
#       'hetero_discuss_fp_blau',
#       'hetero_discuss_fp_iqv',
#       'alt_mean_deg_cen',
#       'alt_percent_used_fp'
#     )
#   )
# 
# # see where all the missing values are
# lapply(dat_mod_c, function(x) {
#   sum(is.na(x))
# })
# 
# # drop missing values
# dat_mod_c <- na.omit(dat_mod_c)
# 
# # run full model
# all_c <- glm(formula = using_any_fp ~ .,
#              family = binomial(link = 'logit'),
#              data = dat_mod_c)
# 
# # run intercept only
# int_c <- glm(formula = using_any_fp ~ 1,
#              family = binomial(link = 'logit'),
#              data = dat_mod_c)
# 
# # run both stepwise
# both_c <- step(all_c, direction = 'both')
# summary(both_c)
# both_c$anova
# 
# # run forward stepwise
# for_c <- step(int_c, scope = formula(all_c), direction = 'forward')
# summary(for_c)
# for_c$anova
