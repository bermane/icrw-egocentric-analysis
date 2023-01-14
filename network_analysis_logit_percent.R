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
  '/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/logit_data_table_percent.RData'
)

##################################
### BASIC DATA TRANSFORMATIONS ###
##################################

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
  'num_alt_not_family',
  'num_alt_is_hw',
  'homo_num_fam',
  'homo_friends',
  'homo_neigh',
  'homo_friends_neigh',
  'homo_age',
  'ego_deg_cen',
  'alt_mean_deg_cen',
  'alt_mean_years_known',
  'alt_mean_talk_subjects',
  'alt_mean_social_influence_index',
  'alt_total_social_influence_index',
  'alt_mean_social_support_index',
  'alt_total_social_support_index'
)

# center all continuous vars around the mean
dat %<>% mutate(across(all_of(cont), ~ scale(.x, scale = F)))

# create list of cont proportion vars to center and scale
cont_pro <- c(
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
  'alt_percent_borrow_rps_hh'
)

# center all continuous vars around the mean
dat %<>% mutate(across(all_of(cont_pro), ~ scale(.x, scale = 0.2)))

# move neighbors_using_modern_fp column to contagion section
dat %<>% relocate(neighbors_using_modern_fp, .after = alt_percent_used_fp)

# move alt_sil and alt_mil
dat %<>% relocate(alt_sil, .after = last_col()) %>%
  relocate(alt_mil, .after = last_col())

################################################################################
### SEPARATE INTO TWO DATASETS BASED ON MOD FP VS. NONE and TRAD FP VS. NONE ###
################################################################################

# remove pregnant
dat %<>% filter(preg == 2)

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

# modern
# dat_mod %>% tabyl(using_mod_fp, state)
# dat_mod %>% tabyl(using_mod_fp, caste)
# dat_mod %>% tabyl(using_mod_fp, want_more_children)
# dat_mod %>% tabyl(using_mod_fp, neighbors_using_modern_fp)
#
# # trad
# dat_trad %>% tabyl(using_trad_fp, state)
# dat_trad %>% tabyl(using_trad_fp, caste)
# dat_trad %>% tabyl(using_trad_fp, want_more_children)
# dat_trad %>% tabyl(using_trad_fp, neighbors_using_modern_fp)

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
      'fp_method'
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
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_modern_none.csv',
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
      'fp_method'
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
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_trad_none.csv',
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

# # create final dat for mod vs. non (not includoing indices)
dat_mod2 <- dat_mod %>% select(-c('using_trad_fp', 'using_any_fp',
                                 'preg', 'ego_id', 'fp_method',
                                 'want_more_children', 'hetero_discuss_fp_blau',
                                 'hetero_discuss_fp_iqv',
                                 'children',
                                 'alt_percent_wants_more_sons',
                                 'homo_using_mod_fp',
                                 'alt_percent_talk_fp_weekly',
                                 'alt_percent_talk_fp_monthly',
                                 'homo_friends_neigh',
                                 'ego_deg_cen',
                                 'alt_mean_deg_cen',
                                 'neighbors_using_modern_fp',
                                 'alt_percent_used_fp',
                                 'alt_total_social_influence_index',
                                 'alt_total_social_support_index',
                                 'alt_mean_social_influence_index',
                                 'alt_mean_social_influence_index',
                                 'sons',
                                 'daughters'))

# create final dat for mod vs. non (including indices)
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
#     'homo_using_mod_fp',
#     'alt_percent_talk_fp_weekly',
#     'alt_percent_talk_fp_monthly',
#     'homo_friends_neigh',
#     'ego_deg_cen',
#     'alt_mean_deg_cen',
#     'neighbors_using_modern_fp',
#     'alt_percent_used_fp',
#     'alt_mean_social_influence_index',
#     'alt_mean_social_support_index',
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
#     'alt_percent_borrow_rps_hh'
#   )
# )


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

# run both stepwise
both_mod <- step(all_mod, direction = 'both')
summary(both_mod)
both_mod$anova
exp(coef(both_mod)) %>% round(3)
confint(both_mod)

# diagnostics
AIC(both_mod)
PseudoR2(both_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
diag <- gof(both_mod, plotROC = F)
diag$gof$pVal
VIF(both_mod)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
AIC(for_mod)
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F)
diag$gof$pVal
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# use interaction of mil and sil
glm <- glm(
  using_mod_fp ~ alt_percent_using_fp +
    parity + alt_percent_borrow_rps_hh + caste +
    alt_percent_discuss_fp_freely + alt_sil*alt_mil,
  family = binomial(link = 'logit'),
  data = dat_mod2
)
summary(glm)
exp(coef(glm)) %>% round(3)
confint(glm)

# diagnostics
AIC(glm)
PseudoR2(glm,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(glm, plotROC = F)
diag$gof$pVal
VIF(glm)

# # best model based on bivariate significance (not using social indices)
# # select model vars
# dat_mod2 <- dat_mod %>% select(
#   c(
#     'using_mod_fp',
#     'parity',
#     'age',
#     'education',
#     'caste',
#     'husband_education',
#     'homo_live_another_village',
#     'alt_percent_wants_more_daughters',
#     'alt_percent_using_fp',
#     'alt_percent_talked_number_children',
#     'alt_percent_not_say_bad_things_fp',
#     'alt_percent_talked_fp_methods',
#     'alt_percent_info_get_fp',
#     'alt_percent_helped_by_ego',
#     'alt_percent_borrow_rps_hh'
#   )
# )
# 
# # drop missing values
# dat_mod2 <- na.omit(dat_mod2)

# # run mod
# sig_mod <- glm(
#   formula = using_mod_fp ~ parity + age + education +
#     caste + husband_education +
#     homo_live_another_village +
#     alt_percent_wants_more_daughters + alt_percent_using_fp +
#     alt_percent_not_say_bad_things_fp +
#     alt_percent_talked_fp_methods +
#     alt_percent_info_get_fp +
#     alt_percent_helped_by_ego +
#     alt_percent_borrow_rps_hh,
#   family = binomial(link = 'logit'),
#   data = dat_mod2
# )
# summary(sig_mod)
# sig_mod$anova
# exp(coef(sig_mod)) %>% round(3)
# confint(sig_mod)
# 
# # diagnostics
# AIC(sig_mod)
# PseudoR2(sig_mod,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(sig_mod, plotROC = F)
# diag$gof$pVal
# VIF(sig_mod)
# 
# # plot residuals
# # should have a norm dist
# # over large enough sample size
# d <- density(residuals(sig_mod, "pearson"))
# plot(d, main = "")
# 
# # run intercept only
# int_mod <- glm(formula = using_mod_fp ~ 1,
#                family = binomial(link = 'logit'),
#                data = dat_mod2)
# 
# # run forward stepwise
# for_mod <-
#   step(int_mod, scope = formula(sig_mod), direction = 'forward')
# summary(for_mod)
# for_mod$anova
# exp(coef(for_mod)) %>% round(3)
# confint(for_mod)
# 
# # diagnostics
# PseudoR2(for_mod,
#          which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))
# 
# diag <- gof(for_mod, plotROC = F)
# diag$gof$pVal
# VIF(for_mod)

#################################
### RUN TRAD vs. NONE MODEL ###
#################################

# # create final dat for mod vs. non (not includoing indices)
dat_trad2 <- dat_trad %>% select(-c('using_mod_fp', 'using_any_fp',
                                 'preg', 'ego_id', 'fp_method',
                                 'want_more_children', 'hetero_discuss_fp_blau',
                                 'hetero_discuss_fp_iqv',
                                 'children',
                                 'alt_percent_wants_more_sons',
                                 'homo_using_mod_fp',
                                 'alt_percent_talk_fp_weekly',
                                 'alt_percent_talk_fp_monthly',
                                 'homo_friends_neigh',
                                 'ego_deg_cen',
                                 'alt_mean_deg_cen',
                                 'neighbors_using_modern_fp',
                                 'alt_percent_used_fp',
                                 'alt_total_social_influence_index',
                                 'alt_total_social_support_index',
                                 'alt_mean_social_influence_index',
                                 'alt_mean_social_support_index',
                                 'sons',
                                 'daughters'))

# create final dat for mod vs. non (including indices)
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
#     'homo_using_mod_fp',
#     'alt_percent_talk_fp_weekly',
#     'alt_percent_talk_fp_monthly',
#     'homo_friends_neigh',
#     'ego_deg_cen',
#     'alt_mean_deg_cen',
#     'neighbors_using_modern_fp',
#     'alt_percent_used_fp',
#     'alt_mean_social_influence_index',
#     'alt_mean_social_support_index',
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
#     'alt_percent_borrow_rps_hh'
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

# run both stepwise
both_mod <- step(all_mod, direction = 'both')
summary(both_mod)
both_mod$anova
exp(coef(both_mod)) %>% round(3)
confint(both_mod)

# diagnostics
AIC(both_mod)
PseudoR2(both_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(both_mod, plotROC = F)
diag$gof$pVal
VIF(both_mod)

# run forward stepwise
for_mod <-
  step(int_mod, scope = formula(all_mod), direction = 'forward')
summary(for_mod)
for_mod$anova
exp(coef(for_mod)) %>% round(3)
confint(for_mod)

# diagnostics
PseudoR2(for_mod,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(for_mod, plotROC = F)
diag$gof$pVal %>% round(3)
VIF(for_mod)

# plot residuals
# should have a norm dist
# over large enough sample size
d <- density(residuals(for_mod, "pearson"))
plot(d, main = "")

# use interaction of mil and sil
glm <- glm(
  using_trad_fp ~ state +
    age + alt_mean_talk_subjects + alt_percent_not_say_bad_things_fp +
    alt_percent_easy_follow_advice + alt_sil*alt_mil,
  family = binomial(link = 'logit'),
  data = dat_trad2
)
summary(glm)
exp(coef(glm)) %>% round(3)
confint(glm)

# diagnostics
AIC(glm)
PseudoR2(glm,
         which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur"))

diag <- gof(glm, plotROC = F)
diag$gof$pVal
VIF(glm)

###########################
### SIMPLE RUN OF STATE ###
###########################

# run model A
glm <- glm(
  formula = using_mod_fp ~ state,
  family = binomial(link = 'logit'),
  data = dat
)

summary(glm)

# calculate odds ratio
exp(coef(glm))

# run model B
glm <- glm(
  formula = using_trad_fp ~ state,
  family = binomial(link = 'logit'),
  data = dat
)

summary(glm)

# calculate odds ratio
exp(coef(glm))

# run model C
glm <- glm(
  formula = using_any_fp ~ state,
  family = binomial(link = 'logit'),
  data = dat
)

summary(glm)

# calculate odds ratio
exp(coef(glm))

########################################
### SIMPLE RUN OF ALTER USING MOD FP ###
########################################

# run model A
glm <- glm(
  formula = using_mod_fp ~ alt_percent_using_fp,
  family = binomial(link = 'logit'),
  data = dat
)

summary(glm)

# calculate odds ratio
exp(coef(glm))

#############################
### RUN MODEL A BIVARIATE ###
#############################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric()
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
      'fp_method'
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
      aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_a_percent.csv',
          row.names = F)

#############################
### RUN MODEL B BIVARIATE ###
#############################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric()
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
      'fp_method'
    )
  )) {
    # run model
    glm <- eval(substitute(
      glm(
        using_trad_fp ~ variable,
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
      aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_b_percent.csv',
          row.names = F)

#############################
### RUN MODEL C BIVARIATE ###
#############################

# allocate output table
univar <- tibble(
  model = character(),
  var_odds = character(),
  var_p_sig = character(),
  var_p = character(),
  aic = numeric()
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
      'fp_method'
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
      aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar,
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_c_percent.csv',
          row.names = F)

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

################################
### FORWARD STEPWISE MODEL C ###
################################

# create final dat for model c
dat_mod_c <-
  dat %>% select(
    -c(
      'using_mod_fp',
      'using_trad_fp',
      'ego_id',
      'preg',
      'fp_method',
      'children',
      'num_alt_not_family',
      'homo_neigh',
      'hetero_discuss_fp_blau',
      'hetero_discuss_fp_iqv',
      'alt_mean_deg_cen',
      'alt_percent_used_fp'
    )
  )

# see where all the missing values are
lapply(dat_mod_c, function(x) {
  sum(is.na(x))
})

# drop missing values
dat_mod_c <- na.omit(dat_mod_c)

# run full model
all_c <- glm(formula = using_any_fp ~ .,
             family = binomial(link = 'logit'),
             data = dat_mod_c)

# run intercept only
int_c <- glm(formula = using_any_fp ~ 1,
             family = binomial(link = 'logit'),
             data = dat_mod_c)

# run both stepwise
both_c <- step(all_c, direction = 'both')
summary(both_c)
both_c$anova

# run forward stepwise
for_c <- step(int_c, scope = formula(all_c), direction = 'forward')
summary(for_c)
for_c$anova
