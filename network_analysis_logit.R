# this code runs logit models on ego data

# load packages
library(tidyverse)
library(magrittr)
library(janitor)

# load data
load('/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/logit_data_table.RData')

###########################
### SIMPLE RUN OF STATE ###
###########################

# run model A
glm <- glm(formula = using_mod_fp ~ state,
           family = binomial(link = 'logit'),
           data = dat)

summary(glm)

# calculate odds ratio
exp(coef(glm))

# run model B
glm <- glm(formula = using_trad_fp ~ state,
           family = binomial(link = 'logit'),
           data = dat)

summary(glm)

# calculate odds ratio
exp(coef(glm))

# run model C
glm <- glm(formula = using_any_fp ~ state,
           family = binomial(link = 'logit'),
           data = dat)

summary(glm)

# calculate odds ratio
exp(coef(glm))

########################################
### SIMPLE RUN OF ALTER USING MOD FP ###
########################################

# run model A
glm <- glm(formula = using_mod_fp ~ alt_n_using_mod_fp,
           family = binomial(link = 'logit'),
           data = dat)

summary(glm)

# calculate odds ratio
exp(coef(glm))

#############################
### RUN MODEL A BIVARIATE ###
#############################

# allocate output table
univar <- tibble(model = character(),
                 var_odds = character(),
                 var_p_sig = character(),
                 var_p = character(),
                 aic = numeric())

# loop through all variables
for(var in colnames(dat)){
  # only run for indep vars
  if(!(var %in% c('using_any_fp', 'using_mod_fp', 'using_trad_fp', 'ego_id',
                'preg', 'fp_method'))){
    
    # run model
    glm <- eval(substitute(glm(using_mod_fp ~ variable,
                               family = binomial(link = 'logit'),
                               data = dat),
                           list(variable = as.name(var))))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))),4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x){
      if(x < 0.001){
        p_sig <- '***'
      }else if(x < 0.01 & x >= 0.001){
        p_sig <- '**'
      }else if(x < 0.05 & x >= 0.01){
        p_sig <- '*'
      }else if(x < 0.1 & x >= 0.05){
        p_sig <- '.'
      }else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(model = var,
                        var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
                        var_p_sig = str_c(p_sig, collapse = ', '),
                        var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))),4] %>% round(5), collapse = ', '),
                        aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar, 
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_a.csv',
          row.names = F)

#############################
### RUN MODEL B BIVARIATE ###
#############################

# allocate output table
univar <- tibble(model = character(),
                 var_odds = character(),
                 var_p_sig = character(),
                 var_p = character(),
                 aic = numeric())

# loop through all variables
for(var in colnames(dat)){
  # only run for indep vars
  if(!(var %in% c('using_any_fp', 'using_mod_fp', 'using_trad_fp', 'ego_id',
                  'preg', 'fp_method'))){
    
    # run model
    glm <- eval(substitute(glm(using_trad_fp ~ variable,
                               family = binomial(link = 'logit'),
                               data = dat),
                           list(variable = as.name(var))))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))),4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x){
      if(x < 0.001){
        p_sig <- '***'
      }else if(x < 0.01 & x >= 0.001){
        p_sig <- '**'
      }else if(x < 0.05 & x >= 0.01){
        p_sig <- '*'
      }else if(x < 0.1 & x >= 0.05){
        p_sig <- '.'
      }else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(model = var,
                        var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
                        var_p_sig = str_c(p_sig, collapse = ', '),
                        var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))),4] %>% round(5), collapse = ', '),
                        aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar, 
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_b.csv',
          row.names = F)

#############################
### RUN MODEL C BIVARIATE ###
#############################

# allocate output table
univar <- tibble(model = character(),
                 var_odds = character(),
                 var_p_sig = character(),
                 var_p = character(),
                 aic = numeric())

# loop through all variables
for(var in colnames(dat)){
  # only run for indep vars
  if(!(var %in% c('using_any_fp', 'using_mod_fp', 'using_trad_fp', 'ego_id',
                  'preg', 'fp_method'))){
    
    # run model
    glm <- eval(substitute(glm(using_any_fp ~ variable,
                               family = binomial(link = 'logit'),
                               data = dat),
                           list(variable = as.name(var))))
    
    # grab p valute of variable
    var_p <- coef(summary(glm))[2:NROW(coef(summary(glm))),4]
    
    # calculate p val significance
    p_sig <- sapply(var_p, function(x){
      if(x < 0.001){
        p_sig <- '***'
      }else if(x < 0.01 & x >= 0.001){
        p_sig <- '**'
      }else if(x < 0.05 & x >= 0.01){
        p_sig <- '*'
      }else if(x < 0.1 & x >= 0.05){
        p_sig <- '.'
      }else{
        p_sig <- ' '
      }
    })
    
    # add to output table
    univar %<>% add_row(model = var,
                        var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
                        var_p_sig = str_c(p_sig, collapse = ', '),
                        var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))),4] %>% round(5), collapse = ', '),
                        aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar, 
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_c.csv',
          row.names = F)
