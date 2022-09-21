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

##############################
### RUN MODEL A UNIVARIATE ###
##############################

# allocate output table
univar <- tibble(model = character(),
                 intercept_odds = numeric(),
                 var_odds = character(),
                 intercept_p = numeric(),
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
    
    # add to output table
    univar %<>% add_row(model = var,
                        intercept_odds = exp(coef(glm))[1] %>% round(3),
                        var_odds = str_c(exp(coef(glm))[2:length(exp(coef(glm)))] %>% round(3), collapse = ', '),
                        intercept_p = coef(summary(glm))[1,4] %>% round(4),
                        var_p = str_c(coef(summary(glm))[2:NROW(coef(summary(glm))),4] %>% round(4), collapse = ', '),
                        aic = glm$aic
    )
    
  }
}

# output univariate table
write.csv(univar, 
          file = '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/LOGIT_RESULTS/univar_mod_a.csv',
          row.names = F)
