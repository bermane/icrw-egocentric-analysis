# this code loads csv data of ego and alter interviews
# and calculates tie intensity and strength

# load packages
library(magrittr)
library(egor)
library(janitor)
library(tidyverse)
library(readxl)
library(igraph)
library(readstata13)

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned data
ego <- read_excel(path = "data/ego_clean_11012022.xlsx")
alter <-read_excel(path = "data/alter_clean_11012022.xlsx")

# load dta stata file to get PC data for egos
ego_pc <- read.dta13(file = 'data/ego_clean_12012022de.dta', convert.factors = F)
# alter <- read.dta13(file = 'data/alter_clean_12012022de.dta', convert.factors = F)

# load list of duplicate ids
dup_id <- read_excel(path = 'data/duplicate_ids_02022022.xlsx')

# remove first row with 'qe' names
ego <- ego[-1,]
alter <- alter[-1,]

# clean column names only keep part after last "."
ego_names <- sapply(colnames(ego), FUN = function(x){
  st <- str_split(x, pattern = glob2rx("*-*"))
  st <- st[[1]][length(st[[1]])]
  return(st)
})

names(ego_names) <- NULL
colnames(ego) <- ego_names

alter_names <- sapply(colnames(alter), FUN = function(x){
  st <- str_split(x, pattern = glob2rx("*-*"))
  st <- st[[1]][length(st[[1]])]
  return(st)
})

names(alter_names) <- NULL
colnames(alter) <- alter_names

rm(ego_names, alter_names)

# remove empty columns from "notes"
ego <- ego[,str_detect(colnames(ego), 'note') == F]
alter <- alter[,str_detect(colnames(alter), 'note') == F]

# replace character NA values with NA
ego[ego == "NA"] <- NA
alter[alter == "NA"] <- NA

# fix district, block and village in alter data
alter$district_name <- alter$`district name_clean`
alter$block_name <- alter$`block name_clean`
alter$village_name <- alter$`village name_clean`

# remove "clean" columns
alter <- alter %>% select(-c(`district name_clean`, `block name_clean`, `village name_clean`))

# add alter-alter tie columns to ego dataset so we can build edgelist below
# let's first extract the correct question so easier to look at
alter_know <- ego[,str_detect(colnames(ego), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to 0 since then tie won't exist. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know %<>% as.data.frame %>% 
  mutate(know12 = alter1_know2 + alter2_know1,
         know13 = alter1_know3 + alter3_know1,
         know14 = alter1_know4 + alter4_know1,
         know15 = alter1_know5 + alter5_know1,
         know23 = alter2_know3 + alter3_know2,
         know24 = alter2_know4 + alter4_know2,
         know25 = alter2_know5 + alter5_know2,
         know34 = alter3_know4 + alter4_know3,
         know35 = alter3_know5 + alter5_know3,
         know45 = alter4_know5 + alter5_know4)

# now set 0 to NA since we just want to identify tie or not
alter_know %<>% select(know12:know45)
alter_know[alter_know == 0] <- NA

# add simple know columns back into ego df
ego %<>% add_column(alter_know)
rm(alter_know)

# add aalter-aalter tie columns to alter dataset so we can build edgelist below
# let's first extract the correct question so easier to look at
alter_know <- alter[,str_detect(colnames(alter), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to 0 since then tie won't exist. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know %<>% as.data.frame %>% 
  mutate(know12 = alter1_know2 + alter2_know1,
         know13 = alter1_know3 + alter3_know1,
         know14 = alter1_know4 + alter4_know1,
         know15 = alter1_know5 + alter5_know1,
         know23 = alter2_know3 + alter3_know2,
         know24 = alter2_know4 + alter4_know2,
         know25 = alter2_know5 + alter5_know2,
         know34 = alter3_know4 + alter4_know3,
         know35 = alter3_know5 + alter5_know3,
         know45 = alter4_know5 + alter5_know4)

# now set 0 to NA since we just want to identify tie or not
alter_know %<>% select(know12:know45)
alter_know[alter_know == 0] <- NA

# add simple know columns back into alter df
alter %<>% add_column(alter_know)
rm(alter_know)

##########
## EGO ###
##########

######################################################
### BUILD TIBBLE OF ATTRIBUTES OF INTEREST FOR EGO ###
######################################################

# length of relationship
# talking freq
# talking freq fp
# number of different things discussed

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

# build tibble add all alters
ego_tie <- rbind(tibble(ego_id = ego$woman_id,
                        ego_age = ego_pc$pcq102,
                        ego_age_married = ego_pc$pcq201b,
                        alter_name = ego$alter1,
                        district = ego$district_name,
                        block = ego$block_name,
                        yrs_known = ego$alter1_yrs_known,
                        talk_freq = ego$alter1_talk_freq,
                        talk_freq_fp = ego$alter1_freq_talk_fp,
                        subjects = ego$alter1_subjects,
                        subjects_other = ego$alter1_subjects_other),
               tibble(ego_id = ego$woman_id,
                      ego_age = ego_pc$pcq102,
                      ego_age_married = ego_pc$pcq201b,
                      alter_name = ego$alter2,
                      district = ego$district_name,
                      block = ego$block_name,
                      yrs_known = ego$alter2_yrs_known,
                      talk_freq = ego$alter2_talk_freq,
                      talk_freq_fp = ego$alter2_freq_talk_fp,
                      subjects = ego$alter2_subjects,
                      subjects_other = ego$alter2_subjects_other),
               tibble(ego_id = ego$woman_id,
                      ego_age = ego_pc$pcq102,
                      ego_age_married = ego_pc$pcq201b,
                      alter_name = ego$alter3,
                      district = ego$district_name,
                      block = ego$block_name,
                      yrs_known = ego$alter3_yrs_known,
                      talk_freq = ego$alter3_talk_freq,
                      talk_freq_fp = ego$alter3_freq_talk_fp,
                      subjects = ego$alter3_subjects,
                      subjects_other = ego$alter3_subjects_other),
               tibble(ego_id = ego$woman_id,
                      ego_age = ego_pc$pcq102,
                      ego_age_married = ego_pc$pcq201b,
                      alter_name = ego$alter4,
                      district = ego$district_name,
                      block = ego$block_name,
                      yrs_known = ego$alter4_yrs_known,
                      talk_freq = ego$alter4_talk_freq,
                      talk_freq_fp = ego$alter4_freq_talk_fp,
                      subjects = ego$alter4_subjects,
                      subjects_other = ego$alter4_subjects_other),
               tibble(ego_id = ego$woman_id,
                      ego_age = ego_pc$pcq102,
                      ego_age_married = ego_pc$pcq201b,
                      alter_name = ego$alter5,
                      district = ego$district_name,
                      block = ego$block_name,
                      yrs_known = ego$alter5_yrs_known,
                      talk_freq = ego$alter5_talk_freq,
                      talk_freq_fp = ego$alter5_freq_talk_fp,
                      subjects = ego$alter5_subjects,
                      subjects_other = ego$alter5_subjects_other))

# drop rows for missing alters
ego_tie %<>% filter(is.na(alter_name) == F)

##############################
### LENGTH OF RELATIONSHIP ###
##############################

# check unique values for length of relationship
unique(ego_tie$yrs_known)

# set to numeric
ego_tie$yrs_known <- as.numeric(ego_tie$yrs_known)

# reset values of 99 based on ego age and age of marriage
ego_tie$yrs_known[ego_tie$yrs_known == 99] <- ego_tie$ego_age[ego_tie$yrs_known == 99] - ego_tie$ego_age_married[ego_tie$yrs_known == 99]

# re check unique values for length of relationship
unique(ego_tie$yrs_known)

# calculate summary data overall
ties <- ego_tie %>% 
  summarise(mean_yrs_known = mean(yrs_known, na.rm = T),
                                 med_yrs_known = median(yrs_known, na.rm = T),
                                 mean_yrs_known_perc_age = mean(yrs_known/ego_age, na.rm = T),
                                 med_yrs_known_perc_age = median(yrs_known/ego_age, na.rm = T))

write.csv(ties, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_yrs_known.csv',
          row.names = F)

# calculate summary data by block
ties_block <- ego_tie %>% 
  group_by(block) %>%
  summarise(district = district[1],
            mean_yrs_known = mean(yrs_known, na.rm = T),
            med_yrs_known = median(yrs_known, na.rm = T),
            mean_yrs_known_perc_age = mean(yrs_known/ego_age, na.rm = T),
            med_yrs_known_perc_age = median(yrs_known/ego_age, na.rm = T))

write.csv(ties_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_yrs_known_block.csv',
          row.names = F)

#################################
### TALKING FREQUENCY OVERALL ###
#################################

# check unique values for talking freq overall
table(ego_tie$talk_freq)

# recode column
ego_tie %<>% mutate(talk_freq = recode(talk_freq, 
                                       `1` = 'intensive',
                                       `2` = 'intensive',
                                       `3` = 'intensive',
                                       `4` = 'moderate',
                                       `5` = 'moderate',
                                       `6` = 'minimal',
                                       `7` = 'minimal'))

# overall summary
talk_freq <- ego_tie %>% tabyl(talk_freq)

write.csv(talk_freq, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_talk_freq.csv',
          row.names = F)

# summary by block
talk_freq_block <- ego_tie %>% tabyl(block, talk_freq)
# ego_tie %>% split(.$block) %>% map(~map(.x %>% select(talk_freq), tabyl))

write.csv(talk_freq_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_talk_freq_block.csv',
          row.names = F)

############################################
### TALKING FREQUENCY IN FAMILY PLANNING ###
############################################

# check unique values for talking freq fp
table(ego_tie$talk_freq_fp)

# recode column
ego_tie %<>% mutate(talk_freq_fp = recode(talk_freq_fp, 
                                       `1` = 'intensive',
                                       `2` = 'intensive',
                                       `3` = 'moderate',
                                       `4` = 'moderate',
                                       `5` = 'minimal')) %>%
  mutate(talk_freq_fp = replace_na(talk_freq_fp, 'none'))

# overall summary
talk_freq_fp <- ego_tie %>% tabyl(talk_freq_fp)

write.csv(talk_freq_fp, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_talk_freq_fp.csv',
          row.names = F)

# summary by block
talk_freq_fp_block <- ego_tie %>% tabyl(block, talk_freq_fp)
# ego_tie %>% split(.$block) %>% map(~map(.x %>% select(talk_freq_fp), tabyl))

write.csv(talk_freq_fp_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_talk_freq_fp_block.csv',
          row.names = F)

#################################################
### MULTIPLEXITY NUMBER OF DIFFERENT SUBJECTS ###
#################################################

# check values of subjects
unique(ego_tie$subjects)

# count number of subjects
num_sub <- str_split(ego_tie$subjects, ',')
num_sub <- sapply(num_sub, function(x) length(x))

# add to tibble
ego_tie$num_subjects <- num_sub

# add one if mentioned other subjects
ego_tie$num_subjects[is.na(ego_tie$subjects_other) == F] <- ego_tie$num_subjects[is.na(ego_tie$subjects_other) == F] + 1

# overall summary
num_sub <- ego_tie %>% tabyl(num_subjects)

write.csv(num_sub, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_num_subjects.csv',
          row.names = F)

# summary by block
num_sub_block <- ego_tie %>% tabyl(block, num_subjects)
# ego_tie %>% split(.$block) %>% map(~map(.x %>% select(num_subjects), tabyl))

write.csv(num_sub_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_num_subjects_block.csv',
          row.names = F)

############
## ALTER ###
############

######################################################
### BUILD TIBBLE OF ATTRIBUTES OF INTEREST FOR ALTER ###
######################################################

# length of relationship
# talking freq
# talking freq fp
# number of different things discussed

# build tibble add all aalters
alter_tie <- rbind(tibble(alter_id = alter$alter_id,
                        alter_age = alter$age,
                        aalter_name = alter$alter1,
                        district = alter$district_name,
                        block = alter$block_name,
                        yrs_known = alter$alter1_yrs_known,
                        talk_freq = alter$alter1_talk_freq,
                        talk_freq_fp = alter$alter1_freq_talk_fp,
                        subjects = alter$alter1_subjects,
                        subjects_other = alter$alter1_subjects_other),
                   tibble(alter_id = alter$alter_id,
                          alter_age = alter$age,
                          aalter_name = alter$alter2,
                          district = alter$district_name,
                          block = alter$block_name,
                          yrs_known = alter$alter2_yrs_known,
                          talk_freq = alter$alter2_talk_freq,
                          talk_freq_fp = alter$alter2_freq_talk_fp,
                          subjects = alter$alter2_subjects,
                          subjects_other = alter$alter2_subjects_other),
                   tibble(alter_id = alter$alter_id,
                          alter_age = alter$age,
                          aalter_name = alter$alter3,
                          district = alter$district_name,
                          block = alter$block_name,
                          yrs_known = alter$alter3_yrs_known,
                          talk_freq = alter$alter3_talk_freq,
                          talk_freq_fp = alter$alter3_freq_talk_fp,
                          subjects = alter$alter3_subjects,
                          subjects_other = alter$alter3_subjects_other),
                   tibble(alter_id = alter$alter_id,
                          alter_age = alter$age,
                          aalter_name = alter$alter4,
                          district = alter$district_name,
                          block = alter$block_name,
                          yrs_known = alter$alter4_yrs_known,
                          talk_freq = alter$alter4_talk_freq,
                          talk_freq_fp = alter$alter4_freq_talk_fp,
                          subjects = alter$alter4_subjects,
                          subjects_other = alter$alter4_subjects_other),
                   tibble(alter_id = alter$alter_id,
                          alter_age = alter$age,
                          aalter_name = alter$alter5,
                          district = alter$district_name,
                          block = alter$block_name,
                          yrs_known = alter$alter5_yrs_known,
                          talk_freq = alter$alter5_talk_freq,
                          talk_freq_fp = alter$alter5_freq_talk_fp,
                          subjects = alter$alter5_subjects,
                          subjects_other = alter$alter5_subjects_other))

# drop rows for missing alters
alter_tie %<>% filter(is.na(aalter_name) == F)

# drop rows for incomplete surveys
alter_tie %<>% filter(is.na(yrs_known) == F)

##############################
### LENGTH OF RELATIONSHIP ###
##############################

# check unique values for length of relationship
unique(alter_tie$yrs_known)

# set to numeric
alter_tie$yrs_known <- as.numeric(alter_tie$yrs_known)

# reset values of 99 based on alter age and age of marriage
# we don't have alter age of marriage!!!
# ego_tie$yrs_known[ego_tie$yrs_known == 99] <- ego_tie$ego_age[ego_tie$yrs_known == 99] - ego_tie$ego_age_married[ego_tie$yrs_known == 99]

# re check unique values for length of relationship
unique(alter_tie$yrs_known)

# # calculate summary data overall
# ties <- alter_tie %>% 
#   summarise(mean_yrs_known = mean(yrs_known, na.rm = T),
#             med_yrs_known = median(yrs_known, na.rm = T))
# 
# write.csv(ties, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_yrs_known.csv',
#           row.names = F)
# 
# # calculate summary data by block
# ties_block <- ego_tie %>% 
#   group_by(block) %>%
#   summarise(district = district[1],
#             mean_yrs_known = mean(yrs_known, na.rm = T),
#             med_yrs_known = median(yrs_known, na.rm = T),
#             mean_yrs_known_perc_age = mean(yrs_known/ego_age, na.rm = T),
#             med_yrs_known_perc_age = median(yrs_known/ego_age, na.rm = T))
# 
# write.csv(ties_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/ego_yrs_known_block.csv',
#           row.names = F)

# just show table of alter values
ties <- alter_tie %>% tabyl(yrs_known)

write.csv(ties, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_yrs_known.csv',
          row.names = F)

#################################
### TALKING FREQUENCY OVERALL ###
#################################

# check unique values for talking freq overall
table(alter_tie$talk_freq)

# recode column
alter_tie %<>% mutate(talk_freq = recode(talk_freq, 
                                       `1.0` = 'intensive',
                                       `2.0` = 'intensive',
                                       `3.0` = 'intensive',
                                       `4.0` = 'moderate',
                                       `5.0` = 'moderate',
                                       `6.0` = 'minimal',
                                       `7.0` = 'minimal'))

# overall summary
talk_freq <- alter_tie %>% tabyl(talk_freq)

write.csv(talk_freq, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_talk_freq.csv',
          row.names = F)

# summary by block
talk_freq_block <- alter_tie %>% tabyl(block, talk_freq)
# ego_tie %>% split(.$block) %>% map(~map(.x %>% select(talk_freq), tabyl))

write.csv(talk_freq_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_talk_freq_block.csv',
          row.names = F)

############################################
### TALKING FREQUENCY IN FAMILY PLANNING ###
############################################

# check unique values for talking freq fp
table(alter_tie$talk_freq_fp)

# recode column
alter_tie %<>% mutate(talk_freq_fp = recode(talk_freq_fp, 
                                          `1.0` = 'intensive',
                                          `2.0` = 'intensive',
                                          `3.0` = 'moderate',
                                          `4.0` = 'moderate',
                                          `5.0` = 'minimal')) %>%
  mutate(talk_freq_fp = replace_na(talk_freq_fp, 'none'))

# overall summary
talk_freq_fp <- alter_tie %>% tabyl(talk_freq_fp)

write.csv(talk_freq_fp, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_talk_freq_fp.csv',
          row.names = F)

# summary by block
talk_freq_fp_block <- alter_tie %>% tabyl(block, talk_freq_fp)
# ego_tie %>% split(.$block) %>% map(~map(.x %>% select(talk_freq_fp), tabyl))

write.csv(talk_freq_fp_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_talk_freq_fp_block.csv',
          row.names = F)

#################################################
### MULTIPLEXITY NUMBER OF DIFFERENT SUBJECTS ###
#################################################

# check values of subjects
unique(alter_tie$subjects)

# count number of subjects
num_sub <- str_split(alter_tie$subjects, ',')
num_sub <- sapply(num_sub, function(x) length(x))

# add to tibble
alter_tie$num_subjects <- num_sub

# add one if mentioned other subjects
alter_tie$num_subjects[is.na(alter_tie$subjects_other) == F] <- alter_tie$num_subjects[is.na(alter_tie$subjects_other) == F] + 1

# overall summary
num_sub <- alter_tie %>% tabyl(num_subjects)

write.csv(num_sub, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_num_subjects.csv',
          row.names = F)

# summary by block
num_sub_block <- alter_tie %>% tabyl(block, num_subjects)
# ego_tie %>% split(.$block) %>% map(~map(.x %>% select(num_subjects), tabyl))

write.csv(num_sub_block, '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/tie_strength_intensity/alter_num_subjects_block.csv',
          row.names = F)

