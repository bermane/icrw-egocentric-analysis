# this code loads csv data of ego and alter interviews creates a single data table to be used for 
# logit modelling

# load packages
library(magrittr)
library(egor)
library(janitor)
library(tidyverse)
library(readxl)
library(igraph)
library(readstata13)
library(writexl)

#######################
### LOAD BIHAR DATA ###
#######################

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned and completre data
ego <- read_excel(path = "data/ego_clean_complete_11012022.xlsx")
alter <-read_excel(path = "data/alter_clean_complete_11012022.xlsx")

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/ego_clean_12012022de.dta', convert.factors = F)
# 
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_12012022.xlsx")

# load pc data for egos
ego_pc <- read_excel(path = 'data/ego_pc_complete_12012022.xlsx')

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

###########################################
### CREATE INITIAL DATA TABLE OF VALUES ###
###########################################

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

dat_bihar <- tibble(ego_id = ego$woman_id,
              state = 'bihar',
              mcontra_neigh_num = ego$contra_neighbour_num,
              preg = ego_pc$pcq230,
              using_fp = ego_pc$pcq311,
              fp_method = ego_pc$pcq312,
              age = ego_pc$pcq102,
              education = ego_pc$pcq103,
              caste = ego_pc$pcq111,
              husband_education = ego_pc$pcq115)

##################################
### CALCULATE HOMOPHILY OF EGO ###
##################################

# gender
# education
# number of family members
# number of non-family members close to
# practicing FP
# network composition: family/non-family (?)

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

# build tibble of ego and alter attributes
ea <- rbind(tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter1,
                   ego_sex = 2,
                   alt_sex = ego$alter1_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter1_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter1_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter1_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter1_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter1_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter1_asha,
                   alt_know_anm = ego$alter1_anm,
                   alt_know_aww = ego$alter1_aww,
                   alt_know_shg = ego$alter1_shg,
                   alt_know_pra = ego$alter1_pradhan,
                   alt_know_pha = ego$alter1_pharmacist,
                   alt_know_doc = ego$alter1_doctor,
                   alt_know_rel = ego$alter1_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter1_sons,
                   alt_daughters = ego$alter1_daughters,
                   alt_residence = ego$alter1_residence,
                   alt_relationship = ego$alter1r,
                   alt_learned = ego$alter1_learned,
                   alt_encourage_fp = ego$alter1_encourage_fp,
                   alt_discuss_fp = ego$alter1_discuss_fp,
                   alt_help_ego = ego$alter1_help,
                   alt_helped_by_ego = ego$alter1_helped,
                   alt_against_advice = ego$alter1_follow_advice,
                   alt_support_no_child = ego$alter1_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter2,
                   ego_sex = 2,
                   alt_sex = ego$alter2_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter2_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter2_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter2_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter2_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter2_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter2_asha,
                   alt_know_anm = ego$alter2_anm,
                   alt_know_aww = ego$alter2_aww,
                   alt_know_shg = ego$alter2_shg,
                   alt_know_pra = ego$alter2_pradhan,
                   alt_know_pha = ego$alter2_pharmacist,
                   alt_know_doc = ego$alter2_doctor,
                   alt_know_rel = ego$alter2_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter2_sons,
                   alt_daughters = ego$alter2_daughters,
                   alt_residence = ego$alter2_residence,
                   alt_relationship = ego$alter2r,
                   alt_learned = ego$alter2_learned,
                   alt_encourage_fp = ego$alter2_encourage_fp,
                   alt_discuss_fp = ego$alter2_discuss_fp,
                   alt_help_ego = ego$alter2_help,
                   alt_helped_by_ego = ego$alter2_helped,
                   alt_against_advice = ego$alter2_follow_advice,
                   alt_support_no_child = ego$alter2_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter3,
                   ego_sex = 2,
                   alt_sex = ego$alter3_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter3_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter3_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter3_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter3_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter3_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter3_asha,
                   alt_know_anm = ego$alter3_anm,
                   alt_know_aww = ego$alter3_aww,
                   alt_know_shg = ego$alter3_shg,
                   alt_know_pra = ego$alter3_pradhan,
                   alt_know_pha = ego$alter3_pharmacist,
                   alt_know_doc = ego$alter3_doctor,
                   alt_know_rel = ego$alter3_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter3_sons,
                   alt_daughters = ego$alter3_daughters,
                   alt_residence = ego$alter3_residence,
                   alt_relationship = ego$alter3r,
                   alt_learned = ego$alter3_learned,
                   alt_encourage_fp = ego$alter3_encourage_fp,
                   alt_discuss_fp = ego$alter3_discuss_fp,
                   alt_help_ego = ego$alter3_help,
                   alt_helped_by_ego = ego$alter3_helped,
                   alt_against_advice = ego$alter3_follow_advice,
                   alt_support_no_child = ego$alter3_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter4,
                   ego_sex = 2,
                   alt_sex = ego$alter4_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter4_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter4_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter4_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter4_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter4_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter4_asha,
                   alt_know_anm = ego$alter4_anm,
                   alt_know_aww = ego$alter4_aww,
                   alt_know_shg = ego$alter4_shg,
                   alt_know_pra = ego$alter4_pradhan,
                   alt_know_pha = ego$alter4_pharmacist,
                   alt_know_doc = ego$alter4_doctor,
                   alt_know_rel = ego$alter4_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter4_sons,
                   alt_daughters = ego$alter4_daughters,
                   alt_residence = ego$alter4_residence,
                   alt_relationship = ego$alter4r,
                   alt_learned = ego$alter4_learned,
                   alt_encourage_fp = ego$alter4_encourage_fp,
                   alt_discuss_fp = ego$alter4_discuss_fp,
                   alt_help_ego = ego$alter4_help,
                   alt_helped_by_ego = ego$alter4_helped,
                   alt_against_advice = ego$alter4_follow_advice,
                   alt_support_no_child = ego$alter4_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter5,
                   ego_sex = 2,
                   alt_sex = ego$alter5_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter5_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter5_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter5_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter5_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter5_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter5_asha,
                   alt_know_anm = ego$alter5_anm,
                   alt_know_aww = ego$alter5_aww,
                   alt_know_shg = ego$alter5_shg,
                   alt_know_pra = ego$alter5_pradhan,
                   alt_know_pha = ego$alter5_pharmacist,
                   alt_know_doc = ego$alter5_doctor,
                   alt_know_rel = ego$alter5_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter5_sons,
                   alt_daughters = ego$alter5_daughters,
                   alt_residence = ego$alter5_residence,
                   alt_relationship = ego$alter5r,
                   alt_learned = ego$alter5_learned,
                   alt_encourage_fp = ego$alter5_encourage_fp,
                   alt_discuss_fp = ego$alter5_discuss_fp,
                   alt_help_ego = ego$alter5_help,
                   alt_helped_by_ego = ego$alter5_helped,
                   alt_against_advice = ego$alter5_follow_advice,
                   alt_support_no_child = ego$alter5_nochild))

# drop rows for missing alters
ea %<>% filter(is.na(alter_name) == F)

##############
### GENDER ###
##############

# check unique values
# unique(ea$ego_sex)
# unique(ea$alt_sex)

# set to numeric
ea$alt_sex <- as.numeric(ea$alt_sex)

# summarise and create homophily output (since first var)
homo <- ea %>% 
  group_by(ego_id) %>% # group by ego id
  mutate(sum = sum(ego_sex == alt_sex, na.rm = T), 
         n = NROW(ego_sex[is.na(ego_sex) == F & is.na(alt_sex) == F]), 
         homo_gender = sum/n) %>% # calculate homophily
  select(ego_id, homo_gender) %>% # only keep homophily
  distinct %>% # remove duplicates
  arrange(ego_id) # arrange by ego id

#############
### CASTE ###
#############

# check unique values
# unique(ea$ego_caste)
# unique(ea$alt_caste)

# set to numeric
ea$alt_caste <- as.numeric(ea$alt_caste)

# set don't know to NA
ea$alt_caste[ea$alt_caste == 9] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_caste == alt_caste, na.rm = T), 
                     n = NROW(ego_caste[is.na(ego_caste) == F & is.na(alt_caste) == F]), 
                     homo_caste = sum/n) %>% # calculate homophily
              select(ego_id, homo_caste) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
  , by = 'ego_id')

#################
### EDUCATION ###
#################

# DON'T HAVE ALTER EDUCATION FROM EGO SURVEY !!!

################################
### NUMBER OF FAMILY MEMBERS ###
################################

# check unique values
# unique(ea$ego_fam)
# unique(ea$alt_fam)

# convert to numeric
ea$ego_fam <- as.numeric(ea$ego_fam)
ea$alt_fam <- as.numeric(ea$alt_fam)

# set don't know to NA
ea$alt_fam[ea$alt_fam == 99] <- NA

# create formula to calculate sd homophily
homo_sd <- function(ego, alt){
  out <- alt - ego
  out <- out*out
  n <- length(out[is.na(out) == F])
  out <- sum(out, na.rm = T)
  out <- out/n
  return(c(round(sqrt(out), 2), n))
}

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(homo_num_fam = homo_sd(ego_fam, alt_fam)[1]) %>% # calculate homophily
              select(ego_id, homo_num_fam) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#######################################
### NUMBER OF FRIENDS AND NEIGHBORS ###
#######################################

# check unique values
# unique(ea$ego_friends)
# unique(ea$alt_friends)
# unique(ea$ego_neigh)
# unique(ea$alt_neigh)

# convert to numeric
ea$ego_friends <- as.numeric(ea$ego_friends)
ea$alt_friends <- as.numeric(ea$alt_friends)
ea$ego_neigh <- as.numeric(ea$ego_neigh)
ea$alt_neigh <- as.numeric(ea$alt_neigh)

# set don't know to NA
ea$alt_friends[ea$alt_friends == 99] <- NA
ea$alt_neigh[ea$alt_neigh == 99] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(homo_friends = homo_sd(ego_friends, alt_friends)[1],
                     homo_neigh = homo_sd(ego_neigh, alt_neigh)[1],
                     homo_friends_neigh = homo_sd(ego_friends + ego_neigh, alt_friends + alt_neigh)[1]) %>% # calculate homophily
              select(ego_id, homo_friends, homo_neigh, homo_friends_neigh) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
# unique(ea$ego_using_fp)
# unique(ea$alt_using_fp)

# set values to numeric
ea$alt_using_fp <- as.numeric(ea$alt_using_fp)

# if alter is husband change to same value as ego
ea$alt_using_fp[str_detect(ea$alt_relationship, 'Husband')] <- ea$ego_using_fp[str_detect(ea$alt_relationship, 'Husband')]

# set don't know to NA
ea$alt_using_fp[ea$alt_using_fp == 9] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_using_fp == alt_using_fp, na.rm = T), 
                     n = NROW(ego_using_fp[is.na(ego_using_fp) == F & is.na(alt_using_fp) == F]), 
                     homo_using_fp = sum/n) %>% # calculate homophily
              select(ego_id, homo_using_fp) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

###############################
### PEOPLE KNOWN IN VILLAGE ###
###############################

# check some values
# unique(ea$ego_know_asha)
# unique(ea$alt_know_asha)

# for ego, 1 is Yes, 2 is heard, 9 is don't know/haven't heard
# for alter 1 is Yes, 2 is No, 9 is not sure
# we should change ego values of 9 to 2 to match alter
# remove alter rows with 9

# do individuals first
# asha
# recode values
ea$ego_know_asha[ea$ego_know_asha == '9'] <- '2'
ea$alt_know_asha[ea$alt_know_asha == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_asha == alt_know_asha, na.rm = T), 
                     n = NROW(ego_know_asha[is.na(ego_know_asha) == F & is.na(alt_know_asha) == F]), 
                     homo_know_asha = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_asha) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# anm
# recode values
ea$ego_know_anm[ea$ego_know_anm == '9'] <- '2'
ea$alt_know_anm[ea$alt_know_anm == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_anm == alt_know_anm, na.rm = T), 
                     n = NROW(ego_know_anm[is.na(ego_know_anm) == F & is.na(alt_know_anm) == F]), 
                     homo_know_anm = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_anm) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# aww
# recode values
ea$ego_know_aww[ea$ego_know_aww == '9'] <- '2'
ea$alt_know_aww[ea$alt_know_aww == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_aww == alt_know_aww, na.rm = T), 
                     n = NROW(ego_know_aww[is.na(ego_know_aww) == F & is.na(alt_know_aww) == F]), 
                     homo_know_aww = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_aww) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# shg
# recode values
ea$ego_know_shg[ea$ego_know_shg == '9'] <- '2'
ea$alt_know_shg[ea$alt_know_shg == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_shg == alt_know_shg, na.rm = T), 
                     n = NROW(ego_know_shg[is.na(ego_know_shg) == F & is.na(alt_know_shg) == F]), 
                     homo_know_shg = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_shg) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# pradhan
# recode values
ea$ego_know_pra[ea$ego_know_pra == '9'] <- '2'
ea$alt_know_pra[ea$alt_know_pra == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_pra == alt_know_pra, na.rm = T), 
                     n = NROW(ego_know_pra[is.na(ego_know_pra) == F & is.na(alt_know_pra) == F]), 
                     homo_know_pra = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_pra) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# pharmacist
# recode values
ea$ego_know_pha[ea$ego_know_pha == '9'] <- '2'
ea$alt_know_pha[ea$alt_know_pha == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_pha == alt_know_pha, na.rm = T), 
                     n = NROW(ego_know_pha[is.na(ego_know_pha) == F & is.na(alt_know_pha) == F]), 
                     homo_know_pha = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_pha) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# doctor
# recode values
ea$ego_know_doc[ea$ego_know_doc == '9'] <- '2'
ea$alt_know_doc[ea$alt_know_doc == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_doc == alt_know_doc, na.rm = T), 
                     n = NROW(ego_know_doc[is.na(ego_know_doc) == F & is.na(alt_know_doc) == F]), 
                     homo_know_doc = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_doc) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# religious leader
# recode values
ea$ego_know_rel[ea$ego_know_rel == '9'] <- '2'
ea$alt_know_rel[ea$alt_know_rel == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_rel == alt_know_rel, na.rm = T), 
                     n = NROW(ego_know_rel[is.na(ego_know_rel) == F & is.na(alt_know_rel) == F]), 
                     homo_know_rel = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_rel) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')


### NEED TO RECODE BY CATEGORY ### !!!!

# # now by category
# # first all values to numeric
# ea %<>% mutate(across(c(ego_know_asha,
#                         ego_know_anm,
#                         ego_know_aww,
#                         ego_know_shg,
#                         ego_know_pra,
#                         ego_know_pha,
#                         ego_know_doc,
#                         ego_know_rel,
#                         alt_know_asha,
#                         alt_know_anm,
#                         alt_know_aww,
#                         alt_know_shg,
#                         alt_know_pra,
#                         alt_know_pha,
#                         alt_know_doc,
#                         alt_know_rel), as.numeric))
# 
# # exchange 2s for 0s
# ea %<>% mutate(across(c(ego_know_asha,
#                         ego_know_anm,
#                         ego_know_aww,
#                         ego_know_shg,
#                         ego_know_pra,
#                         ego_know_pha,
#                         ego_know_doc,
#                         ego_know_rel,
#                         alt_know_asha,
#                         alt_know_anm,
#                         alt_know_aww,
#                         alt_know_shg,
#                         alt_know_pra,
#                         alt_know_pha,
#                         alt_know_doc,
#                         alt_know_rel),
#                       ~ ifelse(. == 2, 0, .)))
# 
# # calculate health worker category
# ego_hw <- ea %>% 
#   select(ego_know_asha,
#          ego_know_anm,
#          ego_know_aww,
#          ego_know_pha,
#          ego_know_doc) %>%
#   rowSums(na.rm = T)
# 
# ego_hw <- ego_hw > 0
# 
# alt_hw <- ea %>% 
#   select(alt_know_asha,
#          alt_know_anm,
#          alt_know_aww,
#          alt_know_pha,
#          alt_know_doc) %>%
#   rowSums(na.rm = T)
# 
# alt_hw <- alt_hw > 0
# 
# # calculate homophily
# hw <- ego_hw == alt_hw
# dat <- tabyl(hw) %>%
#   adorn_pct_formatting(digits = 2)
# 
# # calculate village leader category
# ego_vl <- ea %>% 
#   select(ego_know_pra,
#          ego_know_rel) %>%
#   rowSums(na.rm = T)
# 
# ego_vl <- ego_vl > 0
# 
# alt_vl <- ea %>% 
#   select(alt_know_pra,
#          alt_know_rel) %>%
#   rowSums(na.rm = T)
# 
# alt_vl <- alt_vl > 0
# 
# # calculate homophily
# vl <- ego_vl == alt_vl
# dat <- tabyl(vl) %>%
#   adorn_pct_formatting(digits = 2)
# 
# # if valid percent remove percent and change name of valid percent
# if('valid_percent' %in% colnames(dat)){
#   dat %<>% select(-percent) %>% rename(percent = valid_percent)
# }
# 
# # add to homophily output
# homo %<>% add_row(metric = 'Know Village Leader Category (%)',
#                   value = dat %>% filter(vl == 'TRUE') %>% select(percent) %>% as.character,
#                   N = str_c(dat %>% filter(vl == 'TRUE') %>% select(n),
#                             '/',
#                             dat %>% filter(vl == 'TRUE') %>% select(n) +
#                               dat %>% filter(vl == 'FALSE') %>% select(n)))
# 
# # now total number of people
# ego_ppl <- ea %>%
#   select(ego_know_asha,
#          ego_know_anm,
#          ego_know_aww,
#          ego_know_shg,
#          ego_know_pra,
#          ego_know_pha,
#          ego_know_doc,
#          ego_know_rel) %>%
#   rowSums(na.rm = T)
# 
# alt_ppl <- ea %>%
#   select(alt_know_asha,
#          alt_know_anm,
#          alt_know_aww,
#          alt_know_shg,
#          alt_know_pra,
#          alt_know_pha,
#          alt_know_doc,
#          alt_know_rel) %>%
#   rowSums(na.rm = T)
# 
# # calculate sd homophily
# ppl_sd <- homo_sd(ego_ppl, alt_ppl)
# dat <- tibble(aed = ppl_sd[1],
#               n = ppl_sd[2])
# 
# # add to homophily output
# homo %<>% add_row(metric = 'Total Number of Important People Known (AED)',
#                   value = dat %>% select(aed) %>% as.character,
#                   N = dat %>% select(n) %>% as.character)

##########################
### NUMBER OF CHILDREN ###
##########################

# check unique values
# unique(ea$ego_sons)
# unique(ea$alt_sons)
# unique(ea$ego_daughters)
# unique(ea$alt_daughters)

# change husband values to same as ego
ea$alt_sons[ea$alt_relationship == "Husband पति"] <- ea$ego_sons[ea$alt_relationship == "Husband पति"]
ea$alt_daughters[ea$alt_relationship == "Husband पति"] <- ea$ego_daughters[ea$alt_relationship == "Husband पति"]

# convert to numeric
ea$ego_sons <- as.numeric(ea$ego_sons)
ea$alt_sons <- as.numeric(ea$alt_sons)
ea$ego_daughters <- as.numeric(ea$ego_daughters)
ea$alt_daughters <- as.numeric(ea$alt_daughters)

# set don't know to NA
ea$alt_sons[ea$alt_sons == 99] <- NA
ea$alt_daughters[ea$alt_daughters == 99] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(homo_num_sons = homo_sd(ego_sons, alt_sons)[1],
                     homo_num_daughters = homo_sd(ego_daughters, alt_daughters)[1],
                     homo_num_children = homo_sd(ego_sons + ego_daughters, alt_sons + alt_daughters)[1]) %>% # calculate homophily
              select(ego_id, homo_num_sons, homo_num_daughters, homo_num_children) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

##############################################
### BASIC PROPORTION OF PLACE OF RESIDENCE ###
##############################################

# check unique values
# unique(ea$alt_residence)

# recode values
ea %<>% mutate(alt_residence = recode(alt_residence, 
                                      '1' = 'Same Household',
                                      '2' = 'Same Village',
                                      '3' = 'Another Village (same district)',
                                      '4' = 'Outside this District'))

# build tabyl
dat <- ea %>% tabyl(alt_residence) %>%
  adorn_pct_formatting(digits = 2)

# if valid percent remove percent and change name of valid percent
if('valid_percent' %in% colnames(dat)){
  dat %<>% select(-percent) %>% rename(percent = valid_percent)
}

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Same Household', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_household = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_household) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Same Village', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_village = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_village) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Another Village (same district)', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_another_village = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_another_village) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Outside this District', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_another_district = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_another_district) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#####################################
### HETEROGENEITY OF EGO'S ALTERS ###
#####################################

###########
### SEX ###
###########

# check unique values
# unique(ea$alt_sex)

# create a function to calculate blaus and iqv
blau <- function(perc){
  b <- perc*perc
  k <- length(perc)
  b <- (1 - sum(b)) %>% round(2)
  iqv <- (b / (1-(1/k))) %>% round(2)
  return(c(b, iqv, k))
}

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_sex[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_gender_blau',
                   'hetero_gender_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output (since first var)
hetero <- tib %>% select(-k)

#############
### CASTE ###
#############

# check unique values
# unique(ea$alt_caste)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_caste[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_caste_blau',
                   'hetero_caste_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

################################
### NUMBER OF FAMILY MEMBERS ###
################################

# check unique values
# unique(ea$alt_fam)

# # calculate heterogeneity
# fam <- tibble(fam_sd = sd(ea$alt_fam, na.rm = T) %>% round(2),
#               n = length(ea$alt_fam[is.na(ea$alt_fam) == F]))

#######################################
### NUMBER OF FRIENDS AND NEIGHBORS ###
#######################################

# check unique values
# unique(ea$alt_friends)
# unique(ea$alt_neigh)

# # calculate heterogeneity
# friends_neigh <- tibble(friends_sd = sd(ea$alt_friends, na.rm = T) %>% round(2),
#               friends_n = length(ea$alt_friends[is.na(ea$alt_friends) == F]),
#               neigh_sd = sd(ea$alt_neigh, na.rm = T) %>% round(2),
#               neigh_n = length(ea$alt_neigh[is.na(ea$alt_neigh) == F]),
#               friends_neigh_sd = sd((ea$alt_friends + ea$alt_neigh), na.rm = T) %>% round(2),
#               friends_neigh_n = length((ea$alt_friends + ea$alt_neigh)[is.na((ea$alt_friends + ea$alt_neigh)) == F]))

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
# unique(ea$alt_using_fp)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_using_fp[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_using_fp_blau',
                   'hetero_using_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###############################
### PEOPLE KNOWN IN VILLAGE ###
###############################

# check some values
# unique(ea$alt_know_asha)

# for ego, 1 is Yes, 2 is heard, 9 is don't know/haven't heard
# for alter 1 is Yes, 2 is No, 9 is not sure
# we should change ego values of 9 to 2 to match alter
# remove alter rows with 9

# asha
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_asha[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_asha_blau',
                   'hetero_know_asha_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# anm
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_anm[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_anm_blau',
                   'hetero_know_anm_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# aww
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_aww[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_aww_blau',
                   'hetero_know_aww_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# shg
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_shg[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_shg_blau',
                   'hetero_know_shg_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# pradhan
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_pra[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_pra_blau',
                   'hetero_know_pra_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# pharmacist
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_pha[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_pha_blau',
                   'hetero_know_pha_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# doctor
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_doc[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_doc_blau',
                   'hetero_know_doc_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# religious leader
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_rel[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_rel_blau',
                   'hetero_know_rel_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# calculate health worker category


# # calculate homophily
# # calculate heterogeneity
# ppl <- tabyl(alt_hw)
# 
# if('valid_percent' %in% colnames(ppl)){
#   ppl %<>% select(-percent) %>% rename(percent = valid_percent)
# }
# 
# n <- ppl$n[is.na(ppl$percent) == F] %>% sum
# ppl <- blau(na.omit(ppl$percent))
# dat <- tibble(blau = ppl[1],
#               iqv = ppl[2],
#               k = ppl[3])
# 
# # add to heterogeneity output
# hetero %<>% add_row(metric = 'Know Health Worker Category',
#                     blau_index = dat %>% select(blau) %>% as.character,
#                     iqv = dat %>% select(iqv) %>% as.character,
#                     k = dat %>% select(k) %>% as.character,
#                     n = n %>% as.character)
# 
# # calculate village leader category
# # calculate heterogeneity
# ppl <- tabyl(alt_vl)
# 
# if('valid_percent' %in% colnames(ppl)){
#   ppl %<>% select(-percent) %>% rename(percent = valid_percent)
# }
# 
# n <- ppl$n[is.na(ppl$percent) == F] %>% sum
# ppl <- blau(na.omit(ppl$percent))
# dat <- tibble(blau = ppl[1],
#               iqv = ppl[2],
#               k = ppl[3])
# 
# # add to heterogeneity output
# hetero %<>% add_row(metric = 'Know Village Leader Category',
#                     blau_index = dat %>% select(blau) %>% as.character,
#                     iqv = dat %>% select(iqv) %>% as.character,
#                     k = dat %>% select(k) %>% as.character,
#                     n = n %>% as.character)
# 
# # # now total number of people
# # # calculate heterogeneity
# # ppl <- tibble(ppl_known_village_sd = sd(alt_ppl, na.rm = T) %>% round(2),
# #               n = length(alt_ppl[is.na(alt_ppl) == F]))

##########################
### NUMBER OF CHILDREN ###
##########################

# check unique values
# unique(ea$alt_sons)
# unique(ea$alt_daughters)

# # calculate heterogeneity
# children <- tibble(sons_sd = sd(ea$alt_sons, na.rm = T) %>% round(2),
#               sons_n = length(ea$alt_sons[is.na(ea$alt_sons) == F]),
#               daughters_sd = sd(ea$alt_daughters, na.rm = T) %>% round(2),
#               daughters_n = length(ea$alt_daughters[is.na(ea$alt_daughters) == F]),
#               children_sd = sd((ea$alt_sons + ea$alt_daughters), na.rm = T) %>% round(2),
#               children_n = length((ea$alt_sons + ea$alt_daughters)[is.na((ea$alt_sons + ea$alt_daughters)) == F]))

##########################
### PLACE OF RESIDENCE ###
##########################

# check unique values
# unique(ea$alt_residence)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_residence[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_residence_blau',
                   'hetero_residence_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

#######################
### SOCIAL LEARNING ###
#######################

# check unique values
#unique(ea$alt_learned)

# a
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "A"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_a_blau',
                   'hetero_soc_learn_a_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# b
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "B"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_b_blau',
                   'hetero_soc_learn_b_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# c
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "C"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_c_blau',
                   'hetero_soc_learn_c_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# d
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "D"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_d_blau',
                   'hetero_soc_learn_d_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# E
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "E"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_e_blau',
                   'hetero_soc_learn_e_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# f
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "F"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_f_blau',
                   'hetero_soc_learn_f_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# g
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "G"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_g_blau',
                   'hetero_soc_learn_g_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###########################
### ENCOURAGE USE OF FP ###
###########################

# check unique values
# unique(ea$alt_encourage_fp)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_encourage_fp[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_encourage_fp_blau',
                   'hetero_encourage_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

#################################
### DISCUSS FP METHODS FREELY ###
#################################

# check unique values
# unique(ea$alt_discuss_fp)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_discuss_fp[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_discuss_fp_blau',
                   'hetero_discuss_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###########################
### ALT HELP/ADVISE EGO ###
###########################

# check unique values
# unique(ea$alt_help_ego)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_help_ego[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_help_ego_blau',
                   'hetero_alt_help_ego_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###########################
### EGO HELP/ADVISE ALT ###
###########################

# check unique values
# unique(ea$alt_helped_by_ego)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_helped_by_ego[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_helped_by_ego_blau',
                   'hetero_alt_helped_by_ego_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

############################
### GOING AGAINST ADVICE ###
############################

# check unique values
# unique(ea$alt_against_advice)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_against_advice[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_against_advice_blau',
                   'hetero_alt_against_advice_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###################################
### SUPPORT NOT HAVING CHILDREN ###
###################################

# check unique values
# unique(ea$alt_support_no_child)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_support_no_child[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_support_no_child_blau',
                   'hetero_alt_support_no_child_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# add homophily and hetero measures to dat
dat_bihar %<>% full_join(homo, by = 'ego_id') %>%
  full_join(hetero, by = 'ego_id')

# clean environment
rm(list=setdiff(ls(), "dat_bihar"))

####################
### LOAD UP DATA ###
####################

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned and completre data
ego <- read_excel(path = "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/SNA study_EGO_clean_Uttar Pradesh_08112022.xlsx")
alter <-read_excel(path = "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/SNA study_ALTER_clean_Uttar Pradesh_08112022.xlsx")

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/PC_EGO_data_Merge_Uttar Pradesh.dta', convert.factors = F)
# 
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_up_07122022.xlsx")

# load pc data for egos
ego_pc <- read_excel(path = "data/ego_pc_complete_up_08112022.xlsx")

# load list of duplicate ids
dup_id <- read_excel(path = 'data/duplicate_ids_up_07112022.xlsx')

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

# convert date columns to character
inx <- sapply(ego, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
ego[inx] <- lapply(ego[inx], as.character)

inx <- sapply(alter, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))
alter[inx] <- lapply(alter[inx], as.character)

# replace character NA values with NA
ego[ego == "NA"] <- NA
ego[ego == "Na"] <- NA
alter[alter == "NA"] <- NA
alter[alter == "Na"] <- NA

# # fix district, block and village in alter data
# alter$district_name <- alter$`district name_clean`
# alter$block_name <- alter$`block name_clean`
# alter$village_name <- alter$`village name_clean`

# # remove "clean" columns
# alter <- alter %>% select(-c(`district name_clean`, `block name_clean`, `village name_clean`))

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

###########################################
### CREATE INITIAL DATA TABLE OF VALUES ###
###########################################

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

dat_up <- tibble(ego_id = ego$woman_id,
                    state = 'up',
                    mcontra_neigh_num = ego$contra_neighbour_num,
                    preg = ego_pc$pcq230,
                    using_fp = ego_pc$pcq311,
                    fp_method = ego_pc$pcq312,
                    age = ego_pc$pcq102,
                    education = ego_pc$pcq103,
                    caste = ego_pc$pcq111,
                    husband_education = ego_pc$pcq115)

##################################
### CALCULATE HOMOPHILY OF EGO ###
##################################

# gender
# education
# number of family members
# number of non-family members close to
# practicing FP
# network composition: family/non-family (?)

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

# build tibble of ego and alter attributes
ea <- rbind(tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter1,
                   ego_sex = 2,
                   alt_sex = ego$alter1_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter1_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter1_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter1_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter1_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter1_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter1_asha,
                   alt_know_anm = ego$alter1_anm,
                   alt_know_aww = ego$alter1_aww,
                   alt_know_shg = ego$alter1_shg,
                   alt_know_pra = ego$alter1_pradhan,
                   alt_know_pha = ego$alter1_pharmacist,
                   alt_know_doc = ego$alter1_doctor,
                   alt_know_rel = ego$alter1_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter1_sons,
                   alt_daughters = ego$alter1_daughters,
                   alt_residence = ego$alter1_residence,
                   alt_relationship = ego$alter1r,
                   alt_learned = ego$alter1_learned,
                   alt_encourage_fp = ego$alter1_encourage_fp,
                   alt_discuss_fp = ego$alter1_discuss_fp,
                   alt_help_ego = ego$alter1_help,
                   alt_helped_by_ego = ego$alter1_helped,
                   alt_against_advice = ego$alter1_follow_advice,
                   alt_support_no_child = ego$alter1_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter2,
                   ego_sex = 2,
                   alt_sex = ego$alter2_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter2_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter2_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter2_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter2_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter2_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter2_asha,
                   alt_know_anm = ego$alter2_anm,
                   alt_know_aww = ego$alter2_aww,
                   alt_know_shg = ego$alter2_shg,
                   alt_know_pra = ego$alter2_pradhan,
                   alt_know_pha = ego$alter2_pharmacist,
                   alt_know_doc = ego$alter2_doctor,
                   alt_know_rel = ego$alter2_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter2_sons,
                   alt_daughters = ego$alter2_daughters,
                   alt_residence = ego$alter2_residence,
                   alt_relationship = ego$alter2r,
                   alt_learned = ego$alter2_learned,
                   alt_encourage_fp = ego$alter2_encourage_fp,
                   alt_discuss_fp = ego$alter2_discuss_fp,
                   alt_help_ego = ego$alter2_help,
                   alt_helped_by_ego = ego$alter2_helped,
                   alt_against_advice = ego$alter2_follow_advice,
                   alt_support_no_child = ego$alter2_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter3,
                   ego_sex = 2,
                   alt_sex = ego$alter3_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter3_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter3_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter3_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter3_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter3_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter3_asha,
                   alt_know_anm = ego$alter3_anm,
                   alt_know_aww = ego$alter3_aww,
                   alt_know_shg = ego$alter3_shg,
                   alt_know_pra = ego$alter3_pradhan,
                   alt_know_pha = ego$alter3_pharmacist,
                   alt_know_doc = ego$alter3_doctor,
                   alt_know_rel = ego$alter3_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter3_sons,
                   alt_daughters = ego$alter3_daughters,
                   alt_residence = ego$alter3_residence,
                   alt_relationship = ego$alter3r,
                   alt_learned = ego$alter3_learned,
                   alt_encourage_fp = ego$alter3_encourage_fp,
                   alt_discuss_fp = ego$alter3_discuss_fp,
                   alt_help_ego = ego$alter3_help,
                   alt_helped_by_ego = ego$alter3_helped,
                   alt_against_advice = ego$alter3_follow_advice,
                   alt_support_no_child = ego$alter3_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter4,
                   ego_sex = 2,
                   alt_sex = ego$alter4_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter4_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter4_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter4_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter4_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter4_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter4_asha,
                   alt_know_anm = ego$alter4_anm,
                   alt_know_aww = ego$alter4_aww,
                   alt_know_shg = ego$alter4_shg,
                   alt_know_pra = ego$alter4_pradhan,
                   alt_know_pha = ego$alter4_pharmacist,
                   alt_know_doc = ego$alter4_doctor,
                   alt_know_rel = ego$alter4_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter4_sons,
                   alt_daughters = ego$alter4_daughters,
                   alt_residence = ego$alter4_residence,
                   alt_relationship = ego$alter4r,
                   alt_learned = ego$alter4_learned,
                   alt_encourage_fp = ego$alter4_encourage_fp,
                   alt_discuss_fp = ego$alter4_discuss_fp,
                   alt_help_ego = ego$alter4_help,
                   alt_helped_by_ego = ego$alter4_helped,
                   alt_against_advice = ego$alter4_follow_advice,
                   alt_support_no_child = ego$alter4_nochild),
            tibble(ego_id = ego$woman_id,
                   alter_name = ego$alter5,
                   ego_sex = 2,
                   alt_sex = ego$alter5_sex,
                   ego_caste = ego_pc$pcq111,
                   alt_caste = ego$alter5_caste,
                   ego_edu = ego_pc$pcq103,
                   alt_edu = NA,
                   ego_fam = ego$family_members,
                   alt_fam = ego$alter5_family,
                   ego_friends = ego$friends,
                   alt_friends = ego$alter5_friends,
                   ego_neigh = ego$neighbours,
                   alt_neigh = ego$alter5_neighbours,
                   ego_using_fp = ego_pc$pcq311,
                   alt_using_fp = ego$alter5_using_fp,
                   ego_know_asha = ego$know_asha,
                   ego_know_anm = ego$know_anm,
                   ego_know_aww = ego$know_aww,
                   ego_know_shg = ego$know_shg,
                   ego_know_pra = ego$know_pradhan,
                   ego_know_pha = ego$know_pharmacist,
                   ego_know_doc = ego$know_doctor,
                   ego_know_rel = ego$know_religious_leader,
                   alt_know_asha = ego$alter5_asha,
                   alt_know_anm = ego$alter5_anm,
                   alt_know_aww = ego$alter5_aww,
                   alt_know_shg = ego$alter5_shg,
                   alt_know_pra = ego$alter5_pradhan,
                   alt_know_pha = ego$alter5_pharmacist,
                   alt_know_doc = ego$alter5_doctor,
                   alt_know_rel = ego$alter5_religious_leader,
                   ego_sons = ego_pc$pcq205s,
                   ego_daughters = ego_pc$pcq205d,
                   alt_sons = ego$alter5_sons,
                   alt_daughters = ego$alter5_daughters,
                   alt_residence = ego$alter5_residence,
                   alt_relationship = ego$alter5r,
                   alt_learned = ego$alter5_learned,
                   alt_encourage_fp = ego$alter5_encourage_fp,
                   alt_discuss_fp = ego$alter5_discuss_fp,
                   alt_help_ego = ego$alter5_help,
                   alt_helped_by_ego = ego$alter5_helped,
                   alt_against_advice = ego$alter5_follow_advice,
                   alt_support_no_child = ego$alter5_nochild))

# drop rows for missing alters
ea %<>% filter(is.na(alter_name) == F)

##############
### GENDER ###
##############

# check unique values
# unique(ea$ego_sex)
# unique(ea$alt_sex)

# set to numeric
ea$alt_sex <- as.numeric(ea$alt_sex)

# summarise and create homophily output (since first var)
homo <- ea %>% 
  group_by(ego_id) %>% # group by ego id
  mutate(sum = sum(ego_sex == alt_sex, na.rm = T), 
         n = NROW(ego_sex[is.na(ego_sex) == F & is.na(alt_sex) == F]), 
         homo_gender = sum/n) %>% # calculate homophily
  select(ego_id, homo_gender) %>% # only keep homophily
  distinct %>% # remove duplicates
  arrange(ego_id) # arrange by ego id

#############
### CASTE ###
#############

# check unique values
# unique(ea$ego_caste)
# unique(ea$alt_caste)

# set to numeric
ea$alt_caste <- as.numeric(ea$alt_caste)

# set don't know to NA
ea$alt_caste[ea$alt_caste == 9] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_caste == alt_caste, na.rm = T), 
                     n = NROW(ego_caste[is.na(ego_caste) == F & is.na(alt_caste) == F]), 
                     homo_caste = sum/n) %>% # calculate homophily
              select(ego_id, homo_caste) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#################
### EDUCATION ###
#################

# DON'T HAVE ALTER EDUCATION FROM EGO SURVEY !!!

################################
### NUMBER OF FAMILY MEMBERS ###
################################

# check unique values
# unique(ea$ego_fam)
# unique(ea$alt_fam)

# convert to numeric
ea$ego_fam <- as.numeric(ea$ego_fam)
ea$alt_fam <- as.numeric(ea$alt_fam)

# set don't know to NA
ea$alt_fam[ea$alt_fam == 99] <- NA

# create formula to calculate sd homophily
homo_sd <- function(ego, alt){
  out <- alt - ego
  out <- out*out
  n <- length(out[is.na(out) == F])
  out <- sum(out, na.rm = T)
  out <- out/n
  return(c(round(sqrt(out), 2), n))
}

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(homo_num_fam = homo_sd(ego_fam, alt_fam)[1]) %>% # calculate homophily
              select(ego_id, homo_num_fam) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#######################################
### NUMBER OF FRIENDS AND NEIGHBORS ###
#######################################

# check unique values
# unique(ea$ego_friends)
# unique(ea$alt_friends)
# unique(ea$ego_neigh)
# unique(ea$alt_neigh)

# convert to numeric
ea$ego_friends <- as.numeric(ea$ego_friends)
ea$alt_friends <- as.numeric(ea$alt_friends)
ea$ego_neigh <- as.numeric(ea$ego_neigh)
ea$alt_neigh <- as.numeric(ea$alt_neigh)

# set don't know to NA
ea$alt_friends[ea$alt_friends == 99] <- NA
ea$alt_neigh[ea$alt_neigh == 99] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(homo_friends = homo_sd(ego_friends, alt_friends)[1],
                     homo_neigh = homo_sd(ego_neigh, alt_neigh)[1],
                     homo_friends_neigh = homo_sd(ego_friends + ego_neigh, alt_friends + alt_neigh)[1]) %>% # calculate homophily
              select(ego_id, homo_friends, homo_neigh, homo_friends_neigh) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
# unique(ea$ego_using_fp)
# unique(ea$alt_using_fp)

# set values to numeric
ea$alt_using_fp <- as.numeric(ea$alt_using_fp)

# if alter is husband change to same value as ego
ea$alt_using_fp[str_detect(ea$alt_relationship, 'Husband')] <- ea$ego_using_fp[str_detect(ea$alt_relationship, 'Husband')]

# set don't know to NA
ea$alt_using_fp[ea$alt_using_fp == 9] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_using_fp == alt_using_fp, na.rm = T), 
                     n = NROW(ego_using_fp[is.na(ego_using_fp) == F & is.na(alt_using_fp) == F]), 
                     homo_using_fp = sum/n) %>% # calculate homophily
              select(ego_id, homo_using_fp) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

###############################
### PEOPLE KNOWN IN VILLAGE ###
###############################

# check some values
# unique(ea$ego_know_asha)
# unique(ea$alt_know_asha)

# for ego, 1 is Yes, 2 is heard, 9 is don't know/haven't heard
# for alter 1 is Yes, 2 is No, 9 is not sure
# we should change ego values of 9 to 2 to match alter
# remove alter rows with 9

# do individuals first
# asha
# recode values
ea$ego_know_asha[ea$ego_know_asha == '9'] <- '2'
ea$alt_know_asha[ea$alt_know_asha == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_asha == alt_know_asha, na.rm = T), 
                     n = NROW(ego_know_asha[is.na(ego_know_asha) == F & is.na(alt_know_asha) == F]), 
                     homo_know_asha = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_asha) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# anm
# recode values
ea$ego_know_anm[ea$ego_know_anm == '9'] <- '2'
ea$alt_know_anm[ea$alt_know_anm == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_anm == alt_know_anm, na.rm = T), 
                     n = NROW(ego_know_anm[is.na(ego_know_anm) == F & is.na(alt_know_anm) == F]), 
                     homo_know_anm = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_anm) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# aww
# recode values
ea$ego_know_aww[ea$ego_know_aww == '9'] <- '2'
ea$alt_know_aww[ea$alt_know_aww == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_aww == alt_know_aww, na.rm = T), 
                     n = NROW(ego_know_aww[is.na(ego_know_aww) == F & is.na(alt_know_aww) == F]), 
                     homo_know_aww = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_aww) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# shg
# recode values
ea$ego_know_shg[ea$ego_know_shg == '9'] <- '2'
ea$alt_know_shg[ea$alt_know_shg == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_shg == alt_know_shg, na.rm = T), 
                     n = NROW(ego_know_shg[is.na(ego_know_shg) == F & is.na(alt_know_shg) == F]), 
                     homo_know_shg = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_shg) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# pradhan
# recode values
ea$ego_know_pra[ea$ego_know_pra == '9'] <- '2'
ea$alt_know_pra[ea$alt_know_pra == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_pra == alt_know_pra, na.rm = T), 
                     n = NROW(ego_know_pra[is.na(ego_know_pra) == F & is.na(alt_know_pra) == F]), 
                     homo_know_pra = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_pra) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# pharmacist
# recode values
ea$ego_know_pha[ea$ego_know_pha == '9'] <- '2'
ea$alt_know_pha[ea$alt_know_pha == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_pha == alt_know_pha, na.rm = T), 
                     n = NROW(ego_know_pha[is.na(ego_know_pha) == F & is.na(alt_know_pha) == F]), 
                     homo_know_pha = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_pha) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# doctor
# recode values
ea$ego_know_doc[ea$ego_know_doc == '9'] <- '2'
ea$alt_know_doc[ea$alt_know_doc == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_doc == alt_know_doc, na.rm = T), 
                     n = NROW(ego_know_doc[is.na(ego_know_doc) == F & is.na(alt_know_doc) == F]), 
                     homo_know_doc = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_doc) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# religious leader
# recode values
ea$ego_know_rel[ea$ego_know_rel == '9'] <- '2'
ea$alt_know_rel[ea$alt_know_rel == '9'] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(ego_know_rel == alt_know_rel, na.rm = T), 
                     n = NROW(ego_know_rel[is.na(ego_know_rel) == F & is.na(alt_know_rel) == F]), 
                     homo_know_rel = sum/n) %>% # calculate homophily
              select(ego_id, homo_know_rel) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')


### NEED TO RECODE BY CATEGORY ### !!!!

# # now by category
# # first all values to numeric
# ea %<>% mutate(across(c(ego_know_asha,
#                         ego_know_anm,
#                         ego_know_aww,
#                         ego_know_shg,
#                         ego_know_pra,
#                         ego_know_pha,
#                         ego_know_doc,
#                         ego_know_rel,
#                         alt_know_asha,
#                         alt_know_anm,
#                         alt_know_aww,
#                         alt_know_shg,
#                         alt_know_pra,
#                         alt_know_pha,
#                         alt_know_doc,
#                         alt_know_rel), as.numeric))
# 
# # exchange 2s for 0s
# ea %<>% mutate(across(c(ego_know_asha,
#                         ego_know_anm,
#                         ego_know_aww,
#                         ego_know_shg,
#                         ego_know_pra,
#                         ego_know_pha,
#                         ego_know_doc,
#                         ego_know_rel,
#                         alt_know_asha,
#                         alt_know_anm,
#                         alt_know_aww,
#                         alt_know_shg,
#                         alt_know_pra,
#                         alt_know_pha,
#                         alt_know_doc,
#                         alt_know_rel),
#                       ~ ifelse(. == 2, 0, .)))
# 
# # calculate health worker category
# ego_hw <- ea %>% 
#   select(ego_know_asha,
#          ego_know_anm,
#          ego_know_aww,
#          ego_know_pha,
#          ego_know_doc) %>%
#   rowSums(na.rm = T)
# 
# ego_hw <- ego_hw > 0
# 
# alt_hw <- ea %>% 
#   select(alt_know_asha,
#          alt_know_anm,
#          alt_know_aww,
#          alt_know_pha,
#          alt_know_doc) %>%
#   rowSums(na.rm = T)
# 
# alt_hw <- alt_hw > 0
# 
# # calculate homophily
# hw <- ego_hw == alt_hw
# dat <- tabyl(hw) %>%
#   adorn_pct_formatting(digits = 2)
# 
# # calculate village leader category
# ego_vl <- ea %>% 
#   select(ego_know_pra,
#          ego_know_rel) %>%
#   rowSums(na.rm = T)
# 
# ego_vl <- ego_vl > 0
# 
# alt_vl <- ea %>% 
#   select(alt_know_pra,
#          alt_know_rel) %>%
#   rowSums(na.rm = T)
# 
# alt_vl <- alt_vl > 0
# 
# # calculate homophily
# vl <- ego_vl == alt_vl
# dat <- tabyl(vl) %>%
#   adorn_pct_formatting(digits = 2)
# 
# # if valid percent remove percent and change name of valid percent
# if('valid_percent' %in% colnames(dat)){
#   dat %<>% select(-percent) %>% rename(percent = valid_percent)
# }
# 
# # add to homophily output
# homo %<>% add_row(metric = 'Know Village Leader Category (%)',
#                   value = dat %>% filter(vl == 'TRUE') %>% select(percent) %>% as.character,
#                   N = str_c(dat %>% filter(vl == 'TRUE') %>% select(n),
#                             '/',
#                             dat %>% filter(vl == 'TRUE') %>% select(n) +
#                               dat %>% filter(vl == 'FALSE') %>% select(n)))
# 
# # now total number of people
# ego_ppl <- ea %>%
#   select(ego_know_asha,
#          ego_know_anm,
#          ego_know_aww,
#          ego_know_shg,
#          ego_know_pra,
#          ego_know_pha,
#          ego_know_doc,
#          ego_know_rel) %>%
#   rowSums(na.rm = T)
# 
# alt_ppl <- ea %>%
#   select(alt_know_asha,
#          alt_know_anm,
#          alt_know_aww,
#          alt_know_shg,
#          alt_know_pra,
#          alt_know_pha,
#          alt_know_doc,
#          alt_know_rel) %>%
#   rowSums(na.rm = T)
# 
# # calculate sd homophily
# ppl_sd <- homo_sd(ego_ppl, alt_ppl)
# dat <- tibble(aed = ppl_sd[1],
#               n = ppl_sd[2])
# 
# # add to homophily output
# homo %<>% add_row(metric = 'Total Number of Important People Known (AED)',
#                   value = dat %>% select(aed) %>% as.character,
#                   N = dat %>% select(n) %>% as.character)

##########################
### NUMBER OF CHILDREN ###
##########################

# check unique values
# unique(ea$ego_sons)
# unique(ea$alt_sons)
# unique(ea$ego_daughters)
# unique(ea$alt_daughters)

# change husband values to same as ego
ea$alt_sons[ea$alt_relationship == "Husband पति"] <- ea$ego_sons[ea$alt_relationship == "Husband पति"]
ea$alt_daughters[ea$alt_relationship == "Husband पति"] <- ea$ego_daughters[ea$alt_relationship == "Husband पति"]

# convert to numeric
ea$ego_sons <- as.numeric(ea$ego_sons)
ea$alt_sons <- as.numeric(ea$alt_sons)
ea$ego_daughters <- as.numeric(ea$ego_daughters)
ea$alt_daughters <- as.numeric(ea$alt_daughters)

# set don't know to NA
ea$alt_sons[ea$alt_sons == 99] <- NA
ea$alt_daughters[ea$alt_daughters == 99] <- NA

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(homo_num_sons = homo_sd(ego_sons, alt_sons)[1],
                     homo_num_daughters = homo_sd(ego_daughters, alt_daughters)[1],
                     homo_num_children = homo_sd(ego_sons + ego_daughters, alt_sons + alt_daughters)[1]) %>% # calculate homophily
              select(ego_id, homo_num_sons, homo_num_daughters, homo_num_children) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

##############################################
### BASIC PROPORTION OF PLACE OF RESIDENCE ###
##############################################

# check unique values
# unique(ea$alt_residence)

# recode values
ea %<>% mutate(alt_residence = recode(alt_residence, 
                                      '1' = 'Same Household',
                                      '2' = 'Same Village',
                                      '3' = 'Another Village (same district)',
                                      '4' = 'Outside this District'))

# build tabyl
dat <- ea %>% tabyl(alt_residence) %>%
  adorn_pct_formatting(digits = 2)

# if valid percent remove percent and change name of valid percent
if('valid_percent' %in% colnames(dat)){
  dat %<>% select(-percent) %>% rename(percent = valid_percent)
}

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Same Household', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_household = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_household) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Same Village', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_village = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_village) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Another Village (same district)', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_another_village = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_another_village) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

# summarise and update homophily output
homo <- homo %>% 
  full_join(ea %>% 
              group_by(ego_id) %>% # group by ego id
              mutate(sum = sum(alt_residence == 'Outside this District', na.rm = T), 
                     n = NROW(alt_residence[is.na(alt_residence) == F]), 
                     homo_live_another_district = sum/n) %>% # calculate homophily
              select(ego_id, homo_live_another_district) %>% # only keep homophily
              distinct %>% # remove duplicates
              arrange(ego_id) # arrange by ego id
            , by = 'ego_id')

#####################################
### HETEROGENEITY OF EGO'S ALTERS ###
#####################################

###########
### SEX ###
###########

# check unique values
# unique(ea$alt_sex)

# create a function to calculate blaus and iqv
blau <- function(perc){
  b <- perc*perc
  k <- length(perc)
  b <- (1 - sum(b)) %>% round(2)
  iqv <- (b / (1-(1/k))) %>% round(2)
  return(c(b, iqv, k))
}

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_sex[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_gender_blau',
                   'hetero_gender_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output (since first var)
hetero <- tib %>% select(-k)

#############
### CASTE ###
#############

# check unique values
# unique(ea$alt_caste)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_caste[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_caste_blau',
                   'hetero_caste_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

################################
### NUMBER OF FAMILY MEMBERS ###
################################

# check unique values
# unique(ea$alt_fam)

# # calculate heterogeneity
# fam <- tibble(fam_sd = sd(ea$alt_fam, na.rm = T) %>% round(2),
#               n = length(ea$alt_fam[is.na(ea$alt_fam) == F]))

#######################################
### NUMBER OF FRIENDS AND NEIGHBORS ###
#######################################

# check unique values
# unique(ea$alt_friends)
# unique(ea$alt_neigh)

# # calculate heterogeneity
# friends_neigh <- tibble(friends_sd = sd(ea$alt_friends, na.rm = T) %>% round(2),
#               friends_n = length(ea$alt_friends[is.na(ea$alt_friends) == F]),
#               neigh_sd = sd(ea$alt_neigh, na.rm = T) %>% round(2),
#               neigh_n = length(ea$alt_neigh[is.na(ea$alt_neigh) == F]),
#               friends_neigh_sd = sd((ea$alt_friends + ea$alt_neigh), na.rm = T) %>% round(2),
#               friends_neigh_n = length((ea$alt_friends + ea$alt_neigh)[is.na((ea$alt_friends + ea$alt_neigh)) == F]))

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
# unique(ea$alt_using_fp)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_using_fp[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_using_fp_blau',
                   'hetero_using_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###############################
### PEOPLE KNOWN IN VILLAGE ###
###############################

# check some values
# unique(ea$alt_know_asha)

# for ego, 1 is Yes, 2 is heard, 9 is don't know/haven't heard
# for alter 1 is Yes, 2 is No, 9 is not sure
# we should change ego values of 9 to 2 to match alter
# remove alter rows with 9

# asha
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_asha[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_asha_blau',
                   'hetero_know_asha_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# anm
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_anm[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_anm_blau',
                   'hetero_know_anm_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# aww
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_aww[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_aww_blau',
                   'hetero_know_aww_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# shg
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_shg[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_shg_blau',
                   'hetero_know_shg_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# pradhan
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_pra[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_pra_blau',
                   'hetero_know_pra_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# pharmacist
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_pha[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_pha_blau',
                   'hetero_know_pha_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# doctor
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_doc[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_doc_blau',
                   'hetero_know_doc_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# religious leader
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_know_rel[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_know_rel_blau',
                   'hetero_know_rel_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# calculate health worker category


# # calculate homophily
# # calculate heterogeneity
# ppl <- tabyl(alt_hw)
# 
# if('valid_percent' %in% colnames(ppl)){
#   ppl %<>% select(-percent) %>% rename(percent = valid_percent)
# }
# 
# n <- ppl$n[is.na(ppl$percent) == F] %>% sum
# ppl <- blau(na.omit(ppl$percent))
# dat <- tibble(blau = ppl[1],
#               iqv = ppl[2],
#               k = ppl[3])
# 
# # add to heterogeneity output
# hetero %<>% add_row(metric = 'Know Health Worker Category',
#                     blau_index = dat %>% select(blau) %>% as.character,
#                     iqv = dat %>% select(iqv) %>% as.character,
#                     k = dat %>% select(k) %>% as.character,
#                     n = n %>% as.character)
# 
# # calculate village leader category
# # calculate heterogeneity
# ppl <- tabyl(alt_vl)
# 
# if('valid_percent' %in% colnames(ppl)){
#   ppl %<>% select(-percent) %>% rename(percent = valid_percent)
# }
# 
# n <- ppl$n[is.na(ppl$percent) == F] %>% sum
# ppl <- blau(na.omit(ppl$percent))
# dat <- tibble(blau = ppl[1],
#               iqv = ppl[2],
#               k = ppl[3])
# 
# # add to heterogeneity output
# hetero %<>% add_row(metric = 'Know Village Leader Category',
#                     blau_index = dat %>% select(blau) %>% as.character,
#                     iqv = dat %>% select(iqv) %>% as.character,
#                     k = dat %>% select(k) %>% as.character,
#                     n = n %>% as.character)
# 
# # # now total number of people
# # # calculate heterogeneity
# # ppl <- tibble(ppl_known_village_sd = sd(alt_ppl, na.rm = T) %>% round(2),
# #               n = length(alt_ppl[is.na(alt_ppl) == F]))

##########################
### NUMBER OF CHILDREN ###
##########################

# check unique values
# unique(ea$alt_sons)
# unique(ea$alt_daughters)

# # calculate heterogeneity
# children <- tibble(sons_sd = sd(ea$alt_sons, na.rm = T) %>% round(2),
#               sons_n = length(ea$alt_sons[is.na(ea$alt_sons) == F]),
#               daughters_sd = sd(ea$alt_daughters, na.rm = T) %>% round(2),
#               daughters_n = length(ea$alt_daughters[is.na(ea$alt_daughters) == F]),
#               children_sd = sd((ea$alt_sons + ea$alt_daughters), na.rm = T) %>% round(2),
#               children_n = length((ea$alt_sons + ea$alt_daughters)[is.na((ea$alt_sons + ea$alt_daughters)) == F]))

##########################
### PLACE OF RESIDENCE ###
##########################

# check unique values
# unique(ea$alt_residence)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_residence[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_residence_blau',
                   'hetero_residence_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

#######################
### SOCIAL LEARNING ###
#######################

# check unique values
#unique(ea$alt_learned)

# a
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "A"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_a_blau',
                   'hetero_soc_learn_a_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# b
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "B"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_b_blau',
                   'hetero_soc_learn_b_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# c
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "C"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_c_blau',
                   'hetero_soc_learn_c_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# d
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "D"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_d_blau',
                   'hetero_soc_learn_d_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# E
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "E"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_e_blau',
                   'hetero_soc_learn_e_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# f
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "F"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_f_blau',
                   'hetero_soc_learn_f_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# g
# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(str_detect(ea$alt_learned[ea$ego_id == x], "G"))
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_soc_learn_g_blau',
                   'hetero_soc_learn_g_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###########################
### ENCOURAGE USE OF FP ###
###########################

# check unique values
# unique(ea$alt_encourage_fp)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_encourage_fp[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_encourage_fp_blau',
                   'hetero_encourage_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

#################################
### DISCUSS FP METHODS FREELY ###
#################################

# check unique values
# unique(ea$alt_discuss_fp)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_discuss_fp[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_discuss_fp_blau',
                   'hetero_discuss_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###########################
### ALT HELP/ADVISE EGO ###
###########################

# check unique values
# unique(ea$alt_help_ego)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_help_ego[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_help_ego_blau',
                   'hetero_alt_help_ego_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###########################
### EGO HELP/ADVISE ALT ###
###########################

# check unique values
# unique(ea$alt_helped_by_ego)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_helped_by_ego[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_helped_by_ego_blau',
                   'hetero_alt_helped_by_ego_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

############################
### GOING AGAINST ADVICE ###
############################

# check unique values
# unique(ea$alt_against_advice)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_against_advice[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_against_advice_blau',
                   'hetero_alt_against_advice_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

###################################
### SUPPORT NOT HAVING CHILDREN ###
###################################

# check unique values
# unique(ea$alt_support_no_child)

# build proportions
tib <- sapply(unique(ea$ego_id), 
              FUN = function(x){
                t <- tabyl(ea$alt_support_no_child[ea$ego_id == x])
                if('valid_percent' %in% colnames(t)){
                  t %<>% select(-percent) %>% rename(percent = valid_percent)
                }
                blau(na.omit(t$percent))
              })

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_alt_support_no_child_blau',
                   'hetero_alt_support_no_child_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output
hetero %<>% full_join(tib %>% select(-k),
                      by = 'ego_id')

# add homophily and hetero measures to dat
dat_up %<>% full_join(homo, by = 'ego_id') %>%
  full_join(hetero, by = 'ego_id')

# clean environment
rm(list=setdiff(ls(), c("dat_bihar", "dat_up")))