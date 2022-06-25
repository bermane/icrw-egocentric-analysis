# this code loads csv data of ego and alter interviews
# and calculates homophily and homogeneity metrics

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

###############################
### HOMOPHILY EGO AND ALTER ###
###############################

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
unique(ea$ego_sex)
unique(ea$alt_sex)

# set to numeric
ea$alt_sex <- as.numeric(ea$alt_sex)

# summarise
sex <- ea$ego_sex == ea$alt_sex
sex <- tabyl(sex)

write.csv(sex, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_gender.csv',
          row.names = F)

#############
### CASTE ###
#############

# check unique values
unique(ea$ego_caste)
unique(ea$alt_caste)

# set to numeric
ea$alt_caste <- as.numeric(ea$alt_caste)

# set don't know to NA
ea$alt_caste[ea$alt_caste == 9] <- NA

# summarise
caste <- ea$ego_caste == ea$alt_caste
caste <- tabyl(caste)

write.csv(caste, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_caste.csv',
          row.names = F)

#################
### EDUCATION ###
#################

# DON'T HAVE ALTER EDUCATION FROM EGO SURVEY !!!

################################
### NUMBER OF FAMILY MEMBERS ###
################################

# check unique values
unique(ea$ego_fam)
unique(ea$alt_fam)

# convert to numeric
ea$ego_fam <- as.numeric(ea$ego_fam)
ea$alt_fam <- as.numeric(ea$alt_fam)

# set don't know to NA
ea$alt_fam[ea$alt_fam == 99] <- NA

# calculate bias homophily
fam <- ea$ego_fam - ea$alt_fam
fam <- tibble(mean_fam = mean(fam, na.rm = T),
              med_fam = median(fam, na.rm = T),
              n = length(fam[is.na(fam) == F]))

write.csv(fam, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_fam_bias.csv',
          row.names = F)

# create formula to calculate sd homophily
homo_sd <- function(ego, alt){
  out <- alt - ego
  out <- out*out
  n <- length(out[is.na(out) == F])
  out <- sum(out, na.rm = T)
  out <- out/n
  return(c(round(sqrt(out), 2), n))
}

# calculate sd homophily
fam_sd <- homo_sd(ea$ego_fam, ea$alt_fam)
fam_sd <- tibble(avg_eucli_dist_fam = fam_sd[1],
                 n = fam_sd[2])

write.csv(fam_sd, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_fam_sd.csv',
          row.names = F)

#######################################
### NUMBER OF FRIENDS AND NEIGHBORS ###
#######################################

# check unique values
unique(ea$ego_friends)
unique(ea$alt_friends)
unique(ea$ego_neigh)
unique(ea$alt_neigh)

# convert to numeric
ea$ego_friends <- as.numeric(ea$ego_friends)
ea$alt_friends <- as.numeric(ea$alt_friends)
ea$ego_neigh <- as.numeric(ea$ego_neigh)
ea$alt_neigh <- as.numeric(ea$alt_neigh)

# set don't know to NA
ea$alt_friends[ea$alt_friends == 99] <- NA
ea$alt_neigh[ea$alt_neigh == 99] <- NA

# calculate homophily
friends <- ea$ego_friends - ea$alt_friends
neigh <- ea$ego_neigh - ea$alt_neigh
friends_neigh <- (ea$ego_friends + ea$ego_neigh) - (ea$alt_friends + ea$alt_neigh)

friends_neigh <- tibble(mean_friends = mean(friends, na.rm = T),
                        med_friends = median(friends, na.rm = T),
                        n_friends = length(friends[is.na(friends) == F]),
                        mean_neigh = mean(neigh, na.rm = T),
                        med_neigh = median(neigh, na.rm = T),
                        n_neigh = length(neigh[is.na(neigh) == F]),
                        mean_friends_neigh = mean(friends_neigh, na.rm = T),
                        med_friends_neigh = median(friends_neigh, na.rm = T),
                        n_friends_neigh = length(friends_neigh[is.na(friends_neigh) == F]))

write.csv(friends_neigh, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_friends_neighbors_bias.csv',
          row.names = F)

# calculate sd homophily
friends_sd <- homo_sd(ea$ego_friends, ea$alt_friends)
neigh_sd <- homo_sd(ea$ego_neigh, ea$alt_neigh)
friends_neigh_sd <- homo_sd((ea$ego_friends + ea$ego_neigh), (ea$alt_friends + ea$alt_neigh))
friends_neigh_sd_out <- tibble(avg_eucli_dist_friends = friends_sd[1],
                               n_friends = friends_sd[2],
                               avg_eucli_dist_neigh = neigh_sd[1],
                               n_neigh = neigh_sd[2],
                               avg_eucli_dist_friends_neigh = friends_neigh_sd[1],
                               n_friends_neigh = friends_neigh_sd[2])

write.csv(friends_neigh_sd_out, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_friends_neighbors_sd.csv',
          row.names = F)

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
unique(ea$ego_using_fp)
unique(ea$alt_using_fp)

# set values to numeric
ea$alt_using_fp <- as.numeric(ea$alt_using_fp)

# if alter is husband change to same value as ego
ea$alt_using_fp[str_detect(ea$alt_relationship, 'Husband')] <- ea$ego_using_fp[str_detect(ea$alt_relationship, 'Husband')]

# set don't know to NA
ea$alt_using_fp[ea$alt_using_fp == 9] <- NA

# calculate homophily
using_fp <- ea$ego_using_fp == ea$alt_using_fp
using_fp <- tabyl(using_fp)

write.csv(using_fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_using_fp.csv',
          row.names = F)

###############################
### PEOPLE KNOWN IN VILLAGE ###
###############################

# check some values
unique(ea$ego_know_asha)
unique(ea$alt_know_asha)

# for ego, 1 is Yes, 2 is heard, 9 is don't know/haven't heard
# for alter 1 is Yes, 2 is No, 9 is not sure
# we should change ego values of 9 to 2 to match alter
# remove alter rows with 9

# do individuals first
# asha
# recode values
ea$ego_know_asha[ea$ego_know_asha == '9'] <- '2'
ea$alt_know_asha[ea$alt_know_asha == '9'] <- NA

# calculate homophily
asha <- ea$ego_know_asha == ea$alt_know_asha
asha <- tabyl(asha)

write.csv(asha, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_asha.csv',
          row.names = F)

# anm
# recode values
ea$ego_know_anm[ea$ego_know_anm == '9'] <- '2'
ea$alt_know_anm[ea$alt_know_anm == '9'] <- NA

# calculate homophily
anm <- ea$ego_know_anm == ea$alt_know_anm
anm <- tabyl(anm)

write.csv(anm, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_anm.csv',
          row.names = F)

# aww
# recode values
ea$ego_know_aww[ea$ego_know_aww == '9'] <- '2'
ea$alt_know_aww[ea$alt_know_aww == '9'] <- NA

# calculate homophily
aww <- ea$ego_know_aww == ea$alt_know_aww
aww <- tabyl(aww)

write.csv(aww, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_aww.csv',
          row.names = F)

# shg
# recode values
ea$ego_know_shg[ea$ego_know_shg == '9'] <- '2'
ea$alt_know_shg[ea$alt_know_shg == '9'] <- NA

# calculate homophily
shg <- ea$ego_know_shg == ea$alt_know_shg
shg <- tabyl(shg)

write.csv(shg, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_shg.csv',
          row.names = F)

# pradhan
# recode values
ea$ego_know_pra[ea$ego_know_pra == '9'] <- '2'
ea$alt_know_pra[ea$alt_know_pra == '9'] <- NA

# calculate homophily
pra <- ea$ego_know_pra == ea$alt_know_pra
pra <- tabyl(pra)

write.csv(pra, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_pradhan.csv',
          row.names = F)

# pharmacist
# recode values
ea$ego_know_pha[ea$ego_know_pha == '9'] <- '2'
ea$alt_know_pha[ea$alt_know_pha == '9'] <- NA

# calculate homophily
pha <- ea$ego_know_pha == ea$alt_know_pha
pha <- tabyl(pha)

write.csv(pha, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_pharmacist.csv',
          row.names = F)

# doctor
# recode values
ea$ego_know_doc[ea$ego_know_doc == '9'] <- '2'
ea$alt_know_doc[ea$alt_know_doc == '9'] <- NA

# calculate homophily
doc <- ea$ego_know_doc == ea$alt_know_doc
doc <- tabyl(doc)

write.csv(doc, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_doc.csv',
          row.names = F)

# religious leader
# recode values
ea$ego_know_rel[ea$ego_know_rel == '9'] <- '2'
ea$alt_know_rel[ea$alt_know_rel == '9'] <- NA

# calculate homophily
rel <- ea$ego_know_rel == ea$alt_know_rel
rel <- tabyl(rel)

write.csv(rel, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_religious_leader.csv',
          row.names = F)

# now by category
# first all values to numeric
ea %<>% mutate(across(c(ego_know_asha,
                       ego_know_anm,
                       ego_know_aww,
                       ego_know_shg,
                       ego_know_pra,
                       ego_know_pha,
                       ego_know_doc,
                       ego_know_rel,
                       alt_know_asha,
                       alt_know_anm,
                       alt_know_aww,
                       alt_know_shg,
                       alt_know_pra,
                       alt_know_pha,
                       alt_know_doc,
                       alt_know_rel), as.numeric))

# exchange 2s for 0s
ea %<>% mutate(across(c(ego_know_asha,
                        ego_know_anm,
                        ego_know_aww,
                        ego_know_shg,
                        ego_know_pra,
                        ego_know_pha,
                        ego_know_doc,
                        ego_know_rel,
                        alt_know_asha,
                        alt_know_anm,
                        alt_know_aww,
                        alt_know_shg,
                        alt_know_pra,
                        alt_know_pha,
                        alt_know_doc,
                        alt_know_rel),
                      ~ ifelse(. == 2, 0, .)))

# calculate health worker category
ego_hw <- ea %>% 
  select(ego_know_asha,
         ego_know_anm,
         ego_know_aww,
         ego_know_pha,
         ego_know_doc) %>%
  rowSums(na.rm = T)

ego_hw <- ego_hw > 0

alt_hw <- ea %>% 
  select(alt_know_asha,
         alt_know_anm,
         alt_know_aww,
         alt_know_pha,
         alt_know_doc) %>%
  rowSums(na.rm = T)

alt_hw <- alt_hw > 0

# calculate homophily
hw <- ego_hw == alt_hw
hw <- tabyl(hw)

write.csv(hw, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_health_worker_category.csv',
          row.names = F)

# calculate village leader category
ego_vl <- ea %>% 
  select(ego_know_pra,
         ego_know_rel) %>%
  rowSums(na.rm = T)

ego_vl <- ego_vl > 0

alt_vl <- ea %>% 
  select(alt_know_pra,
         alt_know_rel) %>%
  rowSums(na.rm = T)

alt_vl <- alt_vl > 0

# calculate homophily
vl <- ego_vl == alt_vl
vl <- tabyl(vl)

write.csv(vl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_know_village_leader_category.csv',
          row.names = F)

# now total number of people
ego_ppl <- ea %>%
  select(ego_know_asha,
         ego_know_anm,
         ego_know_aww,
         ego_know_shg,
         ego_know_pra,
         ego_know_pha,
         ego_know_doc,
         ego_know_rel) %>%
  rowSums(na.rm = T)

alt_ppl <- ea %>%
  select(alt_know_asha,
         alt_know_anm,
         alt_know_aww,
         alt_know_shg,
         alt_know_pra,
         alt_know_pha,
         alt_know_doc,
         alt_know_rel) %>%
  rowSums(na.rm = T)

# calculate bias homophily
ppl <- ego_ppl - alt_ppl
ppl <- tibble(mean_people_known_village = mean(ppl, na.rm = T),
              med_people_known_village = median(ppl, na.rm = T),
              n = length(ppl[is.na(ppl) == F]))

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_num_people_known_village_bias.csv',
          row.names = F)

# calculate sd homophily
ppl_sd <- homo_sd(ego_ppl, alt_ppl)
ppl_sd <- tibble(avg_euclid_dist_ppl = ppl_sd[1],
                 n = ppl_sd[2])

write.csv(ppl_sd, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_num_people_known_village_sd.csv',
          row.names = F)

##########################
### NUMBER OF CHILDREN ###
##########################

# check unique values
unique(ea$ego_sons)
unique(ea$alt_sons)
unique(ea$ego_daughters)
unique(ea$alt_daughters)

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

# calculate bias homophily
sons <- ea$ego_sons - ea$alt_sons
daughters <- ea$ego_daughters - ea$alt_daughters
children <- (ea$ego_sons + ea$ego_daughters) - (ea$alt_sons + ea$alt_daughters)

children_bias <- tibble(mean_sons = mean(sons, na.rm = T),
                        med_sons = median(sons, na.rm = T),
                        n_sons = length(sons[is.na(sons) == F]),
                        mean_daughters = mean(daughters, na.rm = T),
                        med_daughters = median(daughters, na.rm = T),
                        n_daughters = length(neigh[is.na(daughters) == F]),
                        mean_children = mean(children, na.rm = T),
                        med_children = median(children, na.rm = T),
                        n_children = length(children[is.na(children) == F]))

write.csv(children_bias, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_children_bias.csv',
          row.names = F)

# calculate sd homophily
sons_sd <- homo_sd(ea$ego_sons, ea$alt_sons)
daughters_sd <- homo_sd(ea$ego_daughters, ea$alt_daughters)
children_sd <- homo_sd((ea$ego_sons + ea$ego_daughters), (ea$alt_sons + ea$alt_daughters))
children_sd_out <- tibble(avg_eucli_dist_sons = sons_sd[1],
                               n_sons = sons_sd[2],
                               avg_eucli_dist_daughters = daughters_sd[1],
                               n_daughters = daughters_sd[2],
                               avg_eucli_dist_children = children_sd[1],
                               n_children = children_sd[2])

write.csv(children_sd_out, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_children_sd.csv',
          row.names = F)

##############################################
### BASIC PROPORTION OF PLACE OF RESIDENCE ###
##############################################

# check unique values
unique(ea$alt_residence)

# recode values
ea %<>% mutate(alt_residence = recode(alt_residence, 
                                     '1' = 'Same Household',
                                     '2' = 'Same Village',
                                     '3' = 'Another Village (same district)',
                                     '4' = 'Outside this District'))

# build tabyl
alt_residence <- ea %>% tabyl(alt_residence)

write.csv(alt_residence, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/homophily/ego_alt_residence.csv',
          row.names = F)

#####################################
### HETEROGENEITY OF EGO'S ALTERS ###
#####################################

###########
### SEX ###
###########

# check unique values
unique(ea$alt_sex)

# build proportions
sex <- ea %>% tabyl(alt_sex)

# create a function to calculate blaus and iqv
blau <- function(perc){
  b <- perc*perc
  k <- length(perc)
  b <- (1 - sum(b)) %>% round(2)
  iqv <- (b / (1-(1/k))) %>% round(2)
  return(c(b, iqv, k))
}

# calculate heterogeneity
sex <- blau(sex$percent)

# create tibble
sex = tibble(sex_blau = sex[1],
             sex_iqv = sex[2],
             k = sex[3])

write.csv(sex, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_gender.csv',
          row.names = F)

#############
### CASTE ###
#############

# check unique values
unique(ea$alt_caste)

# build proportions
caste <- ea %>% tabyl(alt_caste)

# calculate heterogeneity
caste <- blau(na.omit(caste$valid_percent))

# create tibble
caste = tibble(caste_blau = caste[1],
             caste_iqv = caste[2],
             k = caste[3])

write.csv(caste, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_caste.csv',
          row.names = F)

################################
### NUMBER OF FAMILY MEMBERS ###
################################

# check unique values
unique(ea$alt_fam)

# calculate heterogeneity
fam <- tibble(fam_sd = sd(ea$alt_fam, na.rm = T) %>% round(2),
              n = length(ea$alt_fam[is.na(ea$alt_fam) == F]))

write.csv(fam, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_fam.csv',
          row.names = F)

#######################################
### NUMBER OF FRIENDS AND NEIGHBORS ###
#######################################

# check unique values
unique(ea$alt_friends)
unique(ea$alt_neigh)

# calculate heterogeneity
friends_neigh <- tibble(friends_sd = sd(ea$alt_friends, na.rm = T) %>% round(2),
              friends_n = length(ea$alt_friends[is.na(ea$alt_friends) == F]),
              neigh_sd = sd(ea$alt_neigh, na.rm = T) %>% round(2),
              neigh_n = length(ea$alt_neigh[is.na(ea$alt_neigh) == F]),
              friends_neigh_sd = sd((ea$alt_friends + ea$alt_neigh), na.rm = T) %>% round(2),
              friends_neigh_n = length((ea$alt_friends + ea$alt_neigh)[is.na((ea$alt_friends + ea$alt_neigh)) == F]))

write.csv(friends_neigh, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_friends_neigh.csv',
          row.names = F)

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
unique(ea$alt_using_fp)

# calculate heterogeneity
using_fp <- tabyl(ea$alt_using_fp)
using_fp <- blau(na.omit(using_fp$valid_percent))
using_fp <- tibble(using_fp_blau = using_fp[1],
                   using_fp_iqv = using_fp[2],
                   k = using_fp[3])

write.csv(using_fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_using_fp.csv',
          row.names = F)

###############################
### PEOPLE KNOWN IN VILLAGE ###
###############################

# check some values
unique(ea$alt_know_asha)

# for ego, 1 is Yes, 2 is heard, 9 is don't know/haven't heard
# for alter 1 is Yes, 2 is No, 9 is not sure
# we should change ego values of 9 to 2 to match alter
# remove alter rows with 9

# calculate heterogeneity
ppl <- tabyl(ea$alt_know_asha)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_asha_blau = ppl[1],
              know_asha_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_asha.csv',
          row.names = F)

# anm
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_anm)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_anm_blau = ppl[1],
              know_anm_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_anm.csv',
          row.names = F)

# aww
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_aww)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_aww_blau = ppl[1],
              know_aww_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_aww.csv',
          row.names = F)

# shg
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_shg)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_shg_blau = ppl[1],
              know_shg_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_shg.csv',
          row.names = F)

# pradhan
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_pra)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_pra_blau = ppl[1],
              know_pra_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_pradhan.csv',
          row.names = F)

# pharmacist
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_pha)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_pha_blau = ppl[1],
              know_pha_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_pha.csv',
          row.names = F)

# doctor
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_doc)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_doc_blau = ppl[1],
              know_doc_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_doctor.csv',
          row.names = F)

# religious leader
# calculate heterogeneity
ppl <- tabyl(ea$alt_know_rel)
ppl <- blau(na.omit(ppl$valid_percent))
ppl <- tibble(know_rel_blau = ppl[1],
              know_rel_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_rel.csv',
          row.names = F)

# calculate health worker category


# calculate homophily
# calculate heterogeneity
ppl <- tabyl(alt_hw)
ppl <- blau(na.omit(ppl$percent))
ppl <- tibble(know_hw_blau = ppl[1],
              know_hw_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_health_worker.csv',
          row.names = F)

# calculate village leader category
# calculate heterogeneity
ppl <- tabyl(alt_vl)
ppl <- blau(na.omit(ppl$percent))
ppl <- tibble(know_vl_blau = ppl[1],
              know_vl_iqv = ppl[2],
              k = ppl[3])

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_know_village_leader.csv',
          row.names = F)

# now total number of people
# calculate heterogeneity
ppl <- tibble(ppl_known_village_sd = sd(alt_ppl, na.rm = T) %>% round(2),
              n = length(alt_ppl[is.na(alt_ppl) == F]))

write.csv(ppl, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_ppl_known_village.csv',
          row.names = F)

##########################
### NUMBER OF CHILDREN ###
##########################

# check unique values
unique(ea$alt_sons)
unique(ea$alt_daughters)

# calculate heterogeneity
children <- tibble(sons_sd = sd(ea$alt_sons, na.rm = T) %>% round(2),
              sons_n = length(ea$alt_sons[is.na(ea$alt_sons) == F]),
              daughters_sd = sd(ea$alt_daughters, na.rm = T) %>% round(2),
              daughters_n = length(ea$alt_daughters[is.na(ea$alt_daughters) == F]),
              children_sd = sd((ea$alt_sons + ea$alt_daughters), na.rm = T) %>% round(2),
              children_n = length((ea$alt_sons + ea$alt_daughters)[is.na((ea$alt_sons + ea$alt_daughters)) == F]))

write.csv(children, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_children.csv',
          row.names = F)

##########################
### PLACE OF RESIDENCE ###
##########################

# check unique values
unique(ea$alt_residence)

# calculate heterogeneity
res <- tabyl(ea$alt_residence)
res <- blau(na.omit(res$percent))
res <- tibble(residence_blau = res[1],
              residence_iqv = res[2],
              k = res[3])

write.csv(res, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_residence.csv',
          row.names = F)

#######################
### SOCIAL LEARNING ###
#######################

# check unique values
unique(ea$alt_learned)

# calculate homophily
a <- tabyl(str_detect(ea$alt_learned, "A"))
a <- blau(a$percent)
b <- tabyl(str_detect(ea$alt_learned, "B"))
b <- blau(b$percent)
c <- tabyl(str_detect(ea$alt_learned, "C"))
c <- blau(c$percent)
d <- tabyl(str_detect(ea$alt_learned, "D"))
d <- blau(d$percent)
e <- tabyl(str_detect(ea$alt_learned, "E"))
e <- blau(e$percent)
f <- tabyl(str_detect(ea$alt_learned, "F"))
f <- blau(f$percent)
g <- tabyl(str_detect(ea$alt_learned, "G"))
g <- blau(g$percent)

social_learning <- tibble(get_along_with_family_blau = a[1],
                         get_along_with_family_iqv = a[2],
                         get_along_with_family_n = a[3],
                         ways_of_earning_blau = b[1],
                         ways_of_earning_iqv = b[2],
                         ways_of_earning_n = b[3],
                         take_care_of_children_blau = c[1],
                         take_care_of_children_iqv = c[2],
                         take_care_of_children_n = c[3],
                         get_health_care_blau = d[1],
                         get_health_care_iqv = d[2],
                         get_health_care_n = d[3],
                         apply_for_gov_schemes_blau = e[1],
                         apply_for_gov_schemes_iqv = e[2],
                         apply_for_gov_schemes_n = e[3],
                         plan_num_children_blau = f[1],
                         plan_num_children_iqv = f[2],
                         plan_num_children_n = f[3],
                         fp_methods_blau = g[1],
                         fp_methods_iqv = g[2],
                         fp_methods_n = g[3])

write.csv(social_learning, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_social_learning.csv',
          row.names = F)

###########################
### ENCOURAGE USE OF FP ###
###########################

# check unique values
unique(ea$alt_encourage_fp)

# calculate heterogeneity
fp <- tabyl(ea$alt_encourage_fp)
fp <- blau(na.omit(fp$percent))
fp <- tibble(encourage_fp_blau = fp[1],
              encourage_fp_iqv = fp[2],
              k = fp[3])

write.csv(fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_encourage_fp.csv',
          row.names = F)

#################################
### DISCUSS FP METHODS FREELY ###
#################################

# check unique values
unique(ea$alt_discuss_fp)

# calculate heterogeneity
fp <- tabyl(ea$alt_discuss_fp)
fp <- blau(na.omit(fp$percent))
fp <- tibble(discuss_fp_freely_blau = fp[1],
             discuss_fp_freely_iqv = fp[2],
             k = fp[3])

write.csv(fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_discuss_fp_freely.csv',
          row.names = F)

###########################
### ALT HELP/ADVISE EGO ###
###########################

# check unique values
unique(ea$alt_help_ego)

# calculate heterogeneity
fp <- tabyl(ea$alt_help_ego)
fp <- blau(na.omit(fp$percent))
fp <- tibble(alt_help_advise_ego_blau = fp[1],
             alt_help_advise_ego_iqv = fp[2],
             k = fp[3])

write.csv(fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_helped_advised_by_alt.csv',
          row.names = F)

###########################
### EGO HELP/ADVISE ALT ###
###########################

# check unique values
unique(ea$alt_helped_by_ego)

# calculate heterogeneity
fp <- tabyl(ea$alt_helped_by_ego)
fp <- blau(na.omit(fp$percent))
fp <- tibble(ego_help_advise_alt_blau = fp[1],
             ego_help_advise_alt_iqv = fp[2],
             k = fp[3])

write.csv(fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_help_advise_alt.csv',
          row.names = F)

############################
### GOING AGAINST ADVICE ###
############################

# check unique values
unique(ea$alt_against_advice)

# calculate heterogeneity
fp <- tabyl(ea$alt_against_advice)
fp <- blau(na.omit(fp$percent))
fp <- tibble(alt_against_advice_blau = fp[1],
             alt_against_advice_iqv = fp[2],
             k = fp[3])

write.csv(fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_against_advice.csv',
          row.names = F)

###################################
### SUPPORT NOT HAVING CHILDREN ###
###################################

# check unique values
unique(ea$alt_support_no_child)

# calculate heterogeneity
fp <- tabyl(ea$alt_support_no_child)
fp <- blau(na.omit(fp$percent))
fp <- tibble(alt_support_no_child_blau = fp[1],
             alt_support_no_child_iqv = fp[2],
             k = fp[3])

write.csv(fp, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/heterogeneity/ego_support_no_child.csv',
          row.names = F)
