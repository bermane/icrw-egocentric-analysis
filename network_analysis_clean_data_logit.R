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
alter <- read_excel(path = "data/alter_clean_complete_11012022.xlsx")

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
ego <- ego[-1, ]
alter <- alter[-1, ]

# clean column names only keep part after last "."
ego_names <- sapply(
  colnames(ego),
  FUN = function(x) {
    st <- str_split(x, pattern = glob2rx("*-*"))
    st <- st[[1]][length(st[[1]])]
    return(st)
  }
)

names(ego_names) <- NULL
colnames(ego) <- ego_names

alter_names <- sapply(
  colnames(alter),
  FUN = function(x) {
    st <- str_split(x, pattern = glob2rx("*-*"))
    st <- st[[1]][length(st[[1]])]
    return(st)
  }
)

names(alter_names) <- NULL
colnames(alter) <- alter_names

rm(ego_names, alter_names)

# remove empty columns from "notes"
ego <- ego[, str_detect(colnames(ego), 'note') == F]
alter <- alter[, str_detect(colnames(alter), 'note') == F]

# replace character NA values with NA
ego[ego == "NA"] <- NA
alter[alter == "NA"] <- NA

# fix district, block and village in alter data
alter$district_name <- alter$`district name_clean`
alter$block_name <- alter$`block name_clean`
alter$village_name <- alter$`village name_clean`

# remove "clean" columns
alter <-
  alter %>% select(-c(`district name_clean`, `block name_clean`, `village name_clean`))

# add alter-alter tie columns to ego dataset so we can build edgelist below
# let's first extract the correct question so easier to look at
alter_know <-
  ego[, str_detect(colnames(ego), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to 0 since then tie won't exist. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know %<>% as.data.frame %>%
  mutate(
    know12 = alter1_know2 + alter2_know1,
    know13 = alter1_know3 + alter3_know1,
    know14 = alter1_know4 + alter4_know1,
    know15 = alter1_know5 + alter5_know1,
    know23 = alter2_know3 + alter3_know2,
    know24 = alter2_know4 + alter4_know2,
    know25 = alter2_know5 + alter5_know2,
    know34 = alter3_know4 + alter4_know3,
    know35 = alter3_know5 + alter5_know3,
    know45 = alter4_know5 + alter5_know4
  )

# now set 0 to NA since we just want to identify tie or not
alter_know %<>% select(know12:know45)
alter_know[alter_know == 0] <- NA

# add simple know columns back into ego df
ego %<>% add_column(alter_know)
rm(alter_know)

# add aalter-aalter tie columns to alter dataset so we can build edgelist below
# let's first extract the correct question so easier to look at
alter_know <-
  alter[, str_detect(colnames(alter), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to 0 since then tie won't exist. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know %<>% as.data.frame %>%
  mutate(
    know12 = alter1_know2 + alter2_know1,
    know13 = alter1_know3 + alter3_know1,
    know14 = alter1_know4 + alter4_know1,
    know15 = alter1_know5 + alter5_know1,
    know23 = alter2_know3 + alter3_know2,
    know24 = alter2_know4 + alter4_know2,
    know25 = alter2_know5 + alter5_know2,
    know34 = alter3_know4 + alter4_know3,
    know35 = alter3_know5 + alter5_know3,
    know45 = alter4_know5 + alter5_know4
  )

# now set 0 to NA since we just want to identify tie or not
alter_know %<>% select(know12:know45)
alter_know[alter_know == 0] <- NA

# add simple know columns back into alter df
alter %<>% add_column(alter_know)
rm(alter_know)

##################################################
### CREATE PC VARIABLE IF EGO HAS EVER USED FP ###
##################################################

# create single col if ego has EVER used FP from list of questions pcq303a-l
# create df of all rows of interest
ego_ever_used_fp <- ego_pc %>% select(pcq303a, pcq303b, pcq303c, pcq303d, pcq303e,
                                      pcq303f, pcq303g, pcq303h, pcq303i)

# sum if all columns are NA
ego_ever_used_fp_na <- rowSums(is.na(ego_ever_used_fp), na.rm = T)

# create na mask
ego_ever_used_fp_na[ego_ever_used_fp_na < NCOL(ego_ever_used_fp)] <- 0
ego_ever_used_fp_na[ego_ever_used_fp_na == NCOL(ego_ever_used_fp)] <- 1

# sum if ever used
ego_ever_used_fp <- rowSums(ego_ever_used_fp == 1, na.rm = T)

# set all ever used to 1 and never used to 2
ego_ever_used_fp[ego_ever_used_fp > 0] <- 1
ego_ever_used_fp[ego_ever_used_fp == 0] <- 2

# set to NA if all answers are NA
ego_ever_used_fp[ego_ever_used_fp_na == 1] <- NA

# put back into ego_pc
ego_pc$ego_ever_used_fp <- ego_ever_used_fp
rm(ego_ever_used_fp_na, ego_ever_used_fp)

###########################################
### CREATE INITIAL DATA TABLE OF VALUES ###
###########################################

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

dat_bihar <- tibble(
  ego_id = ego$woman_id,
  state = 'bihar',
  parity = ego_pc$pcq209,
  sons = ego_pc$pcq205s,
  daughters = ego_pc$pcq205d,
  neighbors_using_modern_fp = ego$contra_neighbour_num,
  preg = ego_pc$pcq230,
  using_fp = ego_pc$pcq311,
  fp_method = ego_pc$pcq312,
  age = ego_pc$pcq102,
  education = ego_pc$pcq103,
  caste = ego_pc$pcq111,
  husband_education = ego_pc$pcq115,
  want_more_children = ego_pc$pcq404
)

# fix some values
# if missing sons, daughters, parity means 0
# if missing preg, means no (2)
# husband education 98 is NA
dat_bihar %<>% mutate(sons = replace(sons, is.na(sons), 0)) %>%
  mutate(daughters = replace(daughters, is.na(daughters), 0)) %>%
  mutate(parity = replace(parity, is.na(parity), 0)) %>%
  mutate(preg = replace(preg, is.na(preg), 2)) %>%
  mutate(husband_education = replace(husband_education, husband_education == 98, NA)) %>%
  mutate(children = sons + daughters)

# if preg fill want more children from other question
dat_bihar$want_more_children[dat_bihar$preg %in% 1] <- ego_pc$pcq403[dat_bihar$preg %in% 1]

# calc diff ego and husband education
dat_bihar$ego_husband_edu_diff <- dat_bihar$education - dat_bihar$husband_education

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
ea <- rbind(
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter1_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter1_age,
    alt_yrs_known = ego$alter1_yrs_known,
    alt_freq_talk_fp = ego$alter1_freq_talk_fp,
    alt_talk_subjects = ego$alter1_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter1_howmany_child,
    alt_say_bad_things_fp = ego$alter1_approve_fp,
    alt_talked_fp_methods = ego$alter1_preg,
    alt_info_get_fp = ego$alter1_get_fp,
    alt_info_sideeff_fp = ego$alter1_sideeff_fp,
    alt_used_fp = ego$alter1_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter1_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter2_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter2_age,
    alt_yrs_known = ego$alter2_yrs_known,
    alt_freq_talk_fp = ego$alter2_freq_talk_fp,
    alt_talk_subjects = ego$alter2_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter2_howmany_child,
    alt_say_bad_things_fp = ego$alter2_approve_fp,
    alt_talked_fp_methods = ego$alter2_preg,
    alt_info_get_fp = ego$alter2_get_fp,
    alt_info_sideeff_fp = ego$alter2_sideeff_fp,
    alt_used_fp = ego$alter2_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter2_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter3_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter3_age,
    alt_yrs_known = ego$alter3_yrs_known,
    alt_freq_talk_fp = ego$alter3_freq_talk_fp,
    alt_talk_subjects = ego$alter3_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter3_howmany_child,
    alt_say_bad_things_fp = ego$alter3_approve_fp,
    alt_talked_fp_methods = ego$alter3_preg,
    alt_info_get_fp = ego$alter3_get_fp,
    alt_info_sideeff_fp = ego$alter3_sideeff_fp,
    alt_used_fp = ego$alter3_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter3_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter4_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter4_age,
    alt_yrs_known = ego$alter4_yrs_known,
    alt_freq_talk_fp = ego$alter4_freq_talk_fp,
    alt_talk_subjects = ego$alter4_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter4_howmany_child,
    alt_say_bad_things_fp = ego$alter4_approve_fp,
    alt_talked_fp_methods = ego$alter4_preg,
    alt_info_get_fp = ego$alter4_get_fp,
    alt_info_sideeff_fp = ego$alter4_sideeff_fp,
    alt_used_fp = ego$alter4_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter4_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter5_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter5_age,
    alt_yrs_known = ego$alter5_yrs_known,
    alt_freq_talk_fp = ego$alter5_freq_talk_fp,
    alt_talk_subjects = ego$alter5_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter5_howmany_child,
    alt_say_bad_things_fp = ego$alter5_approve_fp,
    alt_talked_fp_methods = ego$alter5_preg,
    alt_info_get_fp = ego$alter5_get_fp,
    alt_info_sideeff_fp = ego$alter5_sideeff_fp,
    alt_used_fp = ego$alter5_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter5_borrow
  )
)

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
  mutate(
    sum = sum(ego_sex == alt_sex, na.rm = T),
    n = NROW(ego_sex[is.na(ego_sex) == F &
                       is.na(alt_sex) == F]),
    homo_gender = sum / n
  ) %>% # calculate homophily
  select(ego_id, homo_gender) %>% # only keep homophily
  distinct %>% # remove duplicates
  arrange(ego_id) # arrange by ego id

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
homo_sd <- function(ego, alt) {
  out <- alt - ego
  out <- out * out
  n <- length(out[is.na(out) == F])
  out <- sum(out, na.rm = T)
  out <- out / n
  return(c(round(sqrt(out), 2), n))
}

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(homo_num_fam = homo_sd(ego_fam, alt_fam)[1]) %>% # calculate homophily
      select(ego_id, homo_num_fam) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

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
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        homo_friends = homo_sd(ego_friends, alt_friends)[1],
        homo_neigh = homo_sd(ego_neigh, alt_neigh)[1],
        homo_friends_neigh = homo_sd(ego_friends + ego_neigh, alt_friends + alt_neigh)[1]
      ) %>% # calculate homophily
      select(ego_id, homo_friends, homo_neigh, homo_friends_neigh) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

###########
### AGE ###
###########

# check unique values
# unique(ea$ego_age)
# unique(ea$alt_age)

# convert to numeric
ea$ego_age <- as.numeric(ea$ego_age)
ea$alt_age <- as.numeric(ea$alt_age)

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(homo_age = homo_sd(ego_age, alt_age)[1]) %>% # calculate homophily
      select(ego_id, homo_age) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1-10 are modern
# 11, 12, 96 are traditional
# 0 are missing

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
# unique(ea$ego_using_fp)
# unique(ea$alt_using_fp)

# set ego values to modern only is 1, 0 is not using any FP, so 2
ea %<>% mutate(ego_using_fp = replace(ego_using_fp, ego_using_fp %in% 1:10, 1))
ea %<>% mutate(ego_using_fp = replace(ego_using_fp, ego_using_fp %in% c(0, 11, 12), 2))

# set values to numeric
ea$alt_using_fp <- as.numeric(ea$alt_using_fp)

# if alter is husband change to same value as ego
ea$alt_using_fp[str_detect(ea$alt_relationship, 'Husband')] <-
  ea$ego_using_fp[str_detect(ea$alt_relationship, 'Husband')]

# set don't know to NA
ea$alt_using_fp[ea$alt_using_fp == 9] <- NA

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(ego_using_fp == alt_using_fp, na.rm = T),
        n = NROW(ego_using_fp[is.na(ego_using_fp) == F &
                                is.na(alt_using_fp) == F]),
        homo_using_fp = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_using_fp) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

##############################################
### BASIC PROPORTION OF PLACE OF RESIDENCE ###
##############################################

# check unique values
# unique(ea$alt_residence)

# recode values
ea %<>% mutate(
  alt_residence = recode(
    alt_residence,
    '1' = 'Same Household',
    '2' = 'Same Village',
    '3' = 'Another Village (same district)',
    '4' = 'Outside this District'
  )
)

# build tabyl
dat <- ea %>% tabyl(alt_residence) %>%
  adorn_pct_formatting(digits = 2)

# if valid percent remove percent and change name of valid percent
if ('valid_percent' %in% colnames(dat)) {
  dat %<>% select(-percent) %>% rename(percent = valid_percent)
}

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Same Household', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_household = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_household) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Same Village', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_village = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_village) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Another Village (same district)', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_another_village = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_another_village) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Outside this District', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_another_district = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_another_district) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

#####################################
### HETEROGENEITY OF EGO'S ALTERS ###
#####################################

# create a function to calculate blaus and iqv
blau <- function(perc) {
  b <- perc * perc
  k <- length(perc)
  b <- (1 - sum(b)) %>% round(2)
  iqv <- (b / (1 - (1 / k))) %>% round(2)
  return(c(b, iqv, k))
}

#################################
### DISCUSS FP METHODS FREELY ###
#################################

# check unique values
# unique(ea$alt_discuss_fp)

# build proportions
tib <- sapply(
  unique(ea$ego_id),
  FUN = function(x) {
    t <- tabyl(ea$alt_discuss_fp[ea$ego_id == x])
    if ('valid_percent' %in% colnames(t)) {
      t %<>% select(-percent) %>% rename(percent = valid_percent)
    }
    blau(na.omit(t$percent))
  }
)

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_discuss_fp_blau',
                   'hetero_discuss_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output (since first var)
hetero <- tib %>% select(-k)

####################################################
### TIE STRENGTH/SOCIAL PATHWAYS OF EGO'S ALTERS ###
####################################################

########################
### MEAN YEARS KNOWN ###
########################

# check unique values
# unique(ea$alt_yrs_known)
# unique(ea$ego_age_married)

# set to numeric
ea$alt_yrs_known <- as.numeric(ea$alt_yrs_known)

# reset values of 99 based on ego age and age of marriage
ea$alt_yrs_known[ea$alt_yrs_known == 99] <- ea$ego_age[ea$alt_yrs_known == 99] - ea$ego_age_married[ea$alt_yrs_known == 99]

ts <- ea %>%
  group_by(ego_id) %>% # group by ego id
  mutate(alt_mean_years_known = mean(alt_yrs_known), na.rm = T) %>% 
  select(ego_id, alt_mean_years_known) %>%
  distinct # remove duplicates

##########################
### TALK FREQ ABOUT FP ###
##########################

# check unique values
# unique(ea$alt_freq_talk_fp)

# set to numeric
ea$alt_freq_talk_fp <- as.numeric(ea$alt_freq_talk_fp)

# lets calc percent of alters talk fp at least once a week, and at least once a month

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_freq_talk_fp == 1, na.rm = T),
        n = NROW(alt_freq_talk_fp[is.na(alt_freq_talk_fp) == F]),
        alt_percent_talk_fp_weekly = sum / n
      ) %>%
      select(ego_id, alt_percent_talk_fp_weekly) %>%
      distinct
    ,
    by = 'ego_id'
  ) %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_freq_talk_fp %in% c(1, 2), na.rm = T),
        n = NROW(alt_freq_talk_fp[is.na(alt_freq_talk_fp) == F]),
        alt_percent_talk_fp_monthly = sum / n
      ) %>%
      select(ego_id, alt_percent_talk_fp_monthly) %>%
      distinct
    ,
    by = 'ego_id'
  )


#########################
### MEAN MULTIPLEXITY ###
#########################

# average of number of things discussed

# check unique values
unique(ea$alt_talk_subjects)

# count number of subjects
num_sub <- str_split(ea$alt_talk_subjects, ',')
num_sub <- sapply(num_sub, function(x) length(x))

# add to tibble
ea$alt_number_talk_subjects <- num_sub

# summarize and join

ts <- ts %>%
  full_join(ea %>%
  group_by(ego_id) %>% # group by ego id
  mutate(alt_mean_talk_subjects = mean(alt_number_talk_subjects), na.rm = T) %>% 
  select(ego_id, alt_mean_talk_subjects) %>%
  distinct
  , by = 'ego_id')

#########################################
### TALKED ABOUT NUM CHILDREN TO HAVE ###
#########################################

# proportion of ego's alters

# check unique values
# unique(ea$alt_talked_number_children)

# set to numeric
ea$alt_talked_number_children <- as.numeric(ea$alt_talked_number_children)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_talked_number_children == 1, na.rm = T),
        n = NROW(alt_talked_number_children[is.na(alt_talked_number_children) == F]),
        alt_percent_talked_number_children = sum / n
      ) %>%
      select(ego_id, alt_percent_talked_number_children) %>%
      distinct
    ,
    by = 'ego_id'
  )

#########################
### DISCUSS FP FREELY ###
#########################

# check unique values
# unique(ea$alt_discuss_fp)

# set to numeric
ea$alt_discuss_fp <- as.numeric(ea$alt_discuss_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_discuss_fp == 1, na.rm = T),
        n = NROW(alt_discuss_fp[is.na(alt_discuss_fp) == F]),
        alt_percent_discuss_fp_freely = sum / n
      ) %>%
      select(ego_id, alt_percent_discuss_fp_freely) %>%
      distinct
    ,
    by = 'ego_id'
  )

############################
### ENCOURAGE TO USE FP ###
############################

# check unique values
# unique(ea$alt_encourage_fp)

# set to numeric
ea$alt_encourage_fp <- as.numeric(ea$alt_encourage_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_encourage_fp == 1, na.rm = T),
        n = NROW(alt_encourage_fp[is.na(alt_encourage_fp) == F]),
        alt_percent_encourage_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_encourage_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

###################################
### SUPPORT NOT HAVING CHILDREN ###
###################################

# check unique values
# unique(ea$alt_support_no_child)

# set to numeric
ea$alt_support_no_child <- as.numeric(ea$alt_support_no_child)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_support_no_child == 1, na.rm = T),
        n = NROW(alt_support_no_child[is.na(alt_support_no_child) == F]),
        alt_percent_support_no_child = sum / n
      ) %>%
      select(ego_id, alt_percent_support_no_child) %>%
      distinct
    ,
    by = 'ego_id'
  )

###############################################
### WOULD NOT SAY BAD THINGS ABOUT USING FP ###
###############################################

# check unique values
# unique(ea$alt_say_bad_things_fp)

# set to numeric
ea$alt_say_bad_things_fp <- as.numeric(ea$alt_say_bad_things_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_say_bad_things_fp == 2, na.rm = T),
        n = NROW(alt_say_bad_things_fp[is.na(alt_say_bad_things_fp) == F]),
        alt_percent_not_say_bad_things_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_not_say_bad_things_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#############################################
### INDEX OF 5 SOCIAL INFLUENCE VARIABLES ###
#############################################

# add index to ea
ea$social_influence_index <- (ea$alt_talked_number_children == 1) +
  (ea$alt_discuss_fp == 1) +
  (ea$alt_encourage_fp == 1) +
  (ea$alt_support_no_child == 1) +
  (ea$alt_say_bad_things_fp == 2)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_mean_social_influence_index = mean(social_influence_index, na.rm = T)) %>%
      select(ego_id, alt_mean_social_influence_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_total_social_influence_index = sum(social_influence_index, na.rm = T)) %>%
      select(ego_id, alt_total_social_influence_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

###############################
### TALKED ABOUT FP METHODS ###
###############################

# check unique values
# unique(ea$alt_talked_fp_methods)

# set to numeric
ea$alt_talked_fp_methods <- as.numeric(ea$alt_talked_fp_methods)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_talked_fp_methods == 1, na.rm = T),
        n = NROW(alt_talked_fp_methods[is.na(alt_talked_fp_methods) == F]),
        alt_percent_talked_fp_methods = sum / n
      ) %>%
      select(ego_id, alt_percent_talked_fp_methods) %>%
      distinct
    ,
    by = 'ego_id'
  )

#####################################
### RECEIVED INFO WHERE TO GET FP ###
#####################################

# check unique values
# unique(ea$alt_info_get_fp)

# set to numeric
ea$alt_info_get_fp <- as.numeric(ea$alt_info_get_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_info_get_fp == 1, na.rm = T),
        n = NROW(alt_info_get_fp[is.na(alt_info_get_fp) == F]),
        alt_percent_info_get_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_info_get_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#####################################
### RECEIVED INFO SIDE EFFECTS FP ###
#####################################

# check unique values
# unique(ea$alt_info_sideeff_fp)

# set to numeric
ea$alt_info_sideeff_fp <- as.numeric(ea$alt_info_sideeff_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_info_sideeff_fp == 1, na.rm = T),
        n = NROW(alt_info_sideeff_fp[is.na(alt_info_sideeff_fp) == F]),
        alt_percent_info_sideeff_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_info_sideeff_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#######################################
### ALTER CURRENTLY USING MODERN FP ###
#######################################

# check unique values
# unique(ea$alt_using_fp)

# set NA values back to 9
# some of these values will be 2 (or missing since not using)
# but it doesn't matter since we are looking at the proportion of 1 vs any other answer
ea$alt_using_fp[is.na(ea$alt_using_fp)] <- 9

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_using_fp == 1, na.rm = T),
        n = NROW(alt_using_fp[is.na(alt_using_fp) == F]),
        alt_percent_using_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_using_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#################################
### ALTER EVER USED MODERN FP ###
#################################

# check unique values
# unique(ea$alt_used_fp)

# set to numeric
ea$alt_used_fp <- as.numeric(ea$alt_used_fp)

# if using fp is 1 set used fp to 1
ea$alt_used_fp[ea$alt_using_fp == 1] <- 1

# do not set NA values to 2 (have not ever used)
# because we don't know if that person has ever used
# with CURRENT we assume the skipped people are not 
# CURRENTLY USING

# if alter is husband change to same value as ego
ea$alt_used_fp[str_detect(ea$alt_relationship, 'Husband')] <-
  ea$ego_used_fp[str_detect(ea$alt_relationship, 'Husband')]

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_used_fp == 1, na.rm = T),
        n = NROW(alt_used_fp[is.na(alt_used_fp) == F]),
        alt_percent_used_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_used_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

######################
### ALTER HELP EGO ###
######################

# check unique values
# unique(ea$alt_help_ego)

# set to numeric
ea$alt_help_ego <- as.numeric(ea$alt_help_ego)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_help_ego == 1, na.rm = T),
        n = NROW(alt_help_ego[is.na(alt_help_ego) == F]),
        alt_percent_help_ego = sum / n
      ) %>%
      select(ego_id, alt_percent_help_ego) %>%
      distinct
    ,
    by = 'ego_id'
  )

######################
### EGO HELP ALTER ###
######################

# check unique values
# unique(ea$alt_helped_by_ego)

# set to numeric
ea$alt_helped_by_ego <- as.numeric(ea$alt_helped_by_ego)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_helped_by_ego == 1, na.rm = T),
        n = NROW(alt_helped_by_ego[is.na(alt_helped_by_ego) == F]),
        alt_percent_helped_by_ego = sum / n
      ) %>%
      select(ego_id, alt_percent_helped_by_ego) %>%
      distinct
    ,
    by = 'ego_id'
  )

#############################
### EASY TO FOLLOW ADVICE ###
#############################

# check unique values
# unique(ea$alt_against_advice)

# set to numeric
ea$alt_against_advice <- as.numeric(ea$alt_against_advice)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_against_advice %in% c(3, 4), na.rm = T),
        n = NROW(alt_against_advice[is.na(alt_against_advice) == F]),
        alt_percent_easy_follow_advice = sum / n
      ) %>%
      select(ego_id, alt_percent_easy_follow_advice) %>%
      distinct
    ,
    by = 'ego_id'
  )

#################################
### BORROW RUPEES OR HH ITEMS ###
#################################

# check unique values
# unique(ea$alt_borrow_rupees)

# set to numeric
ea$alt_borrow_rupees <- as.numeric(ea$alt_borrow_rupees)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_borrow_rupees == 1, na.rm = T),
        n = NROW(alt_borrow_rupees[is.na(alt_borrow_rupees) == F]),
        alt_percent_borrow_rps_hh = sum / n
      ) %>%
      select(ego_id, alt_percent_borrow_rps_hh) %>%
      distinct
    ,
    by = 'ego_id'
  )

###########################################
### INDEX OF 4 SOCIAL SUPPORT VARIABLES ###
###########################################

# add index to ea
ea$social_support_index <- (ea$alt_help_ego == 1) +
  (ea$alt_helped_by_ego == 1) +
  (ea$alt_against_advice %in% c(3, 4)) +
  (ea$alt_borrow_rupees == 1)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_mean_social_support_index = mean(social_support_index, na.rm = T)) %>%
      select(ego_id, alt_mean_social_support_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_total_social_support_index = sum(social_support_index, na.rm = T)) %>%
      select(ego_id, alt_total_social_support_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

########################################
### CALCULATE BASIC NETWORK MEASURES ###
########################################

# load ego igraph
load(
  "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/ego_igraph.rda"
)

# measures based on ego
ego_net <- gr_list_ego %>%
  map_dfr( ~ tibble(ego_deg_cen = degree(.x, v = 'ego')),
           .id = 'ego_id')

# add mean values based on alters
ego_net %<>% full_join(gr_list %>%
                         map_dfr( ~ tibble(
                           alt_mean_deg_cen = mean(degree(.x)),
                           density = edge_density(.x)
                         ),
                         .id = 'ego_id'))

###################################
### CALCULATE NON-FAMILY ALTERS ###
###################################

# check unique values
# unique(ea$alt_relationship)

# clean up relationship values
ea$alt_relationship <-
  str_remove_all(ea$alt_relationship, "[^[\\da-zA-Z\\-]]")

# create key for relationship categories
rela_key <- c(
  `Bhabi` = "Sister-in-law",
  `Bhagnee` = "Other Family",
  `Brother-in-law` = "Brother-in-law",
  `Chacherisas` = "Other Family",
  `Chachi` = "Other Family",
  `Chachisash` = "Other Family",
  `Father` = "Father",
  `Father-in-law` = "Father-in-law",
  `Husband` = "Husband",
  `Mother` = "Mother",
  `Mother-in-law` = "Mother-in-law",
  `OtherRelative` = "Other Family",
  `Sister` = "Sister",
  `Sister-in-law` = "Sister-in-law",
  `ANM` = "ANM",
  `ASHA` = "ASHA",
  `AWW` = "AWW",
  `BahnoiDoctor` = "Doctor",
  `DR` = "Doctor",
  `Dactor` = "Doctor",
  `Docter` = "Doctor",
  `Doctor` = "Doctor",
  `Dr` = "Doctor",
  `Friend` = "Friend",
  `Landlord` = "Other Non-Family",
  `Neighbor` = "Neighbour",
  `Bcm` = "Other Non-Family",
  `Bahan` = "Sister",
  `Bahu` = "Daughter-in-law",
  `Barebahu` = "Daughter-in-law",
  `Barebeta` = "Son",
  `Barebeti` = "Daughter",
  `Beta` = "Son",
  `Beti` = "Daughter",
  `Bhabhi` = "Sister-in-law",
  `Bhabi` = "Sister-in-law",
  `Bhaganee` = "Other Family",
  `Bhu` = "Daughter-in-law",
  `Aunty` = "Other Family",
  `Bohu` = "Daughter-in-law",
  `Brother` = "Brother",
  `Brother-in-law` = "Brother-in-law",
  `Buasas` = "Other Family",
  `Buaa` = "Other Family",
  `Chacha` = "Other Family",
  `Chachaji` = "Other Family",
  `Chachaji` = "Other Family",
  `Chacherisash` = "Other Family",
  `Chachi` = "Other Family",
  `Chhotebahu` = "Daughter-in-law",
  `Chhotebeti` = "Daughter",
  `Chhotibeti` = "Daughter",
  `Dadi` = "Other Family",
  `Daughter` = "Daughter",
  `HusbandWife` = "HusbandWife",
  `Jethanikalarka` = "Other Family",
  `Jija` = "Brother-in-law",
  `Mami` = "Other Family",
  `Mausisash` = "Other Family",
  `Mother` = "Mother",
  `Mother-in-law` = "Mother-in-law",
  `Father` = "Father",
  `Father-in-law` = "Father-in-law",
  `OthRelative` = "Other Family",
  `Patoh` = "Other Family",
  `Sister` = "Sister",
  `Sister-in-law` = "Sister-in-law",
  `Son` = "Son",
  `ANM` = "ANM",
  `ASHA` = "ASHA",
  `AWW` = "AWW",
  `Shgvolentear` = "Other Health Worker",
  `Trainer` = "Other Health Worker",
  `VCM` = "Other Non-Family",
  `AnmFaciletor` = "ANM",
  `Awwsahsyika` = "AWW",
  `Dr` = "Doctor",
  `Doctor` = "Doctor",
  `Dr` = "Doctor",
  `Facilater` = "Other Health Worker",
  `Facilator` = "Other Health Worker",
  `Faciletor` = "Other Health Worker",
  `Nurse` = "Other Health Worker",
  `Acquaintance` = "Other Non-Family",
  `Blockmanager` = "Other Non-Family",
  `Councilor` = "Other Non-Family",
  `Friend` = "Friend",
  `Neighbour` = "Neighbour",
  `Saheli` = "Friend"
)

# create key for major relationship categories
maj_rela_key <- c(
  `Sister-in-law` = "Family",
  `Other Family` = "Other Family",
  `Brother-in-law` = "Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Father` = "Family",
  `Father-in-law` = "Family",
  `Husband` = "Family",
  `Mother` = "Family",
  `Mother-in-law` = "Family",
  `Other Family` = "Other Family",
  `Sister` = "Family",
  `Sister-in-law` = "Family",
  `ANM` = "Health Worker",
  `ASHA` = "Health Worker",
  `AWW` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Friend` = "Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Neighbour` = "Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Sister` = "Family",
  `Daughter-in-law` = "Family",
  `Daughter-in-law` = "Family",
  `Son` = "Family",
  `Daughter` = "Family",
  `Son` = "Family",
  `Daughter` = "Family",
  `Sister-in-law` = "Family",
  `Sister-in-law` = "Family",
  `Other Family` = "Other Family",
  `Daughter-in-law` = "Family",
  `Other Family` = "Other Family",
  `Daughter-in-law` = "Family",
  `Brother` = "Family",
  `Brother-in-law` = "Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Daughter-in-law` = "Family",
  `Daughter` = "Family",
  `Daughter` = "Family",
  `Other Family` = "Other Family",
  `Daughter` = "Family",
  `HusbandWife` = "Family",
  `Husband` = "Family",
  `Wife` = "Family",
  `Other Family` = "Other Family",
  `Brother-in-law` = "Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Mother` = "Family",
  `Mother-in-law` = "Family",
  `Father` = "Family",
  `Father-in-law` = "Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Sister` = "Family",
  `Sister-in-law` = "Family",
  `Son` = "Family",
  `ANM` = "Health Worker",
  `ASHA` = "Health Worker",
  `AWW` = "Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Non-Family` = "Other Non-Family",
  `ANM` = "Health Worker",
  `AWW` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Non-Family` = "Other Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Friend` = "Non-Family",
  `Neighbour` = "Non-Family",
  `Friend` = "Non-Family"
)

# recode relationship vals into relationships
ea %<>% mutate(alt_relationship = recode(alt_relationship, !!!rela_key))

# recode relationship vals into categories
ea %<>% mutate(alt_relationship_cat = recode(alt_relationship, !!!maj_rela_key))

# create variable to say if not family members
ea %<>% mutate(
  alt_not_family = alt_relationship_cat %in% c(
    'Non-Family',
    'Health Worker',
    'Other Non-Family',
    'Other Health Worker'
  )
)

# create variable to say if health worker
ea %<>% mutate(alt_is_hw = alt_relationship_cat %in% c('Health Worker',
                                                       'Other Health Worker'))

# add data to main tibble
dat_bihar %<>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(num_alt_not_family = sum(alt_not_family == T)) %>% # calculate number of non-fam
      select(ego_id, num_alt_not_family) %>%
      distinct %>%
      arrange(ego_id)
    ,
    by = 'ego_id'
  ) %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(num_alt_is_hw = sum(alt_is_hw == T)) %>% # calculate number of non-fam
      select(ego_id, num_alt_is_hw) %>%
      distinct %>%
      arrange(ego_id)
    ,
    by = 'ego_id'
  )

######################################
### ADD ALL MEASURES TO Bihar DATA ###
######################################

# add homophily and hetero measures to dat
dat_bihar %<>% full_join(homo, by = 'ego_id') %>%
  full_join(hetero, by = 'ego_id')

# add network measures to dat
dat_bihar %<>% full_join(ego_net, by = 'ego_id')

# add tie strength measures to dat
dat_bihar %<>% full_join(ts, by = 'ego_id')

# clean environment
rm(list = setdiff(ls(), "dat_bihar"))

####################
### LOAD UP DATA ###
####################

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned and completre data
ego <-
  read_excel(path = "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/SNA study_EGO_clean_Uttar Pradesh_08112022.xlsx")
alter <-
  read_excel(path = "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/SNA study_ALTER_clean_Uttar Pradesh_08112022.xlsx")

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/PC_EGO_data_Merge_Uttar Pradesh.dta', convert.factors = F)
#
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_up_07122022.xlsx")

# load pc data for egos
ego_pc <-
  read_excel(path = "data/ego_pc_complete_up_08112022.xlsx")

# load list of duplicate ids
dup_id <- read_excel(path = 'data/duplicate_ids_up_07112022.xlsx')

# clean column names only keep part after last "."
ego_names <- sapply(
  colnames(ego),
  FUN = function(x) {
    st <- str_split(x, pattern = glob2rx("*-*"))
    st <- st[[1]][length(st[[1]])]
    return(st)
  }
)

names(ego_names) <- NULL
colnames(ego) <- ego_names

alter_names <- sapply(
  colnames(alter),
  FUN = function(x) {
    st <- str_split(x, pattern = glob2rx("*-*"))
    st <- st[[1]][length(st[[1]])]
    return(st)
  }
)

names(alter_names) <- NULL
colnames(alter) <- alter_names

rm(ego_names, alter_names)

# remove empty columns from "notes"
ego <- ego[, str_detect(colnames(ego), 'note') == F]
alter <- alter[, str_detect(colnames(alter), 'note') == F]

# convert date columns to character
inx <-
  sapply(ego, function(x)
    inherits(x, "Date") || inherits(x, "POSIXt"))
ego[inx] <- lapply(ego[inx], as.character)

inx <-
  sapply(alter, function(x)
    inherits(x, "Date") || inherits(x, "POSIXt"))
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
alter_know <-
  ego[, str_detect(colnames(ego), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to 0 since then tie won't exist. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know %<>% as.data.frame %>%
  mutate(
    know12 = alter1_know2 + alter2_know1,
    know13 = alter1_know3 + alter3_know1,
    know14 = alter1_know4 + alter4_know1,
    know15 = alter1_know5 + alter5_know1,
    know23 = alter2_know3 + alter3_know2,
    know24 = alter2_know4 + alter4_know2,
    know25 = alter2_know5 + alter5_know2,
    know34 = alter3_know4 + alter4_know3,
    know35 = alter3_know5 + alter5_know3,
    know45 = alter4_know5 + alter5_know4
  )

# now set 0 to NA since we just want to identify tie or not
alter_know %<>% select(know12:know45)
alter_know[alter_know == 0] <- NA

# add simple know columns back into ego df
ego %<>% add_column(alter_know)
rm(alter_know)

# add aalter-aalter tie columns to alter dataset so we can build edgelist below
# let's first extract the correct question so easier to look at
alter_know <-
  alter[, str_detect(colnames(alter), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to 0 since then tie won't exist. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know %<>% as.data.frame %>%
  mutate(
    know12 = alter1_know2 + alter2_know1,
    know13 = alter1_know3 + alter3_know1,
    know14 = alter1_know4 + alter4_know1,
    know15 = alter1_know5 + alter5_know1,
    know23 = alter2_know3 + alter3_know2,
    know24 = alter2_know4 + alter4_know2,
    know25 = alter2_know5 + alter5_know2,
    know34 = alter3_know4 + alter4_know3,
    know35 = alter3_know5 + alter5_know3,
    know45 = alter4_know5 + alter5_know4
  )

# now set 0 to NA since we just want to identify tie or not
alter_know %<>% select(know12:know45)
alter_know[alter_know == 0] <- NA

# add simple know columns back into alter df
alter %<>% add_column(alter_know)
rm(alter_know)

##################################################
### CREATE PC VARIABLE IF EGO HAS EVER USED FP ###
##################################################

# create single col if ego has EVER used FP from list of questions pcq303a-l
# create df of all rows of interest
ego_ever_used_fp <- ego_pc %>% select(pcq303a, pcq303b, pcq303c, pcq303d, pcq303e,
                                      pcq303f, pcq303g, pcq303h, pcq303i)

# sum if all columns are NA
ego_ever_used_fp_na <- rowSums(is.na(ego_ever_used_fp), na.rm = T)

# create na mask
ego_ever_used_fp_na[ego_ever_used_fp_na < NCOL(ego_ever_used_fp)] <- 0
ego_ever_used_fp_na[ego_ever_used_fp_na == NCOL(ego_ever_used_fp)] <- 1

# sum if ever used
ego_ever_used_fp <- rowSums(ego_ever_used_fp == 1, na.rm = T)

# set all ever used to 1 and never used to 2
ego_ever_used_fp[ego_ever_used_fp > 0] <- 1
ego_ever_used_fp[ego_ever_used_fp == 0] <- 2

# set to NA if all answers are NA
ego_ever_used_fp[ego_ever_used_fp_na == 1] <- NA

# put back into ego_pc
ego_pc$ego_ever_used_fp <- ego_ever_used_fp
rm(ego_ever_used_fp_na, ego_ever_used_fp)

###########################################
### CREATE INITIAL DATA TABLE OF VALUES ###
###########################################

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

dat_up <- tibble(
  ego_id = ego$woman_id,
  state = 'up',
  parity = ego_pc$pcq209,
  sons = ego_pc$pcq205s,
  daughters = ego_pc$pcq205d,
  neighbors_using_modern_fp = ego$contra_neighbour_num,
  preg = ego_pc$pcq230,
  using_fp = ego_pc$pcq311,
  fp_method = ego_pc$pcq312,
  age = ego_pc$pcq102,
  education = ego_pc$pcq103,
  caste = ego_pc$pcq111,
  husband_education = ego_pc$pcq115,
  want_more_children = ego_pc$pcq404
)

# fix some values
# if missing sons, daughters, parity means 0
# if missing preg, means no (2)
# husband education 98 is NA
dat_up %<>% mutate(sons = replace(sons, is.na(sons), 0)) %>%
  mutate(daughters = replace(daughters, is.na(daughters), 0)) %>%
  mutate(parity = replace(parity, is.na(parity), 0)) %>%
  mutate(preg = replace(preg, is.na(preg), 2)) %>%
  mutate(husband_education = replace(husband_education, husband_education == 98, NA)) %>%
  mutate(children = sons + daughters)

# if preg fill want more children from other question
dat_up$want_more_children[dat_up$preg %in% 1] <- ego_pc$pcq403[dat_up$preg %in% 1]

# calc diff ego and husband education
dat_up$ego_husband_edu_diff <- dat_up$education - dat_up$husband_education

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
ea <- rbind(
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter1_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter1_age,
    alt_yrs_known = ego$alter1_yrs_known,
    alt_freq_talk_fp = ego$alter1_freq_talk_fp,
    alt_talk_subjects = ego$alter1_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter1_howmany_child,
    alt_say_bad_things_fp = ego$alter1_approve_fp,
    alt_talked_fp_methods = ego$alter1_preg,
    alt_info_get_fp = ego$alter1_get_fp,
    alt_info_sideeff_fp = ego$alter1_sideeff_fp,
    alt_used_fp = ego$alter1_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter1_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter2_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter2_age,
    alt_yrs_known = ego$alter2_yrs_known,
    alt_freq_talk_fp = ego$alter2_freq_talk_fp,
    alt_talk_subjects = ego$alter2_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter2_howmany_child,
    alt_say_bad_things_fp = ego$alter2_approve_fp,
    alt_talked_fp_methods = ego$alter2_preg,
    alt_info_get_fp = ego$alter2_get_fp,
    alt_info_sideeff_fp = ego$alter2_sideeff_fp,
    alt_used_fp = ego$alter2_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter2_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter3_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter3_age,
    alt_yrs_known = ego$alter3_yrs_known,
    alt_freq_talk_fp = ego$alter3_freq_talk_fp,
    alt_talk_subjects = ego$alter3_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter3_howmany_child,
    alt_say_bad_things_fp = ego$alter3_approve_fp,
    alt_talked_fp_methods = ego$alter3_preg,
    alt_info_get_fp = ego$alter3_get_fp,
    alt_info_sideeff_fp = ego$alter3_sideeff_fp,
    alt_used_fp = ego$alter3_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter3_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter4_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter4_age,
    alt_yrs_known = ego$alter4_yrs_known,
    alt_freq_talk_fp = ego$alter4_freq_talk_fp,
    alt_talk_subjects = ego$alter4_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter4_howmany_child,
    alt_say_bad_things_fp = ego$alter4_approve_fp,
    alt_talked_fp_methods = ego$alter4_preg,
    alt_info_get_fp = ego$alter4_get_fp,
    alt_info_sideeff_fp = ego$alter4_sideeff_fp,
    alt_used_fp = ego$alter4_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter4_borrow
  ),
  tibble(
    ego_id = ego$woman_id,
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
    ego_using_fp = ego_pc$pcq312,
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
    alt_support_no_child = ego$alter5_nochild,
    ego_age = ego_pc$pcq102,
    alt_age = ego$alter5_age,
    alt_yrs_known = ego$alter5_yrs_known,
    alt_freq_talk_fp = ego$alter5_freq_talk_fp,
    alt_talk_subjects = ego$alter5_subjects,
    ego_age_married = ego_pc$pcq201b,
    alt_talked_number_children = ego$alter5_howmany_child,
    alt_say_bad_things_fp = ego$alter5_approve_fp,
    alt_talked_fp_methods = ego$alter5_preg,
    alt_info_get_fp = ego$alter5_get_fp,
    alt_info_sideeff_fp = ego$alter5_sideeff_fp,
    alt_used_fp = ego$alter5_used_fp,
    neighbors_using_fp = ego$contra_neighbour_num,
    ego_used_fp = ego_pc$ego_ever_used_fp,
    alt_borrow_rupees = ego$alter5_borrow
  )
)

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
  mutate(
    sum = sum(ego_sex == alt_sex, na.rm = T),
    n = NROW(ego_sex[is.na(ego_sex) == F &
                       is.na(alt_sex) == F]),
    homo_gender = sum / n
  ) %>% # calculate homophily
  select(ego_id, homo_gender) %>% # only keep homophily
  distinct %>% # remove duplicates
  arrange(ego_id) # arrange by ego id

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
homo_sd <- function(ego, alt) {
  out <- alt - ego
  out <- out * out
  n <- length(out[is.na(out) == F])
  out <- sum(out, na.rm = T)
  out <- out / n
  return(c(round(sqrt(out), 2), n))
}

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(homo_num_fam = homo_sd(ego_fam, alt_fam)[1]) %>% # calculate homophily
      select(ego_id, homo_num_fam) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

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
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        homo_friends = homo_sd(ego_friends, alt_friends)[1],
        homo_neigh = homo_sd(ego_neigh, alt_neigh)[1],
        homo_friends_neigh = homo_sd(ego_friends + ego_neigh, alt_friends + alt_neigh)[1]
      ) %>% # calculate homophily
      select(ego_id, homo_friends, homo_neigh, homo_friends_neigh) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

###########
### AGE ###
###########

# check unique values
# unique(ea$ego_age)
# unique(ea$alt_age)

# convert to numeric
ea$ego_age <- as.numeric(ea$ego_age)
ea$alt_age <- as.numeric(ea$alt_age)

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(homo_age = homo_sd(ego_age, alt_age)[1]) %>% # calculate homophily
      select(ego_id, homo_age) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

#####################
### PRACTICING FP ###
#####################

# PC ego values
# 1-10 are modern
# 11, 12, 96 are traditional
# 0 are missing

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# check unique values
# unique(ea$ego_using_fp)
# unique(ea$alt_using_fp)

# set ego values to modern only is 1, 0 is not using any FP, so 2
ea %<>% mutate(ego_using_fp = replace(ego_using_fp, ego_using_fp %in% 1:10, 1))
ea %<>% mutate(ego_using_fp = replace(ego_using_fp, ego_using_fp %in% c(0, 11, 12), 2))

# set values to numeric
ea$alt_using_fp <- as.numeric(ea$alt_using_fp)

# if alter is husband change to same value as ego
ea$alt_using_fp[str_detect(ea$alt_relationship, 'Husband')] <-
  ea$ego_using_fp[str_detect(ea$alt_relationship, 'Husband')]

# set don't know to NA
ea$alt_using_fp[ea$alt_using_fp == 9] <- NA

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(ego_using_fp == alt_using_fp, na.rm = T),
        n = NROW(ego_using_fp[is.na(ego_using_fp) == F &
                                is.na(alt_using_fp) == F]),
        homo_using_fp = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_using_fp) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

##############################################
### BASIC PROPORTION OF PLACE OF RESIDENCE ###
##############################################

# check unique values
# unique(ea$alt_residence)

# recode values
ea %<>% mutate(
  alt_residence = recode(
    alt_residence,
    '1' = 'Same Household',
    '2' = 'Same Village',
    '3' = 'Another Village (same district)',
    '4' = 'Outside this District'
  )
)

# build tabyl
dat <- ea %>% tabyl(alt_residence) %>%
  adorn_pct_formatting(digits = 2)

# if valid percent remove percent and change name of valid percent
if ('valid_percent' %in% colnames(dat)) {
  dat %<>% select(-percent) %>% rename(percent = valid_percent)
}

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Same Household', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_household = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_household) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Same Village', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_village = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_village) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Another Village (same district)', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_another_village = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_another_village) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

# summarise and update homophily output
homo <- homo %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(
        sum = sum(alt_residence == 'Outside this District', na.rm = T),
        n = NROW(alt_residence[is.na(alt_residence) == F]),
        homo_live_another_district = sum / n
      ) %>% # calculate homophily
      select(ego_id, homo_live_another_district) %>% # only keep homophily
      distinct %>% # remove duplicates
      arrange(ego_id) # arrange by ego id
    ,
    by = 'ego_id'
  )

#####################################
### HETEROGENEITY OF EGO'S ALTERS ###
#####################################

# create a function to calculate blaus and iqv
blau <- function(perc) {
  b <- perc * perc
  k <- length(perc)
  b <- (1 - sum(b)) %>% round(2)
  iqv <- (b / (1 - (1 / k))) %>% round(2)
  return(c(b, iqv, k))
}

#################################
### DISCUSS FP METHODS FREELY ###
#################################

# check unique values
# unique(ea$alt_discuss_fp)

# build proportions
tib <- sapply(
  unique(ea$ego_id),
  FUN = function(x) {
    t <- tabyl(ea$alt_discuss_fp[ea$ego_id == x])
    if ('valid_percent' %in% colnames(t)) {
      t %<>% select(-percent) %>% rename(percent = valid_percent)
    }
    blau(na.omit(t$percent))
  }
)

tib <- t(tib) %>% as_tibble
colnames(tib) <- c('hetero_discuss_fp_blau',
                   'hetero_discuss_fp_iqv',
                   'k')
tib %<>% add_column(ego_id = unique(ea$ego_id), .before = 1)

# create hetero output (since first var)
hetero <- tib %>% select(-k)

####################################################
### TIE STRENGTH/SOCIAL PATHWAYS OF EGO'S ALTERS ###
####################################################

########################
### MEAN YEARS KNOWN ###
########################

# check unique values
# unique(ea$alt_yrs_known)
# unique(ea$ego_age_married)

# set to numeric
ea$alt_yrs_known <- as.numeric(ea$alt_yrs_known)

# reset values of 99 based on ego age and age of marriage
ea$alt_yrs_known[ea$alt_yrs_known == 99] <- ea$ego_age[ea$alt_yrs_known == 99] - ea$ego_age_married[ea$alt_yrs_known == 99]

ts <- ea %>%
  group_by(ego_id) %>% # group by ego id
  mutate(alt_mean_years_known = mean(alt_yrs_known), na.rm = T) %>% 
  select(ego_id, alt_mean_years_known) %>%
  distinct # remove duplicates

##########################
### TALK FREQ ABOUT FP ###
##########################

# check unique values
# unique(ea$alt_freq_talk_fp)

# set to numeric
ea$alt_freq_talk_fp <- as.numeric(ea$alt_freq_talk_fp)

# lets calc percent of alters talk fp at least once a week, and at least once a month

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_freq_talk_fp == 1, na.rm = T),
        n = NROW(alt_freq_talk_fp[is.na(alt_freq_talk_fp) == F]),
        alt_percent_talk_fp_weekly = sum / n
      ) %>%
      select(ego_id, alt_percent_talk_fp_weekly) %>%
      distinct
    ,
    by = 'ego_id'
  ) %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_freq_talk_fp %in% c(1, 2), na.rm = T),
        n = NROW(alt_freq_talk_fp[is.na(alt_freq_talk_fp) == F]),
        alt_percent_talk_fp_monthly = sum / n
      ) %>%
      select(ego_id, alt_percent_talk_fp_monthly) %>%
      distinct
    ,
    by = 'ego_id'
  )


#########################
### MEAN MULTIPLEXITY ###
#########################

# average of number of things discussed

# check unique values
unique(ea$alt_talk_subjects)

# count number of subjects
num_sub <- str_split(ea$alt_talk_subjects, ',')
num_sub <- sapply(num_sub, function(x) length(x))

# add to tibble
ea$alt_number_talk_subjects <- num_sub

# summarize and join

ts <- ts %>%
  full_join(ea %>%
              group_by(ego_id) %>% # group by ego id
              mutate(alt_mean_talk_subjects = mean(alt_number_talk_subjects), na.rm = T) %>% 
              select(ego_id, alt_mean_talk_subjects) %>%
              distinct
            , by = 'ego_id')

#########################################
### TALKED ABOUT NUM CHILDREN TO HAVE ###
#########################################

# proportion of ego's alters

# check unique values
# unique(ea$alt_talked_number_children)

# set to numeric
ea$alt_talked_number_children <- as.numeric(ea$alt_talked_number_children)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_talked_number_children == 1, na.rm = T),
        n = NROW(alt_talked_number_children[is.na(alt_talked_number_children) == F]),
        alt_percent_talked_number_children = sum / n
      ) %>%
      select(ego_id, alt_percent_talked_number_children) %>%
      distinct
    ,
    by = 'ego_id'
  )

#########################
### DISCUSS FP FREELY ###
#########################

# check unique values
# unique(ea$alt_discuss_fp)

# set to numeric
ea$alt_discuss_fp <- as.numeric(ea$alt_discuss_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_discuss_fp == 1, na.rm = T),
        n = NROW(alt_discuss_fp[is.na(alt_discuss_fp) == F]),
        alt_percent_discuss_fp_freely = sum / n
      ) %>%
      select(ego_id, alt_percent_discuss_fp_freely) %>%
      distinct
    ,
    by = 'ego_id'
  )

############################
### ENCOURAGE TO USE FP ###
############################

# check unique values
# unique(ea$alt_encourage_fp)

# set to numeric
ea$alt_encourage_fp <- as.numeric(ea$alt_encourage_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_encourage_fp == 1, na.rm = T),
        n = NROW(alt_encourage_fp[is.na(alt_encourage_fp) == F]),
        alt_percent_encourage_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_encourage_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

###################################
### SUPPORT NOT HAVING CHILDREN ###
###################################

# check unique values
# unique(ea$alt_support_no_child)

# set to numeric
ea$alt_support_no_child <- as.numeric(ea$alt_support_no_child)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_support_no_child == 1, na.rm = T),
        n = NROW(alt_support_no_child[is.na(alt_support_no_child) == F]),
        alt_percent_support_no_child = sum / n
      ) %>%
      select(ego_id, alt_percent_support_no_child) %>%
      distinct
    ,
    by = 'ego_id'
  )

###############################################
### WOULD NOT SAY BAD THINGS ABOUT USING FP ###
###############################################

# check unique values
# unique(ea$alt_say_bad_things_fp)

# set to numeric
ea$alt_say_bad_things_fp <- as.numeric(ea$alt_say_bad_things_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_say_bad_things_fp == 2, na.rm = T),
        n = NROW(alt_say_bad_things_fp[is.na(alt_say_bad_things_fp) == F]),
        alt_percent_not_say_bad_things_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_not_say_bad_things_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#############################################
### INDEX OF 5 SOCIAL INFLUENCE VARIABLES ###
#############################################

# add index to ea
ea$social_influence_index <- (ea$alt_talked_number_children == 1) +
  (ea$alt_discuss_fp == 1) +
  (ea$alt_encourage_fp == 1) +
  (ea$alt_support_no_child == 1) +
  (ea$alt_say_bad_things_fp == 2)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_mean_social_influence_index = mean(social_influence_index, na.rm = T)) %>%
      select(ego_id, alt_mean_social_influence_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_total_social_influence_index = sum(social_influence_index, na.rm = T)) %>%
      select(ego_id, alt_total_social_influence_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

###############################
### TALKED ABOUT FP METHODS ###
###############################

# check unique values
# unique(ea$alt_talked_fp_methods)

# set to numeric
ea$alt_talked_fp_methods <- as.numeric(ea$alt_talked_fp_methods)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_talked_fp_methods == 1, na.rm = T),
        n = NROW(alt_talked_fp_methods[is.na(alt_talked_fp_methods) == F]),
        alt_percent_talked_fp_methods = sum / n
      ) %>%
      select(ego_id, alt_percent_talked_fp_methods) %>%
      distinct
    ,
    by = 'ego_id'
  )

#####################################
### RECEIVED INFO WHERE TO GET FP ###
#####################################

# check unique values
# unique(ea$alt_info_get_fp)

# set to numeric
ea$alt_info_get_fp <- as.numeric(ea$alt_info_get_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_info_get_fp == 1, na.rm = T),
        n = NROW(alt_info_get_fp[is.na(alt_info_get_fp) == F]),
        alt_percent_info_get_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_info_get_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#####################################
### RECEIVED INFO SIDE EFFECTS FP ###
#####################################

# check unique values
# unique(ea$alt_info_sideeff_fp)

# set to numeric
ea$alt_info_sideeff_fp <- as.numeric(ea$alt_info_sideeff_fp)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_info_sideeff_fp == 1, na.rm = T),
        n = NROW(alt_info_sideeff_fp[is.na(alt_info_sideeff_fp) == F]),
        alt_percent_info_sideeff_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_info_sideeff_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#######################################
### ALTER CURRENTLY USING MODERN FP ###
#######################################

# check unique values
# unique(ea$alt_using_fp)

# set NA values back to 9
# some of these values will be 2 (or missing since not using)
# but it doesn't matter since we are looking at the proportion of 1 vs any other answer
ea$alt_using_fp[is.na(ea$alt_using_fp)] <- 9

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_using_fp == 1, na.rm = T),
        n = NROW(alt_using_fp[is.na(alt_using_fp) == F]),
        alt_percent_using_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_using_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

#################################
### ALTER EVER USED MODERN FP ###
#################################

# check unique values
# unique(ea$alt_used_fp)

# set to numeric
ea$alt_used_fp <- as.numeric(ea$alt_used_fp)

# if using fp is 1 set used fp to 1
ea$alt_used_fp[ea$alt_using_fp == 1] <- 1

# do not set NA values to 2 (have not ever used)
# because we don't know if that person has ever used
# with CURRENT we assume the skipped people are not 
# CURRENTLY USING

# if alter is husband change to same value as ego
ea$alt_used_fp[str_detect(ea$alt_relationship, 'Husband')] <-
  ea$ego_used_fp[str_detect(ea$alt_relationship, 'Husband')]

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_used_fp == 1, na.rm = T),
        n = NROW(alt_used_fp[is.na(alt_used_fp) == F]),
        alt_percent_used_fp = sum / n
      ) %>%
      select(ego_id, alt_percent_used_fp) %>%
      distinct
    ,
    by = 'ego_id'
  )

######################
### ALTER HELP EGO ###
######################

# check unique values
# unique(ea$alt_help_ego)

# set to numeric
ea$alt_help_ego <- as.numeric(ea$alt_help_ego)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_help_ego == 1, na.rm = T),
        n = NROW(alt_help_ego[is.na(alt_help_ego) == F]),
        alt_percent_help_ego = sum / n
      ) %>%
      select(ego_id, alt_percent_help_ego) %>%
      distinct
    ,
    by = 'ego_id'
  )

######################
### EGO HELP ALTER ###
######################

# check unique values
# unique(ea$alt_helped_by_ego)

# set to numeric
ea$alt_helped_by_ego <- as.numeric(ea$alt_helped_by_ego)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_helped_by_ego == 1, na.rm = T),
        n = NROW(alt_helped_by_ego[is.na(alt_helped_by_ego) == F]),
        alt_percent_helped_by_ego = sum / n
      ) %>%
      select(ego_id, alt_percent_helped_by_ego) %>%
      distinct
    ,
    by = 'ego_id'
  )

#############################
### EASY TO FOLLOW ADVICE ###
#############################

# check unique values
# unique(ea$alt_against_advice)

# set to numeric
ea$alt_against_advice <- as.numeric(ea$alt_against_advice)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_against_advice %in% c(3, 4), na.rm = T),
        n = NROW(alt_against_advice[is.na(alt_against_advice) == F]),
        alt_percent_easy_follow_advice = sum / n
      ) %>%
      select(ego_id, alt_percent_easy_follow_advice) %>%
      distinct
    ,
    by = 'ego_id'
  )

#################################
### BORROW RUPEES OR HH ITEMS ###
#################################

# check unique values
# unique(ea$alt_borrow_rupees)

# set to numeric
ea$alt_borrow_rupees <- as.numeric(ea$alt_borrow_rupees)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(
        sum = sum(alt_borrow_rupees == 1, na.rm = T),
        n = NROW(alt_borrow_rupees[is.na(alt_borrow_rupees) == F]),
        alt_percent_borrow_rps_hh = sum / n
      ) %>%
      select(ego_id, alt_percent_borrow_rps_hh) %>%
      distinct
    ,
    by = 'ego_id'
  )

###########################################
### INDEX OF 4 SOCIAL SUPPORT VARIABLES ###
###########################################

# add index to ea
ea$social_support_index <- (ea$alt_help_ego == 1) +
  (ea$alt_helped_by_ego == 1) +
  (ea$alt_against_advice %in% c(3, 4)) +
  (ea$alt_borrow_rupees == 1)

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_mean_social_support_index = mean(social_support_index, na.rm = T)) %>%
      select(ego_id, alt_mean_social_support_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

# summarize and join
ts <- ts %>%
  full_join(
    ea %>%
      group_by(ego_id) %>%
      mutate(alt_total_social_support_index = sum(social_support_index, na.rm = T)) %>%
      select(ego_id, alt_total_social_support_index) %>%
      distinct
    ,
    by = 'ego_id'
  )

########################################
### CALCULATE BASIC NETWORK MEASURES ###
########################################

# load ego igraph
load(
  "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/ego_igraph_up.rda"
)

# measures based on ego
ego_net <- gr_list_ego %>%
  map_dfr( ~ tibble(ego_deg_cen = degree(.x, v = 'ego')),
           .id = 'ego_id')

# add mean values based on alters
ego_net %<>% full_join(gr_list %>%
                         map_dfr( ~ tibble(
                           alt_mean_deg_cen = mean(degree(.x)),
                           density = edge_density(.x)
                         ),
                         .id = 'ego_id'))

###################################
### CALCULATE NON-FAMILY ALTERS ###
###################################

# check unique values
# unique(ea$alt_relationship)

# clean up relationship values
ea$alt_relationship <-
  str_remove_all(ea$alt_relationship, "[^[\\da-zA-Z\\-]]")

# create key for relationship categories
# create key for relationship categories
rela_key <- c(
  `OtherRelative` = "Other Family",
  `Sister` = "Sister",
  `Sister-in-law` = "Sister-in-law",
  `Brother` = "Brother",
  `Brother-in-law` = "Brother-in-law",
  `Chachi` = "Other Family",
  `Chachisas` = "Other Family",
  `Chachiyasas` = "Other Family",
  `Dadisas` = "Other Family",
  `Father` = "Father",
  `Father-in-law` = "Father-in-law",
  `Husband` = "Husband",
  `Mother` = "Mother",
  `Mother-in-law` = "Mother-in-law",
  `ANM` = "ANM",
  `ASHA` = "ASHA",
  `AWW` = "AWW",
  `Doctor` = "Doctor",
  `Dr` = "Doctor",
  `Otherchemist` = "Other Health Worker",
  `Acquaintance` = "Other Non-Family",
  `Friend` = "Friend",
  `Neighbor` = "Neighbour",
  `Brother` = "Brother",
  `Brother-in-law` = "Brother-in-law",
  `Father-in-law` = "Father-in-law",
  `Friend` = "Friend",
  `HusbandWife` = "HusbandWife",
  `Mother` = "Mother",
  `Mother-in-law` = "Mother-in-law",
  `Oth Relative` = "Other Family ",
  `Sister` = "Sister",
  `Sister-in-law` = "Sister-in-law",
  `Bhatijee` = "Other Family ",
  `Chachi` = "Other Family ",
  `Chachisas` = "Other Family ",
  `Chachiyasas` = "Other Family ",
  `ASHA` = "ASHA",
  `AWW` = "AWW",
  `Chemist` = "Other Health Worker",
  `Doctor` = "Doctor",
  `Neighbor` = "Neighbour",
  `Neighbour` = "Neighbour",
  `Maternalcousin` = "Other Family",
  `Mausi` = "Other Family",
  `Mausiasaash` = "Mother-in-law",
  `Mother` = "Mother",
  `Mother-in-law` = "Mother-in-law",
  `Nandoi` = "Brother-in-law",
  `Father` = "Father",
  `HusbandWife` = "HusbandWife",
  `Jija` = "Brother-in-law",
  `Jijaji` = "Brother-in-law",
  `Oth Relative` = "Other Family",
  `LHV` = "Other Health Worker",
  `Mamakaladka` = "Other Family",
  `Saale` = "Brother-in-law",
  `Sangani` = "ASHA",
  `Sangini` = "ASHA",
  `Sarita` = "Friend",
  `Sister` = "Sister",
  `Sister-in-law` = "Sister-in-law",
  `Badimaa` = "Other Family",
  `Bahu` = "Daughter-in-law",
  `Bati` = "Daughter",
  `Beta` = "Son",
  `Bete` = "Son",
  `Betha` = "Son",
  `Bethi` = "Daughter",
  `Beti` = "Daughter",
  `Bhabhi` = "Sister-in-law",
  `Bhanji` = "Other Family",
  `Bhau` = "Daughter-in-law",
  `Bhu` = "Daughter-in-law",
  `Bhuakeladke` = "Other Family",
  `Brother-in-law` = "Brother-in-law",
  `Brother` = "Brother",
  `Brother-in-law` = "Brother-in-law",
  `CHO` = "Other Health Worker",
  `Chachaji` = "Other Family",
  `Chachi` = "Other Family",
  `Chachijee` = "Other Family",
  `Chachisas` = "Other Family",
  `Dada` = "Brother",
  `Dadisas` = "Other Family",
  `Damand` = "Son-in-law",
  `Daughter` = "Daughter",
  `AshaGaneshpurki` = "ASHA",
  `Ashasangini` = "ASHA",
  `ANM` = "ANM",
  `ASHA` = "ASHA",
  `AWW` = "AWW",
  `Doctor` = "Doctor",
  `DoctorCMO` = "Doctor",
  `Doctor-Patient` = "Doctor",
  `Dr` = "Doctor",
  `DrDevchandra` = "Doctor",
  `Drkhalid` = "Doctor",
  `Staffnurse` = "Doctor",
  `Friend` = "Friend",
  `Master` = "Other Non-Family",
  `Neighbour` = "Neighbour",
  `Pradhan` = "Other Non-Family",
  `Supervisor` = "Other Non-Family",
  `YejahakaamkartahaidukanMalik` = "Other Non-Family",
  `Acquaintance` = "Other Non-Family",
  `OthRelative` = "Other Family",
  `Brotherinlaw` = "Brother-in-law"
)

# create key for major relationship categories
maj_rela_key <- c(
  `Other Family` = "Other Family",
  `Sister` = "Family",
  `Sister-in-law` = "Family",
  `Brother` = "Family",
  `Brother-in-law` = "Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Father` = "Family",
  `Father-in-law` = "Family",
  `Husband` = "Family",
  `Mother` = "Family",
  `Mother-in-law` = "Family",
  `ANM` = "Health Worker",
  `ASHA` = "Health Worker",
  `AWW` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Other Non-Family` = "Other Non-Family",
  `Friend` = "Non-Family",
  `Neighbour` = "Non-Family",
  `Brother` = "Family",
  `Brother-in-law` = "Family",
  `Father-in-law` = "Family",
  `Friend` = "Non-Family",
  `HusbandWife` = "Family",
  `Mother` = "Family",
  `Mother-in-law` = "Family",
  `Other Family ` = "Other Family ",
  `Sister` = "Family",
  `Sister-in-law` = "Family",
  `Other Family ` = "Other Family ",
  `Other Family ` = "Other Family ",
  `Other Family ` = "Other Family ",
  `Other Family ` = "Other Family ",
  `ASHA` = "Health Worker",
  `AWW` = "Health Worker",
  `Other Health Worker` = "Other Health Worker",
  `Doctor` = "Health Worker",
  `Neighbour` = "Non-Family",
  `Neighbour` = "Non-Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Mother-in-law` = "Family",
  `Mother` = "Family",
  `Mother-in-law` = "Family",
  `Brother-in-law` = "Family",
  `Father` = "Family",
  `HusbandWife` = "Family",
  `Husband` = "Family",
  `Wife` = "Family",
  `Brother-in-law` = "Family",
  `Brother-in-law` = "Family",
  `Other Family` = "Other Family",
  `Other Health Worker` = "Other Health Worker",
  `Other Family` = "Other Family",
  `Brother-in-law` = "Family",
  `ASHA` = "Health Worker",
  `ASHA` = "Health Worker",
  `Friend` = "Non-Family",
  `Sister` = "Family",
  `Sister-in-law` = "Family",
  `Other Family` = "Other Family",
  `Daughter-in-law` = "Family",
  `Daughter` = "Family",
  `Son` = "Family",
  `Son` = "Family",
  `Son` = "Family",
  `Daughter` = "Family",
  `Daughter` = "Family",
  `Sister-in-law` = "Family",
  `Other Family` = "Other Family",
  `Daughter-in-law` = "Family",
  `Daughter-in-law` = "Family",
  `Other Family` = "Other Family",
  `Brother-in-law` = "Family",
  `Brother` = "Family",
  `Brother-in-law` = "Family",
  `Other Health Worker` = "Other Health Worker",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Other Family` = "Other Family",
  `Brother` = "Family",
  `Other Family` = "Other Family",
  `Son-in-law` = "Family",
  `Daughter` = "Family",
  `ASHA` = "Health Worker",
  `ASHA` = "Health Worker",
  `ANM` = "Health Worker",
  `ASHA` = "Health Worker",
  `AWW` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Doctor` = "Health Worker",
  `Friend` = "Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Neighbour` = "Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Other Non-Family` = "Other Non-Family",
  `Other Non-Family` = "Other Non-Family"
)

# recode relationship vals into relationships
ea %<>% mutate(alt_relationship = recode(alt_relationship, !!!rela_key))

# recode relationship vals into categories
ea %<>% mutate(alt_relationship_cat = recode(alt_relationship, !!!maj_rela_key))

# create variable to say if not family members
ea %<>% mutate(
  alt_not_family = alt_relationship_cat %in% c(
    'Non-Family',
    'Health Worker',
    'Other Non-Family',
    'Other Health Worker'
  )
)

# create variable to say if health worker
ea %<>% mutate(alt_is_hw = alt_relationship_cat %in% c('Health Worker',
                                                       'Other Health Worker'))

# add data to main tibble
dat_up %<>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(num_alt_not_family = sum(alt_not_family == T)) %>% # calculate number of non-fam
      select(ego_id, num_alt_not_family) %>%
      distinct %>%
      arrange(ego_id)
    ,
    by = 'ego_id'
  ) %>%
  full_join(
    ea %>%
      group_by(ego_id) %>% # group by ego id
      mutate(num_alt_is_hw = sum(alt_is_hw == T)) %>% # calculate number of non-fam
      select(ego_id, num_alt_is_hw) %>%
      distinct %>%
      arrange(ego_id)
    ,
    by = 'ego_id'
  )

###################################
### ADD ALL MEASURES TO UP DATA ###
###################################

# add homophily and hetero measures to dat
dat_up %<>% full_join(homo, by = 'ego_id') %>%
  full_join(hetero, by = 'ego_id')

# add network measures to dat
dat_up %<>% full_join(ego_net, by = 'ego_id')

# add tie strength measures to dat
dat_up %<>% full_join(ts, by = 'ego_id')

###############################
### MERGE BIHAR AND UP DATA ###
###############################

# merge dataframes
dat <- rbind(dat_bihar, dat_up)

# clean environment
rm(list = setdiff(ls(), c("dat")))

########################
### CLEAN JOINT DATA ###
########################

# change NaN to NA
dat[is.na(dat)] <- NA

# caste values of 4 should be 0
dat %<>% mutate(caste = replace(caste, caste == 4, 0))

# husband education 98 is missing
dat %<>% mutate(husband_education = replace(husband_education, husband_education == 98, NA))

# set state as factor
dat %<>% mutate(state = factor(state, levels = c('bihar', 'up')))
