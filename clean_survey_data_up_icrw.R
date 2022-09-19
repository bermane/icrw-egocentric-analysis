# this code loads csv data of ego and alter interviews and builds network objects to
# be used for future analyses

# round 2 UP data!

# load packages
library(magrittr)
library(egor)
library(janitor)
library(tidyverse)
library(readxl)
library(igraph)
library(readstata13)
library(writexl)

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# set wd
setwd('/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis')

# load ego and alter cleaned and completre data
ego <- read_excel(path = "data/SNA study_EGO_clean_Uttar Pradesh_08112022.xlsx")
alter <-read_excel(path = "data/SNA study_ALTER_clean_Uttar Pradesh_08112022.xlsx")

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/PC_EGO_data_Merge_Uttar Pradesh.dta', convert.factors = F)
# 
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_up_07122022.xlsx")

# load pc data for egos
ego_pc <- read_excel(path = 'data/ego_pc_complete_up_08112022.xlsx')

# load list of duplicate ids
dup_id <- read_excel(path = 'data/duplicate_ids_up_09142022.xlsx')

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

#########################################################################################
### ORGANIZE EGO-ALTER NETWORK DATA (EGO SURVEY ONLY) AND IMPORT INTO EGOR AND IGRAPH ###
#########################################################################################

# create tibble of ego attributes
ego_df <- as_tibble(ego) %>% 
  select(district_name:contra_neighbour_type_other) %>% 
  add_column(ego_id = ego$woman_id, .before = 1)

# sort ego data and PC data by woman ID so can merge
ego_df %<>% arrange(ego_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego_df$ego_id)

# create single col if ego has EVER used FP from list of questions pcq303a-l
# create df of all rows of interest
ego_ever_used_fp <- ego_pc %>% select(pcq303a, pcq303b, pcq303c, pcq303d, pcq303e,
                                      pcq303f, pcq303g, pcq303h, pcq303i, pcq303j,
                                      pcq303k, pcq303l)

# sum if all columns are NA
ego_ever_used_fp_na <- rowSums(is.na(ego_ever_used_fp), na.rm = T)

# create na mask
ego_ever_used_fp_na[ego_ever_used_fp_na < NCOL(ego_ever_used_fp)] <- 0
ego_ever_used_fp_na[ego_ever_used_fp_na == NCOL(ego_ever_used_fp)] <- 1

# sum if ever used
ego_ever_used_fp <- rowSums(ego_ever_used_fp==1, na.rm = T)

# set all ever used to 1 and never used to 2
ego_ever_used_fp[ego_ever_used_fp > 0] <- 1
ego_ever_used_fp[ego_ever_used_fp == 0] <- 2

# set to NA if all answers are NA
ego_ever_used_fp[ego_ever_used_fp_na == 1] <- NA

# add back to PC data
ego_pc$ego_ever_used_fp <- ego_ever_used_fp

# bind PC columns of interest
ego_df %<>% bind_cols(ego_pc %>% select(pcq102, 
                                        pcq103, 
                                        pcq111,
                                        pcq311,
                                        pcq201b,
                                        pcq209,
                                        pcq118,
                                        pcq106,
                                        pcq107,
                                        ego_ever_used_fp) %>%
                        rename(ego_age = pcq102, 
                               ego_edu = pcq103, 
                               ego_caste = pcq111,
                               ego_using_fp = pcq311,
                               ego_age_married = pcq201b,
                               ego_parity = pcq209,
                               ego_hus_mig = pcq118,
                               ego_ever_worked = pcq106,
                               ego_worked_lastyr = pcq107))

# convert all character variables to factor
ego_df %<>% 
  mutate(across(where(is.character), as.factor))

# create empty tibble of alter attributes
alter_attr <- tibble(alter_id = character(),
                     ego_id = character(),
                     alter_num = numeric(),
                     relationship = character(),
                     sex = numeric(),
                     age = numeric(),
                     yrs_known = numeric(),
                     talk_freq = numeric(),
                     preg = numeric(),
                     using_fp = numeric(),
                     subjects = character(),
                     subjects_other = character(),
                     talk_freq_fp = numeric(),
                     know_asha = numeric(),
                     know_anm = numeric(),
                     know_aww = numeric(),
                     know_doc = numeric(),
                     know_pha = numeric(),
                     know_shg = numeric(),
                     know_rel = numeric(),
                     know_pra = numeric())

# create empty tibble of alter-alter ties
alter_ties <- tibble(from = character(),
                     to = character(),
                     ego_id = character(),
                     weight = numeric())

# loop through ego IDs and add rows based on alters
for(i in 1:NROW(ego)){
  
  # go through alters and add alter attr
  if(is.na(ego$alter1[i]) == F){
    
    # pre-set age and using_fp from alter info if alter was interviewed
    if(str_c(ego$woman_id[i], 1) %in% alter$alter_id){
      age_hold <- alter$age[alter$alter_id == str_c(ego$woman_id[i], 1)]
      using_fp_hold <- alter$current_method[alter$alter_id == str_c(ego$woman_id[i], 1)]
    } else{
      age_hold <- ego$alter1_age[i]
      using_fp_hold <- ego$alter1_using_fp[i]
    }
    
    # fill alter attributes from ego dataset
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '1'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 1,
                            relationship = ego$alter1r[i],
                            sex = ego$alter1_sex[i],
                            age = age_hold,
                            yrs_known = ego$alter1_yrs_known[i],
                            talk_freq = ego$alter1_talk_freq[i],
                            preg = ego$alter1_preg[i],
                            using_fp = using_fp_hold,
                            subjects = ego$alter1_subjects[i],
                            subjects_other = ego$alter1_subjects_other[i],
                            talk_freq_fp = ego$alter1_freq_talk_fp[i],
                            know_asha = ego$alter1_asha[i],
                            know_anm = ego$alter1_anm[i],
                            know_aww = ego$alter1_aww[i],
                            know_doc = ego$alter1_doctor[i],
                            know_pha = ego$alter1_pharmacist[i],
                            know_shg = ego$alter1_shg[i],
                            know_rel = ego$alter1_religious_leader[i],
                            know_pra = ego$alter1_pradhan[i])
  } 
  if(is.na(ego$alter2[i]) == F){
    
    # pre-set age and using_fp from alter info if alter was interviewed
    if(str_c(ego$woman_id[i], 2) %in% alter$alter_id){
      age_hold <- alter$age[alter$alter_id == str_c(ego$woman_id[i], 2)]
      using_fp_hold <- alter$current_method[alter$alter_id == str_c(ego$woman_id[i], 2)]
    } else{
      age_hold <- ego$alter2_age[i]
      using_fp_hold <- ego$alter2_using_fp[i]
    }
    
    # fill alter attributes from ego dataset
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '2'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 2,
                            relationship = ego$alter2r[i],
                            sex = ego$alter2_sex[i],
                            age = age_hold,
                            yrs_known = ego$alter2_yrs_known[i],
                            talk_freq = ego$alter2_talk_freq[i],
                            preg = ego$alter2_preg[i],
                            using_fp = using_fp_hold,
                            subjects = ego$alter2_subjects[i],
                            subjects_other = ego$alter2_subjects_other[i],
                            talk_freq_fp = ego$alter2_freq_talk_fp[i],
                            know_asha = ego$alter2_asha[i],
                            know_anm = ego$alter2_anm[i],
                            know_aww = ego$alter2_aww[i],
                            know_doc = ego$alter2_doctor[i],
                            know_pha = ego$alter2_pharmacist[i],
                            know_shg = ego$alter2_shg[i],
                            know_rel = ego$alter2_religious_leader[i],
                            know_pra = ego$alter2_pradhan[i])
  } 
  if(is.na(ego$alter3[i]) == F){
    
    # pre-set age and using_fp from alter info if alter was interviewed
    if(str_c(ego$woman_id[i], 3) %in% alter$alter_id){
      age_hold <- alter$age[alter$alter_id == str_c(ego$woman_id[i], 3)]
      using_fp_hold <- alter$current_method[alter$alter_id == str_c(ego$woman_id[i], 3)]
    } else{
      age_hold <- ego$alter3_age[i]
      using_fp_hold <- ego$alter3_using_fp[i]
    }
    
    # fill alter attributes from ego dataset
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '3'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 3,
                            relationship = ego$alter3r[i],
                            sex = ego$alter3_sex[i],
                            age = age_hold,
                            yrs_known = ego$alter3_yrs_known[i],
                            talk_freq = ego$alter3_talk_freq[i],
                            preg = ego$alter3_preg[i],
                            using_fp = using_fp_hold,
                            subjects = ego$alter3_subjects[i],
                            subjects_other = ego$alter3_subjects_other[i],
                            talk_freq_fp = ego$alter3_freq_talk_fp[i],
                            know_asha = ego$alter3_asha[i],
                            know_anm = ego$alter3_anm[i],
                            know_aww = ego$alter3_aww[i],
                            know_doc = ego$alter3_doctor[i],
                            know_pha = ego$alter3_pharmacist[i],
                            know_shg = ego$alter3_shg[i],
                            know_rel = ego$alter3_religious_leader[i],
                            know_pra = ego$alter3_pradhan[i])
  } 
  if(is.na(ego$alter4[i]) == F){
    
    # pre-set age and using_fp from alter info if alter was interviewed
    if(str_c(ego$woman_id[i], 4) %in% alter$alter_id){
      age_hold <- alter$age[alter$alter_id == str_c(ego$woman_id[i], 4)]
      using_fp_hold <- alter$current_method[alter$alter_id == str_c(ego$woman_id[i], 4)]
    } else{
      age_hold <- ego$alter4_age[i]
      using_fp_hold <- ego$alter4_using_fp[i]
    }
    
    # fill alter attributes from ego dataset
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '4'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 4,
                            relationship = ego$alter4r[i],
                            sex = ego$alter4_sex[i],
                            age = age_hold,
                            yrs_known = ego$alter4_yrs_known[i],
                            talk_freq = ego$alter4_talk_freq[i],
                            preg = ego$alter4_preg[i],
                            using_fp = using_fp_hold,
                            subjects = ego$alter4_subjects[i],
                            subjects_other = ego$alter4_subjects_other[i],
                            talk_freq_fp = ego$alter4_freq_talk_fp[i],
                            know_asha = ego$alter4_asha[i],
                            know_anm = ego$alter4_anm[i],
                            know_aww = ego$alter4_aww[i],
                            know_doc = ego$alter4_doctor[i],
                            know_pha = ego$alter4_pharmacist[i],
                            know_shg = ego$alter4_shg[i],
                            know_rel = ego$alter4_religious_leader[i],
                            know_pra = ego$alter4_pradhan[i])
  } 
  if(is.na(ego$alter5[i]) == F){
    
    # pre-set age and using_fp from alter info if alter was interviewed
    if(str_c(ego$woman_id[i], 5) %in% alter$alter_id){
      age_hold <- alter$age[alter$alter_id == str_c(ego$woman_id[i], 5)]
      using_fp_hold <- alter$current_method[alter$alter_id == str_c(ego$woman_id[i], 5)]
    } else{
      age_hold <- ego$alter5_age[i]
      using_fp_hold <- ego$alter5_using_fp[i]
    }
    
    # fill alter attributes from ego dataset
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '5'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 5,
                            relationship = ego$alter5r[i],
                            sex = ego$alter5_sex[i],
                            age = age_hold,
                            yrs_known = ego$alter5_yrs_known[i],
                            talk_freq = ego$alter5_talk_freq[i],
                            preg = ego$alter5_preg[i],
                            using_fp = using_fp_hold,
                            subjects = ego$alter5_subjects[i],
                            subjects_other = ego$alter5_subjects_other[i],
                            talk_freq_fp = ego$alter5_freq_talk_fp[i],
                            know_asha = ego$alter5_asha[i],
                            know_anm = ego$alter5_anm[i],
                            know_aww = ego$alter5_aww[i],
                            know_doc = ego$alter5_doctor[i],
                            know_pha = ego$alter5_pharmacist[i],
                            know_shg = ego$alter5_shg[i],
                            know_rel = ego$alter5_religious_leader[i],
                            know_pra = ego$alter5_pradhan[i])
  } 
  
  # go through alter-alter ties and add to edgelist
  if(is.na(ego$know12[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '1'),
                            to = str_c(ego$woman_id[i], '2'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know12[i])
  }
  if(is.na(ego$know13[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '1'),
                            to = str_c(ego$woman_id[i], '3'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know13[i])
  }
  if(is.na(ego$know14[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '1'),
                            to = str_c(ego$woman_id[i], '4'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know14[i])
  }
  if(is.na(ego$know15[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '1'),
                            to = str_c(ego$woman_id[i], '5'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know15[i])
  }
  if(is.na(ego$know23[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '2'),
                            to = str_c(ego$woman_id[i], '3'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know23[i])
  }
  if(is.na(ego$know24[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '2'),
                            to = str_c(ego$woman_id[i], '4'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know24[i])
  }
  if(is.na(ego$know25[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '2'),
                            to = str_c(ego$woman_id[i], '5'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know25[i])
  }
  if(is.na(ego$know34[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '3'),
                            to = str_c(ego$woman_id[i], '4'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know34[i])
  }
  if(is.na(ego$know35[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '3'),
                            to = str_c(ego$woman_id[i], '5'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know35[i])
  }
  if(is.na(ego$know45[i]) == F) {
    alter_ties %<>% add_row(from = str_c(ego$woman_id[i], '4'),
                            to = str_c(ego$woman_id[i], '5'),
                            ego_id = ego$woman_id[i],
                            weight = ego$know45[i])
  }
}

# convert character vars to numeric
alter_attr %<>% mutate(across(c(sex, age, talk_freq, preg, using_fp, know_asha,
                                know_anm, know_aww, know_doc, know_pha, know_shg, know_rel), as.numeric))

# check freq table of alter tie weights
alter_ties %>% tabyl(weight) 

# since 97% of alter ties are not directional just make them all that way
alter_ties %<>% mutate(weight = replace(weight, weight == 2, 1))

# convert all character variables to factor
alter_attr %<>% 
  mutate(across(where(is.character), as.factor))
alter_ties %<>% 
  mutate(across(where(is.character), as.factor))

# create an egor object
egor_obj <- threefiles_to_egor(egos = ego_df,
                               alters.df= alter_attr, 
                               edges= alter_ties, 
                               ID.vars = list(ego = "ego_id", 
                                              alter = "alter_id", 
                                              source = "from", 
                                              target = "to"))

# convert into list of igraph networks
gr_list <- as_igraph(egor_obj)

# see the result
head(gr_list)
names(gr_list)

# create the same list with ego nodes
gr_list_ego <- as.igraph(egor_obj, include.ego = T)

# replace ego weight with 2
for (i in seq_along(gr_list_ego)) {
  E(gr_list_ego[[i]])$weight %<>%
    replace_na(., 2)
}

# add ego id as a graph attribute
# List of graphs without the ego.
for (i in seq_along(gr_list)) {
  gr_list[[i]]$ego_id <- names(gr_list)[[i]]
}

# List of graphs with the ego.
for (i in seq_along(gr_list_ego)) {
  gr_list_ego[[i]]$ego_ID <- names(gr_list_ego)[[i]]
}

# Save all data to file
save(ego_df, alter_attr, gr_list, gr_list_ego, file="data/ego_igraph_up.rda")

##############################################################################################
### ORGANIZE ALTER-AALTER NETWORK DATA (ALTER SURVEY ONLY) AND IMPORT INTO EGOR AND IGRAPH ###
##############################################################################################

# create tibble of alter attributes
alter_df <- as_tibble(alter) %>% 
  select(district_name:advice_religious_leader)

# convert all character variables to factor
alter_df %<>% 
  mutate(across(where(is.character), as.factor))

# create empty tibble of aalter attributes
aalter_attr <- tibble(aalter_id = character(),
                      alter_id = character(),
                      aalter_num = numeric(),
                      relationship = character(),
                      sex = numeric(),
                      age = numeric(),
                      yrs_known = numeric(),
                      talk_freq = numeric(),
                      preg = numeric(),
                      using_fp = numeric(),
                      subjects = character(),
                      subjects_other = character(),
                      talk_freq_fp = numeric())

# create empty tibble of aalter-aalter ties
aalter_ties <- tibble(from = character(),
                      to = character(),
                      alter_id = character(),
                      weight = numeric())

# loop through alters IDs and add rows based on aalters
for(i in 1:NROW(alter)){
  
  # go through aalters and add aalter attr
  if(is.na(alter$alter1[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '1'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 1,
                             relationship = alter$alter1r[i],
                             sex = alter$alter1_sex[i],
                             age = alter$alter1_age[i],
                             yrs_known = alter$alter1_yrs_known[i],
                             talk_freq = alter$alter1_talk_freq[i],
                             preg = alter$alter1_preg[i],
                             using_fp = alter$alter1_using_fp[i],
                             subjects = alter$alter1_subjects[i],
                             subjects_other = alter$alter1_subjects_other[i],
                             talk_freq_fp = alter$alter1_freq_talk_fp[i])
  } 
  if(is.na(alter$alter2[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '2'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 2,
                             relationship = alter$alter2r[i],
                             sex = alter$alter2_sex[i],
                             age = alter$alter2_age[i],
                             yrs_known = alter$alter2_yrs_known[i],
                             talk_freq = alter$alter2_talk_freq[i],
                             preg = alter$alter2_preg[i],
                             using_fp = alter$alter2_using_fp[i],
                             subjects = alter$alter2_subjects[i],
                             subjects_other = alter$alter2_subjects_other[i],
                             talk_freq_fp = alter$alter2_freq_talk_fp[i])
  } 
  if(is.na(alter$alter3[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '3'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 3,
                             relationship = alter$alter3r[i],
                             sex = alter$alter3_sex[i],
                             age = alter$alter3_age[i],
                             yrs_known = alter$alter3_yrs_known[i],
                             talk_freq = alter$alter3_talk_freq[i],
                             preg = alter$alter3_preg[i],
                             using_fp = alter$alter3_using_fp[i],
                             subjects = alter$alter3_subjects[i],
                             subjects_other = alter$alter3_subjects_other[i],
                             talk_freq_fp = alter$alter3_freq_talk_fp[i])
  } 
  if(is.na(alter$alter4[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '4'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 4,
                             relationship = alter$alter4r[i],
                             sex = alter$alter4_sex[i],
                             age = alter$alter4_age[i],
                             yrs_known = alter$alter4_yrs_known[i],
                             talk_freq = alter$alter4_talk_freq[i],
                             preg = alter$alter4_preg[i],
                             using_fp = alter$alter4_using_fp[i],
                             subjects = alter$alter4_subjects[i],
                             subjects_other = alter$alter4_subjects_other[i],
                             talk_freq_fp = alter$alter4_freq_talk_fp[i])
  } 
  if(is.na(alter$alter5[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 5,
                             relationship = alter$alter5r[i],
                             sex = alter$alter5_sex[i],
                             age = alter$alter5_age[i],
                             yrs_known = alter$alter5_yrs_known[i],
                             talk_freq = alter$alter5_talk_freq[i],
                             preg = alter$alter5_preg[i],
                             using_fp = alter$alter5_using_fp[i],
                             subjects = alter$alter5_subjects[i],
                             subjects_other = alter$alter5_subjects_other[i],
                             talk_freq_fp = alter$alter5_freq_talk_fp[i])
  } 
  
  # go through aalter-aalter ties and add to edgelist
  if(is.na(alter$know12[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '1'),
                             to = str_c(alter$alter_id[i], '2'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know12[i])
  }
  if(is.na(alter$know13[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '1'),
                             to = str_c(alter$alter_id[i], '3'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know13[i])
  }
  if(is.na(alter$know14[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '1'),
                             to = str_c(alter$alter_id[i], '4'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know14[i])
  }
  if(is.na(alter$know15[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '1'),
                             to = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know15[i])
  }
  if(is.na(alter$know23[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '2'),
                             to = str_c(alter$alter_id[i], '3'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know23[i])
  }
  if(is.na(alter$know24[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '2'),
                             to = str_c(alter$alter_id[i], '4'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know24[i])
  }
  if(is.na(alter$know25[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '2'),
                             to = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know25[i])
  }
  if(is.na(alter$know34[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '3'),
                             to = str_c(alter$alter_id[i], '4'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know34[i])
  }
  if(is.na(alter$know35[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '3'),
                             to = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know35[i])
  }
  if(is.na(alter$know45[i]) == F) {
    aalter_ties %<>% add_row(from = str_c(alter$alter_id[i], '4'),
                             to = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i],
                             weight = alter$know45[i])
  }
}

# convert character vars to numeric
aalter_attr %<>% mutate(across(c(sex, age, talk_freq, preg, using_fp), as.numeric))

# check freq table of alter tie weights
aalter_ties %>% tabyl(weight) 

# since 97% of aalter ties are not directional just make them all that way
aalter_ties %<>% mutate(weight = replace(weight, weight == 2, 1))

# convert all character variables to factor
aalter_attr %<>% 
  mutate(across(where(is.character), as.factor))
aalter_ties %<>% 
  mutate(across(where(is.character), as.factor))

# create an egor object
egor_obj_a <- threefiles_to_egor(egos = alter_df,
                                 alters.df= aalter_attr, 
                                 edges= aalter_ties, 
                                 ID.vars = list(ego = "alter_id", 
                                                alter = "aalter_id", 
                                                source = "from", 
                                                target = "to"))

# convert into list of igraph networks
gr_list_a <- as_igraph(egor_obj_a)

# see the result
head(gr_list_a)
names(gr_list_a)

# create the same list with alter nodes
gr_list_a_alt <- as.igraph(egor_obj_a, include.ego = T)

# replace alter weight with 2
for (i in seq_along(gr_list_a_alt)) {
  E(gr_list_a_alt[[i]])$weight %<>%
    replace_na(., 2)
}

# add alter id as a graph attribute
# List of graphs without the alter
for (i in seq_along(gr_list_a)) {
  gr_list_a[[i]]$alter_id <- names(gr_list_a)[[i]]
}

# List of graphs with the ego.
for (i in seq_along(gr_list_a_alt)) {
  gr_list_a_alt[[i]]$alter_id <- names(gr_list_a_alt)[[i]]
}

# Save all data to file
save(alter_df, aalter_attr, gr_list_a, gr_list_a_alt, file="data/alter_igraph_up.rda")

###############################################
### COMBINE NETWORKS USING A LARGE EDGELIST ###
###############################################

# extract edgelists from ego igraph objects and combine
for(i in seq_along(gr_list_ego)){
  if(i == 1){
    edge <- as_edgelist(gr_list_ego[[i]])
  } else{
    edge <- rbind(edge, as_edgelist(gr_list_ego[[i]]))
  }
}

# change ego names in edge list
edge %<>% as_tibble
edge$V2[edge$V2 == 'ego'] <- str_sub(edge$V1[edge$V2 == 'ego'], start = 1, end = -2)

# extract edgelists from alter igraph objects and combine
for(i in seq_along(gr_list_a_alt)){
  if(i == 1){
    edge_alt <- as_edgelist(gr_list_a_alt[[i]])
  } else{
    edge_alt <- rbind(edge_alt, as_edgelist(gr_list_a_alt[[i]]))
  }
}

# change ego names in edge list
edge_alt %<>% as_tibble
edge_alt$V2[edge_alt$V2 == 'ego'] <- str_sub(edge_alt$V1[edge_alt$V2 == 'ego'], start = 1, end = -2)

# combine edgelists
edge <- rbind(edge, edge_alt)
rm (edge_alt)

# add edge weights to edge list
edge$weight <- NA

# ego to ego weight 2
edge$weight[str_length(edge$V1) == 9 & str_length(edge$V2) == 9] <- 2

#ego to alter weight 2
edge$weight[str_length(edge$V1) == 9 & str_length(edge$V2) == 10] <- 2
edge$weight[str_length(edge$V1) == 10 & str_length(edge$V2) == 9] <- 2

#alter to aalter weight 2
edge$weight[str_length(edge$V1) == 11 & str_length(edge$V2) == 10] <- 2
edge$weight[str_length(edge$V1) == 10 & str_length(edge$V2) == 11] <- 2

#alter to alter weight 1
edge$weight[str_length(edge$V1) == 10 & str_length(edge$V2) == 10] <- 1

#aalter to aalter weight 1
edge$weight[str_length(edge$V1) == 11 & str_length(edge$V2) == 11] <- 1

# add duplicate ids as edges
edge %<>% rbind(edge,
                tibble(V1 = dup_id$id,
                       V2 = dup_id$duplicate_id,
                       weight = 4))

# create igraph object using edgelist
gr_comb <- graph_from_edgelist(edge[,1:2] %>% as.matrix, directed = F)
gr_comb

# create a tibble of node attributes
v_attr <- tibble(id = names(V(gr_comb)))
v_attr$district <- NA
v_attr$block <- NA
v_attr$relationship <- NA
v_attr$sex <- NA
v_attr$age <- NA
v_attr$using_fp <- NA
v_attr$know_asha <- NA
v_attr$know_anm <- NA
v_attr$know_aww <- NA
v_attr$know_doc <- NA
v_attr$know_pha <- NA
v_attr$know_shg <- NA
v_attr$know_rel <- NA
v_attr$know_pra <- NA

# fill sex for ego
v_attr$sex[str_length(v_attr$id) == 9] <- 'Female'

for(i in 1:NROW(v_attr)){
  v_attr$district[i] <- ego_df$district_name[ego_df$ego_id %>% as.character == str_sub(v_attr$id[i], 1, 9)] %>% as.character
  v_attr$block[i] <- ego_df$block_name[ego_df$ego_id %>% as.character == str_sub(v_attr$id[i], 1, 9)] %>% as.character
  
  # fill other attributes for ego
  if(str_length(v_attr$id[i]) == 9){
    v_attr$age[i] <- ego_df$ego_age[ego_df$ego_id == v_attr$id[i]]
    v_attr$using_fp[i] <- ego_df$ego_using_fp[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_asha[i] <- ego_df$know_asha[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_anm[i] <- ego_df$know_anm[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_aww[i] <- ego_df$know_aww[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_doc[i] <- ego_df$know_doctor[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_pha[i] <- ego_df$know_pharmacist[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_shg[i] <- ego_df$know_shg[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_rel[i] <- ego_df$know_religious_leader[ego_df$ego_id == v_attr$id[i]]
    v_attr$know_pra[i] <- ego_df$know_pradhan[ego_df$ego_id == v_attr$id[i]]
  }
  
  # fill other attributes for alter
  if(str_length(v_attr$id[i]) == 10){
    v_attr$relationship[i] <- alter_attr$relationship[alter_attr$alter_id == v_attr$id[i]] %>% as.character
    v_attr$sex[i] <- alter_attr$sex[alter_attr$alter_id == v_attr$id[i]]
    v_attr$age[i] <- alter_attr$age[alter_attr$alter_id == v_attr$id[i]]
    v_attr$using_fp[i] <- alter_attr$using_fp[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_asha[i] <- alter_attr$know_asha[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_anm[i] <- alter_attr$know_anm[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_aww[i] <- alter_attr$know_aww[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_doc[i] <- alter_attr$know_doc[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_pha[i] <- alter_attr$know_pha[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_shg[i] <- alter_attr$know_shg[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_rel[i] <- alter_attr$know_rel[alter_attr$alter_id == v_attr$id[i]]
    v_attr$know_pra[i] <- alter_attr$know_pra[alter_attr$alter_id == v_attr$id[i]]
    
    # if alter was interviewed and gave age/using fp use that
    if(v_attr$id[i] %in% alter_df$alter_id){
      v_attr$age[i] <- alter_df$age[alter_df$alter_id == v_attr$id[i]]
      v_attr$using_fp[i] <- alter_df$current_method[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_asha[i] <- alter_df$know_asha[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_anm[i] <- alter_df$know_anm[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_aww[i] <- alter_df$know_aww[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_doc[i] <- alter_df$know_doctor[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_pha[i] <- alter_df$know_pharmacist[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_shg[i] <- alter_df$know_shg[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_rel[i] <- alter_df$know_religious_leader[alter_df$alter_id == v_attr$id[i]]
      v_attr$know_pra[i] <- alter_df$know_pradhan[alter_df$alter_id == v_attr$id[i]]
    }
  }
  
  # fill other attributes for alter's alter
  if(str_length(v_attr$id[i]) == 11){
    v_attr$relationship[i] <- aalter_attr$relationship[aalter_attr$aalter_id == v_attr$id[i]] %>% as.character
    v_attr$sex[i] <- aalter_attr$sex[aalter_attr$aalter_id == v_attr$id[i]]
    v_attr$age[i] <- aalter_attr$age[aalter_attr$aalter_id == v_attr$id[i]]
    v_attr$using_fp[i] <- aalter_attr$using_fp[aalter_attr$aalter_id == v_attr$id[i]]
    
  }
}

# clean up sex values
v_attr$sex[v_attr$sex %in% c('1', '1.0')] <- 'Male'
v_attr$sex[v_attr$sex %in% c('2', '2.0')] <- 'Female'
v_attr$sex[is.na(v_attr$sex)] <- 'Missing'

# clean up relationship values
v_attr$relationship <- str_remove_all(v_attr$relationship, "[^[\\da-zA-Z\\-]]")

# # create frequency tables of relationships by alter and alter's alter
# # alter
# write.csv(tabyl(v_attr$relationship[str_length(v_attr$id) == 10]), 
#           file = '/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/alt_rela_vals_freq_up.csv',
#           row.names = F)
# 
# # aalter
# write.csv(tabyl(v_attr$relationship[str_length(v_attr$id) == 11]), 
#           file = '/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/aalt_rela_vals_freq_up.csv',
#           row.names = F)


# create key for relationship categories
rela_key <- c(`OtherRelative` = "Other Family",
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
              `Brotherinlaw` = "Brother-in-law")

# create key for acronyms
acr_key <- c(`Brother-in-law` = "BIL",
             `Brother` = "Bro",
             `Daughter` = "D",
             `Daughter-in-law` = "DIL",
             `Father` = "F",
             `Father-in-law` = "FIL",
             `Husband` = "H",
             `Mother` = "M",
             `Mother-in-law` = "MIL",
             `Sister` = "Sis",
             `Sister-in-law` = "SIL",
             `Son` = "Son",
             `Son-in-law` = "SonIL",
             `Wife` = "W",
             `Other Family` = "OF",
             `ANM` = "ANM",
             `ASHA` = "ASHA",
             `AWW` = "AWW",
             `Doctor` = "Doc",
             `Other Health Worker` = "OHW",
             `Friend` = "Fr",
             `Neighbour` = "Neigh",
             `Other Non-Family` = "ONF")

# create key for major relationship categories
maj_rela_key <- c(`Other Family` = "Other Family",
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
                  `Other Non-Family` = "Other Non-Family")

# recode relationship vals into relationships
v_attr %<>% mutate(rela = recode(relationship, !!!rela_key))
v_attr$rela[str_length(v_attr$id) == 9] <- 'Ego'
v_attr$rela[v_attr$relationship == 'HusbandWife' & v_attr$sex == 'Female'] <- 'Wife'
v_attr$rela[v_attr$relationship == 'HusbandWife' & v_attr$sex == 'Male'] <- 'Husband'

# recode relationship vals into categories
v_attr %<>% mutate(rela_vals = recode(rela, !!!maj_rela_key))

# relabel relationship values
v_attr %<>% mutate(rela = recode(rela,!!!acr_key))

# clean up using_fp
v_attr$using_fp[v_attr$using_fp == 1] <- 'Yes'
v_attr$using_fp[v_attr$using_fp == 2] <- 'No'
v_attr$using_fp[v_attr$using_fp %in% c(3, 9)] <- 'NA'
v_attr$using_fp[is.na(v_attr$using_fp)] <- 'NA'

# if husband set using_fp to same as ego or alter
for(i in 1:NROW(v_attr)){
  if(v_attr$relationship[i] %in% c('Husband', 'HusbandWife') & is.na(v_attr$relationship[i]) == F){
    v_attr$using_fp[i] <- v_attr$using_fp[v_attr$id == str_sub(v_attr$id[i], 1, str_length(v_attr$id[i])-1)]
  }
}

# add group
v_attr$group <- NA
v_attr$group[str_length(v_attr$id) == 9] <- 'Ego'
v_attr$group[str_length(v_attr$id) == 10] <- 'Alter'
v_attr$group[str_length(v_attr$id) == 11] <- 'AAlter'

# add whether interviewed or not
v_attr$intv <- 'No'
v_attr$intv[str_length(v_attr$id) == 9] <- 'Yes'
v_attr$intv[v_attr$id %in% alter_df$alter_id] <- 'Yes'

# add intv_stroke column
v_attr %<>% mutate(intv_stroke = recode(intv, Yes = 2, No = 0.5) %>% as.numeric)

# add column of who all ego and alter know
v_attr$key_ppl <- ''

# set know A3 to 0 if not 1
v_attr$know_asha[is.na(v_attr$know_asha)] <- 0
v_attr$know_asha[v_attr$know_asha != 1] <- 0
v_attr$know_anm[is.na(v_attr$know_anm)] <- 0
v_attr$know_anm[v_attr$know_anm != 1] <- 0
v_attr$know_aww[is.na(v_attr$know_aww)] <- 0
v_attr$know_aww[v_attr$know_aww != 1] <- 0

# create sum column
v_attr$know_a <- v_attr$know_asha + v_attr$know_anm + v_attr$know_aww

# add AAA
v_attr$key_ppl[which(v_attr$know_a == 1)] <- 'A1'
v_attr$key_ppl[which(v_attr$know_a == 2)] <- 'A2'
v_attr$key_ppl[which(v_attr$know_a == 3)] <- 'A3'

# add DOC
v_attr$key_ppl[which(v_attr$know_doc == 1)] <- str_c(v_attr$key_ppl[which(v_attr$know_doc == 1)], 'D')

# add PHA
v_attr$key_ppl[which(v_attr$know_pha == 1)] <- str_c(v_attr$key_ppl[which(v_attr$know_pha == 1)], 'P')

# add MUK
v_attr$key_ppl[which(v_attr$know_pra == 1)] <- str_c(v_attr$key_ppl[which(v_attr$know_pra == 1)], 'M')

# add SHG
v_attr$key_ppl[which(v_attr$know_shg == 1)] <- str_c(v_attr$key_ppl[which(v_attr$know_shg == 1)], 'S')

# add REL
v_attr$key_ppl[which(v_attr$know_rel == 1)] <- str_c(v_attr$key_ppl[which(v_attr$know_rel == 1)], 'R')

# add symbol to end of rela if a duplicate person
for(i in unique(dup_id$id)){
  v_attr$rela[v_attr$id == i] <- str_c(v_attr$rela[v_attr$id == i], dup_id$symbol[dup_id$id == i][1])
}

for(i in unique(dup_id$duplicate_id)){
  v_attr$rela[v_attr$id == i] <- str_c(v_attr$rela[v_attr$id == i], dup_id$symbol[dup_id$duplicate_id == i][1])
}

# set nodes attributes
gr_comb %<>% set_vertex_attr(name = 'district', value = v_attr$district)
gr_comb %<>% set_vertex_attr(name = 'block', value = v_attr$block)
gr_comb %<>% set_vertex_attr(name = 'relationship', value = v_attr$relationship)
gr_comb %<>% set_vertex_attr(name = 'sex', value = v_attr$sex)
gr_comb %<>% set_vertex_attr(name = 'age', value = v_attr$age)
gr_comb %<>% set_vertex_attr(name = 'using_fp', value = v_attr$using_fp)
gr_comb %<>% set_vertex_attr(name = 'group', value = v_attr$group)
gr_comb %<>% set_vertex_attr(name = 'intv', value = v_attr$intv)
gr_comb %<>% set_vertex_attr(name = 'intv_stroke', value = v_attr$intv_stroke)
gr_comb %<>% set_vertex_attr(name = 'rela_vals', value = v_attr$rela_vals)
gr_comb %<>% set_vertex_attr(name = 'rela', value = v_attr$rela)
gr_comb %<>% set_vertex_attr(name = 'key_ppl', value = v_attr$key_ppl)

# add additional edge attributes
edge$talk_freq <- NA
edge$discuss_fp <- NA

for(i in 1:NROW(edge)){
  if(str_length(edge$V1[i]) == 10 & str_length(edge$V2[i]) == 9){
    edge$talk_freq[i] <- alter_attr$talk_freq[alter_attr$alter_id == edge$V1[i]]
    edge$discuss_fp[i] <- alter_attr$preg[alter_attr$alter_id == edge$V1[i]]
  }
  if(str_length(edge$V1[i]) == 9 & str_length(edge$V2[i]) == 10){
    edge$talk_freq[i] <- alter_attr$talk_freq[alter_attr$alter_id == edge$V2[i]]
    edge$discuss_fp[i] <- alter_attr$preg[alter_attr$alter_id == edge$V2[i]]
  }
  if(str_length(edge$V1[i]) == 11 & str_length(edge$V2[i]) == 10){
    edge$talk_freq[i] <- aalter_attr$talk_freq[aalter_attr$aalter_id == edge$V1[i]]
    edge$discuss_fp[i] <- aalter_attr$preg[aalter_attr$aalter_id == edge$V1[i]]
  }
  if(str_length(edge$V1[i]) == 10 & str_length(edge$V2[i]) == 11){
    edge$talk_freq[i] <- aalter_attr$talk_freq[aalter_attr$aalter_id == edge$V2[i]]
    edge$discuss_fp[i] <- aalter_attr$preg[aalter_attr$aalter_id == edge$V2[i]]
  }
}

# remove additional edge attributes for duplicate id rows
edge$talk_freq[(length(edge$talk_freq) - NROW(dup_id) + 1):length(edge$talk_freq)] <- NA
edge$discuss_fp[(length(edge$discuss_fp) - NROW(dup_id) + 1):length(edge$discuss_fp)] <- NA

# reset values
edge %<>% mutate(talk_freq = recode(talk_freq,
                                    `1` = 'Daily',
                                    `2` = '3xW',
                                    `3` = '1xW',
                                    `4` = '1xM',
                                    `5` = '1x3M',
                                    `6` = '1x6M',
                                    `7` = '1xY'), 
                 discuss_fp = recode(discuss_fp, 
                                     `1` = 'Yes Talked',
                                     `2` = 'Yes Heard',
                                     `3` = 'No',
                                     `9` = 'DKDR'))

# Create categories for freq_talk
edge$talk_freq[is.na(edge$talk_freq)] <- 'NA'
edge %<>% mutate(talk_freq_cat = recode(talk_freq,
                                        '1x3M' = 'Monthly',
                                        '1x6M' = 'Yearly',
                                        '1xM' = 'Monthly',
                                        '1xW' = 'Weekly',
                                        '1xY' = 'Yearly',
                                        '3xW' = 'Weekly',
                                        'Daily' = 'Daily',
                                        'NA' = 'NA')) 

# fix NA for discuss_fp
edge$discuss_fp[is.na(edge$discuss_fp)] <- 'NA'

# set edge attributes 
gr_comb %<>% set_edge_attr(name = 'weight', value = edge$weight)
gr_comb %<>% set_edge_attr(name = 'talk_freq', value = edge$talk_freq)
gr_comb %<>% set_edge_attr(name = 'discuss_fp', value = edge$discuss_fp)
gr_comb %<>% set_edge_attr(name = 'talk_freq_cat', value = edge$talk_freq_cat)

# loop through ids and create new edgelist if id knows key people
# pre allocate new edgelist
edge_add <- tibble(V1 = character(),
                   V2 = character(),
                   weight = numeric(),
                   rela = character(),
                   rela_vals = character(),
                   group = character(),
                   district = character(),
                   block = character())

for(i in 1:NROW(v_attr)){
  
  # asha
  if(v_attr$know_asha[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('asha_', v_attr$id[i]),
                          weight = 3,
                          rela = "AS",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # anm
  if(v_attr$know_anm[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('anm_', v_attr$id[i]),
                          weight = 3,
                          rela = "AN",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # aww
  if(v_attr$know_aww[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('aww_', v_attr$id[i]),
                          weight = 3,
                          rela = "AW",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # doc
  if(v_attr$know_doc[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('doc_', v_attr$id[i]),
                          weight = 3,
                          rela = "DOC",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # pha
  if(v_attr$know_pha[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('pha_', v_attr$id[i]),
                          weight = 3,
                          rela = "P",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # shg
  if(v_attr$know_shg[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('shg_', v_attr$id[i]),
                          weight = 3,
                          rela = "SHG",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # rel
  if(v_attr$know_rel[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('rel_', v_attr$id[i]),
                          weight = 3,
                          rela = "RL",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
  # pra
  if(v_attr$know_pra[i] %in% 1){
    edge_add %<>% add_row(V1 = v_attr$id[i],
                          V2 = str_c('pra_', v_attr$id[i]),
                          weight = 3,
                          rela = "PRA",
                          rela_vals = 'Key Villager',
                          group = 'Key Villager',
                          district = v_attr$district[i],
                          block = v_attr$block[i])
  }
  
}

# create igraph of key villager relationship
# create igraph object using edgelist
gr_rela <- graph_from_edgelist(edge_add[,1:2] %>% as.matrix, directed = F)

# create tibble of node attributes
v_attr_add <- tibble(id = names(V(gr_rela)))
v_attr_add$rela <- NA
v_attr_add$rela_vals <- NA
v_attr_add$group <- NA
v_attr_add$district <- NA
v_attr_add$block <- NA

# loop through ids
for(i in 1:NROW(v_attr_add)){
  
  # if id is in v_attr take those attributes
  if(v_attr_add$id[i] %in% v_attr$id){
    v_attr_add$rela[i] <- v_attr$rela[v_attr$id == v_attr_add$id[i]]
    v_attr_add$rela_vals[i] <- v_attr$rela_vals[v_attr$id == v_attr_add$id[i]]
    v_attr_add$group[i] <- v_attr$group[v_attr$id == v_attr_add$id[i]]
    v_attr_add$district[i] <- v_attr$district[v_attr$id == v_attr_add$id[i]]
    v_attr_add$block[i] <- v_attr$block[v_attr$id == v_attr_add$id[i]]
  }else{ # else take values from edge_add
    v_attr_add$rela[i] <- edge_add$rela[edge_add$V2 == v_attr_add$id[i]]
    v_attr_add$rela_vals[i] <- edge_add$rela_vals[edge_add$V2 == v_attr_add$id[i]]
    v_attr_add$group[i] <- edge_add$group[edge_add$V2 == v_attr_add$id[i]]
    v_attr_add$district[i] <- edge_add$district[edge_add$V2 == v_attr_add$id[i]]
    v_attr_add$block[i] <- edge_add$block[edge_add$V2 == v_attr_add$id[i]]
  }
}

# set nodes attributes
gr_rela %<>% set_vertex_attr(name = 'district', value = v_attr_add$district)
gr_rela %<>% set_vertex_attr(name = 'block', value = v_attr_add$block)
gr_rela %<>% set_vertex_attr(name = 'group', value = v_attr_add$group)
gr_rela %<>% set_vertex_attr(name = 'rela', value = v_attr_add$rela)
gr_rela %<>% set_vertex_attr(name = 'rela_vals', value = v_attr_add$rela_vals)

# set edge attributes 
gr_rela %<>% set_edge_attr(name = 'weight', value = edge_add$weight)

# combine igraph objects
gr_u <- gr_comb %u% gr_rela

# create final tibble of attributes
v_attr_u <- tibble(id = names(V(gr_u)),
                   district_1 = vertex_attr(gr_u, name = 'district_1'),
                   district_2 = vertex_attr(gr_u, name = 'district_2'),
                   block_1 = vertex_attr(gr_u, name = 'block_1'),
                   block_2 = vertex_attr(gr_u, name = 'block_2'),
                   group_1 = vertex_attr(gr_u, name = 'group_1'),
                   group_2 = vertex_attr(gr_u, name = 'group_2'),
                   rela_vals_1 = vertex_attr(gr_u, name = 'rela_vals_1'),
                   rela_vals_2 = vertex_attr(gr_u, name = 'rela_vals_2'),
                   rela_1 = vertex_attr(gr_u, name = 'rela_1'),
                   rela_2 = vertex_attr(gr_u, name = 'rela_2'),
                   intv_stroke = vertex_attr(gr_u, name = 'intv_stroke'))

# consolidate columns
v_attr_u$district_1[is.na(v_attr_u$district_1)] <- v_attr_u$district_2[is.na(v_attr_u$district_1)]
v_attr_u$block_1[is.na(v_attr_u$block_1)] <- v_attr_u$block_2[is.na(v_attr_u$block_1)]
v_attr_u$group_1[is.na(v_attr_u$group_1)] <- v_attr_u$group_2[is.na(v_attr_u$group_1)]
v_attr_u$rela_vals_1[is.na(v_attr_u$rela_vals_1)] <- v_attr_u$rela_vals_2[is.na(v_attr_u$rela_vals_1)]
v_attr_u$rela_1[is.na(v_attr_u$rela_1)] <- v_attr_u$rela_2[is.na(v_attr_u$rela_1)]
v_attr_u$intv_stroke[is.na(v_attr_u$intv_stroke)] <- 0.5

# set final node attributes
gr_u %<>% set_vertex_attr(name = 'district', value = v_attr_u$district_1)
gr_u %<>% set_vertex_attr(name = 'block', value = v_attr_u$block_1)
gr_u %<>% set_vertex_attr(name = 'group', value = v_attr_u$group_1)
gr_u %<>% set_vertex_attr(name = 'rela', value = v_attr_u$rela_1)
gr_u %<>% set_vertex_attr(name = 'rela_vals', value = v_attr_u$rela_vals_1)
gr_u %<>% set_vertex_attr(name = 'intv_stroke', value = v_attr_u$intv_stroke)

# delete erroneous node attributes
gr_u %<>% delete_vertex_attr(name = 'district_1')
gr_u %<>% delete_vertex_attr(name = 'district_2')
gr_u %<>% delete_vertex_attr(name = 'block_1')
gr_u %<>% delete_vertex_attr(name = 'block_2')
gr_u %<>% delete_vertex_attr(name = 'group_1')
gr_u %<>% delete_vertex_attr(name = 'group_2')
gr_u %<>% delete_vertex_attr(name = 'rela_1')
gr_u %<>% delete_vertex_attr(name = 'rela_2')
gr_u %<>% delete_vertex_attr(name = 'rela_vals_1')
gr_u %<>% delete_vertex_attr(name = 'rela_vals_2')

# create final tibble of edge attributes
e_attr_u <- tibble(weight_1 = edge_attr(gr_u, name = 'weight_1'),
                   weight_2 = edge_attr(gr_u, name = 'weight_2'))

# consolidate columns
e_attr_u$weight_1[is.na(e_attr_u$weight_1)] <- e_attr_u$weight_2[is.na(e_attr_u$weight_1)]

# set final edge attributes
gr_u %<>% set_edge_attr(name = 'weight', value = e_attr_u$weight_1)

# delete erroneous edge attributes
gr_u %<>% delete_edge_attr(name = 'weight_1')
gr_u %<>% delete_edge_attr(name = 'weight_2')

# create new igraph of one key villager node per group of key villagers
edge_kv <- tibble(id = names(V(gr_comb)),
                  rela = vertex_attr(gr_comb, 'key_ppl'),
                  district = vertex_attr(gr_comb, 'district'),
                  block = vertex_attr(gr_comb, 'block'))

# remove rows w/o key people
edge_kv %<>% filter(rela != '')

# add kv id
edge_kv %<>% mutate(kv = str_c(id, '_kv'))

# add additional columns
edge_kv %<>% mutate(rela_vals = 'Key Villager',
                    group = 'Key Villager',
                    weight = 3)

# create igraph
gr_kv <- graph_from_edgelist(edge_kv %>% select(id, kv) %>% as.matrix, directed = F)

# create tibble of node attributes
v_attr_kv <- tibble(id = names(V(gr_kv)))
v_attr_kv$rela <- NA
v_attr_kv$rela_vals <- NA
v_attr_kv$group <- NA
v_attr_kv$district <- NA
v_attr_kv$block <- NA

# loop through ids
for(i in 1:NROW(v_attr_kv)){
  
  # if id is in v_attr take those attributes
  if(v_attr_kv$id[i] %in% v_attr$id){
    v_attr_kv$rela[i] <- v_attr$rela[v_attr$id == v_attr_kv$id[i]]
    v_attr_kv$rela_vals[i] <- v_attr$rela_vals[v_attr$id == v_attr_kv$id[i]]
    v_attr_kv$group[i] <- v_attr$group[v_attr$id == v_attr_kv$id[i]]
    v_attr_kv$district[i] <- v_attr$district[v_attr$id == v_attr_kv$id[i]]
    v_attr_kv$block[i] <- v_attr$block[v_attr$id == v_attr_kv$id[i]]
  }else{ # else take values from edge_add
    v_attr_kv$rela[i] <- edge_kv$rela[edge_kv$kv == v_attr_kv$id[i]]
    v_attr_kv$rela_vals[i] <- edge_kv$rela_vals[edge_kv$kv == v_attr_kv$id[i]]
    v_attr_kv$group[i] <- edge_kv$group[edge_kv$kv == v_attr_kv$id[i]]
    v_attr_kv$district[i] <- edge_kv$district[edge_kv$kv == v_attr_kv$id[i]]
    v_attr_kv$block[i] <- edge_kv$block[edge_kv$kv == v_attr_kv$id[i]]
  }
}

# set nodes attributes
gr_kv %<>% set_vertex_attr(name = 'district', value = v_attr_kv$district)
gr_kv %<>% set_vertex_attr(name = 'block', value = v_attr_kv$block)
gr_kv %<>% set_vertex_attr(name = 'group', value = v_attr_kv$group)
gr_kv %<>% set_vertex_attr(name = 'rela', value = v_attr_kv$rela)
gr_kv %<>% set_vertex_attr(name = 'rela_vals', value = v_attr_kv$rela_vals)

# set edge attributes 
gr_kv %<>% set_edge_attr(name = 'weight', value = edge_kv$weight)

# combine igraph objects
gr_kv <- gr_comb %u% gr_kv

# create final tibble of attributes
v_attr_kv <- tibble(id = names(V(gr_kv)),
                    district_1 = vertex_attr(gr_kv, name = 'district_1'),
                    district_2 = vertex_attr(gr_kv, name = 'district_2'),
                    block_1 = vertex_attr(gr_kv, name = 'block_1'),
                    block_2 = vertex_attr(gr_kv, name = 'block_2'),
                    group_1 = vertex_attr(gr_kv, name = 'group_1'),
                    group_2 = vertex_attr(gr_kv, name = 'group_2'),
                    rela_vals_1 = vertex_attr(gr_kv, name = 'rela_vals_1'),
                    rela_vals_2 = vertex_attr(gr_kv, name = 'rela_vals_2'),
                    rela_1 = vertex_attr(gr_kv, name = 'rela_1'),
                    rela_2 = vertex_attr(gr_kv, name = 'rela_2'),
                    intv_stroke = vertex_attr(gr_kv, name = 'intv_stroke'))

# consolidate columns
v_attr_kv$district_1[is.na(v_attr_kv$district_1)] <- v_attr_kv$district_2[is.na(v_attr_kv$district_1)]
v_attr_kv$block_1[is.na(v_attr_kv$block_1)] <- v_attr_kv$block_2[is.na(v_attr_kv$block_1)]
v_attr_kv$group_1[is.na(v_attr_kv$group_1)] <- v_attr_kv$group_2[is.na(v_attr_kv$group_1)]
v_attr_kv$rela_vals_1[is.na(v_attr_kv$rela_vals_1)] <- v_attr_kv$rela_vals_2[is.na(v_attr_kv$rela_vals_1)]
v_attr_kv$rela_1[is.na(v_attr_kv$rela_1)] <- v_attr_kv$rela_2[is.na(v_attr_kv$rela_1)]
v_attr_kv$intv_stroke[is.na(v_attr_kv$intv_stroke)] <- 0.5

# set final node attributes
gr_kv %<>% set_vertex_attr(name = 'district', value = v_attr_kv$district_1)
gr_kv %<>% set_vertex_attr(name = 'block', value = v_attr_kv$block_1)
gr_kv %<>% set_vertex_attr(name = 'group', value = v_attr_kv$group_1)
gr_kv %<>% set_vertex_attr(name = 'rela', value = v_attr_kv$rela_1)
gr_kv %<>% set_vertex_attr(name = 'rela_vals', value = v_attr_kv$rela_vals_1)
gr_kv %<>% set_vertex_attr(name = 'intv_stroke', value = v_attr_kv$intv_stroke)

# delete erroneous node attributes
gr_kv %<>% delete_vertex_attr(name = 'district_1')
gr_kv %<>% delete_vertex_attr(name = 'district_2')
gr_kv %<>% delete_vertex_attr(name = 'block_1')
gr_kv %<>% delete_vertex_attr(name = 'block_2')
gr_kv %<>% delete_vertex_attr(name = 'group_1')
gr_kv %<>% delete_vertex_attr(name = 'group_2')
gr_kv %<>% delete_vertex_attr(name = 'rela_1')
gr_kv %<>% delete_vertex_attr(name = 'rela_2')
gr_kv %<>% delete_vertex_attr(name = 'rela_vals_1')
gr_kv %<>% delete_vertex_attr(name = 'rela_vals_2')

# create final tibble of edge attributes
e_attr_kv <- tibble(weight_1 = edge_attr(gr_kv, name = 'weight_1'),
                    weight_2 = edge_attr(gr_kv, name = 'weight_2'))

# consolidate columns
e_attr_kv$weight_1[is.na(e_attr_kv$weight_1)] <- e_attr_kv$weight_2[is.na(e_attr_kv$weight_1)]

# set final edge attributes
gr_kv %<>% set_edge_attr(name = 'weight', value = e_attr_kv$weight_1)

# delete erroneous edge attributes
gr_kv %<>% delete_edge_attr(name = 'weight_1')
gr_kv %<>% delete_edge_attr(name = 'weight_2')

# Save data to file
save(gr_comb, v_attr, edge, file = "data/combined_igraph_up.rda")
save(gr_u, v_attr_u, file = "data/combined_igraph_key_villagers_separate_up.rda")
save(gr_kv, v_attr_kv, file = "data/combined_igraph_key_villagers_up.rda")