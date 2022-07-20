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
ego <- read_excel(path = "data/SNA study_EGO_clean_Uttar Pradesh_30June2022.xlsx")
alter <-read_excel(path = "data/SNA study_ALTER_clean_Uttar Pradesh_09Jul2022.xlsx")

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/PC_EGO_data_Merge_Uttar Pradesh.dta', convert.factors = F)
# 
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_up_07122022.xlsx")

# load pc data for egos
ego_pc <- read_excel(path = 'data/ego_pc_complete_up_07122022.xlsx')

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
alter[alter == "NA"] <- NA

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

# sort ego data and PC data by pc ID so can merge
ego_df %<>% arrange(ego_id)
ego_pc %<>% arrange(qe6)

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
                                        pcq119,
                                        pcq106,
                                        pcq107,
                                        ego_ever_used_fp) %>%
                        rename(ego_age = pcq102, 
                               ego_edu = pcq103, 
                               ego_caste = pcq111,
                               ego_using_fp = pcq311,
                               ego_age_married = pcq201b,
                               ego_parity = pcq209,
                               ego_hus_mig = pcq119,
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
                     sex = character(),
                     age = character(),
                     yrs_known = character(),
                     talk_freq = character(),
                     preg = character(),
                     using_fp = character(),
                     subjects = character(),
                     subjects_other = character(),
                     talk_freq_fp = character())

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
                            sex = ego$alter1_sex[i] %>% as.character,
                            age = age_hold %>% as.character,
                            yrs_known = ego$alter1_yrs_known[i] %>% as.character,
                            talk_freq = ego$alter1_talk_freq[i] %>% as.character,
                            preg = ego$alter1_preg[i] %>% as.character,
                            using_fp = using_fp_hold %>% as.character,
                            subjects = ego$alter1_subjects[i],
                            subjects_other = ego$alter1_subjects_other[i],
                            talk_freq_fp = ego$alter1_freq_talk_fp[i] %>% as.character)
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
                            sex = ego$alter2_sex[i] %>% as.character,
                            age = age_hold %>% as.character,
                            yrs_known = ego$alter2_yrs_known[i] %>% as.character,
                            talk_freq = ego$alter2_talk_freq[i] %>% as.character,
                            preg = ego$alter2_preg[i] %>% as.character,
                            using_fp = using_fp_hold %>% as.character,
                            subjects = ego$alter2_subjects[i],
                            subjects_other = ego$alter2_subjects_other[i],
                            talk_freq_fp = ego$alter2_freq_talk_fp[i] %>% as.character)
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
                            sex = ego$alter3_sex[i] %>% as.character,
                            age = age_hold %>% as.character,
                            yrs_known = ego$alter3_yrs_known[i] %>% as.character,
                            talk_freq = ego$alter3_talk_freq[i] %>% as.character,
                            preg = ego$alter3_preg[i] %>% as.character,
                            using_fp = using_fp_hold %>% as.character,
                            subjects = ego$alter3_subjects[i],
                            subjects_other = ego$alter3_subjects_other[i],
                            talk_freq_fp = ego$alter3_freq_talk_fp[i] %>% as.character)
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
                            sex = ego$alter4_sex[i] %>% as.character,
                            age = age_hold %>% as.character,
                            yrs_known = ego$alter4_yrs_known[i] %>% as.character,
                            talk_freq = ego$alter4_talk_freq[i] %>% as.character,
                            preg = ego$alter4_preg[i] %>% as.character,
                            using_fp = using_fp_hold %>% as.character,
                            subjects = ego$alter4_subjects[i],
                            subjects_other = ego$alter4_subjects_other[i],
                            talk_freq_fp = ego$alter4_freq_talk_fp[i] %>% as.character)
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
                            sex = ego$alter5_sex[i] %>% as.character,
                            age = age_hold %>% as.character,
                            yrs_known = ego$alter5_yrs_known[i] %>% as.character,
                            talk_freq = ego$alter5_talk_freq[i] %>% as.character,
                            preg = ego$alter5_preg[i] %>% as.character,
                            using_fp = using_fp_hold %>% as.character,
                            subjects = ego$alter5_subjects[i],
                            subjects_other = ego$alter5_subjects_other[i],
                            talk_freq_fp = ego$alter5_freq_talk_fp[i] %>% as.character)
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
alter_attr %<>% mutate(across(c(sex, age, talk_freq, preg, using_fp), as.numeric))

# change duplicate ids so we know same person
# for(i in 1:NROW(dup_id)){
#   alter_attr[alter_attr == dup_id$duplicate_id[i]] <- dup_id$id[i]
#   alter_ties[alter_ties == dup_id$duplicate_id[i]] <- dup_id$id[i]
# }

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
                      sex = character(),
                      age = character(),
                      yrs_known = character(),
                      talk_freq = character(),
                      preg = character(),
                      using_fp = character(),
                      subjects = character(),
                      subjects_other = character(),
                      talk_freq_fp = character())

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
                             sex = alter$alter1_sex[i] %>% as.character,
                             age = alter$alter1_age[i] %>% as.character,
                             yrs_known = alter$alter1_yrs_known[i] %>% as.character,
                             talk_freq = alter$alter1_talk_freq[i] %>% as.character,
                             preg = alter$alter1_preg[i] %>% as.character,
                             using_fp = alter$alter1_using_fp[i] %>% as.character,
                             subjects = alter$alter1_subjects[i],
                             subjects_other = alter$alter1_subjects_other[i],
                             talk_freq_fp = alter$alter1_freq_talk_fp[i] %>% as.character)
  } 
  if(is.na(alter$alter2[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '2'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 2,
                             relationship = alter$alter2r[i],
                             sex = alter$alter2_sex[i] %>% as.character,
                             age = alter$alter2_age[i] %>% as.character,
                             yrs_known = alter$alter2_yrs_known[i] %>% as.character,
                             talk_freq = alter$alter2_talk_freq[i] %>% as.character,
                             preg = alter$alter2_preg[i] %>% as.character,
                             using_fp = alter$alter2_using_fp[i] %>% as.character,
                             subjects = alter$alter2_subjects[i],
                             subjects_other = alter$alter2_subjects_other[i],
                             talk_freq_fp = alter$alter2_freq_talk_fp[i] %>% as.character)
  } 
  if(is.na(alter$alter3[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '3'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 3,
                             relationship = alter$alter3r[i],
                             sex = alter$alter3_sex[i] %>% as.character,
                             age = alter$alter3_age[i] %>% as.character,
                             yrs_known = alter$alter3_yrs_known[i] %>% as.character,
                             talk_freq = alter$alter3_talk_freq[i] %>% as.character,
                             preg = alter$alter3_preg[i] %>% as.character,
                             using_fp = alter$alter3_using_fp[i] %>% as.character,
                             subjects = alter$alter3_subjects[i],
                             subjects_other = alter$alter3_subjects_other[i],
                             talk_freq_fp = alter$alter3_freq_talk_fp[i] %>% as.character)
  } 
  if(is.na(alter$alter4[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '4'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 4,
                             relationship = alter$alter4r[i],
                             sex = alter$alter4_sex[i] %>% as.character,
                             age = alter$alter4_age[i] %>% as.character,
                             yrs_known = alter$alter4_yrs_known[i] %>% as.character,
                             talk_freq = alter$alter4_talk_freq[i] %>% as.character,
                             preg = alter$alter4_preg[i] %>% as.character,
                             using_fp = alter$alter4_using_fp[i] %>% as.character,
                             subjects = alter$alter4_subjects[i],
                             subjects_other = alter$alter4_subjects_other[i],
                             talk_freq_fp = alter$alter4_freq_talk_fp[i] %>% as.character)
  } 
  if(is.na(alter$alter5[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 5,
                             relationship = alter$alter5r[i],
                             sex = alter$alter5_sex[i] %>% as.character,
                             age = alter$alter5_age[i] %>% as.character,
                             yrs_known = alter$alter5_yrs_known[i] %>% as.character,
                             talk_freq = alter$alter5_talk_freq[i] %>% as.character,
                             preg = alter$alter5_preg[i] %>% as.character,
                             using_fp = alter$alter5_using_fp[i] %>% as.character,
                             subjects = alter$alter5_subjects[i],
                             subjects_other = alter$alter5_subjects_other[i],
                             talk_freq_fp = alter$alter5_freq_talk_fp[i] %>% as.character)
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

# change duplicate ids so we know same person
for(i in 1:NROW(dup_id)){
  edge[edge == dup_id$duplicate_id[i]] <- dup_id$id[i]
}

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

# fill sex for ego
v_attr$sex[str_length(v_attr$id) == 9] <- 'Female'

for(i in 1:NROW(v_attr)){
  v_attr$district[i] <- ego_df$district_name[ego_df$ego_id %>% as.character == str_sub(v_attr$id[i], 1, 9)] %>% as.character
  v_attr$block[i] <- ego_df$block_name[ego_df$ego_id %>% as.character == str_sub(v_attr$id[i], 1, 9)] %>% as.character
  
  # fill other attributes for ego
  if(str_length(v_attr$id[i]) == 9){
    v_attr$age[i] <- ego_df$ego_age[ego_df$ego_id == v_attr$id[i]]
    v_attr$using_fp[i] <- ego_df$ego_using_fp[ego_df$ego_id == v_attr$id[i]]
  }
  
  # fill other attributes for alter
  if(str_length(v_attr$id[i]) == 10){
    v_attr$relationship[i] <- alter_attr$relationship[alter_attr$alter_id == v_attr$id[i]] %>% as.character
    v_attr$sex[i] <- alter_attr$sex[alter_attr$alter_id == v_attr$id[i]]
    v_attr$age[i] <- alter_attr$age[alter_attr$alter_id == v_attr$id[i]]
    v_attr$using_fp[i] <- alter_attr$using_fp[alter_attr$alter_id == v_attr$id[i]]
    
    # if alter was interviewed and gave age/using fp use that
    if(v_attr$id[i] %in% alter_df$alter_id){
      v_attr$age[i] <- alter_df$age[alter_df$alter_id == v_attr$id[i]]
      v_attr$using_fp[i] <- alter_df$current_method[alter_df$alter_id == v_attr$id[i]]
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

# clean up sex vales
v_attr$sex[v_attr$sex %in% c('1', '1.0')] <- 'Male'
v_attr$sex[v_attr$sex %in% c('2', '2.0')] <- 'Female'
v_attr$sex[is.na(v_attr$sex)] <- 'Missing'

# clean up relationship values
v_attr$relationship <- str_remove_all(v_attr$relationship, "[^[\\da-zA-Z\\-]]")

# create groups of relationship types
fam <- c('Husband', 'Mother-in-law', 'Mother', 'Sister-in-law', 'Sister',
         'Brother-in-law', 'OtherRelative', 'Father', 'Father-in-law', 'HusbandWife',
         'OthRelative', 'Brother', 'Daughter', 'Son', 'Beti', 'Barebeti', 'Beta',
         'Chachisash', 'Chachi',
         'Bhagnee', 'Chacherisas', 'Bhabi', 'Beta',
         'Bahu', 'Chacha', 'Bhu', 'Chacherisash', 'Jija', 'Bohu',
         'Jethanikalarka', 'Buasas', 'Chachaji', 'Awwsahsyika', 
         'Bhabhi', 'Mausisash', 'Bhaganee', 'Buaa', 'Mami',
         'Dadi', 'Patoh', 'Barebeta',
         'Chhotibeti', 'Barebahu', 'Chhotebahu', 'Bahan', 'Chhotebeti',
         "Chachiyasas", "Dadisas", "Chachisas", "Bhau", "Bhuakeladke", 
         "Saale", "Nandoi", "Bhanji", "Betha", "Bethi", "Badimaa", "Chachijee", 
         "Mausi", "Dada", "Mamakaladka",  "Damand", "Mausiasaash", "Brotherinlaw", 
         "Maternalcousin", "Bete", "Jijaji")

nonfam <- c('Friend', 'Neighbor', 'Landlord', 'Acquaintance', 'Neighbour', 'Blockmanager',
            'Shgvolentear', 'Trainer', 'Saheli', 'Bcm', 'Aunty', 'VCM',
            'Facilator', 'Councilor', 'Faciletor', 'Facilater',
            "Anti", "Supervisor", "Master", "Pradhan", "YejahakaamkartahaidukanMalik")

health <- c('ASHA', 'Doctor', 'ANM', 'AWW', 'Docter', 'Dactor', 'Dr', 'BahnoiDoctor', 'DR',
            'Nurse', 'AnmFaciletor', "DrDevchandra", "Staffnurse", "Drkhalid", "Sangini", "Sangani", 
            "CHO", "Ashasangini", "DoctorCMO", "AshaGaneshpurki", "LHV", "Doctor-Patient")

# recode relationship vals
v_attr$rela_vals <- v_attr$relationship
v_attr$rela_vals[v_attr$rela_vals %in% fam] <- 'Family'
v_attr$rela_vals[v_attr$rela_vals %in% nonfam] <- 'Non-Family'
v_attr$rela_vals[v_attr$rela_vals %in% health] <- 'Health Worker'
v_attr$rela_vals[str_length(v_attr$id) == 9] <- 'Ego'

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
v_attr %<>% mutate(intv_stroke = recode(intv, Yes = 2, No = 1) %>% as.numeric)

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

# # check if all duplicates are from the same block
# # can only check if don't remove the duplicates above
# for(i in 1:NROW(dup_id)){
#   print(v_attr$block[v_attr$id == dup_id$id[i]] == v_attr$block[v_attr$id == dup_id$duplicate_id[i]])
# }

# Save data to file
save(gr_comb, v_attr, edge, file="data/combined_igraph_up.rda")

