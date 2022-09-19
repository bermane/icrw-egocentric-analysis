# this code merges the ego and alter interview data

# load packages
library(magrittr)
library(egor)
library(janitor)
library(tidyverse)
library(readxl)
library(igraph)
library(readstata13)
library(writexl)

#####################################
###SET FILE LOCATIONS TO RUN CODE####
#####################################

##################
### BIHAR DATA ###
##################

# ego data file
ego_data_bihar <- "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/ego_clean_complete_11012022.xlsx"

# alter data file
alter_data_bihar <- "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/alter_clean_complete_11012022.xlsx"

# ego pc data file
ego_pc_bihar <- "data/ego_pc_complete_12012022.xlsx"

################
### UP FILES ###
################

# ego data file
ego_data_up <- "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/SNA study_EGO_clean_Uttar Pradesh_08112022.xlsx"

# alter data file
alter_data_up <- "/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/SNA study_ALTER_clean_Uttar Pradesh_08112022.xlsx"

# ego pc data file
ego_pc_up <- "data/ego_pc_complete_up_08112022.xlsx"

#######################
### LOAD BIHAR DATA ###
#######################

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned and completre data
ego <- read_excel(path = ego_data_bihar)
alter <-read_excel(path = alter_data_bihar)

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/ego_clean_12012022de.dta', convert.factors = F)
# 
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_12012022.xlsx")

# load pc data for egos
ego_pc <- read_excel(path = ego_pc_bihar)

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

################################
### MERGE EGO AND ALTER DATA ###
################################

# first change column names
colnames(ego) <- str_c('ego_dat_', colnames(ego))
colnames(alter) <- str_c('alter_dat_', colnames(alter))

# change woman_id column names to match
ego %<>% rename('woman_id' = "ego_dat_woman_id")
alter %<>% rename('woman_id' = "alter_dat_woman_id")

# join data
dat <- left_join(alter, ego, by = 'woman_id')

# check ids
check_id <- tibble(woman_id = dat$woman_id, 
                   alter_id = dat$alter_dat_alter_id)

# change woman_id column name in pc data
ego_pc %<>% rename('woman_id' = "qe6")

# join pc data too
dat <- left_join(dat, ego_pc, by = 'woman_id')

# write csv
write.csv(dat, file = '/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/ego_alt_full_merge_bihar.csv',
          row.names = F)

####################
### LOAD UP DATA ###
####################

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned and completre data
ego <- read_excel(path = ego_data_up)
alter <-read_excel(path = alter_data_up)

# # load dta stata file to get PC data for egos
# ego_pc <- read.dta13(file = 'data/ego_clean_12012022de.dta', convert.factors = F)
# 
# # write to file as xlsx
# write_xlsx(ego_pc, "data/ego_pc_complete_12012022.xlsx")

# load pc data for egos
ego_pc <- read_excel(path = ego_pc_up)

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

################################
### MERGE EGO AND ALTER DATA ###
################################

# first change column names
colnames(ego) <- str_c('ego_dat_', colnames(ego))
colnames(alter) <- str_c('alter_dat_', colnames(alter))

# change woman_id column names to match
ego %<>% rename('woman_id' = "ego_dat_woman_id")
alter %<>% rename('woman_id' = "alter_dat_woman_id")

# join data
dat <- left_join(alter, ego, by = 'woman_id')

# check ids
check_id <- tibble(woman_id = dat$woman_id, 
                   alter_id = dat$alter_dat_alter_id)

# change woman_id column name in pc data
ego_pc %<>% rename('woman_id' = "qe6")

# join pc data too
dat <- left_join(dat, ego_pc, by = 'woman_id')

# write csv
write.csv(dat, file = '/Users/bermane/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/data/ego_alt_full_merge_up.csv',
          row.names = F)
