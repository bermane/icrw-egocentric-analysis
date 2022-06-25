# this code loads csv data of ego and alter interviews
# and calculates summary measures

# load packages
library(magrittr)
library(egor)
library(janitor)
library(tidyverse)
library(readxl)
library(igraph)
library(readstata13)
library(reshape2)

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

#############################################
### CAN TALK FREELY ABOUT PERSONAL ISSUES ###
#############################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     personal1r, 
                     personal2r, 
                     personal3r, 
                     personal4r, 
                     personal5r)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'relationship',
             na.rm = T)

# create relationship category column
df %<>% mutate(rela_cat = relationship)

# categorize relationships
df %<>% mutate(rela_cat = str_replace(rela_cat, '.*Husband.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Mother .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Father .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Mother-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Father-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Brother-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Sister-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Brother .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Sister .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Other_Relative.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Friend.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Neighbor.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Acquaintance.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*ASHA.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*ANM.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*AWW.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Other.*', 'FNAO'))

# rename general relationships
df %<>% mutate(relationship = str_replace(relationship, '.*Husband.*', 'Husband')) %>%
  mutate(relationship = str_replace(relationship, '.*Mother-in-law.*', 'Mother-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Father-in-law.*', 'Father-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Brother-in-law.*', 'Brother-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Sister-in-law.*', 'Sister-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Mother .*', 'Mother')) %>%
  mutate(relationship = str_replace(relationship, '.*Father .*', 'Father')) %>%
  mutate(relationship = str_replace(relationship, '.*Brother .*', 'Brother')) %>%
  mutate(relationship = str_replace(relationship, '.*Sister .*', 'Sister')) %>%
  mutate(relationship = str_replace(relationship, '.*Other_Relative.*', 'Other-relative')) %>%
  mutate(relationship = str_replace(relationship, '.*Friend.*', 'Friend')) %>%
  mutate(relationship = str_replace(relationship, '.*Neighbor.*', 'Neighbor')) %>%
  mutate(relationship = str_replace(relationship, '.*Acquaintance.*', 'Acquaintance')) %>%
  mutate(relationship = str_replace(relationship, '.*ASHA.*', 'Asha')) %>%
  mutate(relationship = str_replace(relationship, '.*ANM.*', 'Anm')) %>%
  mutate(relationship = str_replace(relationship, '.*AWW.*', 'Aww'))

# calculate count table by ego and category
sum_cat <- df %>% group_by(woman_id) %>% summarise(total = sum(is.na(rela_cat) == F),
                                                   fam = sum(rela_cat == 'FAM'),
                                                   fnao = sum(rela_cat == 'FNAO'),
                                                   hw = sum(rela_cat == 'HW'))

# calculate summary stats table
stat_cat <- tibble(variable = c('Total',
                                'Family',
                                'Friend_Neighbour_Acquaintance_Other',
                                'Health_Worker'),
                   mean = c(mean(sum_cat$total),
                            mean(sum_cat$fam),
                            mean(sum_cat$fnao),
                            mean(sum_cat$hw)),
                   sd = c(sd(sum_cat$total),
                          sd(sum_cat$fam),
                          sd(sum_cat$fnao),
                          sd(sum_cat$hw)),
                   med = c(median(sum_cat$total),
                           median(sum_cat$fam),
                           median(sum_cat$fnao),
                           median(sum_cat$hw)),
                   min = c(min(sum_cat$total),
                           min(sum_cat$fam),
                           min(sum_cat$fnao),
                           min(sum_cat$hw)),
                   max = c(max(sum_cat$total),
                           max(sum_cat$fam),
                           max(sum_cat$fnao),
                           max(sum_cat$hw)),
                   n_egos = c(NROW(na.omit(sum_cat$total)),
                         NROW(na.omit(sum_cat$fam)),
                         NROW(na.omit(sum_cat$fnao)),
                         NROW(na.omit(sum_cat$hw))))

# round numbers
stat_cat %<>% mutate(mean = mean %>% round(2),
                     sd = sd %>% round(2))

# write
write.csv(stat_cat, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_freely_personal_by_ego_category.csv',
          row.names = F)

# calculate overall by category
overall_cat <- df %>% tabyl(rela_cat)

# write
write.csv(overall_cat, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_freely_personal_overall_category.csv',
          row.names = F)

# now by relationship
# calculate count table by ego and relationship
sum_r <- df %>% group_by(woman_id) %>% summarise(husband = sum(relationship == 'Husband'),
                                                 mother = sum(relationship == 'Mother'),
                                                 father = sum(relationship == 'Father'),
                                                 mil = sum(relationship == 'Mother-in-law'),
                                                 fil = sum(relationship == 'Father-in-law'),
                                                 bil = sum(relationship == 'Brother-in-law'),
                                                 sil = sum(relationship == 'Sister-in-law'),
                                                 brother = sum(relationship == 'Brother'),
                                                 sister = sum(relationship == 'Sister'),
                                                 or = sum(relationship == 'Other-relative'),
                                                 friend = sum(relationship == 'Friend'),
                                                 neighbor = sum(relationship == 'Neighbor'),
                                                 acq = sum(relationship == 'Acquaintance'),
                                                 asha = sum(relationship == 'Asha'),
                                                 anm = sum(relationship == 'Anm'),
                                                 aww = sum(relationship == 'Aww'),
                                                 oth = sum(relationship == 'Other'))

# calculate summary stats table
stat_r <- tibble(variable = c('Husband',
                              'Mother',
                              'Father',
                              'Mother-in-law',
                              'Father-in-law',
                              'Brother-in-law',
                              'Sister-in-law',
                              'Brother',
                              'Sister',
                              'Other-relative',
                              'Friend',
                              'Neighbor',
                              'Acquaintance',
                              'Asha',
                              'Anm',
                              'Aww',
                              'Other'),
                   mean = c(mean(sum_r$husband),
                            mean(sum_r$mother),
                            mean(sum_r$father),
                            mean(sum_r$mil),
                            mean(sum_r$fil),
                            mean(sum_r$bil),
                            mean(sum_r$sil),
                            mean(sum_r$brother),
                            mean(sum_r$sister),
                            mean(sum_r$or),
                            mean(sum_r$friend),
                            mean(sum_r$neighbor),
                            mean(sum_r$acq),
                            mean(sum_r$asha),
                            mean(sum_r$anm),
                            mean(sum_r$aww),
                            mean(sum_r$oth)),
                   sd = c(sd(sum_r$husband),
                          sd(sum_r$mother),
                          sd(sum_r$father),
                          sd(sum_r$mil),
                          sd(sum_r$fil),
                          sd(sum_r$bil),
                          sd(sum_r$sil),
                          sd(sum_r$brother),
                          sd(sum_r$sister),
                          sd(sum_r$or),
                          sd(sum_r$friend),
                          sd(sum_r$neighbor),
                          sd(sum_r$acq),
                          sd(sum_r$asha),
                          sd(sum_r$anm),
                          sd(sum_r$aww),
                          sd(sum_r$oth)),
                   med = c(median(sum_r$husband),
                           median(sum_r$mother),
                           median(sum_r$father),
                           median(sum_r$mil),
                           median(sum_r$fil),
                           median(sum_r$bil),
                           median(sum_r$sil),
                           median(sum_r$brother),
                           median(sum_r$sister),
                           median(sum_r$or),
                           median(sum_r$friend),
                           median(sum_r$neighbor),
                           median(sum_r$acq),
                           median(sum_r$asha),
                           median(sum_r$anm),
                           median(sum_r$aww),
                           median(sum_r$oth)),
                   min = c(min(sum_r$husband),
                           min(sum_r$mother),
                           min(sum_r$father),
                           min(sum_r$mil),
                           min(sum_r$fil),
                           min(sum_r$bil),
                           min(sum_r$sil),
                           min(sum_r$brother),
                           min(sum_r$sister),
                           min(sum_r$or),
                           min(sum_r$friend),
                           min(sum_r$neighbor),
                           min(sum_r$acq),
                           min(sum_r$asha),
                           min(sum_r$anm),
                           min(sum_r$aww),
                           min(sum_r$oth)),
                   max = c(max(sum_r$husband),
                           max(sum_r$mother),
                           max(sum_r$father),
                           max(sum_r$mil),
                           max(sum_r$fil),
                           max(sum_r$bil),
                           max(sum_r$sil),
                           max(sum_r$brother),
                           max(sum_r$sister),
                           max(sum_r$or),
                           max(sum_r$friend),
                           max(sum_r$neighbor),
                           max(sum_r$acq),
                           max(sum_r$asha),
                           max(sum_r$anm),
                           max(sum_r$aww),
                           max(sum_r$oth)),
                   n_egos = c(NROW(sum_r$husband),
                              NROW(sum_r$mother),
                              NROW(sum_r$father),
                              NROW(sum_r$mil),
                              NROW(sum_r$fil),
                              NROW(sum_r$bil),
                              NROW(sum_r$sil),
                              NROW(sum_r$brother),
                              NROW(sum_r$sister),
                              NROW(sum_r$or),
                              NROW(sum_r$friend),
                              NROW(sum_r$neighbor),
                              NROW(sum_r$acq),
                              NROW(sum_r$asha),
                              NROW(sum_r$anm),
                              NROW(sum_r$aww),
                              NROW(sum_r$oth)))

# round numbers
stat_r %<>% mutate(mean = mean %>% round(2),
                     sd = sd %>% round(2))

# write
write.csv(stat_r, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_freely_personal_by_ego_relationship.csv',
          row.names = F)

# calculate overall by category
overall_r <- df %>% tabyl(relationship)

# write
write.csv(overall_r, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_freely_personal_overall_relationship.csv',
          row.names = F)

###############################################
### TALK TO OR GET INFO ABOUT FP AND ADVICE ###
###############################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     advice1r, 
                     advice2r, 
                     advice3r, 
                     advice4r, 
                     advice5r)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'relationship',
             na.rm = T)

# create relationship category column
df %<>% mutate(rela_cat = relationship)

# categorize relationships
df %<>% mutate(rela_cat = str_replace(rela_cat, '.*Husband.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Mother .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Father .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Mother-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Father-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Brother-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Sister-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Brother .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Sister .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Other_Relative.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Friend.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Neighbor.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Acquaintance.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*ASHA.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*ANM.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*AWW.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Other.*', 'FNAO'))

# rename general relationships
df %<>% mutate(relationship = str_replace(relationship, '.*Husband.*', 'Husband')) %>%
  mutate(relationship = str_replace(relationship, '.*Mother-in-law.*', 'Mother-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Father-in-law.*', 'Father-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Brother-in-law.*', 'Brother-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Sister-in-law.*', 'Sister-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Mother .*', 'Mother')) %>%
  mutate(relationship = str_replace(relationship, '.*Father .*', 'Father')) %>%
  mutate(relationship = str_replace(relationship, '.*Brother .*', 'Brother')) %>%
  mutate(relationship = str_replace(relationship, '.*Sister .*', 'Sister')) %>%
  mutate(relationship = str_replace(relationship, '.*Other_Relative.*', 'Other-relative')) %>%
  mutate(relationship = str_replace(relationship, '.*Friend.*', 'Friend')) %>%
  mutate(relationship = str_replace(relationship, '.*Neighbor.*', 'Neighbor')) %>%
  mutate(relationship = str_replace(relationship, '.*Acquaintance.*', 'Acquaintance')) %>%
  mutate(relationship = str_replace(relationship, '.*ASHA.*', 'Asha')) %>%
  mutate(relationship = str_replace(relationship, '.*ANM.*', 'Anm')) %>%
  mutate(relationship = str_replace(relationship, '.*AWW.*', 'Aww'))

# calculate count table by ego and category
sum_cat <- df %>% group_by(woman_id) %>% summarise(total = sum(is.na(rela_cat) == F),
                                                   fam = sum(rela_cat == 'FAM'),
                                                   fnao = sum(rela_cat == 'FNAO'),
                                                   hw = sum(rela_cat == 'HW'))

# calculate summary stats table
stat_cat <- tibble(variable = c('Total',
                                'Family',
                                'Friend_Neighbour_Acquaintance_Other',
                                'Health_Worker'),
                   mean = c(mean(sum_cat$total),
                            mean(sum_cat$fam),
                            mean(sum_cat$fnao),
                            mean(sum_cat$hw)),
                   sd = c(sd(sum_cat$total),
                          sd(sum_cat$fam),
                          sd(sum_cat$fnao),
                          sd(sum_cat$hw)),
                   med = c(median(sum_cat$total),
                           median(sum_cat$fam),
                           median(sum_cat$fnao),
                           median(sum_cat$hw)),
                   min = c(min(sum_cat$total),
                           min(sum_cat$fam),
                           min(sum_cat$fnao),
                           min(sum_cat$hw)),
                   max = c(max(sum_cat$total),
                           max(sum_cat$fam),
                           max(sum_cat$fnao),
                           max(sum_cat$hw)),
                   n_egos = c(NROW(na.omit(sum_cat$total)),
                              NROW(na.omit(sum_cat$fam)),
                              NROW(na.omit(sum_cat$fnao)),
                              NROW(na.omit(sum_cat$hw))))

# round numbers
stat_cat %<>% mutate(mean = mean %>% round(2),
                     sd = sd %>% round(2))

# write
write.csv(stat_cat, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_advice_fp_by_ego_category.csv',
          row.names = F)

# calculate overall by category
overall_cat <- df %>% tabyl(rela_cat)

# write
write.csv(overall_cat, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_advice_fp_overall_category.csv',
          row.names = F)

# now by relationship
# calculate count table by ego and relationship
sum_r <- df %>% group_by(woman_id) %>% summarise(husband = sum(relationship == 'Husband'),
                                                 mother = sum(relationship == 'Mother'),
                                                 father = sum(relationship == 'Father'),
                                                 mil = sum(relationship == 'Mother-in-law'),
                                                 fil = sum(relationship == 'Father-in-law'),
                                                 bil = sum(relationship == 'Brother-in-law'),
                                                 sil = sum(relationship == 'Sister-in-law'),
                                                 brother = sum(relationship == 'Brother'),
                                                 sister = sum(relationship == 'Sister'),
                                                 or = sum(relationship == 'Other-relative'),
                                                 friend = sum(relationship == 'Friend'),
                                                 neighbor = sum(relationship == 'Neighbor'),
                                                 acq = sum(relationship == 'Acquaintance'),
                                                 asha = sum(relationship == 'Asha'),
                                                 anm = sum(relationship == 'Anm'),
                                                 aww = sum(relationship == 'Aww'),
                                                 oth = sum(relationship == 'Other'))

# calculate summary stats table
stat_r <- tibble(variable = c('Husband',
                              'Mother',
                              'Father',
                              'Mother-in-law',
                              'Father-in-law',
                              'Brother-in-law',
                              'Sister-in-law',
                              'Brother',
                              'Sister',
                              'Other-relative',
                              'Friend',
                              'Neighbor',
                              'Acquaintance',
                              'Asha',
                              'Anm',
                              'Aww',
                              'Other'),
                 mean = c(mean(sum_r$husband),
                          mean(sum_r$mother),
                          mean(sum_r$father),
                          mean(sum_r$mil),
                          mean(sum_r$fil),
                          mean(sum_r$bil),
                          mean(sum_r$sil),
                          mean(sum_r$brother),
                          mean(sum_r$sister),
                          mean(sum_r$or),
                          mean(sum_r$friend),
                          mean(sum_r$neighbor),
                          mean(sum_r$acq),
                          mean(sum_r$asha),
                          mean(sum_r$anm),
                          mean(sum_r$aww),
                          mean(sum_r$oth)),
                 sd = c(sd(sum_r$husband),
                        sd(sum_r$mother),
                        sd(sum_r$father),
                        sd(sum_r$mil),
                        sd(sum_r$fil),
                        sd(sum_r$bil),
                        sd(sum_r$sil),
                        sd(sum_r$brother),
                        sd(sum_r$sister),
                        sd(sum_r$or),
                        sd(sum_r$friend),
                        sd(sum_r$neighbor),
                        sd(sum_r$acq),
                        sd(sum_r$asha),
                        sd(sum_r$anm),
                        sd(sum_r$aww),
                        sd(sum_r$oth)),
                 med = c(median(sum_r$husband),
                         median(sum_r$mother),
                         median(sum_r$father),
                         median(sum_r$mil),
                         median(sum_r$fil),
                         median(sum_r$bil),
                         median(sum_r$sil),
                         median(sum_r$brother),
                         median(sum_r$sister),
                         median(sum_r$or),
                         median(sum_r$friend),
                         median(sum_r$neighbor),
                         median(sum_r$acq),
                         median(sum_r$asha),
                         median(sum_r$anm),
                         median(sum_r$aww),
                         median(sum_r$oth)),
                 min = c(min(sum_r$husband),
                         min(sum_r$mother),
                         min(sum_r$father),
                         min(sum_r$mil),
                         min(sum_r$fil),
                         min(sum_r$bil),
                         min(sum_r$sil),
                         min(sum_r$brother),
                         min(sum_r$sister),
                         min(sum_r$or),
                         min(sum_r$friend),
                         min(sum_r$neighbor),
                         min(sum_r$acq),
                         min(sum_r$asha),
                         min(sum_r$anm),
                         min(sum_r$aww),
                         min(sum_r$oth)),
                 max = c(max(sum_r$husband),
                         max(sum_r$mother),
                         max(sum_r$father),
                         max(sum_r$mil),
                         max(sum_r$fil),
                         max(sum_r$bil),
                         max(sum_r$sil),
                         max(sum_r$brother),
                         max(sum_r$sister),
                         max(sum_r$or),
                         max(sum_r$friend),
                         max(sum_r$neighbor),
                         max(sum_r$acq),
                         max(sum_r$asha),
                         max(sum_r$anm),
                         max(sum_r$aww),
                         max(sum_r$oth)),
                 n_egos = c(NROW(sum_r$husband),
                            NROW(sum_r$mother),
                            NROW(sum_r$father),
                            NROW(sum_r$mil),
                            NROW(sum_r$fil),
                            NROW(sum_r$bil),
                            NROW(sum_r$sil),
                            NROW(sum_r$brother),
                            NROW(sum_r$sister),
                            NROW(sum_r$or),
                            NROW(sum_r$friend),
                            NROW(sum_r$neighbor),
                            NROW(sum_r$acq),
                            NROW(sum_r$asha),
                            NROW(sum_r$anm),
                            NROW(sum_r$aww),
                            NROW(sum_r$oth)))

# round numbers
stat_r %<>% mutate(mean = mean %>% round(2),
                   sd = sd %>% round(2))

# write
write.csv(stat_r, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_advice_fp_by_ego_relationship.csv',
          row.names = F)

# calculate overall by category
overall_r <- df %>% tabyl(relationship)

# write
write.csv(overall_r, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_advice_fp_overall_relationship.csv',
          row.names = F)

############################
### VALUE OPINIONS VIEWS ###
############################

# create df with data needed
df <- ego %>% select(woman_id, 
                     opinion1r, 
                     opinion2r, 
                     opinion3r, 
                     opinion4r, 
                     opinion5r)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'relationship',
             na.rm = T)

# create relationship category column
df %<>% mutate(rela_cat = relationship)

# categorize relationships
df %<>% mutate(rela_cat = str_replace(rela_cat, '.*Husband.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Mother .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Father .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Mother-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Father-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Brother-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Sister-in-law.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Brother .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Sister .*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Other_Relative.*', 'FAM')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Friend.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Neighbor.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Acquaintance.*', 'FNAO')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*ASHA.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*ANM.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*AWW.*', 'HW')) %>%
  mutate(rela_cat = str_replace(rela_cat, '.*Other.*', 'FNAO'))

# rename general relationships
df %<>% mutate(relationship = str_replace(relationship, '.*Husband.*', 'Husband')) %>%
  mutate(relationship = str_replace(relationship, '.*Mother-in-law.*', 'Mother-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Father-in-law.*', 'Father-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Brother-in-law.*', 'Brother-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Sister-in-law.*', 'Sister-in-law')) %>%
  mutate(relationship = str_replace(relationship, '.*Mother .*', 'Mother')) %>%
  mutate(relationship = str_replace(relationship, '.*Father .*', 'Father')) %>%
  mutate(relationship = str_replace(relationship, '.*Brother .*', 'Brother')) %>%
  mutate(relationship = str_replace(relationship, '.*Sister .*', 'Sister')) %>%
  mutate(relationship = str_replace(relationship, '.*Other_Relative.*', 'Other-relative')) %>%
  mutate(relationship = str_replace(relationship, '.*Friend.*', 'Friend')) %>%
  mutate(relationship = str_replace(relationship, '.*Neighbor.*', 'Neighbor')) %>%
  mutate(relationship = str_replace(relationship, '.*Acquaintance.*', 'Acquaintance')) %>%
  mutate(relationship = str_replace(relationship, '.*ASHA.*', 'Asha')) %>%
  mutate(relationship = str_replace(relationship, '.*ANM.*', 'Anm')) %>%
  mutate(relationship = str_replace(relationship, '.*AWW.*', 'Aww'))

# calculate count table by ego and category
sum_cat <- df %>% group_by(woman_id) %>% summarise(total = sum(is.na(rela_cat) == F),
                                                   fam = sum(rela_cat == 'FAM'),
                                                   fnao = sum(rela_cat == 'FNAO'),
                                                   hw = sum(rela_cat == 'HW'))

# calculate summary stats table
stat_cat <- tibble(variable = c('Total',
                                'Family',
                                'Friend_Neighbour_Acquaintance_Other',
                                'Health_Worker'),
                   mean = c(mean(sum_cat$total),
                            mean(sum_cat$fam),
                            mean(sum_cat$fnao),
                            mean(sum_cat$hw)),
                   sd = c(sd(sum_cat$total),
                          sd(sum_cat$fam),
                          sd(sum_cat$fnao),
                          sd(sum_cat$hw)),
                   med = c(median(sum_cat$total),
                           median(sum_cat$fam),
                           median(sum_cat$fnao),
                           median(sum_cat$hw)),
                   min = c(min(sum_cat$total),
                           min(sum_cat$fam),
                           min(sum_cat$fnao),
                           min(sum_cat$hw)),
                   max = c(max(sum_cat$total),
                           max(sum_cat$fam),
                           max(sum_cat$fnao),
                           max(sum_cat$hw)),
                   n_egos = c(NROW(na.omit(sum_cat$total)),
                              NROW(na.omit(sum_cat$fam)),
                              NROW(na.omit(sum_cat$fnao)),
                              NROW(na.omit(sum_cat$hw))))

# round numbers
stat_cat %<>% mutate(mean = mean %>% round(2),
                     sd = sd %>% round(2))

# write
write.csv(stat_cat, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_value_opinion_by_ego_category.csv',
          row.names = F)

# calculate overall by category
overall_cat <- df %>% tabyl(rela_cat)

# write
write.csv(overall_cat, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_value_opinion_overall_category.csv',
          row.names = F)

# now by relationship
# calculate count table by ego and relationship
sum_r <- df %>% group_by(woman_id) %>% summarise(husband = sum(relationship == 'Husband'),
                                                 mother = sum(relationship == 'Mother'),
                                                 father = sum(relationship == 'Father'),
                                                 mil = sum(relationship == 'Mother-in-law'),
                                                 fil = sum(relationship == 'Father-in-law'),
                                                 bil = sum(relationship == 'Brother-in-law'),
                                                 sil = sum(relationship == 'Sister-in-law'),
                                                 brother = sum(relationship == 'Brother'),
                                                 sister = sum(relationship == 'Sister'),
                                                 or = sum(relationship == 'Other-relative'),
                                                 friend = sum(relationship == 'Friend'),
                                                 neighbor = sum(relationship == 'Neighbor'),
                                                 acq = sum(relationship == 'Acquaintance'),
                                                 asha = sum(relationship == 'Asha'),
                                                 anm = sum(relationship == 'Anm'),
                                                 aww = sum(relationship == 'Aww'),
                                                 oth = sum(relationship == 'Other'))

# calculate summary stats table
stat_r <- tibble(variable = c('Husband',
                              'Mother',
                              'Father',
                              'Mother-in-law',
                              'Father-in-law',
                              'Brother-in-law',
                              'Sister-in-law',
                              'Brother',
                              'Sister',
                              'Other-relative',
                              'Friend',
                              'Neighbor',
                              'Acquaintance',
                              'Asha',
                              'Anm',
                              'Aww',
                              'Other'),
                 mean = c(mean(sum_r$husband),
                          mean(sum_r$mother),
                          mean(sum_r$father),
                          mean(sum_r$mil),
                          mean(sum_r$fil),
                          mean(sum_r$bil),
                          mean(sum_r$sil),
                          mean(sum_r$brother),
                          mean(sum_r$sister),
                          mean(sum_r$or),
                          mean(sum_r$friend),
                          mean(sum_r$neighbor),
                          mean(sum_r$acq),
                          mean(sum_r$asha),
                          mean(sum_r$anm),
                          mean(sum_r$aww),
                          mean(sum_r$oth)),
                 sd = c(sd(sum_r$husband),
                        sd(sum_r$mother),
                        sd(sum_r$father),
                        sd(sum_r$mil),
                        sd(sum_r$fil),
                        sd(sum_r$bil),
                        sd(sum_r$sil),
                        sd(sum_r$brother),
                        sd(sum_r$sister),
                        sd(sum_r$or),
                        sd(sum_r$friend),
                        sd(sum_r$neighbor),
                        sd(sum_r$acq),
                        sd(sum_r$asha),
                        sd(sum_r$anm),
                        sd(sum_r$aww),
                        sd(sum_r$oth)),
                 med = c(median(sum_r$husband),
                         median(sum_r$mother),
                         median(sum_r$father),
                         median(sum_r$mil),
                         median(sum_r$fil),
                         median(sum_r$bil),
                         median(sum_r$sil),
                         median(sum_r$brother),
                         median(sum_r$sister),
                         median(sum_r$or),
                         median(sum_r$friend),
                         median(sum_r$neighbor),
                         median(sum_r$acq),
                         median(sum_r$asha),
                         median(sum_r$anm),
                         median(sum_r$aww),
                         median(sum_r$oth)),
                 min = c(min(sum_r$husband),
                         min(sum_r$mother),
                         min(sum_r$father),
                         min(sum_r$mil),
                         min(sum_r$fil),
                         min(sum_r$bil),
                         min(sum_r$sil),
                         min(sum_r$brother),
                         min(sum_r$sister),
                         min(sum_r$or),
                         min(sum_r$friend),
                         min(sum_r$neighbor),
                         min(sum_r$acq),
                         min(sum_r$asha),
                         min(sum_r$anm),
                         min(sum_r$aww),
                         min(sum_r$oth)),
                 max = c(max(sum_r$husband),
                         max(sum_r$mother),
                         max(sum_r$father),
                         max(sum_r$mil),
                         max(sum_r$fil),
                         max(sum_r$bil),
                         max(sum_r$sil),
                         max(sum_r$brother),
                         max(sum_r$sister),
                         max(sum_r$or),
                         max(sum_r$friend),
                         max(sum_r$neighbor),
                         max(sum_r$acq),
                         max(sum_r$asha),
                         max(sum_r$anm),
                         max(sum_r$aww),
                         max(sum_r$oth)),
                 n_egos = c(NROW(sum_r$husband),
                            NROW(sum_r$mother),
                            NROW(sum_r$father),
                            NROW(sum_r$mil),
                            NROW(sum_r$fil),
                            NROW(sum_r$bil),
                            NROW(sum_r$sil),
                            NROW(sum_r$brother),
                            NROW(sum_r$sister),
                            NROW(sum_r$or),
                            NROW(sum_r$friend),
                            NROW(sum_r$neighbor),
                            NROW(sum_r$acq),
                            NROW(sum_r$asha),
                            NROW(sum_r$anm),
                            NROW(sum_r$aww),
                            NROW(sum_r$oth)))

# round numbers
stat_r %<>% mutate(mean = mean %>% round(2),
                   sd = sd %>% round(2))

# write
write.csv(stat_r, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_value_opinion_by_ego_relationship.csv',
          row.names = F)

# calculate overall by category
overall_r <- df %>% tabyl(relationship)

# write
write.csv(overall_r, 
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_value_opinion_overall_relationship.csv',
          row.names = F)

##########################
### PLACE OF RESIDENCE ###
##########################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_residence, 
                     alter2_residence, 
                     alter3_residence, 
                     alter4_residence, 
                     alter5_residence)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'residence',
             na.rm = T)

# check unique values
unique(df$residence)

# recode
df %<>% mutate(residence = recode(residence,
                                  `1` = 'Same HH',
                                  `2` = 'Same Village',
                                  `3` = 'Another Village within same District',
                                  `4` = 'Outside this District'))

# write table
write.csv(df %>% tabyl(residence),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_residence.csv',
          row.names = F)

######################
### MODE OF TRAVEL ###
######################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_transport, 
                     alter2_transport, 
                     alter3_transport, 
                     alter4_transport, 
                     alter5_transport)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                                  `1` = 'Walking',
                                  `2` = 'Bicycle',
                                  `3` = 'Rickshaw',
                                  `4` = 'Autorickshaw or other motor vehicle',
                                  `5` = 'Do not travel, only talk over phone'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_mode_travel.csv',
          row.names = F)

######################
### TIME OF TRAVEL ###
######################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_transport_time, 
                     alter2_transport_time, 
                     alter3_transport_time, 
                     alter4_transport_time, 
                     alter5_transport_time)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# change to numeric
df$value <- as.numeric(df$value)

# write table
# write table
write.csv(tibble(mean_time = mean(df$value),
                 sd_time = sd(df$value),
                 med_time = median(df$value),
                 n = NROW(df)),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_travel_time.csv',
          row.names = F)

####################
### TALKING MODE ###
####################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_talk_method, 
                     alter2_talk_method, 
                     alter3_talk_method, 
                     alter4_talk_method, 
                     alter5_talk_method)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Mainly in person',
                              `2` = 'Mainly telephone',
                              `3` = 'Both'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_method.csv',
          row.names = F)

########################################
### TALKING FREQ VS TALKING ABOUT FP ###
########################################

# create df1 with data needed
df1 <- ego %>% select(woman_id, 
                     alter1_talk_freq, 
                     alter2_talk_freq, 
                     alter3_talk_freq, 
                     alter4_talk_freq, 
                     alter5_talk_freq)

# melt df
df1 %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = F)

# check unique values
unique(df1$value)

# recode
df1 %<>% mutate(value = recode(value,
                              `1` = 'Daily',
                              `2` = 'At least 3x/week',
                              `3` = '1x/week',
                              `4` = '1x/month',
                              `5` = '1x/every 3 months',
                              `6` = '1x/every 6 months',
                              `7` = '1x/a year'))

# create df2 with data needed
df2 <- ego %>% select(woman_id, 
                      alter1_freq_talk_fp, 
                      alter2_freq_talk_fp, 
                      alter3_freq_talk_fp, 
                      alter4_freq_talk_fp, 
                      alter5_freq_talk_fp)

# melt df
df2 %<>% melt(id.vars = 'woman_id',
              value.name = 'value',
              na.rm = F)

# check unique values
unique(df2$value)

# recode
df2 %<>% mutate(value = recode(value,
                               `1` = 'At least once a week',
                               `2` = 'At least once a month',
                               `3` = 'At least once in 6 months',
                               `4` = 'At least once a year',
                               `5` = 'At least once in a few years'))

# check woman ids match df1 and df2
sum(df1$woman_id == df2$woman_id)

# merge df
df <- tibble(talk_freq = df1$value,
             talk_freq_fp = df2$value)

# remove missing values
df <- na.omit(df)

# write cross tab
tab <- df %>% tabyl(talk_freq, talk_freq_fp) %>%
  adorn_totals(c("col", "row"), fill = "-", na.rm = TRUE, name = "Total")

write.csv(tab,
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_freq_vs_talk_freq_fp.csv',
          row.names = F)

#################################
### LEARNED FROM THEIR ALTERS ###
#################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_learned, 
                     alter2_learned, 
                     alter3_learned, 
                     alter4_learned, 
                     alter5_learned)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# count total number of things
df$sum_things_learned <- str_count(df$value, ',') + 1

# write total
write.csv(df %>% tabyl(sum_things_learned),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_sum_things_learned.csv',
          row.names = F)

# table of individual things
tab <- tibble(variable = c('Get along with family/in-laws',
                           'Ways of earning',
                           'To take care of children',
                           'To get health care',
                           'To apply for government schemes',
                           'When and how many children to have',
                           'About FP methods',
                           'Others'),
              learned_n = c(sum(str_detect(df$value, 'A')),
                            sum(str_detect(df$value, 'B')),
                            sum(str_detect(df$value, 'C')),
                            sum(str_detect(df$value, 'D')),
                            sum(str_detect(df$value, 'E')),
                            sum(str_detect(df$value, 'F')),
                            sum(str_detect(df$value, 'G')),
                            sum(str_detect(df$value, 'X'))),
              n = NROW(df),
              learned_perc = c(sum(str_detect(df$value, 'A'))/NROW(df),
                               sum(str_detect(df$value, 'B'))/NROW(df),
                               sum(str_detect(df$value, 'C'))/NROW(df),
                               sum(str_detect(df$value, 'D'))/NROW(df),
                               sum(str_detect(df$value, 'E'))/NROW(df),
                               sum(str_detect(df$value, 'F'))/NROW(df),
                               sum(str_detect(df$value, 'G'))/NROW(df),
                               sum(str_detect(df$value, 'X'))/NROW(df)))

# round
tab %<>% mutate(learned_perc = learned_perc %>% round(2))

# write table
write.csv(tab,
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_things_learned.csv',
          row.names = F)

###############################
### TALKED ABOUT FP METHODS ###
###############################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_preg, 
                     alter2_preg, 
                     alter3_preg, 
                     alter4_preg, 
                     alter5_preg)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                               `1` = 'Yes talked',
                               `2` = 'Yes heard',
                               `3` = 'No'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talked_fp_methods.csv',
          row.names = F)

############################
### FP METHODS DISCUSSED ###
############################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_preg_methods, 
                     alter2_preg_methods, 
                     alter3_preg_methods, 
                     alter4_preg_methods, 
                     alter5_preg_methods)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# count total number of things
df$sum_fp_methods_discussed <- str_count(df$value, ',') + 1

# write total
write.csv(df %>% tabyl(sum_fp_methods_discussed),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_sum_fp_methods_discussed.csv',
          row.names = F)

# table of individual things
tab <- tibble(variable = c('Female sterilization',
                           'Male sterilization',
                           'IUD',
                           'Pills (daily, weekly)',
                           'ECP',
                           'Condom',
                           'Injectable',
                           'LAM',
                           'SDM',
                           'Rhythm',
                           'Withdrawal',
                           'Others'),
              discussed_n = c(sum(str_detect(df$value, 'A')),
                            sum(str_detect(df$value, 'B')),
                            sum(str_detect(df$value, 'C')),
                            sum(str_detect(df$value, 'D')),
                            sum(str_detect(df$value, 'E')),
                            sum(str_detect(df$value, 'F')),
                            sum(str_detect(df$value, 'G')),
                            sum(str_detect(df$value, 'H')),
                            sum(str_detect(df$value, 'I')),
                            sum(str_detect(df$value, 'J')),
                            sum(str_detect(df$value, 'K')),
                            sum(str_detect(df$value, 'X'))),
              n = NROW(df),
              discussed_perc = c(sum(str_detect(df$value, 'A'))/NROW(df),
                               sum(str_detect(df$value, 'B'))/NROW(df),
                               sum(str_detect(df$value, 'C'))/NROW(df),
                               sum(str_detect(df$value, 'D'))/NROW(df),
                               sum(str_detect(df$value, 'E'))/NROW(df),
                               sum(str_detect(df$value, 'F'))/NROW(df),
                               sum(str_detect(df$value, 'G'))/NROW(df),
                               sum(str_detect(df$value, 'H'))/NROW(df),
                               sum(str_detect(df$value, 'I'))/NROW(df),
                               sum(str_detect(df$value, 'J'))/NROW(df),
                               sum(str_detect(df$value, 'K'))/NROW(df),
                               sum(str_detect(df$value, 'X'))/NROW(df)))

# round
tab %<>% mutate(discussed_perc = discussed_perc %>% round(2))

# write table
write.csv(tab,
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_fp_methods_discussed.csv',
          row.names = F)

############################
### INFO WHERE TO GET FP ###
############################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_get_fp, 
                     alter2_get_fp, 
                     alter3_get_fp, 
                     alter4_get_fp, 
                     alter5_get_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know/do not remember'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_where_get_fp.csv',
          row.names = F)

############################
### INFO SIDE EFFECTS FP ###
############################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_sideeff_fp, 
                     alter2_sideeff_fp, 
                     alter3_sideeff_fp, 
                     alter4_sideeff_fp, 
                     alter5_sideeff_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know/do not remember'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_side_effects_fp.csv',
          row.names = F)

###################
### TALK TOPICS ###
###################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_subjects, 
                     alter2_subjects, 
                     alter3_subjects, 
                     alter4_subjects, 
                     alter5_subjects)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# count total number of things
df$sum_talk_topics <- str_count(df$value, ',') + 1

# write total
write.csv(df %>% tabyl(sum_talk_topics),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_sum_talk_topics.csv',
          row.names = F)

# table of individual things
tab <- tibble(variable = c('Family/household matters',
                           'Work/earning money',
                           'Health related matters',
                           'Family Planning',
                           'Financial matters',
                           'Government schemes',
                           'Politics',
                           'Childrens care/schooling/health',
                           'Other'),
              topic_n = c(sum(str_detect(df$value, 'A')),
                            sum(str_detect(df$value, 'B')),
                            sum(str_detect(df$value, 'C')),
                            sum(str_detect(df$value, 'D')),
                            sum(str_detect(df$value, 'E')),
                            sum(str_detect(df$value, 'F')),
                            sum(str_detect(df$value, 'G')),
                            sum(str_detect(df$value, 'H')),
                            sum(str_detect(df$value, 'X'))),
              n = NROW(df),
              topic_perc = c(sum(str_detect(df$value, 'A'))/NROW(df),
                               sum(str_detect(df$value, 'B'))/NROW(df),
                               sum(str_detect(df$value, 'C'))/NROW(df),
                               sum(str_detect(df$value, 'D'))/NROW(df),
                               sum(str_detect(df$value, 'E'))/NROW(df),
                               sum(str_detect(df$value, 'F'))/NROW(df),
                               sum(str_detect(df$value, 'G'))/NROW(df),
                               sum(str_detect(df$value, 'H'))/NROW(df),
                               sum(str_detect(df$value, 'X'))/NROW(df)))

# round
tab %<>% mutate(topic_perc = topic_perc %>% round(2))

# write table
write.csv(tab,
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talk_topics.csv',
          row.names = F)

################################
### TALKED NUM CHILDREN HAVE ###
################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_howmany_child, 
                     alter2_howmany_child, 
                     alter3_howmany_child, 
                     alter4_howmany_child, 
                     alter5_howmany_child)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know/do not remember'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_talked_num_children_have.csv',
          row.names = F)

#################################
### DISCUSS FP METHODS FREELY ###
#################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_discuss_fp, 
                     alter2_discuss_fp, 
                     alter3_discuss_fp, 
                     alter4_discuss_fp, 
                     alter5_discuss_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_discuss_fp_freely.csv',
          row.names = F)

#####################
### FP ENCOURAGED ###
#####################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_encourage_fp, 
                     alter2_encourage_fp, 
                     alter3_encourage_fp, 
                     alter4_encourage_fp, 
                     alter5_encourage_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Encouraged to use FP methods',
                              `2` = 'Discouraged to use FP methods',
                              `3` = 'Neither encouraged nor discouraged'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_fp_encouraged.csv',
          row.names = F)

################################
### SUPPORT NO CHILD 2 YEARS ###
################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_nochild, 
                     alter2_nochild, 
                     alter3_nochild, 
                     alter4_nochild, 
                     alter5_nochild)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_support_no_child_2_years.csv',
          row.names = F)

#########################
### SAY BAD THINGS FP ###
#########################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_approve_fp, 
                     alter2_approve_fp, 
                     alter3_approve_fp, 
                     alter4_approve_fp, 
                     alter5_approve_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_say_bad_things_fp.csv',
          row.names = F)

##################################
### NUMBER OF CURRENT CHILDREN ###
##################################

# sons
df1 <- ego %>% select(woman_id, 
                     alter1_sons, 
                     alter2_sons, 
                     alter3_sons, 
                     alter4_sons, 
                     alter5_sons)

# melt df
df1 %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = F)

# check unique values
unique(df1$value)

# change to numeric
df1$value <- as.numeric(df1$value)

# set 99 to NA
df1$value[df1$value == 99] <- NA

# daughters
df2 <- ego %>% select(woman_id, 
                      alter1_daughters, 
                      alter2_daughters, 
                      alter3_daughters, 
                      alter4_daughters, 
                      alter5_daughters)

# melt df
df2 %<>% melt(id.vars = 'woman_id',
              value.name = 'value',
              na.rm = F)

# check unique values
unique(df2$value)

# change to numeric
df2$value <- as.numeric(df2$value)

# set 99 to NA
df2$value[df2$value == 99] <- NA

# cbind dataframes
df <- tibble(sons = df1$value,
             daughters = df2$value)

# add children
df$children <- rowSums(df, na.rm = F)

# drop missing rows
df <- na.omit(df)

# create table
tab <- tibble(mean_sons = mean(df$sons),
              sd_sons = sd(df$sons),
              med_sons = median(df$sons),
              mean_daughters = mean(df$daughters),
              sd_daughters = sd(df$daughters),
              med_daughters = median(df$daughters),
              mean_children = mean(df$children),
              sd_children = sd(df$children),
              med_children = median(df$children),
              n = NROW(df))

tab %<>% round(2)

# write table
write.csv(tab,
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_num_current_children.csv',
          row.names = F)

#######################################
### NUMBER OF MORE CHILDREN DESIRED ###
#######################################

# sons
df1 <- ego %>% select(woman_id, 
                      alter1_future_sons, 
                      alter2_future_sons, 
                      alter3_future_sons, 
                      alter4_future_sons, 
                      alter5_future_sons)

# melt df
df1 %<>% melt(id.vars = 'woman_id',
              value.name = 'value',
              na.rm = F)

# check unique values
unique(df1$value)

# change to numeric
df1$value <- as.numeric(df1$value)

# set 99 to NA
df1$value[df1$value == 99] <- NA

# daughters
df2 <- ego %>% select(woman_id, 
                      alter1_future_daughters, 
                      alter2_future_daughters, 
                      alter3_future_daughters, 
                      alter4_future_daughters, 
                      alter5_future_daughters)

# melt df
df2 %<>% melt(id.vars = 'woman_id',
              value.name = 'value',
              na.rm = F)

# check unique values
unique(df2$value)

# change to numeric
df2$value <- as.numeric(df2$value)

# set 99 to NA
df2$value[df2$value == 99] <- NA

# cbind dataframes
df <- tibble(sons = df1$value,
             daughters = df2$value)

# add children
df3 <- ego %>% select(woman_id, 
                      alter1_future_children, 
                      alter2_future_children, 
                      alter3_future_children, 
                      alter4_future_children, 
                      alter5_future_children)

# melt df
df3 %<>% melt(id.vars = 'woman_id',
              value.name = 'value',
              na.rm = F)

# check unique values
unique(df3$value)

# change to numeric
df3$value <- as.numeric(df3$value)

# set 99 to NA
#df2$value[df2$value == 99] <- NA

# cbind dataframes
df <- cbind(df, tibble(children = df3$value))

# drop missing rows
#df <- na.omit(df)

# create table
tab <- tibble(mean_fut_sons = mean(df$sons, na.rm = T),
              sd_fu_sons = sd(df$sons, na.rm = T),
              med_fu_sons = median(df$sons, na.rm = T),
              n_fu_sons = NROW(df[is.na(df$daughters) == F,]),
              mean_fu_daughters = mean(df$daughters, na.rm = T),
              sd_fu_daughters = sd(df$daughters, na.rm = T),
              med_fu_daughters = median(df$daughters, na.rm = T),
              n_fu_daughters = NROW(df[is.na(df$daughters) == F,]),
              mean_fu_children = mean(df$children[is.na(df$children) == F & !(df$children %in% c(97,98,99))], na.rm = T),
              sd_fu_children = sd(df$children[is.na(df$children) == F & !(df$children %in% c(97,98,99))], na.rm = T),
              med_fu_children = median(df$children[is.na(df$children) == F & !(df$children %in% c(97,98,99))], na.rm = T),
              n_fu_children = NROW(df[is.na(df$children) == F & !(df$children %in% c(97,98,99)),]),
              n_wants_children_dont_know_how_many = length(df$children[df$children %in% 97]),
              doesnt_want_more_children = length(df$children[df$children %in% 98]),
              dont_know = length(df$children[df$children %in% 99]),
              n_total = NROW(df[is.na(df$daughters) == F,]) + NROW(df[is.na(df$children) == F,]))

tab %<>% round(2)

# write table
write.csv(tab,
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_num_future_children_some_issues_w_question.csv',
          row.names = F)

################
### USING FP ###
################

# PC ego values
# 1 = Yes, 2 = No

# alt values
# 1 = Yes, 2 = No, 9 = Don't Know

# sort ego data and PC data by woman ID so can merge
ego %<>% arrange(woman_id)
ego_pc %<>% arrange(qe6) # qe6 is woman_id

# make sure ego_ids match
sum(ego_pc$qe6 == ego$woman_id)

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_using_fp, 
                     alter2_using_fp, 
                     alter3_using_fp, 
                     alter4_using_fp, 
                     alter5_using_fp)

df_age <- ego %>% select(woman_id, 
                         alter1_age, 
                         alter2_age, 
                         alter3_age, 
                         alter4_age, 
                         alter5_age)

df_r <- ego %>% select(woman_id, 
                         alter1r, 
                       alter2r, 
                       alter3r, 
                       alter4r, 
                       alter5r)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = F)

df_age %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = F)

df_r %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = F)

# set values to numeric
df$value <- as.numeric(df$value)
df_age$value <- as.numeric(df_age$value)

# check unique values
unique(df$value)
unique(df_age$value)
unique(df_r$value)

# replace with ego values if Husband
for(i in 1:NROW(df)){
  # if value is missing
  if(is.na(df$value[i])){
    
    # if alter is husband
    if(is.na(df_r$value[i]) == F){    
      if(str_detect(df_r$value[i], 'Husband')){
      df$value[i] <- ego_pc$pcq311[ego_pc$qe6 == df$woman_id[i]]
    }}

    
    #if alter is old
    if(is.na(df_age$value[i]) == F){ 
    if(df_age$value[i] > 49){
      df$value[i] <- 98
    }
    }
  }
}

# change back to character
df$value <- as.character(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know',
                              `98` = 'Alter Older than 49'))

# drop missing values
df <- na.omit(df)

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_currently_using_fp.csv',
          row.names = F)

# save df values because we need them for ever using fp
using_fp <- df

####################
### EVER USED FP ###
####################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_used_fp, 
                     alter2_used_fp, 
                     alter3_used_fp, 
                     alter4_used_fp, 
                     alter5_used_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = F)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

#change using_fp variable to used_fp so we can compare directly to df
using_fp$variable <- str_replace(using_fp$variable, 'using_fp', 'used_fp')

# if currently using fp add Yes to answer
for(i in 1:NROW(using_fp)){
  if(using_fp$value[i] == 'Yes'){
    df$value[df$woman_id == using_fp$woman_id[i] & df$variable == using_fp$variable[i]] <- 'Yes' 
  }
}

# drop na for consistency
df <- na.omit(df)

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_ever_used_fp_some_issues_with_question.csv',
          row.names = F)

######################
### ALTER HELP EGO ###
######################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_help, 
                     alter2_help, 
                     alter3_help, 
                     alter4_help, 
                     alter5_help)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_get_help_advice.csv',
          row.names = F)

###########################
### ALTER HELPED BY EGO ###
###########################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_helped, 
                     alter2_helped, 
                     alter3_helped, 
                     alter4_helped, 
                     alter5_helped)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_give_help_advice.csv',
          row.names = F)

####################################
### EASY DIFFICULT FOLLOW ADVICE ###
####################################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_follow_advice, 
                     alter2_follow_advice, 
                     alter3_follow_advice, 
                     alter4_follow_advice, 
                     alter5_follow_advice)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Very difficult',
                              `2` = 'Difficult',
                              `3` = 'Easy',
                              `4` = 'Very easy'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_easy_difficult_follow_advice.csv',
          row.names = F)

#########################
### DISCUSS FP FREELY ###
#########################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_discuss_fp, 
                     alter2_discuss_fp, 
                     alter3_discuss_fp, 
                     alter4_discuss_fp, 
                     alter5_discuss_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_discuss_fp_freely.csv',
          row.names = F)

#########################
### SAY BAD THINGS FP ###
#########################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_approve_fp, 
                     alter2_approve_fp, 
                     alter3_approve_fp, 
                     alter4_approve_fp, 
                     alter5_approve_fp)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_say_bad_things_fp.csv',
          row.names = F)

#####################
### BORROW RUPEES ###
#####################

# create df with data needed
df <- ego %>% select(woman_id, 
                     alter1_borrow, 
                     alter2_borrow, 
                     alter3_borrow, 
                     alter4_borrow, 
                     alter5_borrow)

# melt df
df %<>% melt(id.vars = 'woman_id',
             value.name = 'value',
             na.rm = T)

# check unique values
unique(df$value)

# recode
df %<>% mutate(value = recode(value,
                              `1` = 'Yes',
                              `2` = 'No',
                              `9` = 'Do not know'))

# write table
write.csv(df %>% tabyl(value),
          '/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results/summary_stats/ego_borrow_rupees.csv',
          row.names = F)

