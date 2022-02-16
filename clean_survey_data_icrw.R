# load packages
library(magrittr)
library(egor)
library(janitor)
library(tidyverse)
library(readxl)
library(igraph)

#########################################
### LOAD EGO AND ALTER DATA AND CLEAN ###
#########################################

# load ego and alter cleaned data
ego <- read_excel(path = "data/ego_clean_11012022.xlsx")
alter <-read_excel(path = "data/alter_clean_11012022.xlsx")
# ego <- read.dta13(file = 'data/ego_clean_12012022de.dta', convert.factors = F)
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

#########################################################################################
### ORGANIZE EGO-ALTER NETWORK DATA (EGO SURVEY ONLY) AND IMPORT INTO EGOR AND IGRAPH ###
#########################################################################################

# create tibble of ego attributes (later need to find all the PC survey attr)
ego_df <- as_tibble(ego) %>% 
  select(district_name:contra_neighbour_type_other) %>% 
  add_column(ego_id = ego$woman_id, .before = 1)

# convert all character variables to factor
ego_df %<>% 
  mutate(across(where(is.character), as.factor))

# create empty tibble of alter attributes
alter_attr <- tibble(alter_id = character(),
                     ego_id = character(),
                     alter_num = numeric())

# create empty tibble of alter-alter ties
alter_ties <- tibble(from = character(),
                     to = character(),
                     ego_id = character(),
                     weight = numeric())

# loop through ego IDs and add rows based on alters
for(i in 1:NROW(ego)){
  
  # go through alters and add alter attr
  if(is.na(ego$alter1[i]) == F){
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '1'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 1)
  } 
  if(is.na(ego$alter2[i]) == F){
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '2'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 2)
  } 
  if(is.na(ego$alter3[i]) == F){
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '3'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 3)
  } 
  if(is.na(ego$alter4[i]) == F){
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '4'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 4)
  } 
  if(is.na(ego$alter5[i]) == F){
    alter_attr %<>% add_row(alter_id = str_c(ego$woman_id[i], '5'),
                            ego_id = ego$woman_id[i], 
                            alter_num = 5)
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

# change duplicate ids so we know same person
for(i in 1:NROW(dup_id)){
  alter_attr[alter_attr == dup_id$duplicate_id[i]] <- dup_id$id[i]
  alter_ties[alter_ties == dup_id$duplicate_id[i]] <- dup_id$id[i]
}

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
save(ego_df, alter_attr, gr_list, gr_list_ego, file="data/ego_igraph.rda")

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
                      aalter_num = numeric())

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
                             aalter_num = 1)
  } 
  if(is.na(alter$alter2[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '2'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 2)
  } 
  if(is.na(alter$alter3[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '3'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 3)
  } 
  if(is.na(alter$alter4[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '4'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 4)
  } 
  if(is.na(alter$alter5[i]) == F){
    aalter_attr %<>% add_row(aalter_id = str_c(alter$alter_id[i], '5'),
                             alter_id = alter$alter_id[i], 
                             aalter_num = 5)
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
save(alter_df, aalter_attr, gr_list_a, gr_list_a_alt, file="data/alter_igraph.rda")
