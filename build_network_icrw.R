# this code loads csv data of ego and alter interviews and builds a network object
# using the edgelist method

# load packages
library(tidyverse)
library(igraph)

###########################
### LOAD DATA AND CLEAN ###
###########################

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

#########################################################################################
### ORGANIZE EGO-ALTER NETWORK DATA (EGO SURVEY ONLY) FOR IMPORT INTO EGOR AND IGRAPH ###
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

######################################
### CALCULATE EGO DEGREE CENTALITY ###
######################################

# first calculate number of alters each ego mentioned
ego$deg_cen <- is.na(ego$alter1) + 
  is.na(ego$alter2) + 
  is.na(ego$alter3) + 
  is.na(ego$alter4) + 
  is.na(ego$alter5)

ego$deg_cen <- 5 - ego$deg_cen

# plot basic degree
ggplot(ego, aes(x = deg_cen %>% as.factor)) +
  geom_bar() +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Egos') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
  ggtitle('Number of Alters Listed by Ego')

# plot degree based on district
ggplot(ego, aes(x = deg_cen %>% as.factor, fill = district_name)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Egos', fill = 'District') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
  ggtitle('Number of Alters Listed by Ego (by District)') +
  scale_fill_discrete(breaks = c('Darbhanga', 'WestChamparan'),
                      labels = c('Darbhanga', 'West Champaran'))

# plot degree based on block
ggplot(ego, aes(x = deg_cen %>% as.factor, fill = block_name)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Egos', fill = 'Block') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
  ggtitle('Number of Alters Listed by Ego (by Block)')

# some sort of frequency table more useful here? perc of total egos from block?
ego_deg_block <- ego %>% 
  group_by(block_name, deg_cen) %>% 
  summarize(n = n()) %>%
  mutate(freq = round(n / sum(n), 2))

########################################################
### CALCULATE ALTER DEGREE CENTRALITY IN FIRST LEVEL ###
########################################################

# SHOULD PROBABLY BUILD THIS INTO A NETWORK AND USE iGRAPH FUNCTIONS???

# let's first extract the correct question so easier to look at
alter_know <- ego[,str_detect(colnames(ego), glob2rx('alter?_know?'))]

# set to numeric
alter_know <- sapply(alter_know, as.numeric)

# set "don't know" to zero. both responses labeled 2 and 9
alter_know[alter_know == 2] <- 0
alter_know[alter_know == 9] <- 0

# create new df with only a single column for combinations
alter_know <- alter_know %>% as.data.frame %>% mutate(know12 = alter1_know2 + alter2_know1,
                                                      know13 = alter1_know3 + alter3_know1,
                                                      know14 = alter1_know4 + alter4_know1,
                                                      know15 = alter1_know5 + alter5_know1,
                                                      know23 = alter2_know3 + alter3_know2,
                                                      know24 = alter2_know4 + alter4_know2,
                                                      know25 = alter2_know5 + alter5_know2,
                                                      know34 = alter3_know4 + alter4_know3,
                                                      know35 = alter3_know5 + alter5_know3,
                                                      know45 = alter4_know5 + alter5_know4)

# set na values to -1 so we can calculate sums
alter_know[is.na(alter_know)] <- -1

# calculate total number of degrees for each alter
alter_know <- alter_know %>% mutate(alt1 = (know12 > 0) + (know13 > 0) + (know14 > 0) + (know15 > 0),
                                    alt2 = (know12 > 0) + (know23 > 0) + (know24 > 0) + (know25 > 0),
                                    alt3 = (know13 > 0) + (know23 > 0) + (know34 > 0) + (know35 > 0),
                                    alt4 = (know14 > 0) + (know24 > 0) + (know34 > 0) + (know45 > 0),
                                    alt5 = (know15 > 0) + (know25 > 0) + (know35 > 0) + (know45 > 0))

# add back to ego dataset
ego <- ego %>% add_column(alt1_deg = alter_know$alt1,
                          alt2_deg = alter_know$alt2,
                          alt3_deg = alter_know$alt3,
                          alt4_deg = alter_know$alt4,
                          alt5_deg = alter_know$alt5)

# set to NA if there is no alter
ego$alt1_deg[is.na(ego$alter1)] <- NA
ego$alt2_deg[is.na(ego$alter2)] <- NA
ego$alt3_deg[is.na(ego$alter3)] <- NA
ego$alt4_deg[is.na(ego$alter4)] <- NA
ego$alt5_deg[is.na(ego$alter5)] <- NA

# melt df to see degrees
alter_deg_1 <- melt(ego, id.vars = 'woman_id', measure.vars = c('alt1_deg', 'alt2_deg', 'alt3_deg', 'alt4_deg', 'alt5_deg'))

# plot alter degrees level 1
ggplot(alter_deg_1, aes(x = value %>% as.factor)) +
  geom_bar() +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Alters') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4')) +
  ggtitle('Number of Alter Connections to other Alters')

#########################################################
### CALCULATE ALTER DEGREE CENTRALITY IN SECOND LEVEL ###
#########################################################

# first calculate number of aalters each alter mentioned
alter$deg_cen <- is.na(alter$alter1) + 
  is.na(alter$alter2) + 
  is.na(alter$alter3) + 
  is.na(alter$alter4) + 
  is.na(alter$alter5)

alter$deg_cen <- 5 - alter$deg_cen

# plot basic degree
ggplot(alter, aes(x = deg_cen %>% as.factor)) +
  geom_bar() +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Alters') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
  ggtitle('Number of Aalters Listed by Alter')

# plot degree based on district
ggplot(alter, aes(x = deg_cen %>% as.factor, fill = district_name)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Alters', fill = 'District') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
  ggtitle('Number of Aalters Listed by Alter (by District)')

# plot degree based on block
ggplot(alter, aes(x = deg_cen %>% as.factor, fill = block_name)) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  labs(x = 'Degree', y = 'Number of Alters', fill = 'District') +
  theme(text = element_text(size=20)) +
  scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
  ggtitle('Number of Aalters Listed by Alter (by Block)')

# some sort of frequency table more useful here? erc of total egos from block?
alter_deg_block <- alter %>% 
  group_by(block_name, deg_cen) %>% 
  summarize(n = n()) %>%
  mutate(freq = round(n / sum(n), 2))

##################################
### FIND INTER-ALTER RELATIONS ###
##################################

# not doing this far now

######################
### BUILD EDGELIST ###
######################

# start building edgelist from ego dataset
# allocate ego and alter columns
edgelist = data.frame(ego_id = character(), alter_id = character())

# loop through ego IDs and add edges based on alters
for(i in 1:NROW(ego)){
  if(is.na(ego$alter1[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '1'))
  } 
  if(is.na(ego$alter2[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '2'))
  } 
  if(is.na(ego$alter3[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '3'))
  } 
  if(is.na(ego$alter4[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '4'))
  } 
  if(is.na(ego$alter5[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '5'))
  } 
}

# now loop through alters and add edges
for(i in 1:NROW(alter)){
  if(is.na(alter$alter1[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '1'))
  } 
  if(is.na(alter$alter2[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '2'))
  } 
  if(is.na(alter$alter3[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '3'))
  } 
  if(is.na(alter$alter4[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '4'))
  } 
  if(is.na(alter$alter5[i]) == F){
    edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '5'))
  } 
}

# replace duplicate ids
for(i in 1:NROW(dup_id)){
  edgelist[edgelist == dup_id$duplicate_id[i]] <- dup_id$id[i]
}

# change col names of edgelist
colnames(edgelist) <- c('from', 'to')

# build network
net <- network(edgelist, directed = F, matrix.type = 'edgelist')
net

# plot network
plot(net)

# create igraph graph
graph <- graph_from_edgelist(edgelist %>% as.matrix, directed = F)
graph

# calculate unique ego and alter ids in graph
# to be able to calculate degree 
# all ego ids have length 9. alter 10
ids <- V(graph)
ego_ids <- ids[str_length(names(ids)) == 9]
alt_ids <- ids[str_length(names(ids)) == 10]
egoalt_ids <- ids[str_length(names(ids)) %in% c(9, 10)]

# calculate degree overall and by ego/alter
deg_tot <- degree_distribution(graph, v = egoalt_ids)
deg_ego <-  degree_distribution(graph, v = ego_ids)
deg_alt <- degree_distribution(graph, v = alt_ids)

# calculate max number of rows
r <- max(NROW(deg), NROW(deg_ego), NROW(deg_alter))

# add values to end if not enough rows
if(NROW(deg_tot) < 7) deg_tot <- append(deg_tot, rep(NA, 7-NROW(deg_tot)))
if(NROW(deg_ego) < 7) deg_ego <- append(deg_ego, rep(NA, 7-NROW(deg_ego)))
if(NROW(deg_alt) < 7) deg_alt <- append(deg_alt, rep(NA, 7-NROW(deg_alt)))

# add all to data frame
deg <- data.frame(degree = 0:(NROW(deg_tot)-1),
                  Ego = deg_ego,
                  Alter = deg_alter,
                  Overall = deg_tot)

# melt df
deg <- melt(deg, id.vars = 'degree')

# generate colorscale
colscale_egoalt <- scale_fill_manual(name = "Group", values = c('Ego' = '#CCBB44', 'Alter' = '#EE6677',
                                                                'Overall' = '#4477AA'))

# plot degree
ggplot(deg %>% subset(degree != 0), aes(x = degree, y = value*100, fill = variable)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() + 
  labs(x = 'Degree', y = 'Percent', fill = 'Group') +
  colscale_egoalt +
  scale_x_discrete(limits = c('1', '2', '3', '4', '5', '6')) +
  ggtitle('Degree Centrality of Ego and Alter') +
  theme(text = element_text(size=20))

ggsave('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/outputs/degree_centrality.png')


