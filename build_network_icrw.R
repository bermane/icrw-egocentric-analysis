# this code loads csv data of ego and alter interviews and builds a network object
# using the edgelist method

# load packages
library(tidyverse)
library(igraph)

#################
### LOAD DATA ###
#################


















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


