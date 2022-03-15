# this code loads network objects for ego and alter and generates network metrics
# and related figures

# load packages
library(tidyverse)
library(igraph)
library(ggraph)
library(janitor)
library(magrittr)

#######################################
### EGO NETWORK DATA BASIC METRICS ####
#######################################

# load ego igraph objects
load("data/ego_igraph.rda")

# measures based on ego
# ego degree centrality
ego_ego_met <- gr_list_ego %>%
  map_dfr(~ tibble(deg = degree(.x, v = 'ego'),
                   bet = betweenness(.x, v = 'ego', weights = NA),
                   clo = closeness(.x, v = 'ego', weights = NA)),
          .id = 'ego_id')

# write ego degree centrality table
write.csv(ego_ego_met %>% tabyl(deg) %>% round(2), file = 'results/not_grouped/ego_deg_cen.csv', row.names = F)

# write ego betweeness centrality table
write.csv(ego_ego_met %>% tabyl(bet) %>% round(2), file = 'results/not_grouped/ego_bet.csv', row.names = F)

# write ego closeness table
write.csv(ego_ego_met %>% tabyl(clo) %>% round(2), file = 'results/not_grouped/ego_clo.csv', row.names = F)

# sort ego_df and ego_ego_met by ego_id
ego_ego_met %<>% arrange(ego_id)
ego_df %<>% arrange(ego_id)

# make sure ego_ids match
sum(ego_ego_met$ego_id == ego_df$ego_id)
  
# add district, block, age, education, caste columns to ego_ego_met
ego_ego_met %<>% add_column(district = ego_df$district_name,
                            block = ego_df$block_name,
                            age = ego_df$ego_age,
                            edu = ego_df$ego_edu,
                            caste = ego_df$ego_caste)

# write ego table by block
write.csv(ego_ego_met %>% 
            group_by(block) %>% 
            summarize(deg = mean(deg, na.rm = T) %>% round(2),
                      bet = mean(bet, na.rm = T) %>% round(2),
                      clo = mean(clo, na.rm = T) %>% round(2)) %>%
            mutate(n = ego_ego_met %>% group_by(block) %>% tally() %>% .$n),
          file = 'results/by_block/ego_metrics_by_block.csv', row.names = F)

# write ego table by district
write.csv(ego_ego_met %>% 
            group_by(district) %>% 
            summarize(deg = mean(deg, na.rm = T) %>% round(2),
                      bet = mean(bet, na.rm = T) %>% round(2),
                      clo = mean(clo, na.rm = T) %>% round(2)) %>%
            mutate(n = ego_ego_met %>% group_by(district) %>% tally() %>% .$n),
          file = 'results/by_district/ego_metrics_by_district.csv', row.names = F)
                                              
# measures based on alter
# alter degree centrality
# alter betweenness centrality
ego_alt_met <- gr_list %>%
  map_dfr(~ tibble(mean_deg = mean(degree(.x)),
                   mean_bet = mean(betweenness(.x, weights = NA)),
                   deg_centr = centr_degree(.x)$centralization,
                   mean_clo = mean(closeness(.x, weights = NA)),
                   dens = edge_density(.x)),
          .id = 'ego_id')

# write alter mean degree table
write.csv(ego_alt_met %>% tabyl(mean_deg) %>% round(2), file = 'results/not_grouped/ego_alt_mean_deg.csv', row.names = F)

# write alter mean betweeness table
write.csv(ego_alt_met %>% tabyl(mean_bet) %>% round(2), file = 'results/not_grouped/ego_alt_mean_bet.csv', row.names = F)

# write alter mean closeness table
write.csv(ego_alt_met %>% tabyl(mean_clo) %>% round(2), file = 'results/not_grouped/ego_alt_mean_clo.csv', row.names = F)

# write alter density table
write.csv(ego_alt_met %>% tabyl(dens) %>% round(2), file = 'results/not_grouped/ego_alt_dens.csv', row.names = F)

# sort ego_df and ego_alt_met by ego_id
ego_alt_met %<>% arrange(ego_id)
ego_df %<>% arrange(ego_id)

# make sure ego_ids match
sum(ego_alt_met$ego_id == ego_df$ego_id)

# add district and block columns to ego_alt_met
ego_alt_met %<>% add_column(district = ego_df$district_name,
                            block = ego_df$block_name)

# write ego_alt table by block
write.csv(ego_alt_met %>% 
            group_by(block) %>% 
            summarize(deg = mean(mean_deg, na.rm = T) %>% round(2),
                      bet = mean(mean_bet, na.rm = T) %>% round(2),
                      clo = mean(mean_clo, na.rm = T) %>% round(2),
                      dens = mean(dens, na.rm = T) %>% round(2)) %>%
            mutate(n = ego_alt_met %>% group_by(block) %>% tally() %>% .$n),
          file = 'results/by_block/ego_alt_metrics_by_block.csv', row.names = F)

# write ego_alt table by district
write.csv(ego_alt_met %>% 
            group_by(district) %>% 
            summarize(deg = mean(mean_deg, na.rm = T) %>% round(2),
                      bet = mean(mean_bet, na.rm = T) %>% round(2),
                      clo = mean(mean_clo, na.rm = T) %>% round(2),
                      dens = mean(dens, na.rm = T) %>% round(2)) %>%
            mutate(n = ego_alt_met %>% group_by(district) %>% tally() %>% .$n),
          file = 'results/by_district/ego_alt_metrics_by_district.csv', row.names = F)

########################################
### ALTER NETWORK DATA BASIC METRICS ###
########################################

# load alter igraph objects
load("data/alter_igraph.rda")

# create table of unique blocks showing district as well
write.csv(alter_df %>% select(district_name, block_name) %>% distinct(block_name, .keep_all = T), 
          file = 'results/alter_district_block.csv', row.names = F)

# measures based on alter
# alter degree centrality
alt_alt_met <- gr_list_a_alt %>%
  map_dfr(~ tibble(deg = degree(.x, v = 'ego'),
                   bet = betweenness(.x, v = 'ego', weights = NA),
                   clo = closeness(.x, v = 'ego', weights = NA)),
          .id = 'alter_id')

# write alter degree centrality table
write.csv(alt_alt_met %>% tabyl(deg) %>% round(2), file = 'results/not_grouped/alt_deg_cen.csv', row.names = F)

# write alter betweeness centrality table
write.csv(alt_alt_met %>% tabyl(bet) %>% round(2), file = 'results/not_grouped/alt_bet.csv', row.names = F)

# write alter closeness table
write.csv(alt_alt_met %>% tabyl(clo) %>% round(2), file = 'results/not_grouped/alt_clo.csv', row.names = F)

# sort alter_df and alt_alt_met by ego_id
alt_alt_met %<>% arrange(alter_id)
alter_df %<>% arrange(alter_id)

# make sure alter_ids match
sum(alt_alt_met$alter_id == alter_df$alter_id)

# add district, block, age, education, caste columns to alt_alt_met
alt_alt_met %<>% add_column(district = alter_df$district_name,
                            block = alter_df$block_name,
                            age = alter_df$age,
                            edu = alter_df$education,
                            caste = alter_df$caste)

# write alt_alt table by block
write.csv(alt_alt_met %>% 
            group_by(block) %>% 
            summarize(deg = mean(deg, na.rm = T) %>% round(2),
                      bet = mean(bet, na.rm = T) %>% round(2),
                      clo = mean(clo, na.rm = T) %>% round(2)) %>%
            mutate(n = alt_alt_met %>% group_by(block) %>% tally() %>% .$n),
          file = 'results/by_block/alt_alt_metrics_by_block.csv', row.names = F)

# write alt_alt table by district
write.csv(alt_alt_met %>% 
            group_by(district) %>% 
            summarize(deg = mean(deg, na.rm = T) %>% round(2),
                      bet = mean(bet, na.rm = T) %>% round(2),
                      clo = mean(clo, na.rm = T) %>% round(2)) %>%
            mutate(n = alt_alt_met %>% group_by(district) %>% tally() %>% .$n),
          file = 'results/by_district/alt_alt_metrics_by_district.csv', row.names = F)

# measures based on aalter
# aalter degree centrality
# aalter betweenness centrality
alt_aalt_met <- gr_list_a %>%
  map_dfr(~ tibble(mean_deg = mean(degree(.x)),
                   mean_bet = mean(betweenness(.x, weights = NA)),
                   deg_centr = centr_degree(.x)$centralization,
                   mean_clo = mean(closeness(.x, weights = NA)),
                   dens = edge_density(.x)),
          .id = 'alter_id')

# write alter mean degree table
write.csv(alt_aalt_met %>% tabyl(mean_deg) %>% round(2), file = 'results/not_grouped/alt_aalt_mean_deg.csv', row.names = F)

# write alter mean betweeness table
write.csv(alt_aalt_met %>% tabyl(mean_bet) %>% round(2), file = 'results/not_grouped/alt_aalt_mean_bet.csv', row.names = F)

# write alter mean closeness table
write.csv(alt_aalt_met %>% tabyl(mean_clo) %>% round(2), file = 'results/not_grouped/alt_aalt_mean_clo.csv', row.names = F)

# write alter density table
write.csv(alt_aalt_met %>% tabyl(dens) %>% round(2), file = 'results/not_grouped/alt_aalt_dens.csv', row.names = F)

# sort alter_df and alt_aalt_met by ego_id
alt_aalt_met %<>% arrange(alter_id)
alter_df %<>% arrange(alter_id)

# make sure alter_ids match
sum(alt_aalt_met$alter_id == alter_df$alter_id)

# add district and block columns to alt_aalt_met
alt_aalt_met %<>% add_column(district = alter_df$district_name,
                            block = alter_df$block_name)

# write alt_aalt table by block
write.csv(alt_aalt_met %>% 
            group_by(block) %>% 
            summarize(deg = mean(mean_deg, na.rm = T) %>% round(2),
                      bet = mean(mean_bet, na.rm = T) %>% round(2),
                      clo = mean(mean_clo, na.rm = T) %>% round(2),
                      dens = mean(dens, na.rm = T) %>% round(2)) %>%
            mutate(n = alt_aalt_met %>% group_by(block) %>% tally() %>% .$n),
          file = 'results/by_block/alt_aalt_metrics_by_block.csv', row.names = F)

# write alt_aalt table by district
write.csv(alt_aalt_met %>% 
            group_by(district) %>% 
            summarize(deg = mean(mean_deg, na.rm = T) %>% round(2),
                      bet = mean(mean_bet, na.rm = T) %>% round(2),
                      clo = mean(mean_clo, na.rm = T) %>% round(2),
                      dens = mean(dens, na.rm = T) %>% round(2)) %>%
            mutate(n = alt_aalt_met %>% group_by(district) %>% tally() %>% .$n),
          file = 'results/by_district/alt_aalt_metrics_by_district.csv', row.names = F)

############################################
### FIGURES OF DEGREE CENTRALITY OVERALL ###
############################################

# degree centrality of ego and alter
# first add ego
deg <- tibble(id = ego_ego_met$ego_id,
              deg = ego_ego_met$deg,
              district = ego_ego_met$district,
              age = ego_ego_met$age,
              edu = ego_ego_met$edu,
              caste = ego_ego_met$caste,
              group = 'ego') 

# second add alter
deg <- rbind(deg, tibble(id = alt_alt_met$alter_id,
                         deg = alt_alt_met$deg,
                         district = alt_alt_met$district,
                         age = alt_alt_met$age,
                         edu = alt_alt_met$edu,
                         caste = alt_alt_met$caste,
                         group = 'alter')) 

# change ego, edu, caste to numeric
deg %<>% mutate(age = as.numeric(age) %>% factor,
                edu = as.numeric(edu) %>% factor,
                caste = as.numeric(caste) %>% factor)

# reshape df
deg_sum <- deg %>% group_by(group) %>% count(deg)

# add missing val for ego
deg_sum <- rbind(deg_sum, tibble(group = 'ego',
                                 deg = 1,
                                 n = 0))

# generate colorscale
# colscale_egoalt <- scale_fill_manual(name = "Group", values = c('ego' = '#CCBB44', 'alter' = '#EE6677',
#                                                                 'Overall' = '#4477AA'))
colscale_egoalt <- scale_fill_manual(name = "Group", values = c('ego' = '#CCBB44', 'alter' = '#EE6677'),
                                     labels = c('ego' = 'Ego', 'alter' = 'Alter'))

# plot degree by n
ggplot(deg_sum, aes(x = deg %>% as.factor, y = n, fill = ordered(group, levels = c('ego', 'alter')))) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  labs(x = 'Degree', y = 'N', fill = 'Group') +
  colscale_egoalt +  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego and Alter') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_alt_plot_n.png')

# plot degree by perc
deg_sum$perc <- NA
deg_sum$perc[deg_sum$group == 'alter'] <- deg_sum$n[deg_sum$group == 'alter']/sum(deg_sum$n[deg_sum$group == 'alter'])
deg_sum$perc[deg_sum$group == 'ego'] <- deg_sum$n[deg_sum$group == 'ego']/sum(deg_sum$n[deg_sum$group == 'ego'])
deg_sum$perc <- round(deg_sum$perc, 2)

ggplot(deg_sum, aes(x = deg %>% as.factor, y = perc*100, fill = ordered(group, levels = c('ego', 'alter')))) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  labs(x = 'Degree', y = 'Percent', fill = 'Group') +
  colscale_egoalt +  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego and Alter') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_alt_plot_perc.png')

########################################
### FIGURES OF DEG CENTRALITY BY AGE ###
########################################

# segment df by ego age
deg_ego_age <- deg %>% 
  filter(group == 'ego') %>% 
  group_by(age) %>% 
  count(deg)

# add in missing values
for(i in unique(deg_ego_age$age)){
  for(j in 0:5){
    check <- deg_ego_age %>%
      filter(age == i,
             deg == j)
    if(NROW(check) == 0){
      deg_ego_age %<>% rbind(tibble(age = i,
                                    deg = j,
                                    n = 0))
    }
  }
}

# generate colorscale
colscale_egoage <- scale_fill_manual(name = "Age", values = c('18' = '#4477AA', '19' = '#66CCEE',
                                                              '20' = '#228833', '21' = '#CCBB44',
                                                              '22' = '#EE6677', '23' = '#AA3377',
                                                              '24' = '#BBBBBB'))

# plot degree by ego age
ggplot(deg_ego_age, aes(x = deg %>% as.factor, y = n, fill = age)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_egoage +
  labs(x = 'Degree', y = 'N', fill = 'Age') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego by Age') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_age_plot_n.png')

# plot degree by perc
deg_ego_age$perc <- NA
for(i in unique(deg_ego_age$age)){
  deg_ego_age$perc[deg_ego_age$age == i] <- deg_ego_age$n[deg_ego_age$age == i]/sum(deg_ego_age$n[deg_ego_age$age == i])
}
deg_ego_age$perc <- round(deg_ego_age$perc, 2)

ggplot(deg_ego_age, aes(x = deg %>% as.factor, y = perc*100, fill = age)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_egoage + 
  labs(x = 'Degree', y = 'Percent of Age Group', fill = 'Age') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego by Age') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_age_plot_perc.png')

# segment df by alter age
deg_alt_age <- deg %>% 
  filter(group == 'alter')

# factor age groups
deg_alt_age %<>% mutate(age = age %>%
                          as.character %>%
                          as.numeric %>%
                          cut(c(17,19,29,39,49,59,101),
                              labels = c('18-19', '20-29', '30-39', '40-49', '50-59','60+'))) %>%
  group_by(age) %>% 
  count(deg)

# add in missing values
for(i in unique(deg_alt_age$age)){
  for(j in 0:5){
    check <- deg_alt_age %>%
      filter(age == i,
             deg == j)
    if(NROW(check) == 0){
      deg_alt_age %<>% rbind(tibble(age = i,
                                    deg = j,
                                    n = 0))
    }
  }
}

# generate colorscale
colscale_altage <- scale_fill_manual(name = "Age", values = c('18-19' = '#4477AA', '20-29' = '#66CCEE',
                                                              '30-39' = '#228833', '40-49' = '#CCBB44',
                                                              '50-59' = '#EE6677', '60+' = '#AA3377'))

# plot degree by alter age
ggplot(deg_alt_age, aes(x = deg %>% as.factor, y = n, fill = age)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_altage +
  labs(x = 'Degree', y = 'N', fill = 'Age') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Alter by Age') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_alt_age_plot_n.png')

# plot degree by perc
deg_alt_age$perc <- NA
for(i in unique(deg_alt_age$age)){
  deg_alt_age$perc[deg_alt_age$age == i] <- deg_alt_age$n[deg_alt_age$age == i]/sum(deg_alt_age$n[deg_alt_age$age == i])
}
deg_alt_age$perc <- round(deg_alt_age$perc, 2)

ggplot(deg_alt_age, aes(x = deg %>% as.factor, y = perc*100, fill = age)) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_altage + 
  labs(x = 'Degree', y = 'Percent of Age Group', fill = 'Age') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Alter by Age') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_alt_age_plot_perc.png')

##########################################
### FIGURES OF DEG CENTRALITY BY CASTE ###
##########################################

# segment df by ego caste
deg_ego_caste <- deg %>% 
  filter(group == 'ego')

# factor caste groups
deg_ego_caste %<>% mutate(caste = caste %>%
                          as.character %>%
                          as.numeric %>%
                          cut(c(0,2,3,4),
                              labels = c('Schedule Caste/Tribe', 'Other Backward Class', 'Other'))) %>%
  group_by(caste) %>% 
  count(deg)

# add in missing values
for(i in unique(deg_ego_caste$caste)){
  for(j in 0:5){
    check <- deg_ego_caste %>%
      filter(caste == i,
             deg == j)
    if(NROW(check) == 0){
      deg_ego_caste %<>% rbind(tibble(caste = i,
                                    deg = j,
                                    n = 0))
    }
  }
}

# generate colorscale
colscale_caste <- scale_fill_manual(name = "Caste", values = c('Schedule Caste/Tribe' = '#4477AA',
                                                              'Other Backward Class' = '#228833', 
                                                              'Other' = '#EE6677'))

# plot degree by ego caste
ggplot(deg_ego_caste, aes(x = deg %>% as.factor, 
                          y = n, 
                          fill = ordered(caste,
                                         levels = c('Schedule Caste/Tribe',
                                                    'Other Backward Class',
                                                    'Other')))) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_caste +
  labs(x = 'Degree', y = 'N', fill = 'Caste') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego by Caste') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_caste_plot_n.png')

# plot degree by perc
deg_ego_caste$perc <- NA
for(i in unique(deg_ego_caste$caste)){
  deg_ego_caste$perc[deg_ego_caste$caste == i] <- deg_ego_caste$n[deg_ego_caste$caste == i]/sum(deg_ego_caste$n[deg_ego_caste$caste == i])
}
deg_ego_caste$perc <- round(deg_ego_caste$perc, 2)

ggplot(deg_ego_caste, aes(x = deg %>% as.factor,
                          y = perc*100, 
                          fill = ordered(caste, 
                                         levels = c('Schedule Caste/Tribe', 
                                                   'Other Backward Class', 
                                                   'Other')))) +                                 
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_caste + 
  labs(x = 'Degree', y = 'Percent of Caste Group', fill = 'Caste') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego by Caste') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_caste_plot_perc.png')

# segment df by alter caste
deg_alt_caste <- deg %>% 
  filter(group == 'alter')

# factor caste groups
deg_alt_caste %<>% mutate(caste = caste %>%
                            as.character %>%
                            as.numeric %>%
                            cut(c(0,2,3,4),
                                labels = c('Schedule Caste/Tribe', 'Other Backward Class', 'Other'))) %>%
  group_by(caste) %>% 
  count(deg)

# add in missing values
for(i in unique(deg_alt_caste$caste)){
  for(j in 0:5){
    check <- deg_alt_caste %>%
      filter(caste == i,
             deg == j)
    if(NROW(check) == 0){
      deg_alt_caste %<>% rbind(tibble(caste = i,
                                      deg = j,
                                      n = 0))
    }
  }
}

# plot degree by alter caste
ggplot(deg_alt_caste, aes(x = deg %>% as.factor, 
                          y = n, 
                          fill = ordered(caste,
                                         levels = c('Schedule Caste/Tribe',
                                                    'Other Backward Class',
                                                    'Other')))) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_caste +
  labs(x = 'Degree', y = 'N', fill = 'Caste') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Alter by Caste') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_alt_caste_plot_n.png')

# plot degree by perc
deg_alt_caste$perc <- NA
for(i in unique(deg_alt_caste$caste)){
  deg_alt_caste$perc[deg_alt_caste$caste == i] <- deg_alt_caste$n[deg_alt_caste$caste == i]/sum(deg_alt_caste$n[deg_alt_caste$caste == i])
}
deg_alt_caste$perc <- round(deg_alt_caste$perc, 2)

ggplot(deg_alt_caste, aes(x = deg %>% as.factor,
                          y = perc*100, 
                          fill = ordered(caste, 
                                         levels = c('Schedule Caste/Tribe', 
                                                    'Other Backward Class', 
                                                    'Other')))) +                                 
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_caste + 
  labs(x = 'Degree', y = 'Percent of Caste Group', fill = 'Caste') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Alter by Caste') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_alt_caste_plot_perc.png')

##############################################
### FIGURES OF DEG CENTRALITY BY EDUCATION ###
##############################################

# segment df by ego education
deg_ego_edu <- deg %>% 
  filter(group == 'ego')

# factor caste groups
deg_ego_edu %<>% mutate(edu = edu %>%
                            as.character %>%
                            as.numeric %>%
                            cut(c(-1,0,8,100),
                                labels = c('None', '1-8', '9+'))) %>%
  group_by(edu) %>% 
  count(deg)

# add in missing values
for(i in unique(deg_ego_edu$edu)){
  for(j in 0:5){
    check <- deg_ego_edu %>%
      filter(edu == i,
             deg == j)
    if(NROW(check) == 0){
      deg_ego_edu %<>% rbind(tibble(edu = i,
                                      deg = j,
                                      n = 0))
    }
  }
}

# generate colorscale
colscale_edu <- scale_fill_manual(name = "Education", values = c('None' = '#4477AA',
                                                               '1-8' = '#228833', 
                                                               '9+' = '#EE6677'))

# plot degree by ego education
ggplot(deg_ego_edu, aes(x = deg %>% as.factor, 
                          y = n, 
                          fill = ordered(edu,
                                         levels = c('None',
                                                    '1-8',
                                                    '9+')))) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_edu +
  labs(x = 'Degree', y = 'N', fill = 'Education') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego by Education') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_edu_plot_n.png')

# plot degree by perc
deg_ego_edu$perc <- NA
for(i in unique(deg_ego_edu$edu)){
  deg_ego_edu$perc[deg_ego_edu$edu == i] <- deg_ego_edu$n[deg_ego_edu$edu == i]/sum(deg_ego_edu$n[deg_ego_edu$edu == i])
}
deg_ego_edu$perc <- round(deg_ego_edu$perc, 2)

ggplot(deg_ego_edu, aes(x = deg %>% as.factor,
                          y = perc*100, 
                          fill = ordered(edu,
                                          levels = c('None',
                                                     '1-8',
                                                     '9+')))) +                                 
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_edu + 
  labs(x = 'Degree', y = 'Percent of Education Group', fill = 'Education') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Ego by Education') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_ego_edu_plot_perc.png')

# segment df by alter education
deg_alt_edu <- deg %>% 
  filter(group == 'alter')

# factor caste groups
deg_alt_edu %<>% mutate(edu = edu %>%
                          as.character %>%
                          as.numeric %>%
                          cut(c(-1,0,8,100),
                              labels = c('None', '1-8', '9+'))) %>%
  group_by(edu) %>% 
  count(deg)

# add in missing values
for(i in unique(deg_alt_edu$edu)){
  for(j in 0:5){
    check <- deg_alt_edu %>%
      filter(edu == i,
             deg == j)
    if(NROW(check) == 0){
      deg_alt_edu %<>% rbind(tibble(edu = i,
                                    deg = j,
                                    n = 0))
    }
  }
}

# plot degree by ego education
ggplot(deg_alt_edu, aes(x = deg %>% as.factor, 
                        y = n, 
                        fill = ordered(edu,
                                       levels = c('None',
                                                  '1-8',
                                                  '9+')))) +
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_edu +
  labs(x = 'Degree', y = 'N', fill = 'Education') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Alter by Education') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_alt_edu_plot_n.png')

# plot degree by perc
deg_alt_edu$perc <- NA
for(i in unique(deg_alt_edu$edu)){
  deg_alt_edu$perc[deg_alt_edu$edu == i] <- deg_alt_edu$n[deg_alt_edu$edu == i]/sum(deg_alt_edu$n[deg_alt_edu$edu == i])
}
deg_alt_edu$perc <- round(deg_alt_edu$perc, 2)

ggplot(deg_alt_edu, aes(x = deg %>% as.factor,
                        y = perc*100, 
                        fill = ordered(edu,
                                       levels = c('None',
                                                  '1-8',
                                                  '9+')))) +                                 
  geom_bar(stat = 'identity',
           position = position_dodge()) +
  theme_bw() +
  colscale_edu + 
  labs(x = 'Degree', y = 'Percent of Education Group', fill = 'Education') +  
  xlim('0', '1', '2', '3', '4', '5') +
  ggtitle('Degree Centrality of Alter by Education') +
  theme(text = element_text(size=20))

ggsave('results/plots/deg_cent_alt_edu_plot_perc.png')

##################################
### PLOTS OF NETWORK STRUCTURE ###
##################################

# density of 1st net
ego_alt_met$dens[1]

# basic plot
ggraph(gr_list_ego[[1]], 'dh') +
  geom_edge_link(aes(linetype = as.factor(weight))) +
  geom_node_label(aes(label = name)) + 
  theme_graph(base_family = 'Helvetica') +
  scale_edge_linetype_manual(values = c('dotted', 'solid')) + 
  theme(legend.position="none") +
  ggtitle('Ego-Alter Network (Density = 1)')

ggsave(filename = 'results/plots/ego_net_1.png')

# density of 1st net
ego_alt_met$dens[109]

# basic plot2
ggraph(gr_list_ego[[109]], 'dh') +
  geom_edge_link(aes(linetype = as.factor(weight))) +
  geom_node_label(aes(label = name)) + 
  theme_graph(base_family = 'Helvetica') +
  scale_edge_linetype_manual(values = c('dotted', 'solid')) + 
  theme(legend.position="none") +
  ggtitle('Ego-Alter Network (Density = 0.7)')

ggsave(filename = 'results/plots/ego_net_109.png')

# density of 1st net
ego_alt_met$dens[81]

# basic plot2
ggraph(gr_list_ego[[81]], 'dh') +
  geom_edge_link(aes(linetype = as.factor(weight))) +
  geom_node_label(aes(label = name)) + 
  theme_graph(base_family = 'Helvetica') +
  scale_edge_linetype_manual(values = c('dotted', 'solid')) + 
  theme(legend.position="none") +
  ggtitle('Ego-Alter Network (Density = 0.6)')

ggsave(filename = 'results/plots/ego_net_81.png')

####################################################################
### BLOCK AND DISTRICT LEVEL TIE INTENSITY AND STRENGTH MEASURES ###
####################################################################

# start with years known
ego_df$alter1


# ######################################
# ### CALCULATE EGO DEGREE CENTALITY ###
# ######################################
# 
# # first calculate number of alters each ego mentioned
# ego$deg_cen <- is.na(ego$alter1) + 
#   is.na(ego$alter2) + 
#   is.na(ego$alter3) + 
#   is.na(ego$alter4) + 
#   is.na(ego$alter5)
# 
# ego$deg_cen <- 5 - ego$deg_cen
# 
# # plot basic degree
# ggplot(ego, aes(x = deg_cen %>% as.factor)) +
#   geom_bar() +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Egos') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
#   ggtitle('Number of Alters Listed by Ego')
# 
# # plot degree based on district
# ggplot(ego, aes(x = deg_cen %>% as.factor, fill = district_name)) +
#   geom_bar(position = position_dodge()) +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Egos', fill = 'District') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
#   ggtitle('Number of Alters Listed by Ego (by District)') +
#   scale_fill_discrete(breaks = c('Darbhanga', 'WestChamparan'),
#                       labels = c('Darbhanga', 'West Champaran'))
# 
# # plot degree based on block
# ggplot(ego, aes(x = deg_cen %>% as.factor, fill = block_name)) +
#   geom_bar(position = position_dodge()) +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Egos', fill = 'Block') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
#   ggtitle('Number of Alters Listed by Ego (by Block)')
# 
# # some sort of frequency table more useful here? perc of total egos from block?
# ego_deg_block <- ego %>% 
#   group_by(block_name, deg_cen) %>% 
#   summarize(n = n()) %>%
#   mutate(freq = round(n / sum(n), 2))
# 
# ########################################################
# ### CALCULATE ALTER DEGREE CENTRALITY IN FIRST LEVEL ###
# ########################################################
# 
# # SHOULD PROBABLY BUILD THIS INTO A NETWORK AND USE iGRAPH FUNCTIONS???
# 
# # let's first extract the correct question so easier to look at
# alter_know <- ego[,str_detect(colnames(ego), glob2rx('alter?_know?'))]
# 
# # set to numeric
# alter_know <- sapply(alter_know, as.numeric)
# 
# # set "don't know" to zero. both responses labeled 2 and 9
# alter_know[alter_know == 2] <- 0
# alter_know[alter_know == 9] <- 0
# 
# # create new df with only a single column for combinations
# alter_know <- alter_know %>% as.data.frame %>% mutate(know12 = alter1_know2 + alter2_know1,
#                                                       know13 = alter1_know3 + alter3_know1,
#                                                       know14 = alter1_know4 + alter4_know1,
#                                                       know15 = alter1_know5 + alter5_know1,
#                                                       know23 = alter2_know3 + alter3_know2,
#                                                       know24 = alter2_know4 + alter4_know2,
#                                                       know25 = alter2_know5 + alter5_know2,
#                                                       know34 = alter3_know4 + alter4_know3,
#                                                       know35 = alter3_know5 + alter5_know3,
#                                                       know45 = alter4_know5 + alter5_know4)
# 
# # set na values to -1 so we can calculate sums
# alter_know[is.na(alter_know)] <- -1
# 
# # calculate total number of degrees for each alter
# alter_know <- alter_know %>% mutate(alt1 = (know12 > 0) + (know13 > 0) + (know14 > 0) + (know15 > 0),
#                                     alt2 = (know12 > 0) + (know23 > 0) + (know24 > 0) + (know25 > 0),
#                                     alt3 = (know13 > 0) + (know23 > 0) + (know34 > 0) + (know35 > 0),
#                                     alt4 = (know14 > 0) + (know24 > 0) + (know34 > 0) + (know45 > 0),
#                                     alt5 = (know15 > 0) + (know25 > 0) + (know35 > 0) + (know45 > 0))
# 
# # add back to ego dataset
# ego <- ego %>% add_column(alt1_deg = alter_know$alt1,
#                           alt2_deg = alter_know$alt2,
#                           alt3_deg = alter_know$alt3,
#                           alt4_deg = alter_know$alt4,
#                           alt5_deg = alter_know$alt5)
# 
# # set to NA if there is no alter
# ego$alt1_deg[is.na(ego$alter1)] <- NA
# ego$alt2_deg[is.na(ego$alter2)] <- NA
# ego$alt3_deg[is.na(ego$alter3)] <- NA
# ego$alt4_deg[is.na(ego$alter4)] <- NA
# ego$alt5_deg[is.na(ego$alter5)] <- NA
# 
# # melt df to see degrees
# alter_deg_1 <- melt(ego, id.vars = 'woman_id', measure.vars = c('alt1_deg', 'alt2_deg', 'alt3_deg', 'alt4_deg', 'alt5_deg'))
# 
# # plot alter degrees level 1
# ggplot(alter_deg_1, aes(x = value %>% as.factor)) +
#   geom_bar() +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Alters') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4')) +
#   ggtitle('Number of Alter Connections to other Alters')
# 
# #########################################################
# ### CALCULATE ALTER DEGREE CENTRALITY IN SECOND LEVEL ###
# #########################################################
# 
# # first calculate number of aalters each alter mentioned
# alter$deg_cen <- is.na(alter$alter1) + 
#   is.na(alter$alter2) + 
#   is.na(alter$alter3) + 
#   is.na(alter$alter4) + 
#   is.na(alter$alter5)
# 
# alter$deg_cen <- 5 - alter$deg_cen
# 
# # plot basic degree
# ggplot(alter, aes(x = deg_cen %>% as.factor)) +
#   geom_bar() +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Alters') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
#   ggtitle('Number of Aalters Listed by Alter')
# 
# # plot degree based on district
# ggplot(alter, aes(x = deg_cen %>% as.factor, fill = district_name)) +
#   geom_bar(position = position_dodge()) +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Alters', fill = 'District') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
#   ggtitle('Number of Aalters Listed by Alter (by District)')
# 
# # plot degree based on block
# ggplot(alter, aes(x = deg_cen %>% as.factor, fill = block_name)) +
#   geom_bar(position = position_dodge()) +
#   theme_bw() +
#   labs(x = 'Degree', y = 'Number of Alters', fill = 'District') +
#   theme(text = element_text(size=20)) +
#   scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5')) +
#   ggtitle('Number of Aalters Listed by Alter (by Block)')
# 
# # some sort of frequency table more useful here? erc of total egos from block?
# alter_deg_block <- alter %>% 
#   group_by(block_name, deg_cen) %>% 
#   summarize(n = n()) %>%
#   mutate(freq = round(n / sum(n), 2))
# 
# ##################################
# ### FIND INTER-ALTER RELATIONS ###
# ##################################
# 
# # not doing this far now
# 
# ######################
# ### BUILD EDGELIST ###
# ######################
# 
# # start building edgelist from ego dataset
# # allocate ego and alter columns
# edgelist = data.frame(ego_id = character(), alter_id = character())
# 
# # loop through ego IDs and add edges based on alters
# for(i in 1:NROW(ego)){
#   if(is.na(ego$alter1[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '1'))
#   } 
#   if(is.na(ego$alter2[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '2'))
#   } 
#   if(is.na(ego$alter3[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '3'))
#   } 
#   if(is.na(ego$alter4[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '4'))
#   } 
#   if(is.na(ego$alter5[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = ego$woman_id[i], alter_id = str_c(ego$woman_id[i], '5'))
#   } 
# }
# 
# # now loop through alters and add edges
# for(i in 1:NROW(alter)){
#   if(is.na(alter$alter1[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '1'))
#   } 
#   if(is.na(alter$alter2[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '2'))
#   } 
#   if(is.na(alter$alter3[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '3'))
#   } 
#   if(is.na(alter$alter4[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '4'))
#   } 
#   if(is.na(alter$alter5[i]) == F){
#     edgelist <- edgelist %>% add_row(ego_id = alter$alter_id[i], alter_id = str_c(alter$alter_id[i], '5'))
#   } 
# }
# 
# # replace duplicate ids
# for(i in 1:NROW(dup_id)){
#   edgelist[edgelist == dup_id$duplicate_id[i]] <- dup_id$id[i]
# }
# 
# # change col names of edgelist
# colnames(edgelist) <- c('from', 'to')
# 
# # build network
# net <- network(edgelist, directed = F, matrix.type = 'edgelist')
# net
# 
# # plot network
# plot(net)
# 
# # create igraph graph
# graph <- graph_from_edgelist(edgelist %>% as.matrix, directed = F)
# graph
# 
# # calculate unique ego and alter ids in graph
# # to be able to calculate degree 
# # all ego ids have length 9. alter 10
# ids <- V(graph)
# ego_ids <- ids[str_length(names(ids)) == 9]
# alt_ids <- ids[str_length(names(ids)) == 10]
# egoalt_ids <- ids[str_length(names(ids)) %in% c(9, 10)]
# 
# # calculate degree overall and by ego/alter
# deg_tot <- degree_distribution(graph, v = egoalt_ids)
# deg_ego <-  degree_distribution(graph, v = ego_ids)
# deg_alt <- degree_distribution(graph, v = alt_ids)
# 
# # calculate max number of rows
# r <- max(NROW(deg), NROW(deg_ego), NROW(deg_alter))
# 
# # add values to end if not enough rows
# if(NROW(deg_tot) < 7) deg_tot <- append(deg_tot, rep(NA, 7-NROW(deg_tot)))
# if(NROW(deg_ego) < 7) deg_ego <- append(deg_ego, rep(NA, 7-NROW(deg_ego)))
# if(NROW(deg_alt) < 7) deg_alt <- append(deg_alt, rep(NA, 7-NROW(deg_alt)))
# 
# # add all to data frame
# deg <- data.frame(degree = 0:(NROW(deg_tot)-1),
#                   Ego = deg_ego,
#                   Alter = deg_alter,
#                   Overall = deg_tot)
# 
# # melt df
# deg <- melt(deg, id.vars = 'degree')
# 
# # generate colorscale
# colscale_egoalt <- scale_fill_manual(name = "Group", values = c('Ego' = '#CCBB44', 'Alter' = '#EE6677',
#                                                                 'Overall' = '#4477AA'))
# 
# # plot degree
# ggplot(deg %>% subset(degree != 0), aes(x = degree, y = value*100, fill = variable)) +
#   geom_bar(stat = 'identity',
#            position = position_dodge()) +
#   theme_bw() + 
#   labs(x = 'Degree', y = 'Percent', fill = 'Group') +
#   colscale_egoalt +
#   scale_x_discrete(limits = c('1', '2', '3', '4', '5', '6')) +
#   ggtitle('Degree Centrality of Ego and Alter') +
#   theme(text = element_text(size=20))
# 
# ggsave('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/outputs/degree_centrality.png')
# 
# 
