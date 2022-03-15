# this code loads an overview network linking all egos, alters, and aalters
# in order to generate full network plots showcasing different attributes

# load packages
library(tidyverse)
library(igraph)
library(ggraph)
library(janitor)
library(magrittr)
library(graphlayouts)
library(snahelper)

# load network
load("data/combined_igraph.rda")

# create vector of unique blocks
blocks <- unique(V(gr_comb)$block)

# create df of blocks and districts together
dist_blo <- cbind(V(gr_comb)$block %>% as.data.frame,
                  V(gr_comb)$district %>% as.data.frame)
colnames(dist_blo) <- c('Block', 'District')

# set colors for group
colscale_group <- scale_fill_manual(name = "Group", values = c('Ego' = '#228833', 
                                                               'Alter' = '#EE6677',
                                                               'AAlter' = '#CCBB44'))

# set shapes for relationships
shape_rela <- scale_shape_manual(name = 'Relationship', values = c('Ego' = 22,
                                                                   'Family' = 21,
                                                                   'Health Worker' = 24,
                                                                   'Non-Family' = 23,
                                                                   'Other' = 25))

####################
### GENERAL PLOT ###
####################

#loop through blocks to output plots
for(i in seq_along(blocks)){
  # subset graph based on block
  gr <- induced_subgraph(gr_comb, 
                         vids = which(V(gr_comb)$block == blocks[i]), 
                         impl = 'create_from_scratch')
  
  # plot using ggraph
  ggraph(gr, layout = "stress") + 
    geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
                    edge_colour = "grey66", edge_width = 0.5) + 
    geom_node_point(aes(fill = group %>% as.factor, 
                        stroke = intv_stroke,
                        shape = rela_vals %>% as.factor), size = 6) +
    theme_graph() + 
    scale_edge_linetype_manual(name = 'Tie Strength', 
                               values = c('1' = 'dashed', '2' = 'solid'),
                               labels = c('1' = 'Indirect', '2' = 'Direct')) +
    theme(text = element_text(size=20)) +
    ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
    colscale_group + shape_rela +
    guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
  
  ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
  'results/network_plots/general/block', i, '.png'))
}

#########################################
### FREQ OF TALKING RELATIONSHIP PLOT ###
#########################################

# set colors for line types
linet_tf <- scale_edge_colour_manual(name = 'Talk Frequency', 
                                     values = c('Daily' = '#882255',
                                                'Weekly' = '#117733',
                                                'Monthly' = '#DDCC77',
                                                'Yearly' = '#88CCEE',
                                                'NA' = '#BBBBBB'),
                                     labels = c('Daily' = 'Daily',
                                                'Weekly' = '> 1 x Week',
                                                'Monthly' = '> 1 x 3 Months',
                                                'Yearly' = '> 1 x Year',
                                                'NA' = 'NA'))

#loop through blocks to output plots
for(i in seq_along(blocks)){
  # subset graph based on block
  gr <- induced_subgraph(gr_comb, 
                         vids = which(V(gr_comb)$block == blocks[i]), 
                         impl = 'create_from_scratch')

# plot using ggraph
ggraph(gr, layout = "stress") + 
  geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1')),
                      edge_colour = ordered(talk_freq_cat, levels = c('Daily', 'Weekly', 'Monthly', 'Yearly', 'NA'))),
                  edge_width = 0.75) + 
  geom_node_point(aes(fill = group %>% as.factor, 
                      stroke = intv_stroke,
                      shape = rela_vals %>% as.factor), size = 6) +
  theme_graph() + 
  scale_edge_linetype_manual(name = 'Tie Strength', 
                             values = c('1' = 'dashed', '2' = 'solid'),
                             labels = c('1' = 'Indirect', '2' = 'Direct')) +
  theme(text = element_text(size=20)) +
  ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
  colscale_group + shape_rela + linet_tf +
  guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)),
         edge_linetype = guide_legend(order = 4), edge_colour = guide_legend(order = 3, override.aes = list(edge_width = 2)))

ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
             'results/network_plots/by_talk_freq_relationship/block', i, '.png'))
}

###################################
### FREQ OF TALKING GENDER PLOT ###
###################################

# set shapes for gender
shape_gen <- scale_shape_manual(name = 'Gender', values = c('Female' = 21,
                                                            'Male' = 22,
                                                            'Missing' = 24))

#loop through blocks to output plots
for(i in seq_along(blocks)){
  # subset graph based on block
  gr <- induced_subgraph(gr_comb, 
                         vids = which(V(gr_comb)$block == blocks[i]), 
                         impl = 'create_from_scratch')
# plot using ggraph
ggraph(gr, layout = "stress") + 
  geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1')),
                      edge_colour = ordered(talk_freq_cat, levels = c('Daily', 'Weekly', 'Monthly', 'Yearly', 'NA'))),
                  edge_width = 0.75) + 
  geom_node_point(aes(fill = group %>% as.factor, 
                      stroke = intv_stroke,
                      shape = ordered(sex, levels = c('Female', 'Male', 'Missing'))), size = 6) +
  theme_graph() + 
  scale_edge_linetype_manual(name = 'Tie Strength', 
                             values = c('1' = 'dashed', '2' = 'solid'),
                             labels = c('1' = 'Indirect', '2' = 'Direct')) +
  theme(text = element_text(size=20)) +
  ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
  colscale_group + shape_gen + linet_tf +
  guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)),
         edge_linetype = guide_legend(order = 4), edge_colour = guide_legend(order = 3, override.aes = list(edge_width = 2)))

ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
             'results/network_plots/by_talk_freq_gender/block', i, '.png'))
}

###################################
### USING AND DISCUSSED FP PLOT ###
###################################

# set colors for line types
linec_fp <- scale_edge_colour_manual(name = 'Discussed Family Planning', 
                                     values = c('Yes Talked' = '#117733',
                                                'Yes Heard' = '#DDCC77',
                                                'No' = '#882255',
                                                'NA' = '#BBBBBB'))

# set shapes
shape_fp <- scale_shape_manual(name = 'Using Family Planning', values = c('Yes' = 21,
                                                            'No' = 22,
                                                            'NA' = 24))

#loop through blocks to output plots
for(i in seq_along(blocks)){
  # subset graph based on block
  gr <- induced_subgraph(gr_comb, 
                         vids = which(V(gr_comb)$block == blocks[i]), 
                         impl = 'create_from_scratch')

# plot using ggraph
ggraph(gr, layout = "stress") + 
  geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1')),
                      edge_colour = ordered(discuss_fp, levels = c('Yes Talked',
                                                                   'Yes Heard',
                                                                   'No', 'NA'))),
                  edge_width = 0.75) + 
  geom_node_point(aes(fill = group %>% as.factor, 
                      stroke = intv_stroke,
                      shape = using_fp), size = 6) +
  theme_graph() + 
  scale_edge_linetype_manual(name = 'Tie Strength', 
                             values = c('1' = 'dashed', '2' = 'solid'),
                             labels = c('1' = 'Indirect', '2' = 'Direct')) +
  theme(text = element_text(size=20)) +
  ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
  colscale_group + shape_fp + linec_fp +
  guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)),
         edge_linetype = guide_legend(order = 4), edge_colour = guide_legend(order = 3, override.aes = list(edge_width = 2)))

ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
             'results/network_plots/by_using_discussed_fp/block', i, '.png'))
}
