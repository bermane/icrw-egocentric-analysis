### HAVE NOT USED THIS CODE JUST TESTING ###

# this code loads individual networks
# in order to generate plots showcasing different attributes

# load packages
library(tidyverse)
library(igraph)
library(ggraph)
library(janitor)
library(magrittr)
library(graphlayouts)
library(snahelper)

# load ego igraph objects
load("data/ego_igraph.rda")

# set colors for group
colscale_group <- scale_fill_manual(name = "Group", values = c('Ego' = '#228833', 
                                                               'Alter' = '#EE6677',
                                                               'AAlter' = '#CCBB44'))

# set shapes for relationships
shape_rela <- scale_shape_manual(name = 'Relationship', values = c('Ego' = 22,
                                                                   'Family' = 21,
                                                                   'Health Worker' = 24,
                                                                   'Non-Family' = 23))

# loop through and output all networks so we can choose one to showcase

#loop through blocks to output plots
for(i in seq_along(gr_list_ego)){
  # subset graph based on ego
  gr <- gr_list_ego[[i]]
  
  # plot using ggraph
  ggraph(gr, layout = "stress") + 
    geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
                    edge_colour = "grey66", edge_width = 0.5) + 
    geom_node_point(aes(fill = group %>% as.factor,
                        shape = rela_vals %>% as.factor), size = 6) +
    theme_graph() + 
    scale_edge_linetype_manual(name = 'Tie Strength', 
                               values = c('1' = 'dashed', '2' = 'solid'),
                               labels = c('1' = 'Indirect', '2' = 'Direct')) +
    theme(text = element_text(size=20)) +
    #ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
    colscale_group + shape_rela +
    guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
  
  ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
               'results/network_plots/general/block', i, '.png'))
}