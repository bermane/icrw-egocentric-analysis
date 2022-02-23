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

# set colors for group
colscale_group <- scale_fill_manual(name = "Group", values = c('Ego' = '#228833', 
                                                               'Alter' = '#EE6677',
                                                               'AAlter' = '#CCBB44'))

# subset graph based on block
gr <- induced_subgraph(gr_comb, 
                       vids = which(V(gr_comb)$block == blocks[3]), 
                       impl = 'create_from_scratch')

# plot using ggraph
ggraph(gr, layout = "stress") + 
	geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
	               edge_colour = "grey66", edge_width = 0.5) + 
	geom_node_point(aes(fill = group %>% as.factor), shape = 21, size = 6) +
  colscale_group + theme_graph() + 
  scale_edge_linetype_manual(name = 'Relationship', 
                             values = c('1' = 'dashed', '2' = 'solid'),
                             labels = c('1' = 'Indirect', '2' = 'Direct')) +
  theme(text = element_text(size=20)) +
  ggtitle(str_c('Networks Within Block of ', blocks[3]))


