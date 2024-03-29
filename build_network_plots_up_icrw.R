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
load("data/combined_igraph_key_villagers_up.rda")

# create vector of unique blocks
blocks <- unique(V(gr_kv)$block)

# create df of blocks and districts together
dist_blo <- cbind(V(gr_kv)$block %>% as.data.frame,
                  V(gr_kv)$district %>% as.data.frame)
colnames(dist_blo) <- c('Block', 'District')

########################################################
### NEW PLOTS WITH LABELS AND SPECIFIC FAMILY VALUES ###
########################################################

# set shape and size for group
shape_group <- scale_shape_manual(name = 'Group', values = c('Ego' = 22,
                                                             'Alter' = 21,
                                                             'AAlter' = 24,
                                                             'Key Villager' = 23),
                                  limits = force)

size_group <- scale_size_manual(name = 'Group', values = c('Ego' = 8,
                                                           'Alter' = 6,
                                                           'AAlter' = 4,
                                                           'Key Villager' = 4),
                                limits = force)

# set colors for relationships
colscale_rela <- scale_fill_manual(name = 'Relationship', values = c('Ego' = '#4477AA',
                                                                     'Family' = '#228833',
                                                                     'Other Family' = '#66CCEE',
                                                                     'Health Worker' = '#EE6677',
                                                                     'Other Health Worker' = '#AA3377',
                                                                     'Non-Family' = '#CCBB44',
                                                                     'Other Non-Family' = '#000000',
                                                                     'Key Villager' = '#BBBBBB'),
                                   limits = force)

########################################
### CREATE OUTPUT PLOTS FOR ALL EGOS ###
########################################

# extract sequence of all egos
id <- vertex_attr(gr_kv, 'name')
id_ego <- id[str_length(id) == 9]

block <- vertex_attr(gr_kv, 'block')
district <- vertex_attr(gr_kv, 'district')

#loop through ego ids to output plots
for(i in seq_along(id_ego)){
  
  eid <- id_ego[i]
  
  #change id value if it is a duplicate
  if(eid %in% c('301009GIT', '301004SAN')){
    eid = "301009GIT|301004SAN"
  }
  
  if(eid %in% c('305039ART', '305040SAB')){
    eid = "305039ART|305040SAB"
  }
  
  if(eid %in% c('309079USH', '309080SUM')){
    eid = "309079USH|309080SUM"
  }
  
  if(eid %in% c('308066SAN', '308065ANJ', '308060MAM')){
    eid = "308066SAN|308065ANJ|308060MAM"
  }
  
  if(eid %in% c('311095RIN', '311093RUB')){
    eid = "311095RIN|311093RUB"
  }
  
  if(eid %in% c('316137SAG', '316141DAM', '316140NEH')){
    eid = "316137SAG|316141DAM|316140NEH"
  }
  
  if(eid %in% c('401001SAR', '401005PRI', '401006ANI', '401004SHA')){
    eid = "401001SAR|401005PRI|401006ANI|401004SHA"
  }
  
  if(eid %in% c('404015ANJ', '404021MAN', '404024SAB')){
    eid = "404015ANJ|404021MAN|404024SAB"
  }
  
  if(eid %in% c('405031REE', '405029RIN')){
    eid = "405031REE|405029RIN"
  }
  
  if(eid %in% c('416101NIT', '416102AAR', '416100MIN', '416104PUS', '416105ASH', '416103NIS')){
    eid = "416101NIT|416102AAR|416100MIN|416104PUS|416105ASH|416103NIS"
  }
  
  if(eid %in% c('408045SHI', '408046ART')){
    eid = "408045SHI|408046ART"
  }
  
  if(eid %in% c('407042SON', '407040ART', '407044PUS')){
    eid = "407042SON|407040ART|407044PUS"
  }
  
  if(eid %in% c('412072SHE', '412076GUL')){
    eid = "412072SHE|412076GUL"
  }
  
  if(eid %in% c('414085SHI', '414084GIT')){
    eid = "414085SHI|414084GIT"
  }
  
  if(eid %in% c('415096ANK', '415092MON', '415093MAN', '415095MAN')){
    eid = "415096ANK|415092MON|415093MAN|415095MAN"
  }
  
  if(eid %in% c('419120MAN', '419119RIN')){
    eid = "419120MAN|419119RIN"
  }
  
  if(eid %in% c('318160MAD', '318161RAJ', '318162PRE')){
    eid = "318160MAD|318161RAJ|318162PRE"
  }
  
  if(eid %in% c('314126RUP', '314127PRA')){
    eid = "314126RUP|314127PRA"
  }
  
  # subset based on nodes that contain one of the ids
  gr <- induced_subgraph(gr_kv, 
                         vids = which(names(V(gr_kv)) %in% str_subset(names(V(gr_kv)), pattern = eid)), 
                         impl = 'create_from_scratch')
  
  # plot using ggraph
  ggraph(gr, layout = "stress") +
    geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1', '3', '4'))),
                    edge_colour = "grey66", edge_width = 0.5) +
    geom_node_point(aes(fill = factor(rela_vals),
                        stroke = intv_stroke,
                        shape = factor(group),
                        size = factor(group))) +
    geom_node_label(aes(label = rela), nudge_y = -0.15, size = 4, label.size = 0,
                    label.padding = unit(0.05, "lines")) +
    theme_graph() +
    scale_edge_linetype_manual(name = 'Tie Strength',
                               values = c('1' = 'dashed', '2' = 'solid', '3' = 'dotdash', '4' = 'blank'),
                               labels = c('1' = 'Indirect', '2' = 'Direct', '3' = 'Key Villager', '4' = '')) +
    theme(text = element_text(size=20)) +
    ggtitle(str_c('Network of ', eid ,' within Block of ', block[which(id == id_ego[i])], ', District of ', district[which(id == id_ego[i])])) +
    shape_group + colscale_rela + size_group +
    guides(size = "none", linetype = guide_legend(override.aes = list(size = 6)),
           shape = guide_legend(order = 2, override.aes = list(size = 6)),
           fill = guide_legend(order = 1, override.aes = list(shape = 25, size = 6)))
  
  ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results_up/', 
               'network_plots/general_by_ego/', id_ego[i],'.png'),
         width = 3862,
         height = 2608,
         units = 'px')
  
}

#############################################
### CREATE OUTPUT PLOTS FOR SPECIFIC EGOS ###
#############################################

# "318160MAD|318161RAJ|318162PRE"
eid = "318160MAD|318161RAJ|318162PRE"

# set i
i <- which(id_ego == "318160MAD")

# subset based on nodes that contain one of the ids
gr <- induced_subgraph(gr_kv, 
                       vids = which(names(V(gr_kv)) %in% str_subset(names(V(gr_kv)), pattern = eid)), 
                       impl = 'create_from_scratch')

# plot using ggraph
ggraph(gr, layout = "stress") +
  geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1', '3', '4'))),
                  edge_colour = "grey66", edge_width = 0.5) +
  geom_node_point(aes(fill = factor(rela_vals),
                      stroke = intv_stroke,
                      shape = factor(group),
                      size = factor(group))) +
  geom_node_label(aes(label = rela), nudge_y = -0.15, size = 4, label.size = 0,
                  label.padding = unit(0.05, "lines")) +
  theme_graph() +
  scale_edge_linetype_manual(name = 'Tie Strength',
                             values = c('1' = 'dashed', '2' = 'solid', '3' = 'dotdash', '4' = 'blank'),
                             labels = c('1' = 'Indirect', '2' = 'Direct', '3' = 'Key Villager', '4' = '')) +
  theme(text = element_text(size=20)) +
  ggtitle(str_c('Network of ', eid ,' within Block of ', block[which(id == id_ego[i])], ', District of ', district[which(id == id_ego[i])])) +
  shape_group + colscale_rela + size_group +
  guides(size = "none", linetype = guide_legend(override.aes = list(size = 6)),
         shape = guide_legend(order = 2, override.aes = list(size = 6)),
         fill = guide_legend(order = 1, override.aes = list(shape = 25, size = 6)))

ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results_up/', 
             'network_plots/general_by_ego/318160MAD_318161RAJ_318162PRE.png'),
       width = 5920*.8,
       height = 4000*.8,
       units = 'px')



# "415096ANK|415092MON|415093MAN|415095MAN"
eid = "415096ANK|415092MON|415093MAN|415095MAN"

# set i
i <- which(id_ego == "415096ANK")

# subset based on nodes that contain one of the ids
gr <- induced_subgraph(gr_kv, 
                       vids = which(names(V(gr_kv)) %in% str_subset(names(V(gr_kv)), pattern = eid)), 
                       impl = 'create_from_scratch')

# plot using ggraph
ggraph(gr, layout = "stress") +
  geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1', '3', '4'))),
                  edge_colour = "grey66", edge_width = 0.5) +
  geom_node_point(aes(fill = factor(rela_vals),
                      stroke = intv_stroke,
                      shape = factor(group),
                      size = factor(group))) +
  geom_node_label(aes(label = rela), nudge_y = -0.15, size = 4, label.size = 0,
                  label.padding = unit(0.05, "lines")) +
  theme_graph() +
  scale_edge_linetype_manual(name = 'Tie Strength',
                             values = c('1' = 'dashed', '2' = 'solid', '3' = 'dotdash', '4' = 'blank'),
                             labels = c('1' = 'Indirect', '2' = 'Direct', '3' = 'Key Villager', '4' = '')) +
  theme(text = element_text(size=20)) +
  ggtitle(str_c('Network of ', eid ,' within Block of ', block[which(id == id_ego[i])], ', District of ', district[which(id == id_ego[i])])) +
  shape_group + colscale_rela + size_group +
  guides(size = "none", linetype = guide_legend(override.aes = list(size = 6)),
         shape = guide_legend(order = 2, override.aes = list(size = 6)),
         fill = guide_legend(order = 1, override.aes = list(shape = 25, size = 6)))

ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/results_up/', 
             'network_plots/general_by_ego/415096ANK_415092MON_415093MAN_415095MAN.png'),
       width = 5920,
       height = 4000,
       units = 'px')

#########################
### OLDER PLOTS BELOW ###
#########################

# ##########################
# ### PLOT WHOLE NETWORK ###
# ##########################
# 
# # plot using ggraph
# ggraph(gr_comb, layout = "stress") + 
#   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
#                   edge_colour = "grey66", edge_width = 0.5) + 
#   geom_node_point(aes(fill = group %>% as.factor, 
#                       stroke = intv_stroke,
#                       shape = rela_vals %>% as.factor), size = 2) +
#   theme_graph() + 
#   scale_edge_linetype_manual(name = 'Tie Strength', 
#                              values = c('1' = 'dashed', '2' = 'solid'),
#                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
#   theme(text = element_text(size=20)) +
#   colscale_group + shape_rela +
#   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25))) +
#   ggtitle('Networks of UP')
# 
# ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
#              'results_up/network_plots/general/whole_network.png'))
# 
# ##########################
# ### CREATE MANUAL PLOT ###
# ##########################
# 
# # ### 113041KAB|113040SAV
# # 
# # # looking in block 9 since there is an example of 1 alter to 2 egos
# # i <- 9
# # 
# # # subset graph based on block
# # gr <- induced_subgraph(gr_comb, 
# #                        vids = which(V(gr_comb)$block == blocks[i]), 
# #                        impl = 'create_from_scratch')
# # 
# # # subset based on nodes that contain one of the ids 113041KAB or 113040SAV
# # gr <- induced_subgraph(gr, 
# #                        vids = which(names(V(gr)) %in% str_subset(names(V(gr)), pattern = "113041KAB|113040SAV")), 
# #                        impl = 'create_from_scratch')
# # 
# # # plot using ggraph
# # ggraph(gr, layout = "stress") + 
# #   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
# #                   edge_colour = "grey66", edge_width = 0.5) + 
# #   geom_node_point(aes(fill = group %>% as.factor, 
# #                       stroke = intv_stroke,
# #                       shape = rela_vals %>% as.factor), size = 6) +
# #   theme_graph() + 
# #   scale_edge_linetype_manual(name = 'Tie Strength', 
# #                              values = c('1' = 'dashed', '2' = 'solid'),
# #                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
# #   theme(text = element_text(size=20)) +
# #   ggtitle(str_c('Network of 113041KAB|113040SAV within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
# #   colscale_group + shape_rela +
# #   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
# # 
# # ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
# #              'results/network_plots/manual/113041KAB-113040SAV.png'))
# # 
# # ### 119060CHA|119058DRO|119061NAN
# # ### Block "Ghanshyampur"
# # 
# # i <- which(blocks == "Ghanshyampur")
# # 
# # # subset graph based on block
# # gr <- induced_subgraph(gr_comb, 
# #                        vids = which(V(gr_comb)$block == blocks[i]), 
# #                        impl = 'create_from_scratch')
# # 
# # # subset based on nodes that contain one of the ids 113041KAB or 113040SAV
# # gr <- induced_subgraph(gr, 
# #                        vids = which(names(V(gr)) %in% str_subset(names(V(gr)), pattern = "119060CHA|119058DRO|119061NAN")), 
# #                        impl = 'create_from_scratch')
# # 
# # # plot using ggraph
# # ggraph(gr, layout = "stress") + 
# #   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
# #                   edge_colour = "grey66", edge_width = 0.5) + 
# #   geom_node_point(aes(fill = group %>% as.factor, 
# #                       stroke = intv_stroke,
# #                       shape = rela_vals %>% as.factor), size = 6) +
# #   theme_graph() + 
# #   scale_edge_linetype_manual(name = 'Tie Strength', 
# #                              values = c('1' = 'dashed', '2' = 'solid'),
# #                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
# #   theme(text = element_text(size=20)) +
# #   ggtitle(str_c('Network of 119060CHA|119058DRO|119061NAN within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
# #   colscale_group + shape_rela +
# #   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
# # 
# # ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
# #              'results/network_plots/manual/119060CHA-119058DRO-119061NAN.png'))
# # 
# # ### 233112MEE|233113LAL
# # ### Block "Bagaha"
# # 
# # i <- which(blocks == "Bagaha")
# # 
# # # subset graph based on block
# # gr <- induced_subgraph(gr_comb, 
# #                        vids = which(V(gr_comb)$block == blocks[i]), 
# #                        impl = 'create_from_scratch')
# # 
# # # subset based on nodes that contain one of the ids 113041KAB or 113040SAV
# # gr <- induced_subgraph(gr, 
# #                        vids = which(names(V(gr)) %in% str_subset(names(V(gr)), pattern = "233112MEE|233113LAL")), 
# #                        impl = 'create_from_scratch')
# # 
# # # plot using ggraph
# # ggraph(gr, layout = "stress") + 
# #   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
# #                   edge_colour = "grey66", edge_width = 0.5) + 
# #   geom_node_point(aes(fill = group %>% as.factor, 
# #                       stroke = intv_stroke,
# #                       shape = rela_vals %>% as.factor), size = 6) +
# #   theme_graph() + 
# #   scale_edge_linetype_manual(name = 'Tie Strength', 
# #                              values = c('1' = 'dashed', '2' = 'solid'),
# #                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
# #   theme(text = element_text(size=20)) +
# #   ggtitle(str_c('Network of 233112MEE|233113LAL within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
# #   colscale_group + shape_rela +
# #   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
# # 
# # ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
# #              'results/network_plots/manual/233112MEE-233113LAL.png'))
# # 
# # ### 230102BAB|230100SOB|230101BIN|235239NEH
# # ### Block "Narkatiaganj"
# # 
# # i <- which(blocks == "Narkatiaganj")
# # 
# # # subset graph based on block
# # gr <- induced_subgraph(gr_comb, 
# #                        vids = which(V(gr_comb)$block == blocks[i]), 
# #                        impl = 'create_from_scratch')
# # 
# # # subset based on nodes that contain one of the ids 113041KAB or 113040SAV
# # gr <- induced_subgraph(gr, 
# #                        vids = which(names(V(gr)) %in% str_subset(names(V(gr)), pattern = "230102BAB|230100SOB|230101BIN|235239NEH")), 
# #                        impl = 'create_from_scratch')
# # 
# # # plot using ggraph
# # ggraph(gr, layout = "stress") + 
# #   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
# #                   edge_colour = "grey66", edge_width = 0.5) + 
# #   geom_node_point(aes(fill = group %>% as.factor, 
# #                       stroke = intv_stroke,
# #                       shape = rela_vals %>% as.factor), size = 6) +
# #   theme_graph() + 
# #   scale_edge_linetype_manual(name = 'Tie Strength', 
# #                              values = c('1' = 'dashed', '2' = 'solid'),
# #                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
# #   theme(text = element_text(size=20)) +
# #   ggtitle(str_c('Network of 230102BAB|230100SOB|230101BIN|235239NEH within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
# #   colscale_group + shape_rela +
# #   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
# # 
# # ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
# #              'results/network_plots/manual/230102BAB-230100SOB-230101BIN-235239NEH.png'))
# 
# ####################
# ### GENERAL PLOT ###
# ####################
# 
# #loop through blocks to output plots
# for(i in seq_along(blocks)){
#   # subset graph based on block
#   gr <- induced_subgraph(gr_comb, 
#                          vids = which(V(gr_comb)$block == blocks[i]), 
#                          impl = 'create_from_scratch')
#   
#   # plot using ggraph
#   ggraph(gr, layout = "stress") + 
#     geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1'))),
#                     edge_colour = "grey66", edge_width = 0.5) + 
#     geom_node_point(aes(fill = group %>% as.factor, 
#                         stroke = intv_stroke,
#                         shape = rela_vals %>% as.factor), size = 6) +
#     theme_graph() + 
#     scale_edge_linetype_manual(name = 'Tie Strength', 
#                                values = c('1' = 'dashed', '2' = 'solid'),
#                                labels = c('1' = 'Indirect', '2' = 'Direct')) +
#     theme(text = element_text(size=20)) +
#     ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
#     colscale_group + shape_rela +
#     guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)))
#   
#   ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
#   'results_up/network_plots/general/block', i, '.png'))
# }
# 
# #########################################
# ### FREQ OF TALKING RELATIONSHIP PLOT ###
# #########################################
# 
# # set colors for line types
# linet_tf <- scale_edge_colour_manual(name = 'Talk Frequency', 
#                                      values = c('Daily' = '#882255',
#                                                 'Weekly' = '#117733',
#                                                 'Monthly' = '#DDCC77',
#                                                 'Yearly' = '#88CCEE',
#                                                 'NA' = '#BBBBBB'),
#                                      labels = c('Daily' = 'Daily',
#                                                 'Weekly' = '> 1 x Week',
#                                                 'Monthly' = '> 1 x 3 Months',
#                                                 'Yearly' = '> 1 x Year',
#                                                 'NA' = 'NA'))
# 
# #loop through blocks to output plots
# for(i in seq_along(blocks)){
#   # subset graph based on block
#   gr <- induced_subgraph(gr_comb, 
#                          vids = which(V(gr_comb)$block == blocks[i]), 
#                          impl = 'create_from_scratch')
# 
# # plot using ggraph
# ggraph(gr, layout = "stress") + 
#   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1')),
#                       edge_colour = ordered(talk_freq_cat, levels = c('Daily', 'Weekly', 'Monthly', 'Yearly', 'NA'))),
#                   edge_width = 0.75) + 
#   geom_node_point(aes(fill = group %>% as.factor, 
#                       stroke = intv_stroke,
#                       shape = rela_vals %>% as.factor), size = 6) +
#   theme_graph() + 
#   scale_edge_linetype_manual(name = 'Tie Strength', 
#                              values = c('1' = 'dashed', '2' = 'solid'),
#                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
#   theme(text = element_text(size=20)) +
#   ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
#   colscale_group + shape_rela + linet_tf +
#   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)),
#          edge_linetype = guide_legend(order = 4), edge_colour = guide_legend(order = 3, override.aes = list(edge_width = 2)))
# 
# ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
#              'results_up/network_plots/by_talk_freq_relationship/block', i, '.png'))
# }
# 
# ###################################
# ### FREQ OF TALKING GENDER PLOT ###
# ###################################
# 
# # set shapes for gender
# shape_gen <- scale_shape_manual(name = 'Gender', values = c('Female' = 21,
#                                                             'Male' = 22,
#                                                             'Missing' = 24))
# 
# #loop through blocks to output plots
# for(i in seq_along(blocks)){
#   # subset graph based on block
#   gr <- induced_subgraph(gr_comb, 
#                          vids = which(V(gr_comb)$block == blocks[i]), 
#                          impl = 'create_from_scratch')
# # plot using ggraph
# ggraph(gr, layout = "stress") + 
#   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1')),
#                       edge_colour = ordered(talk_freq_cat, levels = c('Daily', 'Weekly', 'Monthly', 'Yearly', 'NA'))),
#                   edge_width = 0.75) + 
#   geom_node_point(aes(fill = group %>% as.factor, 
#                       stroke = intv_stroke,
#                       shape = ordered(sex, levels = c('Female', 'Male', 'Missing'))), size = 6) +
#   theme_graph() + 
#   scale_edge_linetype_manual(name = 'Tie Strength', 
#                              values = c('1' = 'dashed', '2' = 'solid'),
#                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
#   theme(text = element_text(size=20)) +
#   ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
#   colscale_group + shape_gen + linet_tf +
#   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)),
#          edge_linetype = guide_legend(order = 4), edge_colour = guide_legend(order = 3, override.aes = list(edge_width = 2)))
# 
# ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
#              'results_up/network_plots/by_talk_freq_gender/block', i, '.png'))
# }
# 
# ###################################
# ### USING AND DISCUSSED FP PLOT ###
# ###################################
# 
# # set colors for line types
# linec_fp <- scale_edge_colour_manual(name = 'Discussed Family Planning', 
#                                      values = c('Yes Talked' = '#117733',
#                                                 'Yes Heard' = '#DDCC77',
#                                                 'No' = '#882255',
#                                                 'NA' = '#BBBBBB'))
# 
# # set shapes
# shape_fp <- scale_shape_manual(name = 'Using Family Planning', values = c('Yes' = 21,
#                                                             'No' = 22,
#                                                             'NA' = 24))
# 
# #loop through blocks to output plots
# for(i in seq_along(blocks)){
#   # subset graph based on block
#   gr <- induced_subgraph(gr_comb, 
#                          vids = which(V(gr_comb)$block == blocks[i]), 
#                          impl = 'create_from_scratch')
# 
# # plot using ggraph
# ggraph(gr, layout = "stress") + 
#   geom_edge_link0(aes(edge_linetype = ordered(weight, levels = c('2', '1')),
#                       edge_colour = ordered(discuss_fp, levels = c('Yes Talked',
#                                                                    'Yes Heard',
#                                                                    'No', 'NA'))),
#                   edge_width = 0.75) + 
#   geom_node_point(aes(fill = group %>% as.factor, 
#                       stroke = intv_stroke,
#                       shape = using_fp), size = 6) +
#   theme_graph() + 
#   scale_edge_linetype_manual(name = 'Tie Strength', 
#                              values = c('1' = 'dashed', '2' = 'solid'),
#                              labels = c('1' = 'Indirect', '2' = 'Direct')) +
#   theme(text = element_text(size=20)) +
#   ggtitle(str_c('Networks Within Block of ', blocks[i], ', District of ', dist_blo$District[dist_blo$Block == blocks[i]][1])) +
#   colscale_group + shape_fp + linec_fp +
#   guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1, override.aes = list(shape = 25)),
#          edge_linetype = guide_legend(order = 4), edge_colour = guide_legend(order = 3, override.aes = list(edge_width = 2)))
# 
# ggsave(str_c('/Users/bermane/Team Braintree Dropbox/ETHAN - ICRW Egocentric data Analysis/Analysis/',
#              'results_up/network_plots/by_using_discussed_fp/block', i, '.png'))
# }
# 
