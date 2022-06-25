# This code separates multiple response questions into individual columns
# can be used for ego or alter data

# install packages if you haven't already
# install.packages(c('tidyverse', 'magrittr'))

# load packages
library(tidyverse)
library(magrittr)
library(readxl)

##############################
### SET INITIAL PARAMETERS ###
##############################

# set working directory to the directory
# with the files I sent
setwd('/Users/bermane/seg_multi_response_example_icrw')

# load ego and alter csv files
# these are the initial csv files you sent me. cleaned.
# I did further cleaning in R for my analysis but will show this example
# directly with the files you sent me and only BASIC cleaning
ego <- read_excel(path = "ego_clean_11012022.xlsx")
alter <-read_excel(path = "alter_clean_11012022.xlsx")

###########################
### BASIC DATA CLEANING ###
###########################

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

###############################################
### SEGMENT MULTIPLE RESPONSE FROM EGO DATA ###
###############################################

# Let's look at household question
var <- ego$household
head(var)

# set column prefix
pref <- 'Household_'

# from the survey, let's make a vector of POSSIBLE responses
# note that if you don't know columns for values that NEVER show up,
# this can be automated. But otherwise R doesn't know all possible 
#responses
resp <- c('A', 'B', 'C',' D', 'E', 'F', 'G', 'H', 'I', 'X')

# lets allocate an empty tibble with the correct column names
sep <- tibble(.rows = NROW(ego))

# loop through responses and add rows to tibble
for(i in seq_along(resp)){
  sep <- cbind(sep,
               tibble(str_detect(var, resp[i])))
}

# set column names
colnames(sep) <- str_c(pref, resp)

# convert to 0 and 1
sep[sep == T] <- 1
sep[sep == F] <- 0

# check data
head(sep)

# write variable to same folder as other data
write.csv(sep, str_c(pref, 'Responses.csv'), row.names = F)


