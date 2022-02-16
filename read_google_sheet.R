# load package
library(googledrive)
library(googlesheets4)

# find file
# you might need to make sure that you created a shortcut to the shared folder in MY DRIVE
# first time make sure to give tidyverse FULL AUTHORIZATION to READ/WRITE/ETC
file <- drive_find(pattern = 'ICRW Egocentric SNA Survey DATA Copy 2021-12-09')

# read data into R
# first time make sure to give tidyverse FULL AUTHORIZATION to READ/WRITE/ETC
data <- read_sheet(file$id)
