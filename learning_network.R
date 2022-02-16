# load package and set seed
library(network)
set.seed(1702)

# load example datasets
data("flo")
data("emon")

# initialize empty network with 5 vertices
net <- network.initialize(5)
net

# create network using adjacency matrix
nmat <- matrix(rbinom(25, 1, 0.5), nr = 5, nc = 5)
net <- network(nmat, loops = TRUE)
net
summary(net)
all(nmat == net[,])

# follow online example for loading in complex network data
# https://www.mjdenny.com/Preparing_Network_Data_In_R.html

# try preparing network data
# set number of nodes and load packages
num_nodes <- 100
library(randomNames)

# set wd
setwd("/Users/Ediz/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/learning_network_data")

# generate a vector of random names
node_names <- randomNames(num_nodes,name.order = "first.last",name.sep = "_")

# make sure number of nodes is same as number of names
length(unique(node_names)) == num_nodes

# create matrix
relational_information <- matrix(NA,nrow = num_nodes,ncol = num_nodes)

# populate receiver matrix
max_receivers <- 0
for(i in 1:num_nodes){
  num_receivers <- min(rpois(n = 1, lambda =2),num_nodes)
  # If there are any recievers for this sender
  if(num_receivers > 0){
    receivers <- sample(x = node_names[-i], size = num_receivers, replace = F)
    relational_information[i,1:num_receivers] <- receivers
  }
  # Keep track on the maximum number of receivers
  if(num_receivers > max_receivers){
    max_receivers <- num_receivers 
  }
}

# create extended edge list with sender and receiver info
relational_information <- cbind(node_names,relational_information[,1:max_receivers])
head(relational_information)

# make up fake node level data
ages <- round(rnorm(n = num_nodes, mean = 40, sd = 5))
genders <- sample(c("Male","Female"),size = num_nodes, replace = T)
node_level_data <- cbind(node_names,ages,genders)

# screw it up a bit
node_level_data <- node_level_data[sample(x = 1:num_nodes, size = num_nodes-10),]

# write to disk
write.table(x = node_level_data, 
            file = "/Users/Ediz/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/learning_network_data/node_level_data.csv",
            sep = ",",
            row.names = F)
write.table(x = relational_information, 
            file = "/Users/Ediz/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/learning_network_data/relational_information.csv",
            sep = ",",
            row.names = F)

# load data
rm(list = ls())
node_covariates <- read.csv(file = "/Users/Ediz/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/learning_network_data/node_level_data.csv",
                        sep = ",",
                        stringsAsFactors = F, # Always include this argument!
                        header = T)
edge_matrix <- read.csv(file = "/Users/Ediz/Team Braintree Dropbox/Ethan Berman/R Projects/icrw-egocentric-analysis/learning_network_data/relational_information.csv",
                            sep = ",",
                            stringsAsFactors = F, # Always include this argument!
                            header = T)

# check to see we have the same number of nodes
nrow(edge_matrix) == nrow(node_covariates)

# function to clean data
Process_Node_and_Relational_Data <- function(node_level_data,
                                             relational_data,
                                             node_id_column = 1
){
  #get our node ids
  node_ids <- node_level_data[,node_id_column]
  #remove any missing or blank entries
  to_remove <- which(node_ids == "" | is.na(node_ids))
  if(length(to_remove) > 0){
    node_ids <- node_ids[-to_remove]
    node_level_data <- node_level_data[-to_remove,]
  }
  # Allocate a blank edgelist to return
  edgelist <- NULL
  # Loop over rows to check them
  for(i in 1:length(relational_data[,1])){
    # Check to see if the sender is in the dataset
    if(length(which(node_ids == relational_data[i,1]) > 0)){
      #' If we have a valid sender, check to see if there is a valid reciever 
      #' and add them to the dataset if they are valid as well for each 
      #' receiver
      for(j in 2:ncol(relational_data)){
        if(!is.na(relational_data[i,j])){
          if(length(which(node_ids == relational_data[i,j]) > 0)){
            edge <- c(relational_data[i,1],relational_data[i,j])
            edgelist <- rbind(edgelist,edge)
          }
        }
      }
    }
  }
  # Give column names to edgelist
  colnames(edgelist) <- c("sender","receiver")
  # Return cleaned data as a list object
  return(list(node_level_data = node_level_data, 
              edgelist = edgelist,
              num_nodes = length(node_level_data[,1]),
              node_names = node_ids))
}

# run function
Clean_Data <- Process_Node_and_Relational_Data(node_level_data = node_covariates, 
                                               relational_data = edge_matrix,
                                               node_id_column = 1)

# initialize the network
net4 <- network.initialize(Clean_Data$num_nodes)

# add in the node names
network.vertex.names(net4) <- Clean_Data$node_names

# add in the edges
net4[as.matrix(Clean_Data$edgelist)] <- 1

# add in vertex attributes
set.vertex.attribute(net4,"Age",Clean_Data$node_level_data$ages)
set.vertex.attribute(net4,"Gender",Clean_Data$node_level_data$genders)

# take a look at network to make sure it looks good
summary.network(net4,print.adj = FALSE)

# generate some colors using gender
node_colors <- rep("",Clean_Data$num_nodes)
for(i in 1:Clean_Data$num_nodes){
  if(get.node.attr(net4,"Gender")[i] == "Female"){
    node_colors[i] <- "yellow"
  }else{
    node_colors[i] <- "green"
  }
}

# plot the network
pdf("Network_Plot_4.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
) 
plot.network(net4, 
             vertex.col = node_colors, #just one color
             displaylabels = F, # no node names
             mode = "kamadakawai", # another layour algorithm
             displayisolates = F # remove isolate nodes from plot
)
dev.off() # finishes plotting and finalizes pdf

# create function to check network integrity
Check_Network_Integrity <- function(network_object, # the network object we created
                                    n_edges_to_check = 10 # defaults to 10 edges
){
  # Make sure we are providing a network object to the function.
  if(class(network_object) != "network"){
    stop("You must provide this function with an object of class network!")
  }
  # Get network information and edges
  num_nodes <- length(network_object$val)
  edgelist <- as.matrix(network_object, matrix.type = "edgelist")
  names <- get.vertex.attribute(network_object,"vertex.names")
  # Make sure we do not ask for more edges than are in our network
  if(n_edges_to_check > length(edgelist[,1])){
    n_edges_to_check <- length(edgelist[,1])
  }
  # Select a sample of edges
  edges_to_check <- sample(x = 1:length(edgelist[,1]), 
                           size = n_edges_to_check, 
                           replace = F)
  #print out the edges to check.
  cat("Check source data file to make sure that the following edges exist:\n\n")
  for(i in 1:n_edges_to_check){
    cat(names[edgelist[edges_to_check[i],1]],"-->",names[edgelist[edges_to_check[i],2]], "\n")
  }
  cat("\nIf any of these edges do not match your source data files, there was likely a problem reading in your data and you should revisit the process.\n")
}

# run on our network
Check_Network_Integrity(net4)
