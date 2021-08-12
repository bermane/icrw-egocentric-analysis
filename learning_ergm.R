# http://statnet.org/Workshops/ergm_tutorial.html

# load package
library(ergm)

# list available terms
?'ergm-terms'

# search for terms
search.ergmTerms(keyword = 'homophily')

# vignette on terms
vignette('ergm-term-crossRef')

# set seed
set.seed(123)

# load example data
data(florentine)
flomarriage

# explore data using graphs
par(mfrow=c(1,2)) # Setup a 2 panel plot
plot(flomarriage, 
     main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage)) # Plot the network
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth

plot(flomarriage, 
     vertex.cex=wealth/25, 
     main="Florentine marriage by wealth", cex.main=0.8) # Plot the network with vertex size proportional to wealth

# run summary command first to inspect covariate
summary(flomarriage ~ edges)

# estimate the model
flomodel.01 <- ergm(flomarriage ~ edges)

# look at the fitted model object
summary(flomodel.01)

# run another model with triangles as well
set.seed(321)
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

# the probabilities of a tie given 0 to 2 triangles
plogis(coef(flomodel.02)[[1]] + (0:2) * coef(flomodel.02)[[2]])

# closer look at the ergm object
class(flomodel.02) # this has the class ergm
names(flomodel.02) # the ERGM object contains lots of components.
coef(flomodel.02) # you can extract/inspect individual components

# summarize wealth
summary(wealth) # summarize the distribution of wealth
# plot(flomarriage, 
#      vertex.cex=wealth/25, 
#      main="Florentine marriage by wealth", 
#      cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

# nodal covariates: homophily
data(faux.mesa.high) 
mesa <- faux.mesa.high

set.seed(1)
mesa

# plot mesa edges with color by grade
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,
       legend=paste('Grade',7:12),cex=0.75)

# create first model with grade and race and also accounting for the similarity of the two between nodes
fauxmodel.01 <- ergm(mesa ~edges + 
                             nodefactor('Grade') + nodematch('Grade',diff=T) +
                             nodefactor('Race') + nodematch('Race',diff=T))

summary(fauxmodel.01)

table(mesa %v% 'Race') # Frequencies of race

mixingmatrix(mesa, "Race")

summary(mesa ~edges  + 
                nodefactor('Grade') + nodematch('Grade',diff=T) +
                nodefactor('Race') + nodematch('Race',diff=T))

# Directed ties
set.seed(2)
data(samplk) 
ls() # directed data: Sampson's Monks

samplk3
plot(samplk3)
summary(samplk3~edges+mutual)
set.seed(3)
sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)
