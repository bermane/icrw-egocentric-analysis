# http://statnet.org/Workshops/ergm.ego/ergm.ego_tutorial.html

library('ergm.ego')

set.seed(1)

# read in the data
data(faux.mesa.high)
mesa <- faux.mesa.high

# quick plot
plot(mesa, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

# turn into egodata object
mesa.ego <- as.egor(mesa)

names(mesa.ego) # what are the components of this object?

mesa.ego # prints a few lines for each component

#View(mesa.ego) # opens the component in the Rstudio source window

class(mesa.ego) # what type of "object" is this?

class(mesa.ego$ego) # and what type of objects are the components?
class(mesa.ego$alter)
class(mesa.ego$aatie)

mesa.ego$alter # first few rows of the alter table

# ties show up twice, but alter info is linked to .altID
mesa.ego$alter %>% filter((.altID==1 & .egoID==25) | (.egoID==1 & .altID==25))

mesa.ego$aatie # first few rows of the alter table

# exploratory analysis
# to reduce typing, we'll pull the ego and alter data frames
egos <- mesa.ego$ego
alters <- mesa.ego$alter

table(egos$Sex, exclude=NULL)
table(egos$Race, exclude=NULL)
barplot(table(egos$Grade), ylab="frequency")

# compare egos and alters...
par(mfrow=c(1,2))
barplot(table(egos$Race)/nrow(egos),
        main="Ego Race Distn", ylab="percent",
        ylim = c(0,0.5))
barplot(table(alters$Race)/nrow(alters),
        main="Alter Race Distn", ylab="percent",
        ylim = c(0,0.5))

# to get the crosstabulated counts of ties:
mixingmatrix(mesa.ego,"Grade")

# contrast with the original network crosstab:
mixingmatrix(mesa, "Grade")

# to get the row conditional probabilities:
round(mixingmatrix(mesa.ego, "Grade", rowprob=T), 2)
round(mixingmatrix(mesa.ego, "Race", rowprob=T), 2)

# first, using the original network
network.edgecount(faux.mesa.high)

# compare to the egodata
# note that the ties are double counted, so we need to divide by 2.
nrow(mesa.ego$alter)/2

# mean degree -- here we want to count each "stub", so we don't divide by 2
# average number of alters per ego ???
nrow(mesa.ego$alter)/nrow(mesa.ego$ego)

# overall degree distribution
# looking at the distribution of number of alters per ego ???
summary(mesa.ego ~ degree(0:20))

# and stratified by sex
summary(mesa.ego ~ degree(0:13, by="Sex"))

summary(mesa.ego ~ degree(0:10), scaleto=100000)

summary(mesa.ego ~ degree(0:10), scaleto=nrow(mesa.ego$ego)*100)

# to get the frequency counts

degreedist(mesa.ego, plot=T)

# to get the proportion at each degree level

degreedist(mesa.ego, by="Sex", plot=T, prob=T)

degreedist(mesa.ego, brg=T)

degreedist(mesa.ego, by="Sex", prob=T, brg=T)

# Fit a simple model
# models always based on MCMC when using the ergm.ego package
fit.edges <- ergm.ego(mesa.ego ~ edges)
summary(fit.edges)

# change pseudo-population size to 1000
summary(ergm.ego(mesa.ego ~ edges, 
                 control = control.ergm.ego(ppopsize=1000)))

# check MCMC diagnostics
mcmc.diagnostics(fit.edges, which ="plots")

# check GOF
plot(gof(fit.edges, GOF="model"))

# GOF assessment to evaluate the model fit
# underestimates isolates by half. need to improve fit.
plot(gof(fit.edges, GOF="degree"))

# improve the fit
set.seed(1)

# try fitting a term with degree 0
fit.deg0 <- ergm.ego(mesa.ego ~ edges + degree(0), control=control.ergm.ego(ppopsize=1000))
summary(fit.deg0)

#check GOF
mcmc.diagnostics(fit.deg0, which = "plots")
plot(gof(fit.deg0, GOF="model"))
plot(gof(fit.deg0, GOF="degree"))

# fit bigger model
fit.full <- ergm.ego(mesa.ego ~ edges + degree(0:1) 
                     + nodefactor("Sex")
                     + nodefactor("Race", levels = -LARGEST)
                     + nodefactor("Grade")
                     + nodematch("Sex") 
                     + nodematch("Race") 
                     + nodematch("Grade"))
summary(fit.full)

# GOF diagnostics
mcmc.diagnostics(fit.full, which = "plots")
plot(gof(fit.full, GOF="model"))
plot(gof(fit.full, GOF="degree"))

# simulate complete network
sim.full <- simulate(fit.full)
summary(mesa.ego ~ edges + degree(0:1)
        + nodefactor("Sex")
        + nodefactor("Race", levels = -LARGEST)
        + nodefactor("Grade")
        + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))
summary(sim.full ~ edges + degree(0:1)
        + nodefactor("Sex")
        + nodefactor("Race", levels = -LARGEST)
        + nodefactor("Grade")
        + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))

# plot simulation
plot(sim.full, vertex.col="Grade")
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

# can simulate different population sizes
sim.full2 <- simulate(fit.full, popsize=network.size(mesa)*2)
summary(mesa~edges + degree(0:1)
        + nodefactor("Sex")
        + nodefactor("Race", levels = -LARGEST)
        + nodefactor("Grade")
        + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))*2
summary(sim.full2~edges + degree(0:1)
        + nodefactor("Sex")
        + nodefactor("Race", levels = -LARGEST)
        + nodefactor("Grade")
        + nodematch("Sex") + nodematch("Race") + nodematch("Grade"))

# parameter recovery and sampling
# load data
data(faux.magnolia.high)
faux.magnolia.high -> fmh
N <- network.size(fmh)

# fit ERGM and look at coefficients
fit.ergm <- ergm(fmh ~ degree(0:3) 
                 + nodefactor("Race", levels=TRUE) + nodematch("Race")
                 + nodefactor("Sex") + nodematch("Sex") 
                 + absdiff("Grade"))
round(coef(fit.ergm), 3)

# model as egocentric census only
fmh.ego <- as.egor(fmh)
head(fmh.ego)

egofit <- ergm.ego(fmh.ego ~ degree(0:3) 
                   + nodefactor("Race", levels=TRUE) + nodematch("Race")
                   + nodefactor("Sex") + nodematch("Sex") 
                   + absdiff("Grade"), popsize=N,
                   control=control.ergm.ego(ppopsize=N))

# A convenience function.
model.se <- function(fit) sqrt(diag(vcov(fit)))

# Parameters recovered:
coef.compare <- data.frame(
  "NW est" = coef(fit.ergm), 
  "Ego Cen est" = coef(egofit)[-1],
  "diff Z" = (coef(fit.ergm)-coef(egofit)[-1])/model.se(egofit)[-1])

round(coef.compare, 3)

# MCMC diagnostics. 
mcmc.diagnostics(egofit, which="plots")

plot(gof(egofit, GOF="model"))

plot(gof(egofit, GOF="degree"))
