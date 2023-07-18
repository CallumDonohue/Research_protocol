
# Clears workspace turns figure windows off
rm(list = ls())
graphics.off()

# Sets working directory
setwd('C:/data/git/....')
# Read in data
myData <- read.csv('data.csv')

# Removes NaN's
sel <- !is.nan(myData$running)
myData <- myData[sel,]

#### Load packages ####

# The standard circular package in R. Most tests and stuff
library(circular)

# Package for Maximum Likelihood Analysis of Circular Data. Has a lot of cool functions, but i used it to test
# for the number of modes in my running data
library(CircMLE)


# To go-to plotting package in R
library(ggplot2)


# Used to run mixed effect circular models. The package had some bugs when i was using it 
# and the author advised me to install the package via github as she could quickly patch
# to do this, use: 
# devtools::install_github("joliencremers/bpnreg")
# However, the author contacted me to say that the bugs i had told her about are now fixed on 
# R cran and therefore you should be able to install the package direction via:
# install.packages("bpnreg").
library(bpnreg)

# The bpnreg packages uses Baysian models chains, which can be slow on computers with poor CPU's
# This code will run Bayesian model chains in parallel on multiple cores
# Also usefull for any computational heavy task being ran in R
# options(mc.cores = parallel::detectCores())

############### Circular statistics ########################

# Set factor variables
myData$sizes <- as.factor(myData$sizes)
myData$crab <- as.factor(myData$crab)
myData$sex <- as.factor(myData$sex)
myData$group <- as.factor(myData$group)

# Tells R that this variable is on a circular scale.
myData$running <- circular(myData$running, units = 'radians', modulo = '2pi')

############### Basic circular tests ########################

# Performs a Rayleigh test of uniformity, assessing the significance of the mean resultant length
ray_test <- rayleigh.test(myData$running)

# Performs Rao's test for homogeneity on k populations of angular data. I like this one because it tests for Equality of Polar Vectors and for Equality of Dispersions
rao_test <- rao.test(myData$running[myData$treatment == 1], myData$running[myData$treatment == 2], alpha = 0.05)

# Performs the Watson-Wheeler test (or Mardia-Watson-Wheeler, or uniform score) for homogeneity on two or more samples of circular data. Non-parametric
wheel_test <- watson.wheeler.test(myData$running, myData$treatment)

# Performs the Watson-Williams test for homogeneity of means between several samples of circular data. Parametric and so has some assumptions. Check these out in the package docs. 
wat_test <- watson.williams.test(myData$running, myData$treatment)


############### Mixed effect circular models ########################

# The below includes code to run the mixed effect circular models. You can treat them just like linear mixed models
# in terms of model but the best fitting model needs to use information criterion (see below). As far as i know, you 
# cannot use likelihood ratio testing. 

# This part is wrapped in an if statement because it takes some time to run each model - there is some permutation step, i think
# There is also some randomization associated with the statistics protocol and therefore you can set a seed
# Jolien Cremers (the author) has a good article explaining the details and how to use the package:
# "One direction? A tutorial for circular data analysis using R with examples in cognitive psychology"

# Anyway, If the variable redo == 1, then the modelling part is ran and the output is saved as a .RDS file. 
# if redo != 1, then the modelling is skipped (quicker) and the .RDS file is read from the last time the models were saved
# You can change where the outputs are saved below using setwd. If you remove the setwd below, it will just do everything from the
# current working directory, but this is not very neat. 

redo = 1

if (redo == 1){
  
  # Null model
  mod0 <- bpnme(pred.I = running ~ (1|ind), data = myData, its = 10000, burn = 1000, n.lag = 3, seed = 101)
  
  # Model with variable
  mod1 <- bpnme(pred.I = running ~ treat + (1|ind), data = myData, its = 10000, burn = 1000, n.lag = 3, seed = 102)

  # Model with multiple regression
  mod2 <- bpnme(pred.I = running ~ treat + sex + (1|ind), data = myData, its = 10000, burn = 1000, n.lag = 3, seed = 103)
  
  # Model with interaction
  mod3 <- bpnme(pred.I = running ~ treat*sex + (1|ind), data = myData, its = 10000, burn = 1000, n.lag = 3, seed = 104)
  
  # where the model outputs are saved
  setwd('.../model_lists')
  # save the model outputs
  saveRDS(mod0, file = "model0.rds")
  saveRDS(mod1, file = "model1.rds")
  saveRDS(mod2, file = "model2.rds")
  saveRDS(mod3, file = "model3.rds")
  # go back to working directory
  setwd('C:/data/git/....')
  
  
  # Model validation
  # there are a few ways to make sure that the Bayesian statistics are appropriate. This part is important
  # essentially, the trace plot should look "consistently variable" with no parts looking very different from the other,
  # but i suggest reading up on this. Jolien Cremers publication on this package explains it well
  windows()
  traceplot(mod0, parameter = 'beta1')
  traceplot(mod1, parameter = 'beta2')
  
} else {
  setwd('.../model_lists')
  
  mod0 <- readRDS("model0.rds")
  mod1 <- readRDS("model1.rds")
  mod2 <- readRDS("model2.rds")
  mod3 <- readRDS("model3.rds")
  
  setwd('C:/data/git/....')
}

# The below code extracts means and variances from the model output. The current code assumes a categorical variable
# it changes slightly if you have a continous viable. Again, i think this is explained in the publication mentioned above


## Extract means and CI from the model ##
mod1_coef <- coef_circ(mod1, type = "categorical", units = "radians")
mod1_coef <- as.data.frame(mod1_coef$Means) 
# Circular mean, standard deviation and confidence intervals (upper and lower bounds)
# Confidence intervals are more powerful in Bayesian statistics and are commonly presented to show significant difference
mod1_coef <- SinlgeCoef1[,c("mean", "sd", "LB", "UB")]
mod1_coef <- SinlgeCoef1[1:3,]

# Rho is not calculated in the model and so i do it manual. Code has to change denpending on the variables that you 
# want to plot
# subsets the running data for each level of the treatment variable and calculates the rho. 
r <- c()
for (i in 1:length(levels(myData$treat))){
  
  r[i] <- rho.circular(subset(myData, treat == levels(myData$treat)[i])$running)
  
}

## Create dataframe of all model outputs
mod1_coef$rho <- r
mod1_coef$mean <- circular(mod1_coef$mean, units = 'radians', modulo = '2pi')
mod1_coef$sd <- circular(mod1_coef$sd, units = 'radians', modulo = '2pi')
mod1_coef$LB <- circular(mod1_coef$LB, units = 'radians', modulo = '2pi')
mod1_coef$UB <- circular(mod1_coef$UB, units = 'radians', modulo = '2pi')
# add the levels of your treatment variable. 
mod1_coef$treat <- factor(levels(myData$treat))




################ Plot Circular data ######################

## Plot for basic circular statistics i.e., manually calculate means and variance ##

RMean <- c()
RSd <- c()
Rrho <- c()


# Loops through and calculates the circular mean, standard deviation and mean resultant length (rho) from the running direction vector, split by each treatment you want to plot
# Make sure that you change the varaible names "running" and "treatment" to the appropriate names for your data
unique_treatments <- unique(myData$treatment)

for (i in 1:length(unique_treatments)){
  
  sel <- myData$treatment == unique_treatments[i]
  
  RMean[i] <- mean.circular(myData$running[sel])
  RSd[i]<- sd.circular(myData$running[sel])
  Rrho[i]<- rho.circular(myData$running[sel])
}

# Turns the calculated values into a dataframe (so that ggplot can read it)
Coefs <- data.frame(cbind(RMean, RSd, Rrho))
Coefs$LB <- Coefs$RMean - Coefs$RSd
Coefs$UB <- Coefs$RMean + Coefs$RSd
Coefs$treatment <- unique_treatments
Coefs$treatment <- factor(Coefs$treatment)


# Plots the data
windows()


ggplot(data = Coefs, aes(y = RMean, x = Rrho)) +
  
  # Plots the all running directions along the outside of the circle (circle perimeter = 1) and colors the points based on treatment
  geom_point(data=myData, aes(y = running, x = 1.15,  color = treatment),pch = 16, alpha = 0.75,
             size = 1 ,position = position_jitter(width = 0.1))+
  
  # Creates a line segment that connects the centre of the circle with the mean running directions
  geom_segment(aes(x = 0, y = RMean, xend = Rrho , yend = RMean, color = preds),size = 0.5) +
  
  # Creates the error bars based on standard deviation
  geom_linerange(aes(x = Rrho, ymin = LB, ymax = UB, color = treatment), size = 0.35)+
  # plots the mean running directions, colored by treatment
  geom_point(aes(fill = treatment),color = 'black',pch = 21,  stroke = 0.35, size = 2)+#, position = position_dodge2(width = 0.5)) +
  
  # Makes the plot pretty
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="none")+
  theme(text = element_text(size=12))+
  
  # This turns the plot to a circular plot. Change the "start" and "direction" values to rotate the position of 0 deg
  coord_polar(theta = 'y', start = rad(90), direction = 1)+
  
  # Adds a line at 1 that becomes the outside of the circle
  geom_vline(xintercept = 1, color = "black", size = 0.5) +
  
  # Adds the degree valuyes you want to include around the perimeter
  scale_y_continuous(limits=c(0, (2*pi)) , breaks = c(0, pi/2, pi, pi/2*3), labels = c(0,90,180,270)) +

  # makes the plot even more pretty
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  # Makes some stuff greyy instead of colored
  scale_color_grey(start=0.6, end=0.2)+ scale_fill_grey(start=0.6, end=0.2)



## Plot using model outputs from circular mixed model ##

# creates a column in the data which specifies the where you want the running data to be plotted
# the mean will be plotted at rho, the circle will be placed at 1 and, in this case, the data just outside the circle
# you can make this equal to rho (or some value a little higher), if you want the data plotted close to the mean 
pos <- rep(1.15, nrow(myData))
myData$pos <- pos

# Open figure window
windows()

# Plot using model output dataframe created above
ggplot(data = mod1_coef, aes(y = mean, x = rho)) +
  
  # Creates a line segment that connects the centre of the circle with the mean running directions
  geom_segment(aes(x = 0, y = mean, xend = rho , yend = mean),color = 'black',size = 0.25) +
  
  # Creates the error bars based on confidence intervals. Overlapping CI's means no sig. diff. between treatment levels
  geom_linerange(aes(x = rho, ymin = LB, ymax = UB), color = 'black',size = 0.25)+
  
  # Point position for mean 
  geom_point(aes(color = treat),pch = 16,  size = 3.5)+
  
  # Point position for all datapoints, coloured by treatment level
  geom_point(data=myData, aes(y = run_rot, x = pos,  color = treat),pch = 16, alpha = 0.75,
             size = 1.5 ,position = position_jitter(width = 0.015))+
  
  # Makes plot pretty
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position=c(0.75,0.5)) +
  theme(text = element_text(size=12))+
  
  #  This turns the plot to a circular plot. Change the "start" and "direction" values to rotate the position of 0 deg
  coord_polar(theta = 'y', start = rad(270), direction = -1)+

  # Adds a line at 1 that becomes the outside of the circle
  geom_vline(xintercept =1, color = "black", size = 0.5) +
  
  scale_y_continuous(limits=c(0, (2*pi)) , breaks = c(0, pi/2, pi, pi/2*3), labels = c(0,90,180,270)) +
 
  # makes the plot even more pretty
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  
  # Colours (point border) and fills (point inside) for treatment levels. I had threee in this case and you will need
  # to add more if you have more levels
  scale_color_manual(values= c("grey20", "grey60", "grey80"))+
  scale_fill_manual(values = c("grey20", "grey60", "grey80"))




## Maximum likelihood estimates of data structure ##
# Please refer to the publication: 
# "Bringing the analysis of animal orientation data full circle: model-based approaches with maximum likelihood"
# The function tests your data structure against possible models of animal orientation. Tells you the best model that fits your data
# i.e., bimodal, axial bimodal, unimodel etc... There are 10 of them

circ_model = circ_mle(myData$running)
BestMod <- circ_model$bestmodel

windows()
plot_circMLE(myData$running,circ_model,BestMod)



