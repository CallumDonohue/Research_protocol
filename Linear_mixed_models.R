
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

# To go-to plotting package in R
library(ggplot2)

# Package contains linear mixed models and generalized linear mixed model
library(lme4)

# For pairwise contrast and to extrace estimated marginal means from the model output
library(emmeans)

# Removes NaN's. Save as seperate dataset so that you can still do probability analysis with glmmer
sel <- !is.nan(myData$running)
dat_sel <- myData[sel,] 


######################## Timing statistics ######################## 

# Tells R which variables are factors
myData$sizes <- as.factor(myData$sizes)
myData$crab <- as.factor(myData$crab)
myData$sex <- as.factor(myData$sex)
myData$group <- as.factor(myData$group)


# Example code of linear mixed models looking at the effect of stimulus size on response distance. Also tests for other factors (e.g. sex and carapace size)
# All models include crab nested within group as a random effect. 
# Make sure to include any transformation in the code for the model itself rather than creating a variable beforehand that had been transformed prior. This is just so emmeans knows about the transformation 
mod0 <- lmer(log10(dist) ~ (1|group/crab), data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
mod1 <- lmer(log10(dist) ~ sizes + (1|group/crab), data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
mod2 <- lmer(log10(dist) ~ csize + (1|group/crab), data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
mod3 <- lmer(log10(dist) ~ sex + (1|group/crab), data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

# The anova's tests a model with a variable included is better than a null model
anova(mod0, mod1)
anova(mod0, mod2)
anova(mod0, mod3)
anova(mod0, mod4)

# Checks that the residuals are normally distributed
hist(residuals(mod1))
shapiro.test(residuals(mod1))


mod6 <- lmer(log10(dist)~ sizes + csize + (1|group/crab),  data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
anova(mod6, mod1)

mod8 <- lmer(log10(dist)~ sizes + sex + (1|group/crab),  data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
anova(mod8, mod1)

mod9 <- lmer(log10(dist)~ sizes + pred_pos + (1|group/crab),  data = dat_sel, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
anova(mod9, mod1)

# Code to extract pairwise contrast between treatment levels. Will also provide estimated margianl means and standard errors. These are more
# precise statistics that consider the other variables in the model as well. The adjust arguments tells the funciton to adjust the p-value for multiple comparisons
# and the type = 'respones' argument will make emmeans back-transform the output from the transformed scale. 
emm_mod1 <- emmeans(mod1, specs = pairwise ~ sizes, adjust = "tukey",type = "response")
emm_sum <- summary(emm_mod1, infer = TRUE)

######################## Probability statistics ######################## 

# Example code of generalized mixed models looking at the effect of stimulus size on response probability. Also tests for other factors (e.g. sex and carapace size)
# All models include crab nested within group as a random effect.
# Basically the same as the code above but uses a different function. The family = binomial will put all the outputs onto a logit scale. The emmeans function will back-transform from this if type = response

mod0 <- glmer(probz ~ (1|group/crab),family = binomial, data = myData)

mod1 <- glmer(probz ~ sizes + (1|group/crab), family = binomial,data = myData)
mod2 <- glmer(probz ~ csize + (1|group/crab), family = binomial, data = myData)
mod4 <- glmer(probz ~ sex + (1|group/crab), family = binomial, data = myData)
mod5 <- glmer(probz ~ pred_pos + (1|group/crab), family = binomial, data = myData)

anova(mod0, mod1)
anova(mod0, mod2)
anova(mod0, mod4)
anova(mod0, mod5)

mod6 <- glmer(probz ~ sizes + csize + (1|group/crab), family = binomial, data = myData)
mod7 <- glmer(probz ~ sizes + sex + (1|group/crab), family = binomial, data = myData)
mod8 <- glmer(probz ~ sizes + pred_pos + (1|group/crab), family = binomial, data = myData)

anova(mod1, mod6)
anova(mod1, mod7)
anova(mod1, mod8)

emm_mod1 <- emmeans(mod1, specs = pairwise ~ sizes, adjust = "tukey",type = "response")
emm_sum <- summary(emm_mod1, infer = TRUE)


