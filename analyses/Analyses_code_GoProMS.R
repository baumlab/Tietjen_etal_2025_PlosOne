# Impacts of a prolonged marine heatwave and chronic local human disturbance on juvenile coral assemblages

# Authors: Kristina L. Tietjen [1*], Nelson F. Perks [1,#a], Niallan C. O'Brien [1,#b], Julia K. Baum [1,2]
#
# Institutions:
# [1] Department of Biology, University of Victoria, Victoria, British Columbia, Canada
# [2] Hawaii Institute of Marine Biology, Kaneohe, Hawaii, USA
# [#a] University Hill Secondary School, Vancouver School Board, Vancouver, British Columbia, Canada
# [#b] WSP Canada, Victoria, British Columbia, Canada
# *Corresponding author: Kristina L. Tietjen, Email: kristinaltietjen@gmail.com 

# Script for analyses preformed in the manuscript

#<-------------------------------------------------------------->

# clear environment
rm(list=ls())

# load packages
library(here)
library(effects)
library(DHARMa)
library(performance)
library(glmmTMB)
library(dplyr)
library(rstatix)

# load data
load("data/Kiritimati_juvenilecoral_modeldata.RData")

#

# Pre-heatwave analyses (hypotheses i and ii) ----------------------------------------------------------------

head(before)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

preheatstress.model <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * lifehistory + exposure + npp.max_rescaled + (1|site) + offset(log(n.quadrats.site)), data=before, family = nbinom1)

summary(preheatstress.model)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(preheatstress.model)

testDispersion(preheatstress.model)

# Calculate and plot scaled residuals vs. fitted values
preheatstress.model.resid <- simulateResiduals(fittedModel = preheatstress.model, plot = F)
plot(preheatstress.model.resid)
hist(preheatstress.model.resid)
testResiduals(preheatstress.model.resid)
testZeroInflation(preheatstress.model.resid)
testOutliers(preheatstress.model.resid)
testDispersion(preheatstress.model.resid)

# Plot residuals against other predictor values
plotResiduals(preheatstress.model.resid, before$disturbance.cont_rescaled)
plotResiduals(preheatstress.model.resid, before$islandside)
plotResiduals(preheatstress.model.resid, before$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(preheatstress.model))
plot(effect("poly(disturbance.cont_rescaled,2) * lifehistory", preheatstress.model))


## make data file to make figure
save(preheatstress.model, file = "data/figures/preheatwavemodelresults.RData")

# Bleaching analysis (hypothesis iii) ----------------------------------------------------------------

head(bleaching)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

bleachingmodel <- glmmTMB(n.bleached/n.site.heatstress ~ poly(disturbance.cont_rescaled,2) * heatstress + npp.max_rescaled + exposure + (1|site) + offset(log(n.quadrats)), weights = n.site.heatstress, data=bleaching, family = binomial())

summary(bleachingmodel)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(bleachingmodel)

testDispersion(bleachingmodel)

# Calculate and plot scaled residuals vs. fitted values
bleachingmodel.resid <- simulateResiduals(fittedModel = bleachingmodel, plot = F)
plot(bleachingmodel.resid)
hist(bleachingmodel.resid)
testResiduals(bleachingmodel.resid)

# Plot residuals against other predictor values
plotResiduals(bleachingmodel.resid, bleaching$disturbance.cont_rescaled)
plotResiduals(bleachingmodel.resid, bleaching$heatstress)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(bleachingmodel))

## make data file to make figure
save(bleachingmodel, file = "data/figures/bleachingmodelresults.RData")

# Overall heat stress analysis (hypothesis iv) ----------------------------------------------------------------

head(overall.heat)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

overall.heatmodel <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * heatstress + lifehistory + exposure + npp.max_rescaled + (1|site) + offset(log(n.quadrats)), data=overall.heat, family = nbinom1)

summary(overall.heatmodel)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(overall.heatmodel)

testDispersion(overall.heatmodel)

# Calculate and plot scaled residuals vs. fitted values
overall.heatmodel.resid <- simulateResiduals(fittedModel = overall.heatmodel, plot = F)
plot(overall.heatmodel.resid)
hist(overall.heatmodel.resid)
testResiduals(overall.heatmodel.resid)

# Plot residuals against other predictor values
plotResiduals(overall.heatmodel.resid, overall.heat$disturbance.cont_rescaled)
plotResiduals(overall.heatmodel.resid, overall.heat$lifehistory)
plotResiduals(overall.heatmodel.resid, overall.heat$heatstress)
plotResiduals(overall.heatmodel.resid, overall.heat$exposure)
plotResiduals(overall.heatmodel.resid, overall.heat$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(overall.heatmodel))

## make data file to make figure
save(overall.heatmodel, file = "data/figures/overallheatstressmodelresults.RData")

# Stress-tolerant corals heat stress analysis (hypothesis v) ----------------------------------------------------------------

head(stress.heat)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

stress.heatmodel <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * heatstress + exposure +  npp.max_rescaled + (1|site) + offset(log(n.quadrats)), data=stress.heat, family = nbinom1)

summary(stress.heatmodel)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(stress.heatmodel)

testDispersion(stress.heatmodel)

# Calculate and plot scaled residuals vs. fitted values
stress.heatmodel.resid <- simulateResiduals(fittedModel = stress.heatmodel, plot = F)
plot(stress.heatmodel.resid)
hist(stress.heatmodel.resid)
testResiduals(stress.heatmodel.resid)

# Plot residuals against other predictor values
plotResiduals(stress.heatmodel.resid, stress.heat$disturbance.cont_rescaled)
plotResiduals(stress.heatmodel.resid, stress.heat$heatstress)
plotResiduals(stress.heatmodel.resid, stress.heat$exposure)
plotResiduals(stress.heatmodel.resid, stress.heat$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(stress.heatmodel))

## make data file to make figure
save(stress.heatmodel, file = "data/figures/stresstolerant_heatstressmodelresults.RData")

# Competitive corals heat stress analysis (hypothesis v) ----------------------------------------------------------------

head(comp.heat)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

comp.heatmodel <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * heatstress + exposure + npp.max_rescaled + (1|site) + offset(log(n.quadrats)), data=comp.heat, family = nbinom1)

summary(comp.heatmodel)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(comp.heatmodel)

testDispersion(comp.heatmodel)

# Calculate and plot scaled residuals vs. fitted values (DHARMa)
comp.heatmodel.resid <- simulateResiduals(fittedModel = comp.heatmodel, plot = F)
plot(comp.heatmodel.resid)
hist(comp.heatmodel.resid)
testResiduals(comp.heatmodel.resid)

# Plot residuals against other predictor values (DHARMa)
# Predictors in the model
plotResiduals(comp.heatmodel.resid, comp.heat$disturbance.cont_rescaled)
plotResiduals(comp.heatmodel.resid, comp.heat$heatstress)
plotResiduals(comp.heatmodel.resid, comp.heat$exposure)
plotResiduals(comp.heatmodel.resid, comp.heat$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(comp.heatmodel))

## make data file to make figure
save(comp.heatmodel, file = "data/figures/competitive_heatstressmodelresults.RData")

# Weedy corals heat stress analysis (hypothesis v) ----------------------------------------------------------------

head(weedy.heat)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

weedy.heatmodel <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * heatstress + exposure +  npp.max_rescaled + (1|site) + offset(log(n.quadrats)), data=weedy.heat, family = nbinom1)

summary(weedy.heatmodel)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(weedy.heatmodel)

testDispersion(weedy.heatmodel)

# Calculate and plot scaled residuals vs. fitted values (DHARMa)
weedy.heatmodel.resid <- simulateResiduals(fittedModel = weedy.heatmodel, plot = F)
plot(weedy.heatmodel.resid)
hist(weedy.heatmodel.resid)
testResiduals(weedy.heatmodel.resid)

# Plot residuals against other predictor values (DHARMa)
# Predictors in the model
plotResiduals(weedy.heatmodel.resid, weedy.heat$disturbance.cont_rescaled)
plotResiduals(weedy.heatmodel.resid, weedy.heat$heatstress)
plotResiduals(weedy.heatmodel.resid, weedy.heat$exposure)
plotResiduals(weedy.heatmodel.resid, weedy.heat$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(weedy.heatmodel))

## make data file to make figure
save(weedy.heatmodel, file = "data/figures/weedy_heatstressmodelresults.RData")

# Before and after analysis (hypothesis vi) ----------------------------------------------------------------

head(before.after)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

species.change <- before.after %>% 
  group_by(genus.sp.short) %>%
  do(broom::tidy(wilcox.test(.$before, .$after, paired = TRUE, conf.int = TRUE) )) %>%
  add_significance()

species.change


# Overall heat stress sensitivity analysis - only the 9 sites sampled in all four periods ----------------------------------------------------------------

head(overall.heat.sens1)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

overall.heatmodel.sens1 <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * heatstress + lifehistory + exposure +  npp.max_rescaled + (1|site) + offset(log(n.quadrats)), data=overall.heat.sens1, family = nbinom1)

summary(overall.heatmodel.sens1)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(overall.heatmodel.sens1)

testDispersion(overall.heatmodel.sens1)

overall.heatmodel.sens1.resid <- simulateResiduals(fittedModel = overall.heatmodel.sens1, plot = F)
plot(overall.heatmodel.sens1.resid)
hist(overall.heatmodel.sens1.resid)
testResiduals(overall.heatmodel.sens1.resid)

# Plot residuals against other predictor values
plotResiduals(overall.heatmodel.sens1.resid, overall.heat.sens1$disturbance.cont_rescaled)
plotResiduals(overall.heatmodel.sens1.resid, overall.heat.sens1$lifehistory)
plotResiduals(overall.heatmodel.sens1.resid, overall.heat.sens1$heatstress)
plotResiduals(overall.heatmodel.sens1.resid, overall.heat.sens1$exposure)
plotResiduals(overall.heatmodel.sens1.resid, overall.heat.sens1$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(overall.heatmodel.sens1))

# Overall heat stress sensitivity analysis - only the 10 sites sampled in both the before and late heat stress time periods  ----------------------------------------------------------------

head(overall.heat.sens2)

#~~~~~~~~~~~~~~~~
##    model   ##
#~~~~~~~~~~~~~~~~

overall.heatmodel.sens2 <- glmmTMB(n.coral.lh.site.trip ~ poly(disturbance.cont_rescaled,2) * heatstress + lifehistory + exposure +  npp.max_rescaled + (1|site) + offset(log(n.quadrats)), data=overall.heat.sens2, family = nbinom1)

summary(overall.heatmodel.sens2)

#~~~~~~~~~~~~~~~~
## diagnostics ##
#~~~~~~~~~~~~~~~~

check_collinearity(overall.heatmodel.sens2)

testDispersion(overall.heatmodel.sens2)

overall.heatmodel.sens2.resid <- simulateResiduals(fittedModel = overall.heatmodel.sens2, plot = F)
plot(overall.heatmodel.sens2.resid)
hist(overall.heatmodel.sens2.resid)
testResiduals(overall.heatmodel.sens2.resid)

# Plot residuals against other predictor values
plotResiduals(overall.heatmodel.sens2.resid, overall.heat.sens2$disturbance.cont_rescaled)
plotResiduals(overall.heatmodel.sens2.resid, overall.heat.sens2$lifehistory)
plotResiduals(overall.heatmodel.sens2.resid, overall.heat.sens2$heatstress)
plotResiduals(overall.heatmodel.sens2.resid, overall.heat.sens2$exposure)
plotResiduals(overall.heatmodel.sens2.resid, overall.heat.sens2$npp.max_rescaled)

#~~~~~~~~~~~~~~~~
##    plots   ##
#~~~~~~~~~~~~~~~~

plot(allEffects(overall.heatmodel.sens2))
