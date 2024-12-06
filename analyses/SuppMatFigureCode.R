# Impacts of a prolonged marine heatwave and chronic local human disturbance on juvenile coral assemblages

# Authors: Kristina L. Tietjen [1*], Nelson F. Perks [1,#a], Niallan C. O'Brien [1,#b], Julia K. Baum [1,2]
#
# Institutions:
# [1] Department of Biology, University of Victoria, Victoria, British Columbia, Canada
# [2] Hawaii Institute of Marine Biology, Kaneohe, Hawaii, USA
# [#a] University Hill Secondary School, Vancouver School Board, Vancouver, British Columbia, Canada
# [#b] WSP Canada, Victoria, British Columbia, Canada
# *Corresponding author: Kristina L. Tietjen, Email: kristinaltietjen@gmail.com 

# Script for figures included in the supplementary materials

#<-------------------------------------------------------------->

# clear environment
rm(list=ls())

# load packages
library(here)
library(ggplot2)
library(interactions)
library(gridExtra)
library(glmmTMB)
library(cowplot)
library(effects)

# load data
load("data/figures/SuppMatFigureData.RData")
load("data/Kiritimati_juvenilecoral_modeldata.RData")
load("data/figures/preheatwavemodelresults.RData")
load("data/figures/bleachingmodelresults.RData")
load("data/figures/overallheatstressmodelresults.RData")
load("data/figures/stresstolerant_heatstressmodelresults.RData")
load("data/figures/competitive_heatstressmodelresults.RData")
load("data/figures/weedy_heatstressmodelresults.RData")

# create color scales

heatstress.cols <- c("lightseagreen", "firebrick2", "darkred")

#

# Figure S3 ----------------------------------------------------------------

head(figs3.data)

#tiff(file="figures/figS3.tiff",width = 10, height = 8,units="in",res=300)

ggplot(figs3.data, aes(x=width)) + 
  geom_histogram(binwidth = 0.5) +
  labs(x = "Width (cm)", y ="") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, color = "black"), axis.title = element_text(size = 16))+
  scale_y_continuous(expand = c(0,0), limits=c(0, 500)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,5,0.5))

#dev.off()


# Figure S4 ----------------------------------------------------------------

head(preheatstress.model)

preheatstress.modelplot <- interact_plot(model = preheatstress.model, pred = disturbance.cont_rescaled, modx = lifehistory, 
                                         interval = TRUE, 
                                         x.label = "Human Disturbance", y.label = expression('Density m'^'-2'), 
                                         legend.main = "",
                                         vary.lty = TRUE, line.thickness = 1,
                          modx.labels = c("stress-tolerant"="Stress-tolerant", "competitive"="Competitive", "weedy"="Weedy"),
                          colors = c("Stress-tolerant"="indianred2", "Competitive"="steelblue3", "Weedy"="mediumpurple3"))

preheatstress.modelplot <- preheatstress.modelplot + 
  theme_classic() +
  theme(plot.background=element_blank(), panel.background = element_blank(), 
        legend.position = "bottom", legend.key.size = unit(4, "line"), 
        legend.text = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 26, color = "black"), axis.title = element_text(size = 28, color = "black"),) + 
  guides(color = guide_legend(nrow = 1)) + 
  scale_y_continuous(expand = c(0.005,0.01), limits = c(0.0, 18.01)) + 
  scale_x_continuous(expand = c(0.003,0)) 

#tiff(file="figures/figS4.tiff",width = 10, height = 11,units="in",res=300)

preheatstress.modelplot

#dev.off()

# Figure S5 ----------------------------------------------------------------

head(bleachingmodel)

bleachmodelplot <- plot(effect("poly(disturbance.cont_rescaled, 2) * heatstress", bleachingmodel), multiline = TRUE,
                      confint = list(style = "bands"),
                      xlab.text = list(cex = 2),
                      main = "",
                      colors = heatstress.cols,
                      lines = list(lwd = list(lty = 3)),
                      key.args = list(space = "bottom", columns = 3, title = "", cex = 2, cex.title = 2),
                      axes = list(x = list(lab = "Human Disturbance", cex = 2), y = list(lab = "Bleached Colony Prevalence", cex = 2)))

#tiff(file="figures/figS5.tiff",width = 10, height = 11,units="in",res=300)

bleachmodelplot

#dev.off()

# Figure S6 ----------------------------------------------------------------

overall.heatmodelplot <- interact_plot(model = overall.heatmodel, pred = disturbance.cont_rescaled, modx = heatstress, 
                                       interval = TRUE, 
                               x.label = "Human Disturbance", y.label = expression('Density m'^'-2'), legend.main = "",
                               vary.lty = TRUE, line.thickness = 1,
                               colors = c("Before"="lightseagreen", "Early"="firebrick2", "Late"="darkred", "After" = "lightgoldenrod2"))

overall.heatmodelplot <- overall.heatmodelplot + 
  theme_classic() +
  theme(plot.background=element_blank(), panel.background = element_blank(), 
        legend.position = "bottom", legend.key.size = unit(4, "line"), legend.text = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 26, color = "black"), axis.title = element_text(size = 28, color = "black"),) + 
  guides(color = guide_legend(nrow = 1)) + 
  scale_y_continuous(expand = c(0.005,0.01)) + 
  scale_x_continuous(expand = c(0.003,0)) 

#tiff(file="figures/figS6.tiff",width = 10, height = 11,units="in",res=300)

overall.heatmodelplot

#dev.off()


# Figure S7 ----------------------------------------------------------------

#~~~~~~~~~~~~
## panel a ##
#~~~~~~~~~~~~
head(figs7a.data)

figs7a <- ggplot(figs7a.data, aes(x = rev(forcats::fct_inorder(Variable)), y = Estimate, color = Variable, fill = Variable)) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_pointrange(aes(x = forcats::fct_inorder(Variable), y = Estimate, ymin = LowerCI, ymax = UpperCI),
                  position = position_dodge(width = 1/2), shape = 21, fatten = 4, size = 1) +
  coord_flip() + 
  theme_cowplot() + 
  xlab("") +  
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, margin = margin(20,0,0,0))) + 
  scale_x_discrete(limits = rev) + 
  scale_colour_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                                 "Early", "Late", "After", 
                                 "Windward", "NPP", 
                                 "Dist. * Early (l)", "Dist. * Early (q)", 
                                 "Dist. * Late (l)", "Dist. * Late (q)", 
                                 "Dist. * After (l)", "Dist. * After (q)"), 
                      values = c("black", "black",
                                 "firebrick2", "darkred", "lightgoldenrod2", 
                                 "black", "black", 
                                 "black", "black",
                                 "black", "black",
                                 "black", "black"), 
                      labels = c(expression(paste("Human Dist. (", italic("l"), ")")),
                                 expression(paste("Human Dist. (", italic("q"), ")")), 
                                 "Early", "Late", "After", 
                                 "Windward", "NPP", 
                                 expression(paste("Dist. * Early (", italic("l"), ")")), 
                                 expression(paste("Dist. * Early (", italic("q"), ")")),
                                 expression(paste("Dist. * Late (", italic("l"), ")")), 
                                 expression(paste("Dist. * Late (", italic("q"), ")")), 
                                 expression(paste("Dist. * After (", italic("l"), ")")), 
                                 expression(paste("Dist. * After (", italic("q"), ")")))) +
  scale_fill_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                               "Early", "Late", "After", 
                               "Windward", "NPP", 
                               "Dist. * Early (l)", "Dist. * Early (q)", 
                               "Dist. * Late (l)", "Dist. * Late (q)", 
                               "Dist. * After (l)", "Dist. * After (q)"), 
                    values = c("black", "black",
                               "firebrick2", "darkred", "lightgoldenrod2", 
                               "black", "black", 
                               "black", "black",
                               "black", "black",
                               "black", "black"), 
                    labels = c(expression(paste("Human Dist. (", italic("l"), ")")),
                               expression(paste("Human Dist. (", italic("q"), ")")), 
                               "Early", "Late", "After", 
                               "Windward", "NPP", 
                               expression(paste("Dist. * Early (", italic("l"), ")")), 
                               expression(paste("Dist. * Early (", italic("q"), ")")),
                               expression(paste("Dist. * Late (", italic("l"), ")")), 
                               expression(paste("Dist. * Late (", italic("q"), ")")), 
                               expression(paste("Dist. * After (", italic("l"), ")")), 
                               expression(paste("Dist. * After (", italic("q"), ")")))) + 
  guides(fill = "none", color = "none") +
  annotate('text', 13.2,.95, label="a", size=7)

figs7a

#~~~~~~~~~~~~
## panel b ##
#~~~~~~~~~~~~
head(figs7b.data)

figs7b <- ggplot(figs7b.data, aes(x = rev(forcats::fct_inorder(Variable)), y = Estimate, color = Variable, fill = Variable)) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_pointrange(aes(x = forcats::fct_inorder(Variable), y = Estimate, ymin = LowerCI, ymax = UpperCI),
                  position = position_dodge(width = 1/2), shape = 21, fatten = 4, size = 1) +
  coord_flip() + 
  theme_cowplot() + 
  xlab("") +  
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, margin = margin(20,0,0,0))) + 
  scale_x_discrete(limits = rev) + 
  scale_colour_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                                 "Early", "Late", "After", 
                                 "Windward", "NPP", 
                                 "Dist. * Early (l)", "Dist. * Early (q)", 
                                 "Dist. * Late (l)", "Dist. * Late (q)", 
                                 "Dist. * After (l)", "Dist. * After (q)"), 
                      values = c("black", "black",
                                 "firebrick2", "darkred", "lightgoldenrod2", 
                                 "black", "black", 
                                 "black", "black",
                                 "black", "black",
                                 "black", "black"), 
                      labels = c(expression(paste("Human Dist. (", italic("l"), ")")),
                                 expression(paste("Human Dist. (", italic("q"), ")")), 
                                 "Early", "Late", "After", 
                                 "Windward", "NPP", 
                                 expression(paste("Dist. * Early (", italic("l"), ")")), 
                                 expression(paste("Dist. * Early (", italic("q"), ")")),
                                 expression(paste("Dist. * Late (", italic("l"), ")")), 
                                 expression(paste("Dist. * Late (", italic("q"), ")")), 
                                 expression(paste("Dist. * After (", italic("l"), ")")), 
                                 expression(paste("Dist. * After (", italic("q"), ")")))) +
  scale_fill_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                               "Early", "Late", "After", 
                               "Windward", "NPP", 
                               "Dist. * Early (l)", "Dist. * Early (q)", 
                               "Dist. * Late (l)", "Dist. * Late (q)", 
                               "Dist. * After (l)", "Dist. * After (q)"), 
                    values = c("black", "black",
                               "firebrick2", "darkred", "lightgoldenrod2", 
                               "black", "black", 
                               "black", "black",
                               "black", "black",
                               "black", "black"), 
                    labels = c(expression(paste("Human Dist. (", italic("l"), ")")),
                               expression(paste("Human Dist. (", italic("q"), ")")), 
                               "Early", "Late", "After", 
                               "Windward", "NPP", 
                               expression(paste("Dist. * Early (", italic("l"), ")")), 
                               expression(paste("Dist. * Early (", italic("q"), ")")),
                               expression(paste("Dist. * Late (", italic("l"), ")")), 
                               expression(paste("Dist. * Late (", italic("q"), ")")), 
                               expression(paste("Dist. * After (", italic("l"), ")")), 
                               expression(paste("Dist. * After (", italic("q"), ")")))) + 
  guides(fill = "none", color = "none") +
  annotate('text', 13,10.2, label="b", size=7)

figs7b

#~~~~~~~~~~~~
## panel c ##
#~~~~~~~~~~~~
head(figs7c.data)

figs7c <- ggplot(figs7c.data, aes(x = rev(forcats::fct_inorder(Variable)), y = Estimate, color = Variable, fill = Variable)) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_pointrange(aes(x = forcats::fct_inorder(Variable), y = Estimate, ymin = LowerCI, ymax = UpperCI),
                  position = position_dodge(width = 1/2), shape = 21, fatten = 4, size = 1) +
  coord_flip() + 
  theme_cowplot() + 
  xlab("") +  
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, margin = margin(20,0,0,0))) + 
  scale_x_discrete(limits = rev) + 
  scale_colour_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                                 "Early", "Late", "After", 
                                 "Windward", "NPP", 
                                 "Dist. * Early (l)", "Dist. * Early (q)", 
                                 "Dist. * Late (l)", "Dist. * Late (q)", 
                                 "Dist. * After (l)", "Dist. * After (q)"), 
                      values = c("black", "black",
                                 "firebrick2", "darkred", "lightgoldenrod2", 
                                 "black", "black", 
                                 "black", "black",
                                 "black", "black",
                                 "black", "black"), 
                      labels = c(expression(paste("Human Dist. (", italic("l"), ")")),
                                 expression(paste("Human Dist. (", italic("q"), ")")), 
                                 "Early", "Late", "After", 
                                 "Windward", "NPP", 
                                 expression(paste("Dist. * Early (", italic("l"), ")")), 
                                 expression(paste("Dist. * Early (", italic("q"), ")")),
                                 expression(paste("Dist. * Late (", italic("l"), ")")), 
                                 expression(paste("Dist. * Late (", italic("q"), ")")), 
                                 expression(paste("Dist. * After (", italic("l"), ")")), 
                                 expression(paste("Dist. * After (", italic("q"), ")")))) +
  scale_fill_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                               "Early", "Late", "After", 
                               "Windward", "NPP", 
                               "Dist. * Early (l)", "Dist. * Early (q)", 
                               "Dist. * Late (l)", "Dist. * Late (q)", 
                               "Dist. * After (l)", "Dist. * After (q)"), 
                    values = c("black", "black",
                               "firebrick2", "darkred", "lightgoldenrod2", 
                               "black", "black", 
                               "black", "black",
                               "black", "black",
                               "black", "black"), 
                    labels = c(expression(paste("Human Dist. (", italic("l"), ")")),
                               expression(paste("Human Dist. (", italic("q"), ")")), 
                               "Early", "Late", "After", 
                               "Windward", "NPP", 
                               expression(paste("Dist. * Early (", italic("l"), ")")), 
                               expression(paste("Dist. * Early (", italic("q"), ")")),
                               expression(paste("Dist. * Late (", italic("l"), ")")), 
                               expression(paste("Dist. * Late (", italic("q"), ")")), 
                               expression(paste("Dist. * After (", italic("l"), ")")), 
                               expression(paste("Dist. * After (", italic("q"), ")")))) + 
  guides(fill = "none", color = "none") +
  annotate('text', 13.2,1.5, label="c", size=7)

figs7c

#~~~~~~~~~~~~
## combine ##
#~~~~~~~~~~~~

#tiff(file="figures/figS7.tiff",width = 7, height = 14,units="in",res=300)

grid.arrange(arrangeGrob(figs7a, figs7b, figs7c, 
                         layout_matrix = rbind(c(1,1,1,1),
                                               c(2,2,2,2), 
                                               c(3,3,3,3))))

#dev.off()

# Figure S8 ----------------------------------------------------------------

#~~~~~~~~~~~~
## panel a ##
#~~~~~~~~~~~~

stresstolerant.heatmodelplot <- interact_plot(model = stress.heatmodel, pred = disturbance.cont_rescaled, modx = heatstress, 
                             interval = TRUE, 
                             x.label = "Human Disturbance", y.label = expression('Density m'^'-2'), legend.main = "",
                             vary.lty = TRUE, line.thickness = 1,
                             colors = c("Before"="lightseagreen", "Early"="firebrick2", "Late"="darkred", "After" = "lightgoldenrod2"))

stresstolerant.heatmodelplot <- stresstolerant.heatmodelplot + 
  theme_classic() +
  theme(plot.background=element_blank(), panel.background = element_blank(), 
        legend.position = "bottom", legend.key.size = unit(4, "line"), legend.text = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 26, color = "black"), axis.title = element_text(size = 28, color = "black"),) + 
  guides(color = guide_legend(nrow = 1)) + 
  scale_y_continuous(expand = c(0.005,0.01)) + 
  scale_x_continuous(expand = c(0.003,0)) +  
  annotate('text', 1.35,21.5, label="a", size=11)

stresstolerant.heatmodelplot

#~~~~~~~~~~~~
## panel b ##
#~~~~~~~~~~~~

comp.heatmodelplot <- interact_plot(model = comp.heatmodel, pred = disturbance.cont_rescaled, modx = heatstress, 
                                    interval = TRUE, 
                            x.label = "Human Disturbance", y.label = expression(paste('Density m'^'-2'), "\n "), legend.main = "",
                            vary.lty = TRUE, line.thickness = 1,
                            colors = c("Before"="lightseagreen", "Early"="firebrick2", "Late"="darkred", "After" = "lightgoldenrod2"))

comp.heatmodelplot <- comp.heatmodelplot + 
  theme_classic() +
  theme(plot.background=element_blank(), panel.background = element_blank(), 
        legend.position = "bottom", legend.key.size = unit(4, "line"), legend.text = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 26, color = "black"), axis.title = element_text(size = 28, color = "black"),) + 
  guides(color = guide_legend(nrow = 1)) + 
  scale_y_continuous(expand = c(0.005,0.01), limits = c(0,8)) + 
  scale_x_continuous(expand = c(0.003,0)) +  
  annotate('text', 1.35,7.5, label="b", size=11)

comp.heatmodelplot

#~~~~~~~~~~~~
## panel c ##
#~~~~~~~~~~~~

weedy.heatmodelplot <- interact_plot(model = weedy.heatmodel, pred = disturbance.cont_rescaled, modx = heatstress, 
                                     interval = TRUE, 
                            x.label = "Human Disturbance", y.label = expression('Density m'^'-2'), legend.main = "",
                            vary.lty = TRUE, line.thickness = 1,
                            colors = c("Before"="lightseagreen", "Early"="firebrick2", "Late"="darkred", "After" = "lightgoldenrod2"))

weedy.heatmodelplot <- weedy.heatmodelplot + 
  theme_classic() +
  theme(plot.background=element_blank(), panel.background = element_blank(), 
        legend.position = "bottom", legend.key.size = unit(4, "line"), legend.text = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 26, color = "black"), axis.title = element_text(size = 28, color = "black"),) + 
  guides(color = guide_legend(nrow = 1)) + 
  scale_y_continuous(expand = c(0.005,0.01), limits = c(0,15)) + 
  scale_x_continuous(expand = c(0.003,0)) +  
  annotate('text', 1.35,14, label="c", size=11)

weedy.heatmodelplot

#~~~~~~~~~~~~
## combine ##
#~~~~~~~~~~~~

#tiff(file="figures/figS8.tiff",width = 9, height = 14,units="in",res=300)

grid.arrange(arrangeGrob(stresstolerant.heatmodelplot + theme(legend.position="none"), 
                         comp.heatmodelplot + theme(legend.position="none"), 
                         weedy.heatmodelplot, 
                         layout_matrix = rbind(c(1,1,1,1),
                                               c(2,2,2,2), 
                                               c(3,3,3,3))))                                                                                           

#dev.off()
