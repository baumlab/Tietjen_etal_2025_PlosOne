# Impacts of a prolonged marine heatwave and chronic local human disturbance on juvenile coral assemblages

# Authors: Kristina L. Tietjen [1*], Nelson F. Perks [1,#a], Niallan C. O'Brien [1,#b], Julia K. Baum [1,2]
#
# Institutions:
# [1] Department of Biology, University of Victoria, Victoria, British Columbia, Canada
# [2] Hawaii Institute of Marine Biology, Kaneohe, Hawaii, USA
# [#a] University Hill Secondary School, Vancouver School Board, Vancouver, British Columbia, Canada
# [#b] WSP Canada, Victoria, British Columbia, Canada
# *Corresponding author: Kristina L. Tietjen, Email: kristinaltietjen@gmail.com 

# Script for making figures in the main manuscript

#<-------------------------------------------------------------->

# clear environment
rm(list=ls())

# load packages
library(here)
library(plyr)
library(ggplot2)
library(Rmisc)
library(cowplot)
library(gridExtra)
library(grid)

# load data
load("data/figures/MainMSFigureData.RData")

# create color scales

lh.heat <- c("indianred4", "indianred1", "steelblue4", "steelblue1", "mediumpurple4", "mediumpurple1", "seagreen4", "seagreen1", "burlywood3", "burlywood1", "gray46", "gray 85", "gray10", "gray28")

lh.heat.colscale <- scale_fill_manual(name= "", values = lh.heat, 
                                     breaks = c("before.stress-tolerant", "after.stress-tolerant", "before.competitive", "after.competitive", "before.weedy", "after.weedy", "before.generalist", "after.generalist", "before.soft.coral", 
                                                                            "after.soft.coral", "before.not_in_db", "after.not_in_db", "before.NA", "after.NA"), labels=c("before.stress-tolerant"= "Stress-tolerant before", "after.stress-tolerant"="Stress-tolerant after", "before.competitive"="Competitive before", "after.competitive"="Competitive after", "before.weedy"="Weedy before", "after.weedy"="Weedy after", "before.generalist"="Generalist before", "after.generalist"="Generalist after", "before.soft.coral"="Soft coral before", "after.soft.coral"="Soft coral after", "before.not_in_db"="LH unknown before", "after.not_in_db"="LH unknown after", "before.NA" = "NA before", "after.NA" = "NA after"))

heatcol <- c("lightseagreen",  "firebrick2", "darkred", "lightgoldenrod2")

lhcols <- c("indianred2", "steelblue3", "mediumpurple3", "burlywood2")

lhcolscale <- scale_fill_manual(name= "", values = lhcols, breaks = c("stress-tolerant",  "competitive",  "weedy",   "soft.coral"), labels=c("stress-tolerant"= "Stress-tolerant", "competitive"="Competitive", "weedy"="Weedy",   "soft.coral"="Soft coral"))

# set up labels

taxa.labels=c("L. mycetoseroides"= expression(italic("L. mycetoseroides")), 
              "P. varians"= expression(italic("P. varians")),  
              "G. planulata" = expression(italic("G. planulata")), 
              "P. lobata"=expression(italic("P. lobata")), 
              "Leptastrea spp."=expression(paste(italic("Leptastrea"), " spp.")),
              "Fungiidae family spp."= "Fungiidae family spp.", 
              "Unidentifiable"="Unidentifiable", 
              "G. stelligera"=expression(italic("G. stelligera")),
              "M. encrusting"=expression(paste(italic("Montipora"), " (encrusting)")),
              "Platygyra spp."=expression(paste(italic("Platygyra"), " spp.")),
              "Pocillopora spp."=expression(paste(italic("Pocillopora"), " spp.")), 
              "Acropora spp."=expression(paste(italic("Acropora"), " spp.")), 
              "P. meandrina"=expression(italic("P. meandrina")),
              "F. halicora"=expression(italic("F. halicora")), 
              "F. pentagona"=expression(italic("F. pentagona")),
              "L. pruinosa" = expression(italic("L. pruinosa")),
              "D. speciosa" = expression(italic("D. speciosa")),
              "A. curta" = expression(italic("A. curta")), 
              "Astrea spp." = expression(paste(italic("Astrea"), " spp.")),
              "P. profundacella" = expression(italic("P. profundacella")),
              "Lobophytum spp." = expression(paste(italic("Lobophytum"), " spp.")),
              "P. grandis" = expression(italic("P. grandis")), 
              "L. purpurea" = expression(italic("L. purpurea")),
              "A. subulata" = expression(italic("A. subulata")), 
              "Dipsastraea spp." = expression(paste(italic("Dipsastraea"), " spp.")), 
              "P. duerdeni" = expression(italic("P. duerdeni")),
              "Astreopora spp." = expression(paste(italic("Astreopora"), " spp.")),
              "Coscinaraea spp." = expression(paste(italic("Coscinaraea"), " spp.")), 
              "L. hemprichii" = expression(italic("L. hemprichii")),
              "A. loripes" = expression(italic("A. loripes")), 
              "Sarcophyton spp." = expression(paste(italic("Sarcophyton"), " spp.")),
              "A. rosaria-subulata" = expression(italic("A. rosaria-subulata")),
              "Cladiella spp." = expression(paste(italic("Cladiella"), " spp.")),
              "H. exesa" = expression(italic("H. exesa")), 
              "H. limax" = expression(italic("H. limax")), 
              "L. bewickensis" = expression(italic("L. bewickensis")),
              "M. platyphylla" = expression(italic("M. platyphylla")), 
              "P. zelli" = expression(italic("P. zelli")),  
              "A. cucullata" = expression(italic("A. cucullata")), 
              "A. globiceps" = expression(italic("A. globiceps")),  
              "E. aspera" = expression(italic("E. aspera")), 
              "Montipora spp." = expression(paste(italic("Montipora"), " spp.")), 
              "T. stellulata" = expression(italic("T. stellulata")),
              "H. microconos" = expression(italic("H. microconos")), 
              "D. matthaii" = expression(italic("D. matthaii")),
              "M. aequituberculata" = expression(italic("M. aequituberculata")))



# Figure 3 ----------------------------------------------------------------

#~~~~~~~~~~~~
## panel a ##
#~~~~~~~~~~~~
head(fig3a.data)


fig3a <- ggplot(fig3a.data, aes(reorder(genus.sp.short, +order), mean.density.taxa, fill =hs.lh)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.8) +
  geom_errorbar(aes(ymin = mean.density.taxa-se, ymax = mean.density.taxa+se), position = position_dodge(0.8), width = 0.7) +
  lh.heat.colscale + 
  labs(x="", y=expression('Density m'^'-2')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.background=element_blank(), legend.key=element_blank(),
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"),
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(-.5,3), breaks = seq(0,3,0.5)) + guides(fill = "none") +
  scale_x_discrete(labels = taxa.labels) +
  annotate('text', 20.3,2.8, label="a", size=7) +
  annotate('text', 3,1.8, label="*", size=7) + #m. aequituberculata
  annotate('text', 4,1.4, label="*", size=7) + #G. planulata
  annotate('text', 7,0.95, label="*", size=7) + #un id
  annotate('text', 8,0.85, label="*", size=7) + #fungiidae
  annotate('text', 14,0.45, label="*", size=7) + #montipora encrust
  annotate('text', 15,0.45, label="*", size=7) + #pocillopora sp
  annotate('text', 18,0.4, label="*", size=7) + #p meandrina
  coord_cartesian(ylim=c(0, 3))

fig3a


#~~~~~~~~~~~~
## panel b ##
#~~~~~~~~~~~~
head(fig3b.data)

fig3b <- ggplot(fig3b.data, aes(reorder(genus.sp.short, +order), mean.density.taxa, fill =hs.lh)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.8) +
  geom_errorbar(aes(ymin = mean.density.taxa-se, ymax = mean.density.taxa+se), position = position_dodge(0.8), width = 0.7) +
  lh.heat.colscale + 
  labs(x="", y=expression('Density m'^'-2'))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        plot.background=element_blank(),legend.key=element_blank(),
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"),
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank(),
        legend.position = "bottom", legend.text = element_text(size = 12)) +
  scale_y_continuous(expand = c(0,0), limits = c(-1,0.501), breaks = seq(0,0.5,0.1)) +
  guides(fill = guide_legend(nrow=4)) +
  scale_x_discrete(labels = taxa.labels) +
  annotate('text', 26,0.48, label="b", size=7) +
  annotate('text', 2,0.27, label="*", size=7) + # f. halicora
  annotate('text', 7,0.18, label="*", size=7) + # p grandis
  annotate('text', 20,0.14, label="*", size=7) + # astreopora
  annotate("segment", x = 16.2, xend = 16.2, y = 0.1919, yend = 0.5) +
  coord_cartesian(ylim=c(0, 0.5))

fig3b

#~~~~~~~~~~~~
## combine ##
#~~~~~~~~~~~~

#tiff(file="figures/Fig3.tiff",width = 8, height = 10,units="in",res=300)

grid.arrange(arrangeGrob(fig3a, fig3b, layout_matrix = rbind(c(1,1,1,1),
                                                             c(2,2,2,2)), 
                         heights=rbind((unit(4, "in")),(unit(5, "in")))))


#dev.off()

# Figure 4 ----------------------------------------------------------------

#~~~~~~~~~~~~
## panel a ##
#~~~~~~~~~~~~
head(fig4a.data)
head(fig4a.nlabel)

nlabelbefore<-paste("n=", fig4a.nlabel[2,], sep="")
nlabelearly<-paste("n=", fig4a.nlabel[3,], sep="")
nlabellate<-paste("n=", fig4a.nlabel[4,], sep="")
nlabelafter<-paste("n=", fig4a.nlabel[1,], sep="")

fig4a<-ggplot(fig4a.data, aes(heatstress, mean.denity.per.quad, fill=heatstress)) +
  geom_bar(stat='identity',position = "dodge") +
  geom_errorbar(aes(ymin = mean.denity.per.quad-se, ymax = mean.denity.per.quad+se)) +
  scale_fill_manual(values = heatcol) +
  labs(x="", y=expression('Density m'^'-2')) +
  annotate('text', 1,13.6, label=nlabelbefore, size=5) +
  annotate('text', 2,16.8, label=nlabelearly, size=5) +
  annotate('text', 3,7.45, label=nlabellate, size=5) +
  annotate('text', 4,11.6, label=nlabelafter, size=5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.background=element_blank(),legend.key=element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"),
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(0,17.5), breaks = seq(0,17,5)) + 
  guides(fill="none") +
  annotate('text', 4.4,16.5, label="a", size=7)

fig4a

#~~~~~~~~~~~~
## panel b ##
#~~~~~~~~~~~~
head(fig4b.data)

dodge <- position_dodge(width = 0.7)

fig4b<-ggplot(fig4b.data, aes(disturbance.num, density, fill=heatstress))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7)+
  geom_errorbar(aes(ymin = density-se, ymax = density+se), position = dodge, width = 0.5)+
  scale_fill_manual(values = heatcol)+
  labs(x="", y=expression('Density m'^'-2'), fill = "Heat Stress")+
  scale_x_discrete(labels=c("1"= "VL", "2"="L", "3"="M", "4"="H", "5"="VH")) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background=element_blank(),
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"),
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank())+
 scale_y_continuous(expand = c(0,0), limits = c(0,24), breaks = seq(0,24,4))+
  annotate('text', 5.4,22, label="b", size=7)+ 
  guides(fill = "none")

fig4b

#~~~~~~~~~~~~
## panel c ##
#~~~~~~~~~~~~
head(fig4c.data)

fig4c<-ggplot(fig4c.data, aes(heatstress, mean.density.per.sitetrip, fill = heatstress))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean.density.per.sitetrip-se, ymax = mean.density.per.sitetrip+se), position = "dodge")+
  labs(x="", y=expression('Density m'^'-2'), fill = "")+
  scale_fill_manual(values = heatcol)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background=element_blank(), 
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"), 
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank())+ 
  guides(fill="none") +
  scale_y_continuous(expand = c(0,0), limits = c(0,12.1), breaks = seq(0,12,2))+
  annotate('text', 4.4,11.7, label="c", size=7)

fig4c

#~~~~~~~~~~~~
## panel d ##
#~~~~~~~~~~~~
head(fig4d.data)

fig4d<-ggplot(fig4d.data, aes(disturbance.num, mean.density.persitetrip, fill=heatstress))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean.density.persitetrip-se, ymax = mean.density.persitetrip+se), position = "dodge")+
  scale_fill_manual(values = heatcol)+
  scale_x_discrete(labels=c("1"= "VL", "2"="L", "3"="M", "4"="H", "5"="VH")) +
  labs(x="", y=expression('Density m'^'-2'), fill = "Heat Stress")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background=element_blank(), 
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), strip.text = element_text(size = 18), 
        axis.text.x = element_text(angle=0, vjust = 0.001),axis.ticks=element_line(color = "black"), 
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"), 
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0,16), breaks = seq(0,16,4))+ 
  guides(fill="none") +
  annotate('text', 5.4,15.26, label="d", size=7)

fig4d

#~~~~~~~~~~~~
## panel e ##
#~~~~~~~~~~~~
head(fig4e.data)

fig4e<-ggplot(fig4e.data, aes(heatstress, mean.density.per.sitetrip, fill = heatstress))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean.density.per.sitetrip-se, ymax = mean.density.per.sitetrip+se), position = "dodge")+
  labs(x="", y=expression('Density m'^'-2'), fill = "Heat Stress")+
  scale_fill_manual(values = heatcol)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.background=element_blank(), panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"), 
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank()) + 
  guides(fill="none") +
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = seq(0,3.5,0.5))+
  annotate('text', 4.4,3.3, label="e", size=7)

fig4e

#~~~~~~~~~~~~
## panel f ##
#~~~~~~~~~~~~
head(fig4f.data)

fig4f<-ggplot(fig4f.data, aes(disturbance.num, mean.density.persitetrip, fill=heatstress))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean.density.persitetrip-se, ymax = mean.density.persitetrip+se), position = "dodge")+
  scale_fill_manual(values = heatcol)+
  scale_x_discrete(labels=c("1"= "VL", "2"="L", "3"="M", "4"="H", "5"="VH")) +
  labs(x="", y=expression('Density m'^'-2'), fill = "Heat Stress")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        plot.background=element_blank(), panel.background = element_blank(),
        axis.text = element_text(color="black", size = 12), axis.text.x = element_text(angle=0, vjust = 0.001),
        axis.ticks=element_line(color = "black"), axis.ticks.length = unit(0.2, "cm"), 
        axis.line = element_line(color = "black"), axis.title = element_text(color = "black", size = 14), 
        panel.border = element_blank(), legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(color="black", size = 12), legend.title = element_text(color="black", size = 14), 
        legend.background = element_blank(),legend.key = element_rect(color = "white"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,7.05), breaks = seq(0,7,1))+
  annotate('text', 5.4,6.5, label="f", size=7)

fig4f

#~~~~~~~~~~~~
## panel g ##
#~~~~~~~~~~~~
head(fig4g.data)

fig4g<-ggplot(fig4g.data, aes(heatstress, mean.density.per.sitetrip, fill = heatstress))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean.density.per.sitetrip-se, ymax = mean.density.per.sitetrip+se), position = "dodge")+
  labs(x="Heat Stress", y=expression('Density m'^'-2'), fill = "Heat Stress")+
  scale_fill_manual(values = heatcol)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.background=element_blank(), 
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), axis.ticks=element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"), 
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank())+ 
  guides(fill="none") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.5), breaks = seq(0,1.5,0.25))+
  annotate('text', 4.4,1.4, label="g", size=7)

fig4g

#~~~~~~~~~~~~
## panel h ##
#~~~~~~~~~~~~
head(fig4h.data)

fig4h<-ggplot(fig4h.data, aes(disturbance.num, mean.density.persitetrip, fill=heatstress))+
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = mean.density.persitetrip-se, ymax = mean.density.persitetrip+se), position = "dodge")+
  scale_fill_manual(values = heatcol)+
  scale_x_discrete(labels=c("1"= "VL", "2"="L", "3"="M", "4"="H", "5"="VH")) +
  labs(x="Human Disturbance", y=expression('Density m'^'-2'), fill = "Heat Stress")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), plot.background=element_blank(), 
        panel.background = element_blank() ,
        axis.text = element_text(color="black", size = 12), strip.text = element_text(size = 18), 
        axis.text.x = element_text(angle=0, vjust = 0.001), axis.ticks=element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"), axis.line = element_line(color = "black"), 
        axis.title = element_text(color = "black", size = 14), panel.border = element_blank(), 
        legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(color="black", size = 12), legend.title = element_text(color="black", size = 14), 
        legend.background = element_blank(),legend.key = element_rect(color = "white"),)+
  scale_y_continuous(expand = c(0,0), limits = c(0,3), breaks = seq(0,3,0.5))+
  annotate('text', 5.4,2.7, label="h", size=7)

fig4h

#~~~~~~~~~~~~
## legend ##
#~~~~~~~~~~~~

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- g_legend(fig4f)


#~~~~~~~~~~~~
## combine ##
#~~~~~~~~~~~~

##!The blanks are no data and not 0s.

#tiff(file="figures/Fig4.tiff",width = 11, height = 10,units="in",res=300)

grid.arrange(arrangeGrob(fig4a, fig4b, 
                         fig4c, fig4d, 
                         fig4e, fig4f + theme(legend.position="none"), 
                         fig4g, fig4h + theme(legend.position="none"), 
                         legend, 
                         layout_matrix = rbind(c(NA,1,1,1,1,1,1,2,2,2,2,2,2),                                                                      
                                               c(NA,3,3,3,3,3,3,4,4,4,4,4,4),                                                                       
                                               c(NA,5,5,5,5,5,5,6,6,6,6,6,6), 
                                               c(NA,7,7,7,7,7,7,8,8,8,8,8,8),
                                               c(NA,NA,NA,NA,NA,NA,NA,9,9,9,9,9,9)), 
                         heights=rbind((unit(3, "in")),(unit(3, "in")), (unit(3, "in")), (unit(3, "in")), (unit(1, "in")))))

grid.text("Stress-tolerant", x = unit(0.05, "npc"), y = unit(0.678, "npc"), gp = gpar(fontsize = 20), rot = 90)
grid.text("Competitive", x = unit(0.05, "npc"), y = unit(0.45, "npc"), gp = gpar(fontsize = 20), rot = 90)
grid.text("Weedy", x = unit(0.05, "npc"), y = unit(0.215, "npc"), gp = gpar(fontsize = 20), rot = 90)

#dev.off()


# Figure 5 ----------------------------------------------------------------

#~~~~~~~~~~~~
## panel a ##
#~~~~~~~~~~~~

head(fig5a.data)

fig5a <- ggplot(fig5a.data, aes(x=heatstress, y=proportion, group=bleach)) +  
  geom_bar(stat="identity", aes(fill=interaction(bleach, heatstress)), colour="black") +     
  geom_errorbar(aes(ymin=adj.se.lower, ymax=adj.se.upper),                                     
                width=0.05,                                                          
                colour="black" ,                                                        
                position=position_dodge(width=0.1)) +                                  
  labs(x="", y="Percent Bleaching and Healthy", colour="", title="") +                              
  theme_classic()+                                                          
  theme(strip.background=element_blank(),                                               
        legend.title=element_blank(),                                                   
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),    
        axis.line.x = element_line(color="black", size = 0.5),                          
        axis.line.y = element_line(color="black", size = 0.5), 
        axis.text = element_text(color="black", size = 12),
        legend.text = element_text(size = 12),
        legend.position="right") +           
  scale_fill_manual(values=c("white", "lightseagreen", "white", "firebrick2", "white", "darkred"), 
                    labels = c("Bleached", "Healthy")) +    
  scale_y_continuous(expand = c(0,0), limits = c(0,105)) 

fig5a

#~~~~~~~~~~~~
## panel b ##
#~~~~~~~~~~~~
head(fig5b.data)

fig5b<-ggplot(fig5b.data, aes(x=heatstress, y=proportion, group=bleach)) +
  geom_bar(stat="identity", aes(fill=interaction(bleach, disturbance)), colour="black") +               
  geom_errorbar(aes(ymin=adj.se.lower, ymax=adj.se.upper), width=0.05, colour="black", position=position_dodge(width=0.2)) +
  facet_wrap(~disturbance, nrow=1, scales="fixed") +  
  labs(x="", y="Percent Bleaching and Healthy", colour="", title="") +             
  theme_classic() +   
  theme(strip.background=element_blank(),                                               
        legend.title=element_blank(),                                                
        axis.title.y=element_text(margin=margin(0,15,0,10), size=14, face="plain"),    
        axis.line.x = element_line(color="black", size = 0.5),                          
        axis.line.y = element_line(color="black", size = 0.5),      
        axis.text = element_text(color="black", size = 12),
        legend.text = element_text(size = 12),
        legend.position=c(0.85,0.93), 
        strip.text = element_text(size = 14)) +                                                  
  scale_fill_manual(values=c("white", "#2A0BD9",                                        # very low
                             "white", "#40A1FF",                                        # low
                             "white", "#ABF8FF",                                        # medium
                             "white", "#FFAD73",                                       # high
                             "white", "#A60021")) +
  guides(fill = "none") +
  scale_y_continuous(expand = c(0,0), limits = c(0,115), breaks = seq(0, 100, 25))     

fig5b

#~~~~~~~~~~~~
## combine ##
#~~~~~~~~~~~~

#tiff(file="figures/Fig5.tiff",width = 12, height = 9,units="in",res=300)

multiplot(fig5a, fig5b,  
          layout = matrix(c(1,NA,NA,
                            1,NA,NA,
                            2,2,2,
                            2,2,2), nrow=4, byrow=TRUE))

grid.text("a", x = unit(0.015, "npc"), y = unit(0.965, "npc"), gp = gpar(fontsize=15, fontface = "bold"))
grid.text("b", x = unit(0.015, "npc"), y = unit(0.42, "npc"), gp = gpar(fontsize=15, fontface = "bold"))
grid.rect(x = unit(0.26, "npc"), y = unit(0.725, "npc"), width = unit(0.05, "npc"), height = unit(0.12, "npc"), gp = gpar(col = "white"))

#dev.off()

# Figure 6 ----------------------------------------------------------------
head(fig6.data)

fig6 <- ggplot(fig6.data, aes(x = rev(forcats::fct_inorder(Variable)), y = Estimate, color = Variable, fill = Variable)) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_pointrange(aes(x = forcats::fct_inorder(Variable), y = Estimate, ymin = LowerCI, ymax = UpperCI),
                  position = position_dodge(width = 1/2), shape = 21, fatten = 4, size = 1) +
  coord_flip() + theme_cowplot() + xlab("") +  
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, margin = margin(20,0,0,0))) + 
  scale_x_discrete(limits = rev) + 
  scale_colour_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                                 "Early", "Late", "After", 
                                 "Competitive", "Weedy", 
                                 "Windward", "NPP", 
                                 "Dist. * Early (l)", "Dist. * Early (q)", 
                                 "Dist. * Late (l)", "Dist. * Late (q)", 
                                 "Dist. * After (l)", "Dist. * After (q)"), 
                      values = c("black", "black",
                                 "firebrick2", "darkred", "lightgoldenrod2", 
                                 "steelblue3", "mediumpurple3", 
                                 "black", "black", 
                                 "black", "black",
                                 "black", "black",
                                 "black", "black"), 
                      labels = c("Human Dist. ("~italic(l)~")",
                                 "Human Dist. (q)"=expression(paste("Human Dist. (", italic("q"), ")")),
                                 "Early", 
                                 "Late", "After", 
                                 "Competitive", "Weedy", 
                                 "Windward", "NPP", 
                                 expression(paste("Dist. * Early (", italic("l"), ")")), 
                                 expression(paste("Dist. * Early (", italic("q"), ")")),
                                 expression(paste("Dist. * Late (", italic("l"), ")")), 
                                 expression(paste("Dist. * Late (", italic("q"), ")")), 
                                 expression(paste("Dist. * After (", italic("l"), ")")), 
                                 expression(paste("Dist. * After (", italic("q"), ")")))) +
  scale_fill_manual(breaks = c("Human Dist. (l)", "Human Dist. (q)", 
                               "Early", "Late", "After", 
                               "Competitive", "Weedy", 
                               "Windward", "NPP", 
                               "Dist. * Early (l)", "Dist. * Early (q)", 
                               "Dist. * Late (l)", "Dist. * Late (q)", 
                               "Dist. * After (l)", "Dist. * After (q)"), 
                    values = c("black", "black",
                               "firebrick2", "darkred", "lightgoldenrod2", 
                               "steelblue3", "mediumpurple3", 
                               "black", "black", 
                               "black", "black",
                               "black", "black",
                               "black", "black"), 
                    labels = c("Human Dist. ("~italic(l)~")",
                               "Human Dist. (q)"=expression(paste("Human Dist. (", italic("q"), ")")), 
                               "Early", "Late", "After", 
                               "Competitive", "Weedy", 
                               "Windward", "NPP", 
                               expression(paste("Dist. * Early (", italic("l"), ")")), 
                               expression(paste("Dist. * Early (", italic("q"), ")")),
                               expression(paste("Dist. * Late (", italic("l"), ")")), 
                               expression(paste("Dist. * Late (", italic("q"), ")")), 
                               expression(paste("Dist. * After (", italic("l"), ")")), 
                               expression(paste("Dist. * After (", italic("q"), ")")))) + 
  guides(fill = "none", color = "none") 

#tiff(file="figures/Fig6.tiff",width = 8, height = 6,units="in",res=300)

fig6

#dev.off()

# Figure 7 ----------------------------------------------------------------
head(fig7.data)

fig7 <- ggplot(fig7.data) +
  geom_segment(aes(x=reorder(genus.sp.short, -percent.loss), xend=genus.sp.short, y=0, yend=percent.loss), color="grey40") +
  geom_point(aes(x=reorder(genus.sp.short, -percent.loss), y=percent.loss, fill = lifehistory), color = "black", size=4, shape = 21) + 
  lhcolscale +
  coord_flip() + theme_light() +
  labs(x = "", y = "Percent Loss") + 
  scale_x_discrete(labels = taxa.labels) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,105), breaks = seq(0,100,20)) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey65"), panel.grid.minor.x = element_line(color = "grey75"),
        panel.border = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black")) +
  guides(fill = guide_legend(position = "bottom"))

#tiff(file="figures/Fig7.tiff",width = 8, height = 6,units="in",res=300)

fig7

#dev.off()
