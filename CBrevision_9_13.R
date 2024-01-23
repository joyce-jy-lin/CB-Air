setwd("/Users/joycelin/Desktop/CB FINAL")
library(readxl)
library(dplyr)
library(tidyverse) 
library(scales)
library(EnvStats)
### Summary table of PM2.5 and PM10 concentrations
PM1025 <-read_excel("PM1025.xlsx")
PM1025 %>% group_by(Site, PM) %>% dplyr:: summarize(n=n(), med = median(Conc), min = min(Conc), max = max(Conc)) %>%
  knitr::kable()

## PM2.5 and PM10 boxplot linear scale ---------
PM1025 <-read_excel("PM1025.xlsx")
png("pm1025.png", width = 6, height = 4, units = 'in', res = 300)#save in high res
PM1025$Site <- factor(PM1025$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
PM1025$PM <- factor(PM1025$PM , levels=c("PM2.5", "PM10"))
yay1<-ggplot(data = PM1025, aes(x=PM, y=Conc, fill=Site))+ geom_boxplot(aes(fill=Site), outlier.size = 0.4) + geom_point(aes(fill = Site), size = 0.4,  position = position_jitterdodge()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_bw() +
  labs(x="Particle Size", y = bquote('Concentration'~(µg/m^3))) + 
  theme(text = element_text(size = 20)) + theme_bw() + 
  scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
yay1 + theme(text = element_text(size = 15)) 

dev.off()

## PM2.5 Metal spatial variation boxplot  ------
##not facet wrapped clearer y axis
CBno <- read_excel("Metal25-meltNO.xlsx")
png("spatialvar.png", width = 5, height = 4, units = 'in', res = 300)#
CBno$Site <- factor(CBno$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay4<-ggplot(data = CBno, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site), outlier.size = 0.7, width=1) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") + annotation_logticks(sides ="l") +
  scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF")) +
  labs(y = bquote('Concentration'~(µg/m^3))) + theme(text = element_text(size = 15))
yay4
dev.off()

### PM2.5 spatial var facet wrapped
CBno <- read_excel("Metal25-meltNO.xlsx")
CBno$Site <- factor(CBno$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay3<-ggplot(data = CBno, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site),  outlier.size = 0.7) + 
  scale_y_continuous(
    trans = "log10",
    breaks = function(x) {
      brks <- extended_breaks(Q = c(0, 5))(log10(x))
      10^(brks[brks %% 1 == 0])
    },
    labels = math_format(format = log10)
  ) + theme_bw() + scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))
yay3 <- yay3 + facet_wrap( ~ Metal, scales="free")
yay3 <- yay3 + labs(y = bquote('Concentration'~(µg/m^3))) + annotation_logticks(sides ="l") 
yay3 <- yay3 + guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
yay3

png("25spatvar.png", width = 7, height = 4, units = 'in', res = 300)#
yay3
dev.off()

##PM10 Metal spatial variation boxplot
CBm10 <- read_excel("Metal10-melt.xlsx")
png("spatialvar.png", width = 5, height = 4, units = 'in', res = 300)#
CBm10$Site <- factor(CBm10$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay5<-ggplot(data = CBm10, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site), outlier.size = 0.7, width=1) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") + annotation_logticks(sides ="l") +
  scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF")) +
  labs(y = bquote('Concentration'~(µg/m^3))) +
  guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
yay5
dev.off()

## PM10 metal var facet wrap
CBm10 <- read_excel("Metal10-melt.xlsx")
CBm10$Site <- factor(CBm10$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay6<-ggplot(data = CBm10, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site),  outlier.size = 0.7) + 
  scale_y_continuous(
    trans = "log10",
    breaks = function(x) {
      brks <- extended_breaks(Q = c(0, 5))(log10(x))
      10^(brks[brks %% 1 == 0])
    },
    labels = math_format(format = log10)
  ) + theme_bw() + scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))
yay6 <- yay6 + facet_wrap( ~ Metal, scales="free")
yay6 <- yay6 + labs(y = bquote('Concentration'~(µg/m^3))) + annotation_logticks(sides ="l") 
yay6 <- yay6 + guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
yay6

png("10spatvar.png", width = 7, height = 4, units = 'in', res = 300)#
yay6
dev.off()

###

install.packages("ggpubr")
library(ggpubr)
figure <- ggarrange(yay4, yay5, 
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure

png("spatialvar.png", width = 12, height = 4, units = 'in', res = 300)#
figure
dev.off()

#######  changes over time using ondov data imputed with lowest/SQRT2 (all sites and metals in one plot) -------
CBm25 <- read_excel("Metal25-melt.xlsx")
CBm25$Site <- factor(CBm25$Site , levels=c( "PONCA", "CLIFTON", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("PONCA 2003", "CLIFTON 2001", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay1<-ggplot(data = CBm25, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site), outlier.size = 0.7) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  annotation_logticks(sides ="l") +
  theme(text = element_text(size = 20)) + theme_bw() +
  labs(y = bquote('Concentration'~(µg/m^3)), x= bquote('Metal'))  +  
  scale_fill_manual(values=c("grey60", "grey90", "#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
yay1 = yay1 + theme(text = element_text(size = 13))
yay1

png("metalchange.png", width = 10, height = 4, units = 'in', res = 300)#save in high res
yay1
dev.off()

## Changes over time comparing Clifton to JHSPH -------
CBbp <- read_excel("Metal25Boxplot.xlsx")
CBbp$Site <- factor(CBbp$Site , levels=c("CLIFTON", "JHSPH"), labels=c("CLIFTON 2001", "JHSPH 2019"))
bp<-ggplot(data = CBbp, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  scale_fill_manual(values=c("grey65" ,"#E64B35FF")) +
  labs(y = bquote('Concentration'~(ug/m^3))) + 
  guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
bp

## Changes over time comparing PONCA to everything else but JHSPH 
CBbp <- read_excel("Metal25Boxplot.xlsx")
CBbp$Site <- factor(CBbp$Site , levels=c("PONCA", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("PONCA 2001", "BF 2019", "PENN 2019", "CURTIS 2019", "FG 2019", "ANN 2019"))
bp1<-ggplot(data = CBbp, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  scale_fill_manual(values=c("grey80" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))  +  
  labs(y = bquote('Concentration'~(ug/m^3))) + annotation_logticks(sides ="l")+
  guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
bp1


### histogram of %<LOD
CBm25 <- read_excel("Metal25-melt.xlsx")
CBm25$Site <- factor(CBm25$Site , levels=c( "PONCA", "CLIFTON", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("PONCA 2003", "CLIFTON 2001", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
CBm25$Metal <- factor(CBm25$Metal , levels=c( "As", "Cd", "Co", "Cr", "Cu", "Fe", "Mn", "Mo", "Ni", "Pb", "Sb", "Zn"), labels=c("As", "Cd", "Co", "Cr", "Cu", "Fe", "Mn", "Mo", "Ni", "Pb", "Sb", "Zn"))
bar<-ggplot(CBm25, aes(x=Metal, y=PM25LOD, fill = Site)) + geom_col(position = "dodge") +  scale_fill_manual(values=c("grey65", "grey80", "#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))  + theme_bw()
bar = bar + labs(y = bquote('% < LOD')) +  ylim(0, 100)

png("LODgraph.png", width = 10, height = 4, units = 'in', res = 300)#save in high res
bar
dev.off()
  