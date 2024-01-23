setwd("/Users/joycelin/Desktop/CB FINAL")
library(readxl)
library(dplyr)
library(tidyverse) 
library(scales)
library(EnvStats) 

## PM2.5 and PM10 boxplot linear scale ---------
PM1025 <-read_excel("PM1025.xlsx")
PM1025$Site <- factor(PM1025$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
PM1025$PM <- factor(PM1025$PM , levels=c("PM2.5", "PM10"))
fig1<-ggplot(data = PM1025, aes(x=PM, y=Conc, fill=Site))+ geom_boxplot(aes(fill=Site), outlier.size = 0.4) + geom_point(aes(fill = Site), size = 0.4,  position = position_jitterdodge()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_bw() +
  labs(x="Particle Size", y = bquote('Concentration'~(µg/m^3))) + 
  theme(text = element_text(size = 20)) + theme_bw() + 
  scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
fig1 + theme(text = element_text(size = 15)) 

# to save in high resolution to folder
png("pm1025.png", width = 6, height = 4, units = 'in', res = 300) 
fig1
dev.off() 

## changes over time (all sites and metals in one plot) -------
CBm25 <- read_excel("Metal25-melt.xlsx")
CBm25$Site <- factor(CBm25$Site , levels=c( "PONCA", "CLIFTON", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("PONCA 2003", "CLIFTON 2001", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
fig2<-ggplot(data = CBm25, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site), outlier.size = 0.7) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  annotation_logticks(sides ="l") +
  theme(text = element_text(size = 20)) + theme_bw() +
  labs(y = bquote('Concentration'~(µg/m^3)), x= bquote('Metal'))  +  
  scale_fill_manual(values=c("grey60", "grey90", "#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
fig2 = fig2 + theme(text = element_text(size = 13))
fig2

# save image in high resolution
png("metalchange.png", width = 10, height = 4, units = 'in', res = 300)#save in high res
fig2
dev.off()

## PM2.5 spatial variation -- facet wrapped for seperate subplots with different y-axes
CBno <- read_excel("Metal25-meltNO.xlsx")
CBno$Site <- factor(CBno$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
fig3<-ggplot(data = CBno, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site),  outlier.size = 0.7) + 
  scale_y_continuous(
    trans = "log10",
    breaks = function(x) {
      brks <- extended_breaks(Q = c(0, 5))(log10(x))
      10^(brks[brks %% 1 == 0])
    },
    labels = math_format(format = log10)
  ) + theme_bw() + scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))
fig3 <- fig3 + facet_wrap( ~ Metal, scales="free")
fig3 <- fig3 + labs(y = bquote('Concentration'~(µg/m^3))) + annotation_logticks(sides ="l") 
fig3 <- fig3 + guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
fig3

png("25spatvar.png", width = 7, height = 4, units = 'in', res = 300)#
fig3
dev.off()
