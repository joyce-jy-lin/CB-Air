library(ggplot2)
library(readxl)
library(GGally)      # for fancier scatterplot matrices
library(broom)       # for tidy model output using tidy() function
library(Hmisc)       # for describe() function
library(dplyr)
library(tidyverse) # general functions for working with data

### PM boxplots
PM25  <- read_excel("PM25.xlsx")
PM10 <- read_excel("PM10.xlsx")
PM1025 <-read_excel("PM1025.xlsx")
library(scales)
require(scales)
library(ggplot2)
library(reshape2)
require(reshape2)
require(ggplot2)
#PM1025 together
PM1025$Site <- factor(PM1025$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay1<-ggplot(data = PM1025, aes(x=PM, y=Conc, fill=Site))+geom_boxplot() + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Particle Size", y="Concentration (ug/m3)") +theme(text = element_text(size = 20)) + theme_bw() +  scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
yay1 + theme(text = element_text(size = 15)) 

# transparent background ------
yay1trans <- yay1 + ggtitle("PM Concentration By Particle Size and Site") + theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
)
yay1trans
ggsave(yay1trans, filename = "pm1025trans.png",  bg = "transparent")

## Boxplot of PM25 and PM10 together ---------
PM1025$PM <- factor(PM1025$PM , levels=c("25", "10"), labels=c("PM2.5", "PM10"))
yay6<-ggplot(data = PM1025, aes(x=Site, y=Conc))+geom_boxplot(aes(fill=PM)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Site", y="Concentration (ug/m3)") +theme(text = element_text(size = 20)) + theme_bw() +labs(x="Site", y="Concentration (ug/m3)") ## ad + scale_fill_grey() + theme_classic() if grayscale
yay6 + theme(text = element_text(size = 15))


#PM25 natural y scale ----------
PM25$Site <- factor(PM25$Site, levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c( "JHSPH (n=19)", "BF (n=27)", "PENN (n=16)", "CURTIS (n=17)", "FG (n=6)", "ANN (n=8)"))
yay1<-ggplot(data = PM25, aes(x=Site, y=PM25))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Site", y="Concentration (ug/m3)") +theme(text = element_text(size = 20)) + theme_bw() +labs(x="Site", y="Concentration (ug/m3)") +  scale_fill_manual(values=c( "#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
yay1<-yay1 + ggtitle("PM2.5 Concentration By Site") + theme(text = element_text(size = 15))+ (geom_hline(yintercept=35, color ='red'))
yay1

# PM10 natural scale ---------
PM10$Site <- factor(PM10$Site, levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c( "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay2<-ggplot(data = PM10, aes(x=Site, y=PM10))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Site", y="Concentration (ug/m3)") +theme(text = element_text(size = 20)) + theme_bw() +labs(x="Site", y="Concentration (ug/m3)") +  scale_fill_manual(values=c( "#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
yay2<-yay2 + ggtitle("PM10 Concentration By Site") + theme(text = element_text(size = 15))+ (geom_hline(yintercept=150, color ='red'))


## PM25 Metals by site for CB study --------
CBno <- read_excel("Metal25-meltNO.xlsx")
library(scales)
library(ggplot2)
library(reshape2)
require(reshape2)
require(ggplot2)
require(scales) 
library(ggsci)
show_col(pal_npg("nrc")(8))
show_col(pal_npg("nrc")(6))
## PM2.5 metals by site CB only
CBno$Site <- factor(CBno$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay3<-ggplot(data = CBno, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site)) + 
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


## PM10 Metals by site for CB study
CBm10 <- read_excel("Metal10-melt.xlsx")
CBm10$Site <- factor(CBm10$Site , levels=c("JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("JHSPH (n=13)", "BF (n=18)", "PENN (n=9)", "CURTIS (n=12)", "FG (n=4)", "ANN (n=4)"))
yay5<-ggplot(data = CBm10, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  scale_fill_manual(values=c("#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF")) 
yay5 <- yay5 + facet_wrap( ~ Metal, scales="free")
yay5 <- yay5 + ylab("Concentration (ug/m3)") + ggtitle("PM10 Metal Concentrations - By Site")
yay5 <- yay5 + guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
yay5

#Metals by site and particle size
CBm1025 <- read_excel("Metal1025-melt.xlsx")
CBm1025
CBm1025$PM <- factor(CBm1025$PM , levels=c("25", "10"), labels=c("PM2.5", "PM10"))
yay6<-ggplot(data = CBm1025, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=PM)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +theme(text = element_text(size = 20)) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") + scale_fill_brewer(palette="RdBu") ## ad + scale_fill_grey() + theme_classic() if grayscale
yay6 + ggtitle("Metal Concentration By Particle Size") + theme(text = element_text(size = 15))
#transparent background
yay6trans <- yay6 + ggtitle("Metal Concentration By Particle Size") + theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
)
yay6trans
ggsave(yay6trans, filename = "metal1025trans.png",  bg = "transparent")

## facet wrap
yay7<-ggplot(data = CBm1025, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=PM)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") + scale_fill_brewer(palette="RdBu") 
yay7 <- yay7 + facet_wrap( ~ Metal, scales="free")
yay7 <- yay7 + ylab("Concentration (ug/m3)") + ggtitle("Metal Concentrations - By PM Size")
yay7 <- yay7 + guides(fill=guide_legend(title="PM Size"))+ theme(text = element_text(size = 15))
yay7


#######  changes over time using ondov data imputed with lowest/SQRT2 (all sites and metals in one plot) -------
CBm25 <- read_excel("Metal25-melt.xlsx")
png("temporalvar.png", width = 13, height = 5, units = 'in', res = 300)#save in high res
CBm25$Site <- factor(CBm25$Site , levels=c( "PONCA", "CLIFTON", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("PONCA 2003", "CLIFTON 2001", "JHSPH", "BF", "PENN", "CURTIS", "FG", "ANN"))
yay1<-ggplot(data = CBm25, aes(x=Metal, y=Conc, fill=Site))+geom_boxplot() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  annotation_logticks(sides ="l") +
  theme(text = element_text(size = 20)) + theme_bw() +
  labs(x="Metal", y = bquote('Concentration'~(µg/m^3)))+  
  scale_fill_manual(values=c("grey65", "grey80", "#E64B35FF" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF"))## ad + scale_fill_grey() + theme_classic() if grayscale
yay1 + theme(text = element_text(size = 15))
dev.off()

## Changes over time comparing Clifton to JHSPH 
CBbp <- read_excel("Metal25Boxplot.xlsx")
CBbp$Site <- factor(CBbp$Site , levels=c("CLIFTON", "JHSPH"), labels=c("CLIFTON 2001", "JHSPH 2019"))
bp<-ggplot(data = CBbp, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  scale_fill_manual(values=c("grey65" ,"#E64B35FF")) 
bp <- bp + facet_wrap( ~ Metal, scales="free")
bp <- bp +  labs(y = bquote('Concentration'~(ug/m^3)))
bp <- bp + guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
bp

## Changes over time comparing PONCA to everything else but JHSPH 
CBbp <- read_excel("Metal25Boxplot.xlsx")
CBbp$Site <- factor(CBbp$Site , levels=c("PONCA", "BF", "PENN", "CURTIS", "FG", "ANN"), labels=c("PONCA 2001", "BF 2019", "PENN 2019", "CURTIS 2019", "FG 2019", "ANN 2019"))
bp1<-ggplot(data = CBbp, aes(x=Metal, y=Conc))+geom_boxplot(aes(fill=Site)) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme_bw() +labs(x="Metal", y="Concentration (ug/m3)") +  scale_fill_manual(values=c("grey80" , "#F39B7FFF","#4DBBD5FF", "#00A087FF", "#91D1C2FF", "#8491B4FF")) 
bp1 <- bp1 + facet_wrap( ~ Metal, scales="free")
bp1 <- bp1 +  labs(y = bquote('Concentration'~(ug/m^3)))
bp1 <- bp1 + guides(fill=guide_legend(title="Site"))+ theme(text = element_text(size = 15))
bp1

#Geomean & GeoSD for each metal by site

CB25 <- read_excel("CBMetal25.xlsx")
CB25 <- read_excel("Metal25-melt.xlsx")
CB10 <- read_excel("Metal10-melt.xlsx")

library(EnvStats)
Cr25<-filter(CB25, Metal =="Cr")
Cr25
Cr25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Mn25<-filter(CB25, Metal =="Mn")
Mn25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

As25<-filter(CB25, Metal =="As")
As25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Cd25<-filter(CB25, Metal =="Cd")
Cd25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Co25<-filter(CB25, Metal =="Co")
Co25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Cu25<-filter(CB25, Metal =="Cu")
Cu25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Fe25<-filter(CB25, Metal =="Fe")
Fe25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Mo25<-filter(CB25, Metal =="Mo")
Mo25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Mn25<-filter(CB25, Metal =="Mn")
Mn25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Ni25<-filter(CB25, Metal =="Ni")
Ni25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Pb25<-filter(CB25, Metal =="Pb")
Pb25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Sb25<-filter(CB25, Metal =="Sb")
Sb25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Zn25<-filter(CB25, Metal =="Zn")
Zn25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
#GM + GSD all sites by metal PM10
As10<-filter(CB10, Metal =="As")
Cd10 <- filter(CB10, Metal =="Cd")
Cr10<-filter(CB10, Metal =="Cr")
Co10<-filter(CB10, Metal =="Co")
Cu10<-filter(CB10, Metal =="Cu")
Fe10<-filter(CB10, Metal =="Fe")
Mn10<-filter(CB10, Metal =="Mn")
Mo10<-filter(CB10, Metal =="Mo")
Ni10<-filter(CB10, Metal =="Ni")
Pb10<-filter(CB10, Metal =="Pb")
Sb10<-filter(CB10, Metal =="Sb")
Zn10<-filter(CB10, Metal =="Zn")

geoMean(As10$Conc)
geoSD(As10$Conc)
geoSD(Cd10$Conc)
geoSD(Co10$Conc)
geoSD(Cr10$Conc)
geoSD(Cu10$Conc)
geoSD(Fe10$Conc)
geoSD(Mn10$Conc)
geoSD(Mo10$Conc)
geoSD(Ni10$Conc)
geoSD(Pb10$Conc)
geoSD(Sb10$Conc)
geoSD(As10$Conc)
geoSD(Zn10$Conc)

###By site by metal PM10
Mn10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

As10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Cd10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Co10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Cu10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Fe10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Mo10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Mn10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Ni10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Pb10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))
Sb10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Zn10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

Cr10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))


#####Basic summary stats for PM
PM  <- read_excel("PM1025.xlsx")
summary(PM)
describe(PM$PM)

PM <- PM1025$PM
install.packages('EnvStats')
library(EnvStats)
PM25<-filter(PM1025, PM =="PM25")
PM25
PM25 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              med = median(Conc),
                                              max = max(Conc),
                                              min = min(Conc),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

PM10<-filter(PM1025, PM =="PM10")
PM10
PM10 %>% group_by(Site) %>% dplyr:: summarize(n=n(),
                                              med = median(Conc),
                                              max = max(Conc),
                                              min = min(Conc),
                                              gm = geoMean(Conc), 
                                              gsd = geoSD(Conc))

