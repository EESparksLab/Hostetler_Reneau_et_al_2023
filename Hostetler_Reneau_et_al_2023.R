#Hostetler et al., 2023 - SMURF: A tool to non-destructively measure root torsional stiffness for understanding root lodging-resistance
library(agricolae)
library(rcompanion)
library(ggplot2)
library(dplyr)
library(tidyr)
library(goeveg)
library(vegan)
library(car)
library(tidyverse)
library("cowplot")
citation("rcompanion")
citation("car")
citation("ggplot2")
citation("cowplot")
#Root system stiffness measurements are reproducible across testing positions and time of day.####
##Repeat testing #####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/")
data0 = read.csv("Database of SMURF Data - 2021_RepeatTestingData.csv", header = TRUE, na.strings = "NA")
data0 = subset(data0, Experiment == "Repeat Testing 2") 
data = subset(data0, displacement_mm == "11.8")
data$ID = paste(data$Plot, data$Replicate, sep="_")
data = subset(data, Test_Type != "D")
##Figure
plot.labs <- c("CML258_Plot1", "CML258_Plot2")
names(plot.labs) <- c("965_1", "965_2")
Fig2A = ggplot(data=data,aes(x=Test_Type, y=line_raw_slope_N.m, color=Additional_Notes))+
  geom_boxplot(size = 0.25, color = "black") + 
  geom_point(size = 1) +
  xlab("Test Order") + 
  ylab("Root System Stiffness (N/m)")+
  labs(color = "Test Position")+
  scale_color_manual(values=c("lightsalmon3","mistyrose3","slategrey"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        #axis.text.x = element_text(size=10, angle = 45),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))+
  facet_grid(~ID, labeller = labeller(ID = plot.labs))
Fig2A

head(data0)
data0$ID = paste(data0$Plot, data0$Replicate, sep="_")
d1 = subset(data0, Additional_Notes == "TEST2")
d1 = subset(d1, Test_Type != "D")

rep.labs <- c("Replicate 1", "Replicate 2")
names(rep.labs) <- c("965_1", "965_2")
FigS5 = ggplot(d1, aes(x=displacement_m, y=force_N, color=Test_Type)) +
  geom_point()+
  xlab("Displacement (m)") + 
  ylab("Force (N)")+
  labs(color = "Repeat Measure")+
  scale_color_manual(name = "Test Order",
                    values = c("mistyrose2", "slategray", "pink4", "palegreen4", "burlywood", "indianred3"))+
  theme_bw()+
  theme(
        axis.text.x = element_text(size=10, angle = 45, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))+
  geom_smooth(method = lm, se=F)+
  facet_grid(~ID, labeller = labeller(ID = rep.labs))
FigS5

##Stats
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ Additional_Notes*Test_Type)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Additional_Notes"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Test_Type"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Additional_Notes","Test_Type"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
detach(data)

##Time of Day Testing######
data0 = read.csv("Hostetler_Reneau_et_al_2024.csv", header = TRUE, na.strings = "NA")
data = subset(data0, Experiment == "Time_of_Day")
data$Year = as.factor(data$Year)
data$Additional_Notes = factor(data$Additional_Notes,levels = c("9:00 AM","12:00 PM","4:00 PM"))
data$Genotype = factor(data$Genotype,levels = c("CML258","B73","W22"))
###Figure
data$DAP = as.character(data$DAP)
data$DAP = factor(data$DAP, levels = c("60","85","101"))
head(data)
data1 = subset(data, Genotype != "CML258")
Fig2B =ggplot(data=data1,aes(x=Additional_Notes, y=line_raw_slope_N.m, color=Year))+
  geom_boxplot(size = 0.25, color="black") + 
  geom_point(size=1, position=position_jitterdodge())+
  xlab("Time of Testing") + 
  ylab("Root System Stiffness (N/m)")+
  labs(color = "Days after Planting")+
  scale_fill_manual(values=c("white","gray"))+
  scale_color_manual(values=c("lightsalmon3"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))+
  facet_grid(~Genotype)
Fig2B
data1 = subset(data, Genotype == "CML258")
head(data1)
year.labs <- c("CML258-2021", "CML258-2022")
names(year.labs) <- c("2021", "2022")
Fig2C =ggplot(data=data1,aes(x=Additional_Notes, y=line_raw_slope_N.m, color=Year))+
  geom_boxplot(size = 0.25, color="black") + 
  geom_point(size=1,position=position_jitterdodge())+
  xlab("Time of Testing") + 
  ylab("Root System Stiffness (N/m)")+
  labs(color = "Days after Planting")+
  scale_fill_manual(values=c("white","gray"))+
  scale_color_manual(values=c("lightsalmon3","slategrey"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))+
  facet_grid(~Year, labeller = labeller(Year = year.labs))
Fig2C
ggdraw() +
  draw_plot(Fig2A, x = 0, y = 0, width = .33, height = 1) +
  draw_plot(Fig2B, x = 0.33, y = 0, width = .33, height = 1) +
  draw_plot(Fig2C, x = 0.66, y = 0, width = .33, height = 1) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 12,
                  x = c(0, 0.33,0.66), 
                  y = c(1, 1,1))
###Stats
#Does Genotype or Time of Day impact RTS
head(data)
data1 = subset(data, Year == "2021")
data1 = subset(data, Genotype != "CML258")
attach(data1)
lm_x <- lm(line_raw_slope_N.m ~ Genotype*Additional_Notes)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Genotype", "Additional_Notes"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Additional_Notes"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data1$line_raw_slope_N.m)
hist(data1$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data1$tukey <- transformTukey(
  data1$line_raw_slope_N.m,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
hist(data1$tukey)
lm_x2 <- lm(data1$tukey ~ Genotype*Additional_Notes)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
resid = residuals(object = lm_x2_aov2)
shapiro.test(x=resid)
HSD.test(lm_x2_aov2, trt = c("Genotype"), console = TRUE)
data1$tukey = NULL
detach(data1)
#Does Time of Day impact RTS
data1 = subset(data, Year == "2021")
data1 = subset(data, Genotype == "CML258")
attach(data1)
lm_x <- lm(line_raw_slope_N.m ~ Additional_Notes)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Additional_Notes"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data1$line_raw_slope_N.m)
hist(data1$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
detach(data1)
#Does Time of Day impact RTS
data1 = subset(data, Year == "2022")
attach(data1)
lm_x <- lm(line_raw_slope_N.m ~ Additional_Notes)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Additional_Notes"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data1$line_raw_slope_N.m)
hist(data1$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
detach(data1)

#Root system stiffness varies among maize hybrids.####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/")
data0 = read.csv("Hostetler_Reneau_et_al_2024.csv", header = TRUE, na.strings = "NA")
data = subset(data0, Experiment == "Lodging Hybrids")
data = subset(data, Test_Type == "A")
data$DAP = as.numeric(data$DAP)
data$Year = as.character(data$Year)
data$DAP_cat = data$DAP
data$DAP_cat = as.numeric(data$DAP_cat)
for (i in 1:length(data$DAP_cat)){
  if (data$DAP[i] >= 100) {
    data$DAP_cat[i] = "110-120"
  } else if (data$DAP[i] >= 80) {
    data$DAP_cat[i] = "80-100"
  } else if (data$DAP[i] >= 50) {
    data$DAP_cat[i] = "50-60"
  }
}
data$DAP_cat = factor(data$DAP_cat,levels = c("50-60","80-100","110-120"))
data$ID = paste(data$Year, data$DAP_cat, sep="_")
data %>%  #Determine how many plants per plot for each genotype in a year
  count(Year, Genotype, Plot)
data$ID = factor(data$ID, levels = c("2020_50-60","2020_80-100","2021_80-100","2021_110-120","2022_110-120"))
unique(data$ID)
data1 = subset(data, ID == "2020_50-60")
data1$Genotype = factor(data1$Genotype,levels = c("LH198 x PHN46","DK3IIH6 x LH198","LH145 x DK3IIH6","PHB47 x PHP02",
                                                "PHB47 x LH198","PHK76 x PHP02","LH82 x LH145","LH82 x DK3IIH6", "LH82 x PHK76",
                                                "PHK56 x PHK76","LH82 x PHP02"))

Fig3A=ggplot(data=data1,aes(x=Genotype, y=line_raw_slope_N.m))+
  geom_boxplot(fill="gray70")+
  xlab("Genotype") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "DAP")+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x=  element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y=  element_blank())
Fig3A
unique(data$ID)
data1 = subset(data, ID == "2020_80-100")
data1$Genotype = factor(data1$Genotype,levels = c("LH198 x PHN46","DK3IIH6 x LH198","LH145 x DK3IIH6","PHB47 x PHP02",
                                                 "PHB47 x LH198","PHK76 x PHP02","LH82 x LH145","LH82 x DK3IIH6", "LH82 x PHK76",
                                                 "PHK56 x PHK76","LH82 x PHP02"))

Fig3B=ggplot(data=data1,aes(x=Genotype, y=line_raw_slope_N.m))+
  geom_boxplot(fill="gray70")+
  xlab("Genotype") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "DAP")+
  #scale_fill_manual(values=c("lightsalmon3","mistyrose3","slategrey"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x=  element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y=  element_blank())
Fig3B
unique(data$ID)
data1 = subset(data, ID == "2021_80-100")
data1$Genotype = factor(data1$Genotype,levels = c("LH198 x PHN46","DK3IIH6 x LH198","LH145 x DK3IIH6","PHB47 x PHP02",
                                                 "PHB47 x LH198","PHK76 x PHP02","LH82 x LH145","LH82 x DK3IIH6", "LH82 x PHK76",
                                                 "PHK56 x PHK76","LH82 x PHP02"))

Fig3C=ggplot(data=data1,aes(x=Genotype, y=line_raw_slope_N.m))+
  geom_boxplot(fill="gray70")+
  xlab("Genotype") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "DAP")+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x=  element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y=  element_blank())
Fig3C
unique(data$ID)
data1 = subset(data, ID == "2021_110-120")
data1$Genotype = factor(data1$Genotype,levels = c("LH198 x PHN46","DK3IIH6 x LH198","LH145 x DK3IIH6","PHB47 x PHP02",
                                                 "PHB47 x LH198","PHK76 x PHP02","LH82 x LH145","LH82 x DK3IIH6", "LH82 x PHK76",
                                                 "PHK56 x PHK76","LH82 x PHP02"))

Fig3D=ggplot(data=data1,aes(x=Genotype, y=line_raw_slope_N.m))+
  geom_boxplot(fill="gray70")+
  xlab("Genotype") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "DAP")+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x=  element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y=  element_blank())
Fig3D
unique(data$ID)
data1 = subset(data, ID == "2022_110-120")
data1$Genotype = factor(data1$Genotype,levels = c("LH198 x PHN46","DK3IIH6 x LH198","LH145 x DK3IIH6","PHB47 x PHP02",
                                                 "PHB47 x LH198","PHK76 x PHP02","LH82 x LH145","LH82 x DK3IIH6", "LH82 x PHK76",
                                                 "PHK56 x PHK76","LH82 x PHP02"))

Fig3E=ggplot(data=data1,aes(x=Genotype, y=line_raw_slope_N.m))+
  geom_boxplot(fill="gray70")+
  xlab("Genotype") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "DAP")+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,2000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x=  element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y=  element_blank())
Fig3E
X=data %>% group_by(Year) %>% count(Genotype, DAP_cat) 
#write.csv(X, file = "GenotypeReplicates.csv", quote = F, row.names = T)
data0 = data
head(data0)
unique(data0$ID)
data = subset(data0, ID == "2020_50-60") 
##Stats###
head(data)
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ Genotype)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Genotype"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ Genotype*DAP_cat)
#transformation data to meet assumptions
par(mfrow=c(2,2))
data$tukey <- transformTukey(
  data$line_raw_slope_N.m,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE)
hist(data$tukey)
lm_x2 <- lm(data$tukey ~ Genotype)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("Genotype"), console = TRUE)
data$tukey = NULL
detach(data)

unique(data0$ID)
data = subset(data0, ID == "2020_80-100") 
##Stats###
head(data)
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ Genotype)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Genotype"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ Genotype)
detach(data)

unique(data0$ID)
data = subset(data0, ID == "2021_80-100") 
##Stats###
head(data)
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ Genotype)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Genotype"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ Genotype)
detach(data)

unique(data0$ID)
data = subset(data0, ID == "2021_110-120") 
##Stats###
head(data)
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ Genotype)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Genotype"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ Genotype)
detach(data)

unique(data0$ID)
data = subset(data0, ID == "2022_110-120") 
##Stats###
head(data)
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ Genotype)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Genotype"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ Genotype)
detach(data)


data2 = read.csv("HybridOrder.csv", header = TRUE, na.strings = "NA")
head(data2)
str(data2)
data2$X = as.character(data2$X)
data2$Hybrid = factor(data2$Hybrid,levels = c("LH198 x PHN46","DK3IIH6 x LH198","LH145 x DK3IIH6","PHB47 x PHP02",
                                                 "PHB47 x LH198","PHK76 x PHP02","LH82 x LH145","LH82 x DK3IIH6", "LH82 x PHK76",
                                                 "PHK56 x PHK76","LH82 x PHP02"))



Fig3F=ggplot(data2, aes(x=Hybrid, y = Rank, color = X)) +
  geom_jitter(size=1)+
  scale_color_manual(name = "Enviornmental Condition",
                    labels = c("Condition 1", "Condition 2", "Condition 3", "Condition 4", "Condition 5", "Condition 6"),
                    values = c("mistyrose2", "slategray", "pink4", "palegreen3", "indianred3"))+
  xlab("Genotype")+
  ylab("Rank within Condition")+
  scale_y_continuous(limits=c(0,10), breaks=seq(0,10,2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.ticks.x=  element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.y=  element_blank())
Fig3F
ggdraw() +
  draw_plot(Fig3A, x = 0, y = 0.5, width = .33, height = 0.5) +
  draw_plot(Fig3B, x = 0.33, y = 0.5, width = .33, height = 0.5) +
  draw_plot(Fig3C, x = 0.66, y = 0.5, width = .33, height = 0.5) +
  draw_plot(Fig3D, x = 0, y = 0, width = .33, height = 0.5) +
  draw_plot(Fig3E, x = 0.33, y = 0, width = .33, height = 0.5) +
  draw_plot(Fig3F, x = 0.66, y = 0, width = .33, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E", "F"), 
                  size = 12,
                  x = c(0, 0.33,0.66,0, 0.33,0.66), 
                  y = c(1,1,1,0.5,0.5,0.5))

#A higher root system stiffness is associated with increased root lodging.####
##Assigning categories to hybrids ######
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/")
data_l = read.csv("2020_LodgingData.csv", header = TRUE, na.strings = "NA")
#Count total number of Y, N, and calculate percent lodged
df = data_l %>% 
  count(Genotype, Lodged.)
df <- spread(df, Lodged., n)
df$N[is.na(df$N)] = 0
df$Y[is.na(df$Y)] = 0
df$Total = df$N + df$Y
df$percent_lodged = round((((df$Y)/(df$Total)) * 100), digits = 2)
df = df %>% 
  group_by(Genotype) %>% 
  mutate(id = row_number())
df = df[,c(1,5)]
df = as.data.frame(df)
mean = mean(df$percent_lodged)
sd = sd(df$percent_lodged)
high = mean + sd
low = mean - sd
df$PL_cat = df$percent_lodged
df$PL_cat = as.numeric(df$PL_cat)
for (i in 1:length(df$PL_cat)){
  if (df$percent_lodged[i] <= low) {
    df$PL_cat[i] = "low"
  } else if (df$percent_lodged[i] < high) {
    df$PL_cat[i] = "mid"
  } else if (df$percent_lodged[i] >= high) {
    df$PL_cat[i] = "high"
  }
}
#write.csv(df, file = "LodgingCategories.csv", quote = F, row.names = T)

##2020####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/")
data0 = read.csv("Hostetler_Reneau_et_al_2024.csv", header = TRUE, na.strings = "NA")
data = subset(data0, Experiment == "Lodging Hybrids")
data = subset(data, Test_Type == "A")
data$DAP = as.numeric(data$DAP)
data = subset(data, Year == "2020")
data$DAP_cat = data$DAP
data$DAP_cat = as.numeric(data$DAP_cat)
for (i in 1:length(data$DAP_cat)){
  if (data$DAP[i] >= 100) {
    data$DAP_cat[i] = "100"
  } else if (data$DAP[i] >= 80) {
    data$DAP_cat[i] = "91"
  } else if (data$DAP[i] >= 50) {
    data$DAP_cat[i] = "55"
  }
}
data$DAP_cat = factor(data$DAP_cat,levels = c("55","91","100"))
data_pl = read.csv("LodgingCategories.csv", header = TRUE, na.strings = "NA")
data = merge(data, data_pl, by = "Genotype")
unique(data$DAP)
head(data)

df = subset(data, DAP_cat == "55")
Fig4A=ggplot(data=df,aes(x=DAP_cat, y=line_raw_slope_N.m, fill=PL_cat))+
  geom_boxplot()+
  xlab("Condition") + 
  xlab("Time of Testing (DAP)") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "Lodging Categories")+
  scale_fill_manual(name = "Lodging Categories", labels = c("Low Lodging", "Moderate Lodging", "Severe Lodging"), values=c("mistyrose3","slategrey"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,1000))+
  scale_x_discrete(labels=c('Condition 1'))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig4A
##Stats###
attach(df)
lm_x <- lm(line_raw_slope_N.m ~ PL_cat)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("PL_cat"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(df$line_raw_slope_N.m)
hist(df$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ PL_cat)
#transformation data to meet assumptions
par(mfrow=c(2,2))
df$tukey <- transformTukey(
  df$line_raw_slope_N.m,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)
hist(df$tukey)
lm_x2 <- lm(df$tukey ~ PL_cat)
anova(lm_x2)
lm_x2_aov2=aov(lm_x2) 
HSD.test(lm_x2_aov2, trt = c("PL_cat"), console = TRUE)
df$tukey = NULL
detach(df)

df = subset(data, DAP_cat == "91")
Fig4B=ggplot(data=df,aes(x=DAP_cat, y=line_raw_slope_N.m, fill=PL_cat))+
  geom_boxplot()+
  xlab("Condition") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "Lodging Categories")+
  scale_fill_manual(name = "Lodging Categories", labels = c("Low Lodging", "Moderate Lodging", "Severe Lodging"), values=c("mistyrose3","slategrey"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,1000))+
  scale_x_discrete(labels=c('Condition 2'))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig4B
##Stats###
attach(df)
lm_x <- lm(line_raw_slope_N.m ~ PL_cat)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("PL_cat"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(df$line_raw_slope_N.m)
hist(df$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ PL_cat)
detach(df)

##2021####
data0 = read.csv("Hostetler_Reneau_et_al_2024.csv", header = TRUE, na.strings = "NA")
data = subset(data0, Experiment == "Lodging Hybrids")
data = subset(data, Test_Type == "A")
data$DAP = as.numeric(data$DAP)
data = subset(data, Year == "2021")
data$Year = as.character(data$Year)
data$DAP = factor(data$DAP,levels = c("83","117"))
#Lodging Data
data_pl = read.csv("LodgingCategories.csv", header = TRUE, na.strings = "NA")
data = merge(data, data_pl, by = "Genotype")
data$PL_cat = as.factor(data$PL_cat)
data$PL_cat = factor(data$PL_cat,levels = c("low","mid","high"))
unique(data$DAP)
df = subset(data, DAP == "83")
Fig4C=ggplot(data=df,aes(x=DAP, y=line_raw_slope_N.m, fill=PL_cat))+
  geom_boxplot()+
  xlab("Condition") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "Lodging Categories")+
  scale_fill_manual(name = "Lodging Categories", labels = c("Low Lodging", "Moderate Lodging", "Severe Lodging"), values=c("mistyrose3","slategrey", "pink4"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,1000))+
  scale_x_discrete(labels=c('Category 3'))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))

Fig4C
attach(df)
lm_x <- lm(line_raw_slope_N.m ~ PL_cat)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("PL_cat"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(df$line_raw_slope_N.m)
hist(df$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ PL_cat*DAP)
detach(df)

unique(data$DAP)
df = subset(data, DAP == "117")
Fig4D=ggplot(data=df,aes(x=DAP, y=line_raw_slope_N.m, fill=PL_cat))+
  geom_boxplot()+
  xlab("Time of Testing (DAP)") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "Lodging Categories")+
  scale_fill_manual(name = "Lodging Categories", labels = c("Low Lodging", "Moderate Lodging", "Severe Lodging"), values=c("mistyrose3","slategrey", "pink4"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,1000))+
  scale_x_discrete(labels=c('Category 4'))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig4D
attach(df)
lm_x <- lm(line_raw_slope_N.m ~ PL_cat)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("PL_cat"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(df$line_raw_slope_N.m)
hist(df$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ PL_cat*DAP)
detach(df)

##2022####
data0 = read.csv("Hostetler_Reneau_et_al_2024.csv", header = TRUE, na.strings = "NA")
data = subset(data0, Experiment == "Lodging Hybrids")
data = subset(data, Test_Type == "A")
data$DAP = as.numeric(data$DAP)
data = subset(data, Year == "2022")
#Lodging Data
data_pl = read.csv("LodgingCategories.csv", header = TRUE, na.strings = "NA")
data = merge(data, data_pl, by = "Genotype")
data$PL_cat = as.factor(data$PL_cat)
data$PL_cat = factor(data$PL_cat,levels = c("low","mid","high"))
Fig4E=ggplot(data=data,aes(x=PL_cat, y=line_raw_slope_N.m, fill=PL_cat))+
  geom_boxplot()+
  xlab("Condition") + 
  ylab("Root System Stiffness (N/m)")+
  scale_fill_manual(values=c("mistyrose3","slategrey","pink4"),
                    labels=c("Low", "Moderate", "Severe"),
                    name = "Lodging Response in 2020")+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,1000))+
  scale_x_discrete(labels=c("", "Condition 5", ""))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig4E
##Stats###
attach(data)
lm_x <- lm(line_raw_slope_N.m ~ PL_cat)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("PL_cat"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ PL_cat)
detach(data)

ggdraw() +
  draw_plot(Fig4A, x = 0, y = 0, width = 0.20, height = 1) +
  draw_plot(Fig4B, x = 0.20, y = 0, width = 0.20, height = 1) +
  draw_plot(Fig4C, x = 0.40, y = 0, width = 0.20, height = 1) +
  draw_plot(Fig4D, x = 0.60, y = 0, width = 0.20, height = 1) +
  draw_plot(Fig4E, x = 0.80, y = 0, width = 0.20, height = 1) +
  draw_plot_label(label = c("A", "B", "C","D","E"), 
                  size = 15,
                  x = c(0, 0.195, 0.395, 0.595, 0.795), 
                  y = c(1,1,1,1,1))
#As planting density increases, the root system stiffness decreases.####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/")
geno = read.csv("Sparks Field 2022 - 2022_MAIZE_Seed_Origin.csv", header = TRUE, na.strings = "NA") #2022 Genotype Data
data0 = read.csv("/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/Hostetler_Reneau_et_al_2024.csv", header = TRUE, na.strings = "NA") #SMURF Database
names(geno)[1] = "Plot"
names(geno)[3] = "DensityInfo"
names(geno)[4] = "Density"
names(geno)[5] = "Rep"
data = subset(data0, Experiment == "Density Study")
data = merge(data, geno, by = "Plot")
data1 = read.csv("2022_DensityData.csv", header = TRUE, na.strings = "NA") #2022 Genotype Data
data1$ID = paste(data1$Plot, data1$PlantNumber, sep="_")
data$ID = paste(data$Plot, data$Replicate, sep="_")
data1 = data1[,c(14,4,5,6,7,8,9)]
data2 = merge(data, data1, by = "ID")
data2$Density = factor(data2$Density,levels = c("6","12","18","24"))
data2 = subset(data2, Plant.Ht > 120)
data2$StalkDiamAvg = (data2$Stalk.Diam.Major + data2$Stalk.DiamMinimum)/2

#Stats - Does Genotype or Density impact RTS
attach(data2)
lm_x <- lm(line_raw_slope_N.m ~ Genotype.x*Density)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Density"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Density","Genotype"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data$line_raw_slope_N.m)
hist(data$line_raw_slope_N.m)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(line_raw_slope_N.m ~ Genotype.x*Density)
detach(data2)

#Stats - Does Genotype or Density impact Plant Height
attach(data2)
lm_x <- lm(Plant.Ht ~ Genotype.x*Density)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Density","Genotype"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Density"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data2$Plant.Ht)
hist(data2$Plant.Ht)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(Plant.Ht ~ Genotype.x*Density)
detach(data2)

#Stats - Does Genotype or Density impact stalk diameter
attach(data2)
lm_x <- lm(StalkDiamAvg ~ Genotype.x*Density)
anova(lm_x)
lm_x_aov=aov(lm_x) 
HSD.test(lm_x_aov, trt = c("Density","Genotype.x"), console = TRUE)
HSD.test(lm_x_aov, trt = c("Density"), console = TRUE)
par(mfrow=c(2,2))
plot(lm_x)
par(mfrow=c(2,1))
plot(data2$StalkDiamAvg)
hist(data2$StalkDiamAvg)
resid = residuals(object = lm_x_aov)
shapiro.test(x=resid)
leveneTest(StalkDiamAvg ~ Genotype.x*Density)
detach(data2)

Fig5A=ggplot(data=data2,aes(x=Density, y=Plant.Ht, fill=Genotype.x))+
  geom_boxplot(size = 0.25) + 
  xlab("Density (plants/row)") + 
  ylab("Stalk Height (cm)")+
  labs(fill = "Density (plants/plot)")+
  scale_fill_manual(values=c("pink4","slategray"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig5A
Fig5B=ggplot(data=data2,aes(x=Density, y=StalkDiamAvg, fill=Genotype.x))+
  geom_boxplot(size = 0.25) + 
  xlab("Density (plants/row)") + 
  ylab("Average Stalk Diameter (mm)")+
  labs(fill = "Density (plants/plot)")+
  scale_fill_manual(values=c("pink4","slategray"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x =  element_blank(),
        axis.title.y =  element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig5B
Fig5C=ggplot(data=data2,aes(x=Density, y=line_raw_slope_N.m, fill=Genotype.x))+
  geom_boxplot(size = 0.25) + 
  xlab("Density (plants/plot)") + 
  ylab("Root System Stiffness (N/m)")+
  labs(fill = "Hybrid")+
  scale_fill_manual(values=c("pink4","slategray"))+
  scale_y_continuous(limits=c(0,12000), breaks=seq(0,12000,1000))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=10),
        axis.title.x =  element_blank(),
        axis.title.y =  element_text(size=10),
        axis.ticks.x=element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
Fig5C
ggdraw() +
  draw_plot(Fig5C, x = 0.5, y = 0, width = 0.5, height = 1) +
  draw_plot(Fig5B, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(Fig5A, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("C", "B", "A"), 
                  size = 15,
                  x = c(0.5,0,0), 
                  y = c(1,0.5,1))

#Supplemental Figure: Root lodging susceptibility is variable among maize hybrids and random in the field.####
cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Reneau_et_al_2023/R Input Files/")
data = read.csv("2020_LodgingData.csv", header = TRUE, na.strings = "NA")
#Count total number of Y, N, and calculate percent lodged
df = data %>% 
  count(Genotype, Plot, Lodged.)
df <- spread(df, Lodged., n)
df$N[is.na(df$N)] = 0
df$Y[is.na(df$Y)] = 0
df$Total = df$N + df$Y
df$percent_lodged = round((((df$Y)/(df$Total)) * 100), digits = 2)
head(df)
df = df %>% 
  group_by(Genotype) %>% 
  mutate(id = row_number())
names(df)[7] = "Rep"
#Figures
df$Rep = as.character(df$Rep)
df$Genotype = factor(df$Genotype,levels = c("PHB47 x LH198","PHB47 x PHP02","LH82 x DK3IIH6","PHK56 x PHK76","PHK76 x PHP02",
                                            "LH145 x DK3IIH6","DK3IIH6 x LH198","LH198 x PHN46","LH82 x PHK76","LH82 x PHP02","LH82 x LH145"))
data1 = read.csv("2020_LodgingData_PlotLocation.csv", header = TRUE, na.strings = "NA")
df = merge(df, data1, by = "Plot")
FigS1A=ggplot(df, aes(Row, Column, fill= percent_lodged)) + 
  geom_tile()+
  geom_text(label=df$Genotype)+
  scale_fill_gradient(low="white", high="indianred4")+
  xlab("Row") + 
  ylab("Column")+
  labs(fill = "%Lodged")+
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, angle = 90, colour = "black"),
        strip.text.y = element_text(size = 12, colour = "black"))
FigS1A
FigS1B=ggplot(data=df,aes(x=Genotype, y=percent_lodged, fill=Rep))+
  geom_bar(position="dodge", stat="identity")+
  xlab("Genotype") + 
  ylab("%Lodged")+
  labs(fill = "Plot Replicate")+
  scale_fill_manual(values=c("mistyrose3","slategrey"))+
  theme(axis.text.x = element_text(size=12,  angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, angle = 90, colour = "black"),
        strip.text.y = element_text(size = 12, colour = "black"))
FigS1B
ggdraw() +
  draw_plot(FigS1A, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot(FigS1B, x = 0, y = 0.5, width = 1, height = 0.5) +
  draw_plot_label(label = c("B", "A"), 
                  size = 15,
                  x = c(0, 0), 
                  y = c(0.5, 1))

