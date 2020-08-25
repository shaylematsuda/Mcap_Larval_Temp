#Project: Larval thermal exposure
#Gyasi, Shayle, Ariana 
#2019-2020

#last updated 13 May 2020


rm(list=ls()) # removes all prior objects

#Read in required libraries
##### Include Versions of libraries
library("car") #levenes test
library("ggplot")
library("ggplot2") #plotting
library("plotrix") #plotting
library("reshape2") #data shaping
require("gridExtra") #Arrange Plots for output
require("plyr") #for ddply
require("dplyr")
require("utils")

#Required Data files
#Temp_Logger_Calibration.csv
#Tank_Temp.csv

#############################################################
#------------------------------------------------
#TANK EXPERIMENTAL TEMPERATURE ANALYSIS
#Load calibration data, once you do this, you can pull intercept and slope to calibrate individual loggers
calib.data.T <- read.csv("Temp_Logger_Calibration.csv", header=TRUE) #load data with a header, separated by commas, with NA as NA
calib.T <- as.data.frame(t(apply(calib.data.T[2:21], 2, function(y)(coef(lm(y ~ calib.data.T$SN_10779069)))))) #apply linear model to obtain calibration equations
colnames(calib.T) <- c("Intercept",  "Slope") #rename columns

#SUMP only
#Load data
AmbSump<-read.csv("Ambient_Sump_10779069.csv",header=TRUE, sep=",", na.strings="NA") #this logger is the standard, no change
#Intercept: -2.757311e-14     slope= 1.0000000, line equation y=1  x +-2.757311e-14
AmbSump$calibratedT<-(AmbSump$Temp*1)+-2.757311e-14
names(AmbSump)[3] <- "AmbientSump" #rename calibrated col to tank name
AmbSump<- AmbSump[ -c(2) ] #drop uncalibrated col

ColdSump<-read.csv("cold_sump_10487935.csv",header=TRUE, sep=",", na.strings="NA")
#Intercept: -5.645523e-02   Slope: 1.0004740, line equation y=1.0004740x + -5.645523e-02
ColdSump$calibratedT<-(ColdSump$Temp*1.0004740)+-5.645523e-02
names(ColdSump)[3] <- "ColdSump" #rename calibrated col to tank name
ColdSump<- ColdSump[ -c(2) ] #drop uncalibrated col

#combine Amb and cold sump for plotting
SUMPdf<-merge(AmbSump,ColdSump, by="Date.Time") #merge and keep only time points over lap
SUMPdf<-melt(SUMPdf,id=c("Date.Time"))
SUMPdf$Date.Time<-as.character(SUMPdf$Date.Time)
SUMPdf$Date <- format(as.POSIXct(strptime(SUMPdf$Date.Time,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d") #to get daily means set up a date only col

SUMP_mean <- ddply(SUMPdf, c("variable", "Date"), summarise, 
                          N    = length(value[!is.na(value)]), #calculate the length of the data frame, excluding NA’s *this part is important*
                          mean = mean(value, na.rm=TRUE), #calculate mean of response variable, removing NA's
                          sd   = sd(value, na.rm=TRUE), #calculate standard deviation
                          se   = sd / sqrt(N), #calculate standard error
                          max = max(value, na.rm=TRUE) #calculate max, could also calculate min () if desired
)
SUMP_mean

SUMP_plot<-ggplot(data=SUMP_mean, aes(x=Date, y=mean, group = variable, color=variable)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=0.1)+
  geom_point(aes(shape=variable), size=3)+
  geom_line(aes(color=variable, linetype=variable))+
  scale_color_manual(values=c("#0072B2","#000000"))+
  #scale_linetype_manual(values=c("solid", "dotted"))+
 # scale_color_manual(values=c("black", "black","black"))+
  ylab("Temp") +
  xlab("Time") + #Label the X Axis
  #ylim(0, 100) + #set Y limits
  theme_bw() + #Set the background color
  theme(axis.line = element_line(color = 'black'), #Set the axes color
        axis.title=element_text(size=14,face="bold"), #Set axis format
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background =element_blank(), #Set the plot background
        legend.key = element_blank()) + #Set plot legend key
  ggtitle("SUMP TEMP")+
  theme(plot.title = element_text(size=20,face = "italic",hjust = 0.5));SUMP_plot




#TANKS ONLY
Tank1<-read.csv("Tank1_5Sept_10779060.csv",header=TRUE, sep=",", na.strings="NA")
#Intercept: -5.899138e-02     Slope: 1.0015297 , line equation y=1.0015297 x + -5.899138e-02  
Tank1$calibratedT<-(Tank1$Temp*1.0015297)+-5.899138e-02
names(Tank1)[3] <- "Tank1" #rename calibrated col to tank name
Tank1<- Tank1[ -c(2) ] #drop uncalibrated col

Tank2<-read.csv("Tank2_5Sept_10779068.csv",header=TRUE, sep=",", na.strings="NA")
#Intercept: 6.285066e-02  Slope: 0.9975927   , line equation y=0.9975927   x + 6.285066e-02  
Tank2$calibratedT<-(Tank2$Temp*1.0015297)+6.285066e-02 
names(Tank2)[3] <- "Tank2" #rename calibrated col to tank name
Tank2<- Tank2[ -c(2) ] #drop uncalibrated col

Tank3<-read.csv("Tank3_5Sept_10487935.csv",header=TRUE, sep=",", na.strings="NA")
#Intercept: -5.645523e-02  Slope: 1.0004740   , line equation y=1.0004740   x +-5.645523e-02  
Tank3$calibratedT<-(Tank3$Temp*1.0004740)+-5.645523e-02
names(Tank3)[3] <- "Tank3" #rename calibrated col to tank name
Tank3<- Tank3[ -c(2) ] #drop uncalibrated col

Tank4<-read.csv("Tank4_5Sept_10779069.csv",header=TRUE, sep=",", na.strings="NA")
#Intercept: -2.757311e-14     slope= 1.0000000, line equation y=1  x +-2.757311e-14
Tank4$calibratedT<-(Tank4$Temp*1)+-2.757311e-14
names(Tank4)[3] <- "Tank4" #rename calibrated col to tank name
Tank4<- Tank4[ -c(2) ] #drop uncalibrated col

#combine tanks for plotting
Tankdf<-merge(Tank1,Tank2,  by="DateTime") #merge and keep only time points over lap #### not sure why won't run all together
Tankdf<-merge(Tankdf,Tank3,  by="DateTime") #merge and keep only time points over lap
Tankdf<-merge(Tankdf,Tank4,  by="DateTime") #merge and keep only time points over lap

Tankdf<-melt(Tankdf,id=c("DateTime"))
Tankdf$Date.Time<-as.character(Tankdf$DateTime)
Tankdf$Date <- format(as.POSIXct(strptime(Tankdf$DateTime,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d") #to get daily means set up a date only col
Tankdf$HOUR <- format(as.POSIXct(strptime(Tankdf$DateTime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H") #to get daily means set up a date only col
Tankdf$DateHour <- format(as.POSIXct(strptime(Tankdf$DateTime,"%m/%d/%Y %H:%M",tz="")) ,format ="%m/%d %H") #to get daily means set up a date only col


TANK_mean <- ddply(Tankdf, c("variable", "Date"), summarise, 
                   N    = length(value[!is.na(value)]), #calculate the length of the data frame, excluding NA’s *this part is important*
                   mean = mean(value, na.rm=TRUE), #calculate mean of response variable, removing NA's
                   sd   = sd(value, na.rm=TRUE), #calculate standard deviation
                   se   = sd / sqrt(N), #calculate standard error
                   max = max(value, na.rm=TRUE), #calculate max, could also calculate min () if desired
                   min = min(value, na.rm=TRUE)
)
TANK_mean

TANK_meanHOUR <- ddply(Tankdf, c("variable", "DateHour"), summarise, 
                   N    = length(value[!is.na(value)]), #calculate the length of the data frame, excluding NA’s *this part is important*
                   mean = mean(value, na.rm=TRUE), #calculate mean of response variable, removing NA's
                   sd   = sd(value, na.rm=TRUE), #calculate standard deviation
                   se   = sd / sqrt(N), #calculate standard error
                   max = max(value, na.rm=TRUE), #calculate max, could also calculate min () if desired
                   min = min(value, na.rm=TRUE)
)
TANK_meanHOUR

TANK_meanHOUR$DateHour<-format(as.POSIXct(strptime(TANK_meanHOUR$DateHour,"%m/%d %H",tz="")) ,format ="%m/%d %H")
TANK_meanHOUR$DateHour<-as.POSIXct(TANK_meanHOUR$DateHour,format="%m/%d %H",tz="", format="%m/%d %H")

library(RColorBrewer)

TANK_plot<-ggplot(data=TANK_meanHOUR, aes(x=DateHour, y=mean, group = variable, color=variable)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=0.1)+
  geom_point(size=1)+
  scale_color_manual(values=c("#E69F00", "#56B4E9","#0072B2", "#D55E00"))+
  geom_line(aes(color=variable, linetype=variable))+
  #geom_ribbon(aes(ymin=min, ymax=max), linetype=2, alpha=0.4, color=NA) +
  ylab("Temp") +
  xlab("Time") + #Label the X Axis
  #ylim(0, 100) + #set Y limits
  theme_bw() + #Set the background color
  theme(axis.line = element_line(color = 'black'), #Set the axes color
        axis.title=element_text(size=14,face="bold"), #Set axis format
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background =element_blank(), #Set the plot background
        legend.key = element_blank()) + #Set plot legend key
  ggtitle("TANK TEMP")+
  theme(plot.title = element_text(size=20,face = "italic",hjust = 0.5));TANK_plot




  






