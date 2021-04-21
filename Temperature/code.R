setwd("~/Projects/baby_corals/tempdata")
library(readxl);library(tidyverse);library(lubridate);library(scales);library(plotrix)
hot<-read_excel("conicals.xlsx",sheet="Hot");str(data)
ambient<-read_excel("conicals.xlsx",sheet="Ambient");str(ambient)
cold<-read_excel("conicals.xlsx",sheet="Cold");str(cold)

data<-full_join(full_join(hot,ambient,by="Date"),cold,by="Date")%>%
     rename(Hot=Temp.x,Ambient=Temp.y)%>%
     gather(Treatment,Temp,-Date)%>%
     filter(Date>as.POSIXct("2019-07-06")&Date<as.POSIXct("2019-07-9"))
data$Treatment <- factor(data$Treatment,levels = c("Cold", "Ambient", "Hot"))

quartz(h=2,w=5)
ggplot(data)+geom_point(aes(Date,Temp,color=Treatment),alpha=0.3,size=1)+
     theme_classic()+
     scale_color_manual(values=c("Blue","Gray","Red"))+
     geom_smooth(aes(Date,Temp,color=Treatment),se=FALSE,span=0.15,size=0.5)+
     scale_x_datetime(date_breaks="1 day",minor_breaks=waiver(),labels=date_format("%m-%d"))+
     scale_y_continuous(breaks=seq(23,30,1))+
     ylab("Temperature (°C)")+
     theme(legend.key.size = unit(0.5,"line"))

data%>%group_by(Treatment)%>%summarise(mean=mean(Temp),se=std.error(Temp))
