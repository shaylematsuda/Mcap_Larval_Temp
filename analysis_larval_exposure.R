#Project: Larval thermal exposure
#Gyasi, Shayle, Ariana 
#2019-2020


####SET UP####
rm(list=ls(all=TRUE)) #clear lists

library("ggplot2") #plotting
library("car") #levenes tests
library("dplyr")
library("lsmeans") #post-hoc tests
library("effects") #plot effects of modeling
library("lme4") #linear mixed modeling
library("lmerTest") #calculate p-values in models
library("emmeans") #post-hoc tests
library("cowplot") #arrange plots
library("tidyverse")  #splitting, applying, and combining data

####LARVAL SURVIVORSHIP####

#load and manipulate data sets
larvae<-read.csv("surv_larvae.csv", header=T, na.strings="NA") #load larval survivorship dataset
larvae$ConicalVol<-as.numeric(larvae$ConicalVol) 

larvae$perML<-larvae$Larvae/larvae$Volume #calculate larval concentration as larvae per mL seawater
larvae$total<-larvae$perML*larvae$ConicalVol #calculate total number of larvae using conical volume and larval concentration

#calculate percent change in larval concentration relative to starting concentration at each subsequent timepoint
#(larvae end - larvae start) / larvae start

#first, summarize by conical to make comparisons  
larvae_sum<- plyr::ddply(larvae, c("Conical", "Day", "Treatment"), summarise, 
                           mean = mean(perML, na.rm=TRUE))
larvae_sum

#Calculate proportion survival
z <- larvae_sum %>%
  group_by(Conical) %>% #group by conical
  arrange(Day, .by_group = TRUE) %>% #arrange by date within conical
  mutate(pct_change = ((mean-first(mean))/first(mean)), prop_change = (mean/first(mean))) #calculate percent change and proportion change relative to the first timepoint

#generate a summary table of larval density over time (days 0, 1, 3, 4) in ambient, cool, and high treatments
larvae_surv_table <- plyr::ddply(z, c("Treatment", "Day"), summarise, 
                 N    = length(prop_change[!is.na(prop_change)]), 
                 mean = mean(prop_change, na.rm=TRUE), 
                 sd   = sd(prop_change, na.rm=TRUE), 
                 se   = sd / sqrt(N), 
                 max = max(prop_change, na.rm=TRUE), 
                 lower = mean-se, 
                 upper = mean+se 
)
larvae_surv_table 
#write.csv(larvae_surv_table,"larvae_surv_table.csv")

#graph larval density over time
LarvalSurvPlot<-ggplot(data=larvae_surv_table, aes(x=Day, y=mean, colour=Treatment)) + 
  scale_colour_manual(name="Larval Treatment",
                    values=c("gray", "blue", "red"),
                    labels=c("Ambient", "Cool", "High"))+
  geom_line(position=position_dodge(0.1), size=1) + 
  geom_point(size=1, position=position_dodge(0.1)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=0.0, size=1, position=position_dodge(0.1), linetype=1)+ 
  theme_classic()+ 
  ylim(0,1.3)+
  theme(text = element_text(size = 18, color="black"))+ 
  theme(axis.text = element_text(size = 18, color="black"))+ 
  theme(legend.title = element_text(size = 18, color="black", face="bold"))+
  theme(legend.text = element_text(size = 18, color="black"))+
  theme(axis.title = element_text(size = 18, color="black"))+ 
  theme(legend.position = "none")+ 
  theme(plot.margin = margin(1, 0.1, 0, 0.1, "cm")) +
  ylab(expression(bold(paste("Proportion Survival")))) + 
  xlab(expression(bold("Days")));LarvalSurvPlot 

#analyze larval density over time between treatments with a linear mixed model
model1<-aov(prop_change~Treatment + Day  + Treatment:Day, data=z) 
summary(model1) 
qqPlot(residuals(model1)) #passes
leveneTest(prop_change~Treatment, data=z) #passes

#Overall, there are significant effects of treatment and day and interaction of treatment by day. 
#1. There is a significant reduction in survivorship over time (sign. effect of day). 
#2. There is a signiicant interaction between day and treatment, specifically that there is a significant reduction in survival in cool temperature over time as compared to ambient over time and there is a reduction in high compared to ambient. Basically there are different rates of survival depending on treatment. 

####LARVAL SETTLEMENT####

#load dataset
settle<-read.csv("settlement.csv", header=T, na.strings="NA")

#calculate total settlement by chamber by adding the number of settlers across all three plugs in each chamber
total_settle <- plyr::ddply(settle, c("Day", "Chamber"), summarise, 
                 total    = sum(Total, na.rm=TRUE), 
                 aggregate = sum(Aggregate, na.rm=TRUE), 
                 individual   = sum(Individual, na.rm=TRUE) 
)

#merge in identifying information of tank, treatment, and number of starting larvae
total_settle$Tank<-settle$Tank[match(total_settle$Chamber, settle$Chamber)]
total_settle$Treatment<-settle$Treatment[match(total_settle$Chamber, settle$Chamber)]
total_settle$Starting.Larvae<-settle$Starting.Larvae[match(total_settle$Chamber, settle$Chamber)]

#calculate proportion setted in each chamber
total_settle$propSettled<-total_settle$total/total_settle$Starting.Larvae
total_settle$propAggregate<-total_settle$aggregate/total_settle$total

#generate a summary table of larval settlement over time originating from each larval treatment - settlement is shown as # larvae settled per 100 larvae
larval_settle_table <- plyr::ddply(total_settle, c("Treatment", "Day"), summarise, 
                 N    = length(total[!is.na(total)]), 
                 mean = mean(total, na.rm=TRUE), 
                 sd   = sd(total, na.rm=TRUE), 
                 se   = sd / sqrt(N), 
                 max = max(total, na.rm=TRUE), 
                 lower = mean-se, 
                 upper = mean+se 
)
larval_settle_table 
#write.csv(larval_settle_table, "larval_settle_table.csv")

#graph larval settlement over time (days 1, 2, 3, 5, 6, 7) originating from each larval treatment (ambient, cool, high)
LarvalSettlePlot<-ggplot(data=larval_settle_table, aes(x=Day, y=mean, colour=Treatment)) + 
  scale_colour_manual(name="Larval Treatment",
                       values=c("gray", "blue", "red"), 
                       labels=c("Ambient", "Cool", "High"))+ 
  geom_line(position=position_dodge(0.1), size=1) + 
  geom_point(size=1, position=position_dodge(0.1)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=0.0, size=1, position=position_dodge(0.1), linetype=1)+ 
  theme_classic()+ 
  ylim(0,100)+
  theme(text = element_text(size = 18, color="black"))+ 
  theme(axis.text = element_text(size = 18, color="black"))+ 
  theme(legend.text = element_text(size = 18, color="black"))+ 
  theme(legend.position = "none")+ 
  theme(legend.title = element_text(size = 18, color="black", face="bold"))+ 
  theme(axis.title = element_text(size = 18, color="black"))+ 
  theme(plot.margin = margin(1, 0.1, 0, 0.1, "cm")) +
  ylab(expression(bold(paste("Larvae Settled (%)")))) + 
  xlab(expression(bold("Days"))); LarvalSettlePlot 


#analyze "total" settlement over time between treatments with a linear mixed model. As all chambers started with 100 larvae, we are not using the vector of success and failure. Because this is count data we will use a poisson distribution. Nest chamber within tank as this is repeated measures. 
hist(total_settle$total)
model2<-glmer(total ~ Treatment + Day  + Treatment:Day + (1|Tank/Chamber), data=total_settle, family=poisson) 
summary(model2) 
Anova(model2, type=2)
qqPlot(residuals(model2)) 

#Overall, there is a significant effect of day and treatment on settlement. 
#1. There is lower settlement in cool temp compared to ambient and higher settlement in high as compared to ambient (sign. effect of treatment) - basically settlement increases with temp
#2. There is significantly higher settlement over time (sign. effect of day)
#3. There is an increase in settlement over time in cool temperature, but less settlement over time in high temperature - there are different rates of settlement between treatments (sign. effect of day * treatment interaction)



####RECRUIT SURVIVORSHIP####  

#load and manipulate data
recruits<-read.csv("surv_recruits.csv", header=T, na.strings="NA") #load recruit survivorship dataset
recruits$Tank<-as.factor(recruits$Tank) 

#calculate proportion survival 
recruits$proportion<-(recruits$Success/(recruits$Success+recruits$Failure))*100

#add in larval temperature treatment information - plug 98 did not have larval treatment information and was removed
recruits$Larval.Treatment<-settle$Treatment[match(recruits$Plug.ID, settle$Plug)]

#generate a summary table of recruit survival over time (days 0, 1, 3, 4) in ambient and high conditions originating from ambient, cool, and high larval treatments treatments
recruit_surv_table <- plyr::ddply(recruits, c("Days", "Juv.Treatment", "Larval.Treatment"), summarise, 
                           N    = length(proportion[!is.na(proportion)]), 
                           mean = mean(proportion, na.rm=TRUE), 
                           sd   = sd(proportion, na.rm=TRUE), 
                           se   = sd / sqrt(N), 
                           max = max(proportion, na.rm=TRUE), 
                           lower = mean-se, 
                           upper = mean+se 
);recruit_surv_table 
#write.csv(recruit_surv_table,"recruit_surv_table.csv")

#graph proportion survival over time in recruits 
RecruitSurvPlot<-ggplot(data=recruit_surv_table, aes(x=Days, y=mean, colour=Larval.Treatment)) + 
  facet_wrap(~Juv.Treatment)+
  scale_colour_manual(name="Larval Treatment",
                      values=c("gray", "blue", "red"),
                      labels=c("Ambient", "Cool", "High"))+
  geom_line(position=position_dodge(0.1), size=1) + 
  geom_point(size=1, position=position_dodge(0.1)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=0.0, size=1, position=position_dodge(0.1), linetype=1)+ 
  theme_classic()+ 
  ylim(0,100)+
  theme(text = element_text(size = 18, color="black"))+ 
  theme(axis.text = element_text(size = 18, color="black"))+ 
  theme(legend.title = element_text(size = 18, color="black", face="bold"))+
  theme(legend.text = element_text(size = 18, color="black"))+
  theme(axis.title = element_text(size = 18, color="black"))+ 
  theme(legend.position = "bottom")+ 
  ylab(expression(bold(paste("Recruit Survivorship (%)")))) + 
  theme(plot.margin = margin(1, 0.1, 0, 0.1, "cm")) +
  xlab(expression(bold("Days")));RecruitSurvPlot 

#analyze recruit survival over time between treatments with a binomial mixed model with a success and failure vector
attach(recruits)

y<-cbind(Success, Failure)

model3<-glmer(y~Juv.Treatment * Larval.Treatment * Days + (1|Tank/Plug.ID), family=binomial, data=recruits) 
summary(model3) 
Anova(model3, type=2)

library(blmeco);dispersion_glmer(model3) #no evidence of overdispersion as value is <1.4

detach(recruits)

#Overall, there are significant effects of juvenile treatment and day, but no effect of larval treatment. 

#1. There is lower survivorship in high temperature for recruits (sign. effect of juv.treatment)
#2. There is a significant reduction in survivorship over time (sign. effect of day). 
#3. There is a signiicant interaction between day and treatment, specifically that there is a significant reduction in survival in high temperature over time as compared to ambient over time. There are different rates of survival depending on treatment. 

####GENERATE FIGURES#### 

#generate figure with larval survival, settlement, and recruit survival in one panel

figure2a<-plot_grid(LarvalSurvPlot, LarvalSettlePlot, labels = c("A", "B"), ncol=2, nrow=1, rel_heights= c(1,1), rel_widths = c(1,1), label_size = 20, label_y=1, align="h");figure2a

figure2b<-plot_grid(figure2a, RecruitSurvPlot, labels = c("", "C"), ncol=1, nrow=2, rel_heights= c(1,1), rel_widths = c(1,1), label_size = 20, label_y=1);figure2b

ggsave(filename="figure2.pdf", plot=figure2b, dpi=500, width=12, height=10, units="in")
