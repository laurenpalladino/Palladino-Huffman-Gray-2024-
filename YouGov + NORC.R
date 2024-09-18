#Lauren Palladino

library(readr)
library(dplyr)
library(haven)
library(stargazer)
library(ggplot2)
library(effects)
library(sjPlot)
library(gridExtra)
library(gridGraphics)
Nicole_theme<- ggplot2:: theme(plot.title=element_text(face="bold", size=22), 
                axis.title.y=element_text(size=20), axis.text=element_text(size=14),
                axis.title.x=element_text(size=20), legend.title = element_text(size=20),
                legend.text = element_text(size=14))

###NORC DATA ANALYSIS###

tess<- read_dta("Documents/[RESEARCH] Diversity Legitimacy Paper/9089.114.TESS HUFFMAN.dta")
attach(tess)

tess<-tess%>%
  dplyr:: rename(Legit_Trust= HUFFMAN1, Legit_Fair=HUFFMAN2, Strong_Exec= HUFFMAN3A, 
                 Pres_No_Others= HUFFMAN3B, Checks_Balances= HUFFMAN3C, treatment= DOV_EXP_HUFFMAN,
                 gender=GENDER, race=RACETHNICITY, educ=EDUC5)

#recoding
tess$treatment<-NA
tess$treatment[DOV_EXP_HUFFMAN==1]<-"Control"
tess$treatment[DOV_EXP_HUFFMAN==2]<-"Diverse"

tess$pid<-NA
tess$pid[PartyID5<=2 & PartyID5>0]<-1
tess$pid[PartyID5==3]<-2
tess$pid[tess$PartyID5>=4]<-3

tess$female<-NA
tess$female[GENDER==2]<-1
tess$female[GENDER==1]<-0
table(tess$female)

tess$race<-as.factor(NA)
levels(tess$race)<-c("White", "Asian", "Black", "Hispanic", "Other")
tess$race[RACETHNICITY==1]<-"White"
tess$race[RACETHNICITY==6]<-"Asian"
tess$race[RACETHNICITY==2]<-"Black"
tess$race[RACETHNICITY==4]<-"Hispanic"
tess$race[RACETHNICITY==3]<-"Other"
tess$race[RACETHNICITY==5]<-"Other"

tess$educ<-NA
tess$educ[EDUC5==1]<-1
tess$educ[EDUC5==2]<-2
tess$educ[EDUC5==3]<-3
tess$educ[EDUC5==4]<-4
tess$educ[EDUC5==5]<-5

tess<-tess%>%
  mutate_at( c("Legit_Fair", "Legit_Trust", "Strong_Exec", "Pres_No_Others", "Checks_Balances"),
             car::recode, "1 = 1; 2 = 3/4; 3 = 2/4; 4=1/4; 5=0; 77= NA; 98=NA; 99= NA")

#Legitimacy
Trust1<-lm(Legit_Trust~treatment*PartyID5+female+race+educ, data=tess)
Fair1<-lm(Legit_Fair~treatment*PartyID5+female+race+educ, data=tess)
trust<-tess$Legit_Trust/2
fair<-tess$Legit_Fair/2
tess$legit0<-(trust+fair)
aggregate(legit0 ~ treatment, data = tess, FUN = mean)

Legit<-lm(legit0~ treatment+PartyID5+ female+ race+ educ, data=tess)
Legit2<-lm(legit0~ treatment*PartyID5+ female+ race+ educ, data=tess)

#Democratic norms regression
SE2<-lm(Strong_Exec~treatment*PartyID5+female+race+educ, data=tess)
PNO2<-lm(Pres_No_Others~treatment*PartyID5+female+race+educ, data=tess)
CB2<-lm(Checks_Balances~treatment*PartyID5+female+race+educ, data=tess)

se<-tess$Strong_Exec/3
pno<-tess$Pres_No_Others/3
cb<-tess$Checks_Balances/3
tess$demnorms0<-(se+pno+cb)
aggregate(demnorms0 ~ treatment, data = tess, FUN = mean)

DemNorms<-lm(demnorms0~ treatment+PartyID5+ female+ race+ educ, data=tess)
DemNorms2<-lm(demnorms0~ treatment*PartyID5+ female+ race+ educ, data=tess)

stargazer(Legit, Legit2, DemNorms, DemNorms2, type="text", report=('vc*p'), omit.table.layout = "n")

###YOUGOV DATA ANALYSIS###

ygdata<-read_csv("Documents/[RESEARCH] Diversity Legitimacy Paper/YouGov_Clean_Data.csv")
attach(ygdata)

#recoding
ygdata$female1<-NA
ygdata$female1[ygdata$female=="Female"]<-1
ygdata$female1[ygdata$female=="Male"]<-0

ygdata$educ1<-NA
ygdata$educ1[ygdata$educ=="Less than a High School Diploma"]<-1
ygdata$educ1[ygdata$educ=="High School Graduate"]<-2
ygdata$educ1[ygdata$educ=="Some College"]<-3
ygdata$educ1[ygdata$educ=="2-year College Degree"]<-4
ygdata$educ1[ygdata$educ=="4-year College Degree"]<-5
ygdata$educ1[ygdata$educ=="Post-Graduate Degree"]<-6

ygdata$newsint1<-NA
ygdata$newsint1[ygdata$newsint=="None"]<-1
ygdata$newsint1[ygdata$newsint=="Low"]<-2
ygdata$newsint1[ygdata$newsint=="Medium"]<-3
ygdata$newsint1[ygdata$newsint=="High"]<-4

ygdata$age1<-NA
ygdata$age1[ygdata$age=="18-29"]<-1
ygdata$age1[ygdata$age=="30-44"]<-2
ygdata$age1[ygdata$age=="45-64"]<-3
ygdata$age1[ygdata$age=="65+"]<-4

ygdata$pid3<-NA
ygdata$pid3[ygdata$pid7==1]<-1
ygdata$pid3[ygdata$pid7==2]<-1
ygdata$pid3[ygdata$pid7==3]<-1
ygdata$pid3[ygdata$pid7==4]<-2
ygdata$pid3[ygdata$pid7==5]<-3
ygdata$pid3[ygdata$pid7==6]<-3
ygdata$pid3[ygdata$pid7==7]<-3

ygdata$treatment1<-as.factor(ygdata$treatment)

ygdata$Race<-as.factor(NA)
levels(ygdata$Race)<-c("White", "Asian", "Black", "Hispanic", "Other")
ygdata$Race[ygdata$race=="White"]<- "White"
ygdata$Race[ygdata$race=="Asian"]<-"Asian"
ygdata$Race[ygdata$race=="Black"]<-"Black"
ygdata$Race[ygdata$race=="Hispanic"]<-"Hispanic"
ygdata$Race[ygdata$race=="Multiracial"]<-"Other"
ygdata$Race[ygdata$race=="Native American"]<-"Other"
ygdata$Race[ygdata$race=="Middle Eastern"]<-"Other"
ygdata$Race[ygdata$race=="Other"]<-"Other"

#legitimacy
Trust1<-lm(Legit_Trust~ treatment1+ pid7+ female1+ Race+ educ1+ newsint1+ age1+ treatment1*pid7, data=ygdata)
summary(Trust1)
Fair1<-lm(Legit_Fair~ treatment1+ pid7+ female1+ Race+ educ1+ newsint1+ age1+ treatment1*pid7, data=ygdata)
summary(Fair1)
stargazer(Trust1, Fair1, type="text")

#aggregate democratic legitimacy
trust<-Legit_Trust/2
fair<-Legit_Fair/2
ygdata$legit<-(trust+fair)

aggregate(legit ~ treatment, data = ygdata, FUN = mean)

ygdata$pairwise1<-as.factor(NA)
levels(ygdata$pairwise1)<-c("Control", "Diverse")
ygdata$pairwise1[ygdata$treatment=="Control"]<-"Control"
ygdata$pairwise1[ygdata$treatment=="Diverse"]<-"Diverse"

ygdata$pairwise2<-as.factor(NA)
levels(ygdata$pairwise2)<-c("Control", "Diverse But Critiqued")
ygdata$pairwise2[ygdata$treatment=="Control"]<-"Control"
ygdata$pairwise2[ygdata$treatment=="Diverse But Critiqued"]<-"Diverse But Critiqued"

t.test(legit~pairwise1, data=ygdata)
t.test(legit~pairwise2, data=ygdata)

Legit1<-lm(legit~ treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)
Legit2<-lm(legit~ treatment1+ pid7+ female1+ Race+ educ1+ age1, data=ygdata)

#democratic norms
SE1<-lm(Strong_Exec~treatment1+ pid7+ female1+ Race+ educ1+ newsint1+ age1+ treatment1*pid7, data=ygdata)
summary(SE1)
PNO1<-lm(Pres_No_Others~treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)
summary(PNO1)
CB1<-lm(Checks_Balances~treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)
summary(CB1)
stargazer(SE1, PNO1, CB1, type="text")

#aggregate democratic norms
exec<-Strong_Exec/3
pres<-Pres_No_Others/3
checks<-Checks_Balances/3
ygdata$demnorms<-(exec+pres+checks)

t.test(demnorms~pairwise1, data=ygdata)
t.test(demnorms~pairwise2, data=ygdata)

DN2<-lm(demnorms~ treatment1+ pid7+ female1+ Race+ educ1+ age1, data=ygdata)
DN1<-lm(demnorms~ treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)

stargazer(Legit2, Legit1, DN2, DN1, type="text", report=('vc*p'), omit.table.layout = "n")

#graphs
ygdata<-ygdata%>%
  mutate(legit= (Legit_Trust+ Legit_Fair)/2, demnorms=(Strong_Exec+ Pres_No_Others+ Checks_Balances)/3)
ggplot(ygdata%>%filter(pid3!= "Not Sure"), aes(x = treatment, y = legit, fill= pid3)) + 
  stat_summary(fun.y=mean, geom="bar", position = "dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9))+
  scale_color_manual(values= c("steelblue3", "grey", "tomato2"), 
                     labels = c("Democrat", "Independent","Republican"))+
  scale_fill_manual(values= c("steelblue3", "grey", "tomato2"), 
                    labels = c("Democrat", "Independent","Republican"))+
  labs(x= "Treatment", y= "Perceived Legitimacy", fill= "Respondent Party")+ 
  Nicole_theme

ggplot(ygdata%>%filter(pid3!= "Not Sure"), aes(x = treatment1, y = demnorms, fill= pid3)) + 
  stat_summary(fun.y=mean, geom="bar", position= "dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9))+
  ggplot2:: scale_color_manual(values= c("steelblue3", "grey", "tomato2"), 
                               labels = c("Democrat", "Independent","Republican"))+
  ggplot2:: scale_fill_manual(values= c("steelblue3", "grey", "tomato2"), 
                              labels = c("Democrat", "Independent", "Republican"))+
  labs(x= "Treatment", y= "Support for Democratic Norms Constraining Executive Power", fill= "Respondent Party")+ 
  Nicole_theme


gender_graph<-ggplot(ygdata, aes(x = treatment, y = legit, fill= female)) + 
  stat_summary(fun.y=mean, geom="bar", position= "dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9))+
  scale_color_manual(values= c("hotpink2", "steelblue"), labels = c("Women", "Man"))+
  scale_fill_manual(values= c("hotpink2", "steelblue"), labels = c("Women", "Man"))+
  labs(x= "Treatment", y= "Perceived Legitimacy", fill= "Respondent \n Gender")+  expand_limits(y = c(0.2, 0.7))+ 
  Nicole_theme

race_graph<-ggplot(ygdata%>%filter(race== "White" | race== "Hispanic" | race== "Black"),
                   aes(x = treatment, y = legit, fill= race)) + 
  stat_summary(fun.y=mean, geom="bar", position= "dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9))+
  ggplot2:: scale_color_manual(values= c("grey23", "tan4", "peachpuff2"), 
                               labels = c("Black","Hispanic", "White"))+
  ggplot2:: scale_fill_manual(values= c("grey23", "tan4", "peachpuff2"), 
                              labels = c("Black","Hispanic", "White"))+
  labs(x= "Treatment", y= "Perceived Legitimacy", fill= "Respondent \n Race")+  expand_limits(y = c(0.2, 0.7))+ 
  Nicole_theme

gridExtra::grid.arrange(gender_graph, race_graph, ncol= 2)
