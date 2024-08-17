####Ben Pyenson
##updated May, 2024




#### Fig.1, Fig. 2; Associations from Est. Gams in unmanipulated cols ####


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/Assoc_eggs_propgam")
getwd()
#clear all
rm(list=ls())


#import  DATA set 
#all from unique source colonies
###therefore no random effects in these data
data<-read.csv("Pyenson_singlecols_2401.csv", header=TRUE)
str(data)


data$Colony<-as.factor(data$Colony)
#data$SourceCol<-as.factor(data$SourceCol)

#approximate egglaying rate using 29 days from Penick's data
data$eggrate<-data$Eggs/data$NumGams/29
data$eggrate


#number of nonrepros

data$nonrepros<- data$GroupSize - data$NumGams



#approximate larva/non-reproductive worker
data$larvacare<-data$Larva/data$nonrepros
data$larvacare


#approximate pupa care/ non-repro worker
data$pupacare <- data$Pupa/data$nonrepros
data$pupacare



#estimate propgam
data$propgam<-data$NumGams/data$GroupSize
data$propgam

#View(data)


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="propgam", groupvars=c("NumGams"), na.rm=TRUE)
sum_data
sum_data<-summarySE(data, measurevar="eggrate", groupvars=c("Colony"), na.rm=TRUE)
sum_data
#n = 49 cols total




#how many colonies have egg data
# using subset function for  final sampling
data_allbrood <- subset(data, allbrood==1)
str(data_allbrood)
#n=35 colonies with all brood available
sum_data<-summarySE(data_allbrood, measurevar="eggrate", groupvars=c("Colony"), na.rm=TRUE)
sum_data
#n = 35 cols total







library(ggplot2)




pprop_larvacare<- ggplot(data, aes(x=propgam, y=larvacare)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="# Dominants / Colony Size ", y=" # Larvae / # Non-reproductive workers") +
  expand_limits(x=c(0,0.3), y=c(0,3))

pprop_larvacare


library(Rmisc)

#n = 35 cols





library(ggplot2)


hist(data$Pupa)



pprop_pupcare<- ggplot(data, aes(x=propgam, y=pupacare)) +
  geom_point() +
  geom_smooth(method = "glm", formula = (y+0.0000000001)~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="# Dominants / Colony Size ", y=" # Pupae / # Non-reproductive workers") #+
  #expand_limits(y=c(0,4))

pprop_pupcare
#n=49 colonies


pprop_pup<- ggplot(data, aes(x=propgam, y=Pupa)) +
  geom_point() +
  geom_smooth(method = "glm", formula = (1+y)~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="# Dominants / # Total Workers ", y=" # Pupae") +
  expand_limits(y=c(0,50), x=c(0,0.3))

pprop_pup
#n=49 colonies

library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="pupacare", groupvars=c("Colony"), na.rm=TRUE)
sum_data
#n = 49 cols






pprop<- ggplot(data, aes(x=propgam, y=eggrate)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method = "glm", formula = (1+y)~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
   theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Proportion of dominants", y=" Average egg-laying rate of dominants") +
  expand_limits(x=c(0,0.3), y=c(0,2))

pprop

pprop_nolabs<- ggplot(data, aes(x=propgam, y=eggrate)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="", y="# Eggs / # Dominants / 29 days") +
  expand_limits(x=c(0,0.3), y=c(0,2))

pprop_nolabs


pprop_larv_nolabs<- ggplot(data, aes(x=propgam, y=larvacare)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="", y="# Larvae / # Non-reproductive workers") +
  expand_limits(x=c(0,0.3), y=c(0,3))

pprop_larv_nolabs






require(gridExtra)
grid.arrange(pprop, pprop_larvacare, ncol=2)





hist(data$eggrate)

##log transform eggrate
data$logeggrate<- log(1+data$eggrate)
data$logeggrate

c1<-cor.test(data$propgam, data$logeggrate, na.rm=TRUE)
c1
#p=1.747e-06, Pearson's rho= -0.710398, df=33



l1 <-lm (logeggrate ~ 
           propgam
         , data )
summary(l1)
#ln(Y+1)= -1.68700*X + 0.78859
#R2=0.50


##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
plot(l1)







hist(data$larvacare)

data$loglarvacare <- log(1+data$larvacare)

hist(data$loglarvacare)

l1 <-lm ( loglarvacare ~ 
            propgam
          , data )
summary(l1)
#ln(Y+1)= 2.18716*X + 0.27088
#R^2=0.5033 or 0.50
#t=5.954, p=1.10e-06




#using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#no effect of group size on propgam
plot(l1)





hist(data$pupacare)

data$logpupacare <- log(1+data$pupacare)
data$logpupacare 



l1 <-lm ( logpupacare ~ 
            propgam
          , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#no effect of group size on propgam
plot(l1)















##plot the #gams

library(ggplot2)

pgam_col<- ggplot(data, aes(x=GroupSize, y=NumGams)) +
  geom_point() +
  #geom_smooth(method = "glm", formula = (1+y)~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method = "lm", se=F) + #, formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Colony size (# workers) ", y="# Dominants") +
  expand_limits(x=c(0,250),y=c(0,15))

pgam_col



pegg_col<- ggplot(data, aes(x=GroupSize, y=Eggs)) +
  geom_point() +
  #geom_smooth(method = "glm", formula = (1+y)~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method = "lm", se=F) + #, formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Colony size (# workers) ", y="# Eggs") +
  expand_limits(x=c(0,250),y=c(0,200))

pegg_col




ppup_col<- ggplot(data, aes(x=GroupSize, y=Pupa)) +
  geom_point() +
  geom_smooth(method = "glm", formula = (0.000000001+y)~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method = "lm", se=F) + #, formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Colony Size  ", y="# Pupae") +
  expand_limits(x=c(0,250),y=c(0,60))

ppup_col
##R^2=0.31 suggesting that there is some relationship


plarv_col<- ggplot(data, aes(x=GroupSize, y=Larva)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  #geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Colony Size ", y="# Larvae") +
  expand_limits(x=c(0,250),y=c(0,100))

plarv_col
##R^2=0.64 suggesting that there is a relationship





require(gridExtra)
grid.arrange(pgam_col, pegg_col, ncol=2)
#grid.arrange(pgam_col, pegg_col, ppup_col, plarv_col, ncol=2)





c1<-cor.test(data$NumGams, data$GroupSize, na.rm=TRUE)
c1


l1 <-lm (NumGams ~ 
            GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#y=4.468540*X + 0.010051
#R^2=0.02534 ; t=1.105; p-value= 0.275


plot(l1)


hist(data$NumGams)


##log transform 
data$lognumgams<- log(1+data$NumGams)
data$lognumgams

hist(data$lognumgams)




l1 <-lm (lognumgams ~ 
           GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#ln(y+1)=1.630581*X + 0.001351
#R2=-0.004958  ; t=0.874; p-value= 0.387


plot(l1)



hist(data$Eggs)


l1 <-lm (Eggs ~ 
           GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#y=78.8712*X + 0.3044
#R2=0.1036; t= 1.953; p-value= 0.05932

plot(l1)


##log transform 
data$logegg<- log(1+data$Eggs)
data$logegg

hist(data$logegg)



hist(data$GroupSize)

##log transform 
data$logcolsize<- log(1+data$GroupSize)
data$logcolsize

hist(data$logcolsize)




l1 <-lm (logegg ~ 
           GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#ln(y+1)=4.244436*X + 0.003622
#R2=0.08285; t= 2.018; p-value= 0.0518

plot(l1)
#wporse 


l1 <-lm (Eggs ~ 
           logcolsize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#ln(y+1)=4.244436*X + 0.003622
#R2=0.08285; t= 2.018; p-value= 0.0518

plot(l1)



l1 <-lm (Larva ~ 
           GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#no effect of group size on propgam
plot(l1)
#y=78.8712*X + 0.3044
#R2=0.07646; t= 1.953; p-value= 0.05932





hist(data$Larva)


##log transform 
data$loglarv<- log(1+data$Larva)
data$loglarv

hist(data$loglarv)


c1<-cor.test(data$loglarv, data$GroupSize, na.rm=TRUE)
c1




l1 <-lm (loglarv ~ 
           GroupSize
         , data )
summary(l1)
#ln(y+1)=0.010154*X+2.633128
#R^2:0.4195; t=5.057; p=1.56e-05


##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#no effect of group size on propgam
plot(l1)




l1 <-lm (Pupa ~ 
           GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#y=0.14330*X + 1.17797
#R^2=0.07352; t=2.193; p-value= 0.0333

plot(l1)




hist(data$Pupa)


##log transform 
data$logpupa<- log(1+data$Pupa)
data$logpupa

hist(data$logpupa)


c1<-cor.test(data$logpupa, data$GroupSize, na.rm=TRUE)
c1


l1 <-lm (logpupa ~ 
           GroupSize
         , data )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#ln(1+y)=0.008147*X + 1.392838
#R2=0.1037; t=2.559; p-value= 0.0138

plot(l1)





#####Fig. 3. CHCs-RP area calculations for  15 compounds, both 12 and 18 week data ############


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/CHCanalysis/FertilitySignal/Index_calculations")
getwd()
#clear all
rm(list=ls())

#import  DATA set 

data<-read.csv("WorkerRemoval_Gam_treatment_CHC_2405.csv", header=TRUE)
#View(data)
str(data)
#215 ants' chromatograms

#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="TOTALAREA", groupvars=c("TREATMENT", "STATUS"))
sum_data

#IP, Gam= 143
#IP, NG=1
#IP, Revert=11
#IP, unclear=4
#IP, workers=3
#SP, Gam=43
#SP, NG=2
#SP, Revert=1
#SP, unclear=2
#SP, worker=5

#make interval a factor
str(data$Interval)
data$Interval <- as.factor(data$Interval)

##calculate propgam

data$propgam <- data$GAMSAMPLE/ data$COLSIZESAMPLE
data$propgam




#make sure that c23 area is a number
data$C23AREA <- as.numeric(data$C23AREA)









##1. calculate Relative Peak area of c23alkane##

data$RPAREAc23<- data$C23AREA / data$TOTALAREA

##2. calculate Relative Peak area of c25alkane##

data$RPAREAc25<- data$C25AREA/data$TOTALAREA

##3. calculate Relative Peak area of c27alkane##

data$RPAREAc27<- data$C27AREA/data$TOTALAREA

##4. calculate Relative Peak area of c29alkene##

data$RPAREAc29alkene<- data$C29ALKENEAREA_TOT/data$TOTALAREA

##5. calculate Relative Peak area of c29alkane##

data$RPAREAc29<- data$C29AREA/data$TOTALAREA

##6. calculate Relative Peak area of mec29alkane##

data$RPAREAmec29<- data$MEC29AREA/data$TOTALAREA

##7. calculate Relative Peak area of c31alkene##

data$RPAREAc31alkene<- data$C31ALKENEAREA_TOT/data$TOTALAREA

##8. calculate Relative Peak area of mec31##

data$RPAREAmec31<- data$ME31AREA/data$TOTALAREA

##9. calculate Relative Peak area of c33alkene##

data$RPAREAc33alkene<- data$C33ALKENEAREA_TOT/data$TOTALAREA

##10. calculate Relative Peak area of mec33##

data$RPAREAmec33<- data$ME33AREA/data$TOTALAREA

##11. calculate Relative Peak area of dimec33##

data$RPAREAdimec33<- data$DIMEC33AREA/data$TOTALAREA

##12. calculate Relative Peak area of c35alkene##

data$RPAREAc35alkene<- data$C35ALKENE_AREA_TOT/data$TOTALAREA

##13. calculate Relative Peak area of mec35##

data$RPAREAmec35<- data$MEC35AREA/data$TOTALAREA

##14. calculate Relative Peak area of dimec35##

data$RPAREAdimec35<- data$DIMEC35AREA/data$TOTALAREA


##15. calculate Relative Peak area of 1323Dimec37##

data$RPAREA1323dimec37 <- data$X1323Dimec37AREA/data$TOTALAREA








####RParea for all 15 compounds based on oocyte # and headwidth ####



#sample sizes
data_yolky <- subset(data, PHOTOYO >= 0)
str(data)
#215 ants
str(data_yolky)
#179 ants
data_hw <- subset(data, headwidth2 >= 0)
str(data_hw)
#134 ants





#plot for C23, 





library(ggplot2)

pc23_oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc23)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("C23 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc23_oocyte

pc23_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc23)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C23 Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc23_size

pc25_oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc25)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("C25 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc25_oocyte

pc25_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc25)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C25 Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc25_size


pc27_oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc27)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("# Yolky Oocytes") + 
  ylab("C27 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc27_oocyte



pc27_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc27)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C27 Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc27_size




pc29alkene_oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc29alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("C29 alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc29alkene_oocyte

pc29alkene_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc29alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C29 alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc29alkene_size





pc29oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc29)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("C29 alkane Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc29oocyte

pc29_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc29)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C29 alkane Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc29_size






pcmec29oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAmec29)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("# Yolky Oocytes") + 
  ylab("MeC29 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec29oocyte

pcmec29_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAmec29)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("MeC29  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec29_size





pcmec31alkeneoocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc31alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("C31alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec31alkeneoocyte

pcmec31alkene_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc31alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C31alkene  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec31alkene_size



pmec31oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAmec31)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("meC31 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pmec31oocyte

pcmec31_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAmec31)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("meC31  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec31_size










pcmec33alkeneoocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc33alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("# Yolky Oocytes") + 
  ylab("C33 alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec33alkeneoocyte

pcmec33alkene_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc33alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C33 alkene  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec33alkene_size





pmec33oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAmec33)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("meC33 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pmec33oocyte

pcmec33_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAmec33)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("meC33  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec33_size






pdimec33oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAdimec33)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("dimeC33 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec33oocyte

pdimec33_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAdimec33)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("dimeC33  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec33_size




pcmec35alkeneoocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAc35alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("# Yolky Oocytes") + 
  ylab("C35alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec35alkeneoocyte

pcmec35alkene_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAc35alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("C35alkene  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec35alkene_size





pmec35oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAmec35)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("meC35 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pmec35oocyte

pcmec35_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAmec35)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("meC35  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec35_size





pdimec35oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREAdimec35)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("dimeC35 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec35oocyte

pdimec35_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREAdimec35)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("dimeC35  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec35_size




pdimec37oocyte<-  ggplot(data, aes(x=PHOTOYO, y=RPAREA1323dimec37)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("# Yolky Oocytes") + 
  ylab("1323dimeC37 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec37oocyte

pdimec37_size<-  ggplot(data, aes(x=headwidth_mm, y=RPAREA1323dimec37)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("Headwidth (mm)") + 
  ylab("1323dimeC37  Rel Peak Area") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec37_size



#plot all oocyte comparisons on 1 graph


require(gridExtra)
grid.arrange(
  pc23_oocyte,
  pc25_oocyte,
  pc27_oocyte,
  pc29alkene_oocyte,  
  pc29oocyte,
  pcmec29oocyte,
  pcmec31alkeneoocyte,
  pmec31oocyte,
  pcmec33alkeneoocyte,
  pmec33oocyte,
  pdimec33oocyte,             
  pcmec35alkeneoocyte,
  pmec35oocyte,
  pdimec35oocyte,
  pdimec37oocyte,
  ncol=3)



require(gridExtra)
grid.arrange(
  pc23_size,
  pc25_size,
  pc27_size,
  pc29alkene_size,  
  pc29_size,
  pcmec29_size,
  pcmec31alkene_size,
  pcmec31_size,
  pcmec33alkene_size,
  pcmec33_size,
  pdimec33_size,             
  pcmec35alkene_size,
  pcmec35_size,
  pdimec35_size,
  pdimec37_size,
  ncol=3)


#only 14 compounds
require(gridExtra)
grid.arrange(
  pc25_size,
  pc27_size,
  pc29alkene_size,  
  pc29_size,
  pcmec29_size,
  pcmec31alkene_size,
  pcmec31_size,
  pcmec33alkene_size,
  pcmec33_size,
  pdimec33_size,             
  pcmec35alkene_size,
  pcmec35_size,
  pdimec35_size,
  pdimec37_size,
  ncol=2)



####RParea for all 15 compounds based on propgam ####



#plot for C23, 





library(ggplot2)

pc23_oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAc23)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("C23 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pc23_oocyte_pg

pc25_oocyte_pg <-  ggplot(data, aes(x=propgam, y=RPAREAc25)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("C25 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pc25_oocyte_pg


pc27_oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAc27)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("#Gam/Colony Size") + 
  ylab("C27 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pc27_oocyte_pg



pc29alkene_oocyte_pg <-  ggplot(data, aes(x=propgam, y=RPAREAc29alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("C29 alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pc29alkene_oocyte_pg






pc29oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAc29)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("C29 alkane Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pc29oocyte_pg








pcmec29oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAmec29)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("#Gam/Colony Size") + 
  ylab("MeC29 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pcmec29oocyte_pg



pcmec31alkeneoocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAc31alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("C31alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pcmec31alkeneoocyte_pg


pmec31oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAmec31)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("meC31 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pmec31oocyte_pg





pcmec33alkeneoocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAc33alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("#Gam/Colony Size") + 
  ylab("C33 alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pcmec33alkeneoocyte_pg





pmec33oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAmec33)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("meC33 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pmec33oocyte_pg


pdimec33oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAdimec33)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("dimeC33 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pdimec33oocyte_pg


pcmec35alkeneoocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAc35alkene)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("#Gam/Colony Size") + 
  ylab("C35alkene Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pcmec35alkeneoocyte_pg


pmec35oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAmec35)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("meC35 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pmec35oocyte_pg


pdimec35oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREAdimec35)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = F) + 
  xlab("#Gam/Colony Size") + 
  ylab("dimeC35 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pdimec35oocyte_pg

pdimec37oocyte_pg<-  ggplot(data, aes(x=propgam, y=RPAREA1323dimec37)) + 
  geom_point(aes(colour=TREATMENT, shape=STATUS), show.legend = T) + 
  xlab("#Gam/Colony Size") + 
  ylab("1323dimeC37 Rel Peak Area") +
  #ylim(0.2,0.7) +
  xlim(0,1) +
  geom_smooth(method=lm, se=FALSE)
pdimec37oocyte_pg




#plot all oocyte comparisons on 1 graph


require(gridExtra)
grid.arrange(
  pc23_oocyte_pg,
  pc25_oocyte_pg,
  pc27_oocyte_pg,
  pc29alkene_oocyte_pg,  
  pc29oocyte_pg,
  pcmec29oocyte_pg,
  pcmec31alkeneoocyte_pg,
  pmec31oocyte_pg,
  pcmec33alkeneoocyte_pg,
  pmec33oocyte_pg,
  pdimec33oocyte_pg,             
  pcmec35alkeneoocyte_pg,
  pmec35oocyte_pg,
  pdimec35oocyte_pg,
  pdimec37oocyte_pg,
  ncol=3)







####RParea for all 15 compounds based on headwidth of gams only ####



data_gam <- subset(data_hw, STATUS == "GAMERGATE")
str(data_gam)
#111 ants

#load package
library(Rmisc)



#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_gam, measurevar="TOTALAREA", groupvars=c("TREATMENT", "Interval"))
sum_data
#IP gams, 12 weeks n=27
#IP gams, 18 weeks, n=46
#SP gams, 12 weeks, n=17
#SP gams, 18 weeks, n=21




#plot for C23, 





library(ggplot2)

pc23_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc23)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C23 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc23_size_gam


pc25_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc25)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C25 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc25_size_gam


pc27_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc27)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C27 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc27_size_gam




pc29alkene_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc29alkene)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C29 alkene Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc29alkene_size_gam





pc29_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc29)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C29 alkane Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pc29_size_gam





pcmec29_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAmec29)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("MeC29 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec29_size_gam




pcmec31alkene_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc31alkene)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C31alkene Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec31alkene_size_gam




pcmec31_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAmec31)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("meC31 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec31_size_gam









pcmec33alkene_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc33alkene)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C33 alkene Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec33alkene_size_gam






pcmec33_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAmec33)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("meC33 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec33_size_gam





pdimec33_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAdimec33)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("dimeC33 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec33_size_gam





pcmec35alkene_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAc35alkene)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("C35alkene Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec35alkene_size_gam






pcmec35_size_gam <-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAmec35)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("meC35 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pcmec35_size_gam




pdimec35_size_gam <-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREAdimec35)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("dimeC35 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec35_size_gam





pdimec37_size_gam<-  ggplot(data_gam, aes(x=headwidth_mm, y=RPAREA1323dimec37)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth (mm)") + 
  ylab("1323dimeC37 Abundance") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
pdimec37_size_gam



#plot all oocyte comparisons on 1 graph


require(gridExtra)
grid.arrange(
  pc23_size_gam,
  pc25_size_gam,
  pc27_size_gam,
  pc29alkene_size_gam,  
  pc29_size_gam,
  pcmec29_size_gam,
  pcmec31alkene_size_gam,
  pcmec31_size_gam,
  pcmec33alkene_size_gam,
  pcmec33_size_gam,
  pdimec33_size_gam,             
  pcmec35alkene_size_gam,
  pcmec35_size_gam,
  pdimec35_size_gam,
  pdimec37_size_gam,
  ncol=3)

require(gridExtra)
grid.arrange(
  pc25_size_gam,
  pc27_size_gam,
  pc29alkene_size_gam,  
  pc29_size_gam,
  pcmec29_size_gam,
  pcmec31alkene_size_gam,
  pcmec31_size_gam,
  pcmec33alkene_size_gam,
  pcmec33_size_gam,
  pdimec33_size_gam,             
  pcmec35alkene_size_gam,
  pcmec35_size_gam,
  pdimec35_size_gam,
  pdimec37_size_gam,
  ncol=3)


###regressions of gamergate headwidth to chc abundance for 14 compounds ####


#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
str(data_gam)

sum_data<-summarySE(data_gam, measurevar="RPAREA1323dimec37", groupvars=c("TREATMENT"))
sum_data

sum_data<-summarySE(data_gam, measurevar="RPAREA1323dimec37", groupvars=c("TREATMENT", "Interval"))
sum_data

#IP gams, 12 weeks n=27
#IP gams, 18 weeks, n=46
#SP gams, 12 weeks, n=17
#SP gams, 18 weeks, n=21



sum_data<-summarySE(data_gam, measurevar="RPAREA1323dimec37", groupvars=c("TREATMENT","Interval", "COLONY"))
sum_data
#IP-12 weeks, n=22-10 colonies=12 coloines
#IP-18 weeks, n=10 colonies
#SP-12 weeks, n=11 colonies
#SP-18 weeks, n=12 colonies





lc25 <- lm(headwidth_mm ~ RPAREAc25, data=data_gam)
lc25
summary(lc25)
Anova(lc25)
#y= -2.53702  *X + 2.72863
#Adjusted R-squared:  0.04299
#t=-2.213 
#p-value=0.029


lc27 <- lm(headwidth_mm ~ RPAREAc27, data=data_gam)
lc27
summary(lc27)
#y= -1.92037  *X + 2.74427
#Adjusted R-squared:  0.04459 
#t= -2.255
#p-value=0.0261

lc29alkene <- lm(headwidth_mm ~ RPAREAc29alkene, data=data_gam)
lc29alkene
summary(lc29alkene)
#y= -2.59467 *X + 2.75725
#R-squared:  0.08392 
#t= -3.028
#p-value=0.00204

lc29 <- lm(headwidth_mm ~ RPAREAc29, data=data_gam)
lc29
summary(lc29)
#y= -2.26105 *X + 2.75140
#Adjusted R-squared:  0.0154 
#t= -1.825
#p-value=0.0707

lcmec29 <- lm(headwidth_mm ~ RPAREAmec29, data=data_gam)
lcmec29
summary(lcmec29)
#y= -1.51189  *X + 2.79197
# R-squared:  0.06937 
#t= -2.85
#p-value=0.00522

lc31alkene <- lm(headwidth_mm ~ RPAREAc31alkene, data=data_gam)
lc31alkene
summary(lc31alkene)
#y= -0.70764 *X + 2.76481
#R-squared:  0.02552  
#t= -1.689 
#p-value=0.094

lmec31 <- lm(headwidth_mm ~ RPAREAmec31, data=data_gam)
lmec31
summary(lmec31)
#y= -0.11700 *X + 2.71701
#R-squared:  0.0007658
#t= -0.289
#p-value=0.773

lc33alkene <- lm(headwidth_mm ~ RPAREAc33alkene, data=data_gam)
lc33alkene
summary(lc33alkene)
#y= 0.4437 *X + 2.6600
#R-squared:  0.009068  
#t= 0.999
#p-value=0.32

lmec33 <- lm(headwidth_mm ~ RPAREAmec33, data=data_gam)
lmec33
summary(lmec33)
#y= 0.59846 *X + 2.66810
#R-squared:  0.004199
#t= 0.678
#p-value=0.499

ldimec33 <- lm(headwidth_mm ~ RPAREAdimec33, data=data_gam)
ldimec33
summary(ldimec33)
#y= 1.48680 *X + 2.57350
#R-squared:  0.06915 
#t= 2.846
#p-value=0.0053


lc35alkene <- lm(headwidth_mm ~ RPAREAc35alkene, data=data_gam)
lc35alkene
summary(lc35alkene)
#y= 0.13754 *X + 2.69832
#R-squared:  0.0003561 
#t= 0.197
#p-value=0.844


lmec35 <- lm(headwidth_mm ~ RPAREAmec35, data=data_gam)
lmec35
summary(lmec35)
#y= 0.86745 *X + 2.68017
#R-squared:  0.004421 
#t= 0.696
#p-value=0.488


ldimec35 <- lm(headwidth_mm ~ RPAREAdimec35, data=data_gam)
ldimec35
summary(ldimec35)
#y= 1.48109 *X + 2.60317
#R-squared:  0.07097 
#t= 2.886
#p-value=0.00471

l1323dimec37 <- lm(headwidth_mm ~ RPAREA1323dimec37, data=data_gam)
l1323dimec37
summary(l1323dimec37)
#y= 1.77523 *X + 2.66569
#R-squared:  0.0269 
#t= 1.62
#p-value=0.108


###prep for of index of medians of relative peak areas of 15 compound classes-both 12 and 18 weeks####




setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/CHCanalysis/FertilitySignal/Index_calculations")
getwd()
#clear all
rm(list=ls())

#import  DATA set 

data<-read.csv("WorkerRemoval_Gam_treatment_CHC_2405.csv", header=TRUE)
#View(data)
str(data)
#215 ants' chromatograms

#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="TOTALAREA", groupvars=c("TREATMENT", "STATUS"))
sum_data

#IP, Gam= 143
#IP, NG=1
#IP, Revert=11
#IP, unclear=4
#IP, workers=3
#SP, Gam=43
#SP, NG=2
#SP, Revert=1
#SP, unclear=2
#SP, worker=5

#make interval a factor
str(data$Interval)
data$Interval <- as.factor(data$Interval)

##calculate propgam

data$propgam <- data$GAMSAMPLE/ data$COLSIZESAMPLE
data$propgam





#make sure that c23 area is a number
data$C23AREA <- as.numeric(data$C23AREA)









##1. calculate Relative Peak area of c23alkane##

data$RPAREAc23<- data$C23AREA / data$TOTALAREA

##2. calculate Relative Peak area of c25alkane##

data$RPAREAc25<- data$C25AREA/data$TOTALAREA

##3. calculate Relative Peak area of c27alkane##

data$RPAREAc27<- data$C27AREA/data$TOTALAREA

##4. calculate Relative Peak area of c29alkene##

data$RPAREAc29alkene<- data$C29ALKENEAREA_TOT/data$TOTALAREA

##5. calculate Relative Peak area of c29alkane##

data$RPAREAc29<- data$C29AREA/data$TOTALAREA

##6. calculate Relative Peak area of mec29alkane##

data$RPAREAmec29<- data$MEC29AREA/data$TOTALAREA

##7. calculate Relative Peak area of c31alkene##

data$RPAREAc31alkene<- data$C31ALKENEAREA_TOT/data$TOTALAREA

##8. calculate Relative Peak area of mec31##

data$RPAREAmec31<- data$ME31AREA/data$TOTALAREA

##9. calculate Relative Peak area of c33alkene##

data$RPAREAc33alkene<- data$C33ALKENEAREA_TOT/data$TOTALAREA

##10. calculate Relative Peak area of mec33##

data$RPAREAmec33<- data$ME33AREA/data$TOTALAREA

##11. calculate Relative Peak area of dimec33##

data$RPAREAdimec33<- data$DIMEC33AREA/data$TOTALAREA

##12. calculate Relative Peak area of c35alkene##

data$RPAREAc35alkene<- data$C35ALKENE_AREA_TOT/data$TOTALAREA

##13. calculate Relative Peak area of mec35##

data$RPAREAmec35<- data$MEC35AREA/data$TOTALAREA

##14. calculate Relative Peak area of dimec35##

data$RPAREAdimec35<- data$DIMEC35AREA/data$TOTALAREA


##15. calculate Relative Peak area of 1323Dimec37##

data$RPAREA1323dimec37 <- data$X1323Dimec37AREA/data$TOTALAREA









#sample sizes
data_yolky <- subset(data, PHOTOYO >= 0)
str(data)
#215 ants
str(data_yolky)
#179 ants
data_hw <- subset(data, headwidth2 >= 0)
str(data_hw)
#134 ants








#subset data for gamergates only

datagam <- subset(data, STATUS=="GAMERGATE")
datagam
#View(datagam)
#


#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagam, measurevar="TOTALAREA", groupvars=c("TREATMENT"))
sum_data
143+43
sum_data<-summarySE(datagam, measurevar="TOTALAREA", groupvars=c("Interval", "TREATMENT"))
sum_data
##EIGHTEEN-week gamergates, n=70
#n=48 IP, n= 22 SP
##TWELVE-week gamergates, n=116
#n=95 IP, n=21 SP

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagam, measurevar="TOTALAREA", groupvars=c("Interval", "TREATMENT", "COLONY"))
sum_data
##EIGHTEEN-week, n=22 colonies
#n=10 IP colonies, n=12 SP colonies
##TWELVE-week, n=28 colonies
#n=14 IP colonies, n=14 SP colonies
sum_data<-summarySE(datagam, measurevar="TOTALAREA", groupvars=c("TREATMENT", "COLONY"))
sum_data
#n=24 IP colonies
#n=50-24=26 SP colonies

#subset data by each treatment

data.incrprop <- subset(datagam, TREATMENT=="INCRPROP")
data.incrprop
#n=143 IP doms


str(data.incrprop)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data.incrprop, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=24 IP colonies

sum_data<-summarySE(data.incrprop, measurevar="TOTALAREA", groupvars="STATUS")
sum_data
#n=143 IP gamergates

data.sameprop<- subset(datagam, TREATMENT=="SAMEPROP")
data.sameprop

#summarize data easily with summarySE, only for sample sizes
sum_data <- summarySE(data.sameprop, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=26 SP colonies

sum_data <- summarySE(data.sameprop, measurevar="TOTALAREA", groupvars="STATUS")
sum_data
#n=43 gamergates in SP colonies




###compare 13,23Dimec37 from both groups####
#boxplot of relative peak area of 13,23DimeC37 for all peaks



library(ggplot2)

#View(datagam)

fert_rp <- ggplot(datagam, aes(y=RPAREA1323dimec37, x=factor(TREATMENT))) + 
  #geom_boxplot() +
  geom_boxplot(aes(fill=Interval), show.legend = T) +
  # facet_wrap(.~status3, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,250, by=50)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatments", y="Area of 13,23DimeC37/ All areas") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

fert_rp


hist(datagam$RPAREA1323dimec37)

datagam$logRP1323dimec37 <- log(1+datagam$RPAREA1323dimec37)

library(car)
#simply linear model!
full.m <-lm (logRP1323dimec37 ~ 
               TREATMENT +
               Interval +
               TREATMENT * Interval
             , datagam)
#excluding subject as a factor since it's already include in Time
summary(full.m)

Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ TREATMENT * Interval)
e1
e1$contrasts
multcomp::cld(e1)
##shows effects of both treatment as well as interval of time for the SP group









###calculate index of medians of relative peak areas of 15 compound classes--both 12 and 18 week data####
##calculate median of each compound class' RPAREA for each treatment

###INCRPROP MEDIANs###


##1. calculate Median of Relative Peak area of c23alkane##

med_c23_incrprop  <- median(data.incrprop$RPAREAc23)
med_c23_incrprop

##2. calculate MEDIAN of Relative Peak area of c25alkane##

med_c25_incrprop  <- median(data.incrprop$RPAREAc25)
med_c25_incrprop

##3. calculate MEDIAN of Relative Peak area of c27alkane##

med_c27_incrprop  <- median(data.incrprop$RPAREAc27)
med_c27_incrprop

##4. calculate MEDIAN of Relative Peak area of c29alkene##

med_c29alkene_incrprop  <- median(data.incrprop$RPAREAc29alkene)
med_c29alkene_incrprop


##5. calculate MEDIAN of Relative Peak area of c29alkane##

med_c29_incrprop  <- median(data.incrprop$RPAREAc29)
med_c29_incrprop


##6. calculate MEDIAN of Relative Peak area of mec29alkane##

med_mec29_incrprop  <- median(data.incrprop$RPAREAmec29)
med_mec29_incrprop

##7. calculate MEDIAN of Relative Peak area of c31alkene##


med_c31alkene_incrprop  <- median(data.incrprop$RPAREAc31alkene)
med_c31alkene_incrprop


##8. calculate MEDIAN of Relative Peak area of mec31##

med_mec31_incrprop  <- median(data.incrprop$RPAREAmec31)
med_mec31_incrprop


##9. calculate MEDIAN of Relative Peak area of c33alkene##

med_c33alkene_incrprop  <- median(data.incrprop$RPAREAc33alkene)
med_c33alkene_incrprop


##10. calculate MEDIAN of Relative Peak area of mec33##

med_mec33_incrprop  <- median(data.incrprop$RPAREAmec33)
med_mec33_incrprop

##11. calculate MEDIAN of Relative Peak area of dimec33##

med_dimec33_incrprop  <- median(data.incrprop$RPAREAdimec33)
med_dimec33_incrprop

##12. calculate MEDIAN of Relative Peak area of c35alkene##


med_c35alkene_incrprop  <- median(data.incrprop$RPAREAc35alkene)
med_c35alkene_incrprop

##13. calculate MEDIAN of Relative Peak area of mec35##

med_mec35_incrprop  <- median(data.incrprop$RPAREAmec35)
med_mec35_incrprop

##14. calculate MEDIAN of Relative Peak area of dimec35##

med_dimec35_incrprop  <- median(data.incrprop$RPAREAdimec35)
med_dimec35_incrprop


##15. calculate MEDIAN of Relative Peak area of 1323Dimec37##

med_1323dimec37_incrprop  <- median(data.incrprop$RPAREA1323dimec37)
med_1323dimec37_incrprop

#SAMEPROP MEDIANs#

##1. calculate Median of Relative Peak area of c23alkane##

med_c23_sameprop <- median(data.sameprop$RPAREAc23)
med_c23_sameprop

##2. calculate MEDIAN of Relative Peak area of c25alkane##

med_c25_sameprop  <- median(data.sameprop$RPAREAc25)
med_c25_sameprop

##3. calculate MEDIAN of Relative Peak area of c27alkane##

med_c27_sameprop  <- median(data.sameprop$RPAREAc27)
med_c27_sameprop

##4. calculate MEDIAN of Relative Peak area of c29alkene##

med_c29alkene_sameprop  <- median(data.sameprop$RPAREAc29alkene)
med_c29alkene_sameprop


##5. calculate MEDIAN of Relative Peak area of c29alkane##

med_c29_sameprop  <- median(data.sameprop$RPAREAc29)
med_c29_sameprop


##6. calculate MEDIAN of Relative Peak area of mec29alkane##

med_mec29_sameprop <- median(data.sameprop$RPAREAmec29)
med_mec29_sameprop

##7. calculate MEDIAN of Relative Peak area of c31alkene##


med_c31alkene_sameprop  <- median(data.sameprop$RPAREAc31alkene)
med_c31alkene_sameprop


##8. calculate MEDIAN of Relative Peak area of mec31##

med_mec31_sameprop  <- median(data.sameprop$RPAREAmec31)
med_mec31_sameprop


##9. calculate MEDIAN of Relative Peak area of c33alkene##

med_c33alkene_sameprop  <- median(data.sameprop$RPAREAc33alkene)
med_c33alkene_sameprop


##10. calculate MEDIAN of Relative Peak area of mec33##

med_mec33_sameprop  <- median(data.sameprop$RPAREAmec33)
med_mec33_sameprop

##11. calculate MEDIAN of Relative Peak area of dimec33##

med_dimec33_sameprop  <- median(data.sameprop$RPAREAdimec33)
med_dimec33_sameprop

##12. calculate MEDIAN of Relative Peak area of c35alkene##


med_c35alkene_sameprop  <- median(data.sameprop$RPAREAc35alkene)
med_c35alkene_sameprop

##13. calculate MEDIAN of Relative Peak area of mec35##

med_mec35_sameprop  <- median(data.sameprop$RPAREAmec35)
med_mec35_sameprop

##14. calculate MEDIAN of Relative Peak area of dimec35##

med_dimec35_sameprop  <- median(data.sameprop$RPAREAdimec35)
med_dimec35_sameprop


##15. calculate MEDIAN of Relative Peak area of 1323Dimec37##

med_1323dimec37_sameprop  <- median(data.sameprop$RPAREA1323dimec37)
med_1323dimec37_sameprop

###calculations for medians DETAILS####
##want to see positive slope as
#I predict that sameprop will have more larger compounds than incrprop ones

##1. 

index_c23 <- med_c23_sameprop / ( med_c23_incrprop + med_c23_sameprop)
index_c23
##answer is NaN since its 0/0+0


##2. 

index_c25 <- med_c25_sameprop / (med_c25_incrprop + med_c25_sameprop)
index_c25

##3. 

index_c27 <- med_c27_sameprop / ( med_c27_incrprop + med_c27_sameprop)
index_c27

##4.

index_c29alkene <- med_c29alkene_sameprop / ( med_c29alkene_incrprop + med_c29alkene_sameprop)
index_c29alkene


##5.


index_c29 <- med_c29_sameprop / ( med_c29_incrprop + med_c29_sameprop)
index_c29


##6. 

index_mec29 <- med_mec29_sameprop / ( med_mec29_incrprop + med_mec29_sameprop)
index_mec29


##7.

index_c31alkene <- med_c31alkene_sameprop / ( med_c31alkene_incrprop + med_c31alkene_sameprop)
index_c31alkene



##8.

index_mec31 <- med_mec31_sameprop / ( med_mec31_incrprop + med_mec31_sameprop)
index_mec31



##9.


index_c33alkene <- med_c33alkene_sameprop / ( med_c33alkene_incrprop + med_c33alkene_sameprop)
index_c33alkene


##10.

index_mec33 <- med_mec33_sameprop / ( med_mec33_incrprop + med_mec33_sameprop)
index_mec33



##11. 


index_dimec33 <- med_dimec33_sameprop / ( med_dimec33_incrprop + med_dimec33_sameprop)
index_dimec33


##12. 

index_c35alkene <- med_c35alkene_sameprop / ( med_c35alkene_incrprop + med_c35alkene_sameprop)
index_c35alkene


##13.

index_mec35 <- med_mec35_sameprop / ( med_mec35_incrprop + med_mec35_sameprop)
index_mec35


##14.

index_dimec35 <- med_dimec35_sameprop / ( med_dimec35_incrprop + med_dimec35_sameprop)
index_dimec35

##15. 

index_1323dimec37 <- med_1323dimec37_sameprop / ( med_1323dimec37_incrprop + med_1323dimec37_sameprop)
index_1323dimec37


###make a table of index

index_data <- matrix(c(
  index_c25,
  index_c27,
  index_c29alkene,
  index_c29,
  index_mec29,
  index_c31alkene,
  index_mec31,
  index_c33alkene,
  index_mec33,
  index_dimec33,
  index_c35alkene,
  index_mec35,
  index_dimec35,
  index_1323dimec37),
  ncol=1,byrow=TRUE)
rownames(index_data) <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)


plot(index_data,
     ylim=c(0,0.7))

df_indexdata<- as.data.frame(index_data)



write.csv(df_indexdata, file="df_indexdata_12and18wk.csv", row.names=F)



#make linear model and check correlation
##entered data in excel, found median of RTs of compound classes

#import  DATA set 

data<-read.csv("index_summary_2405.csv", header=TRUE)
data
str(data)


View(data)


linearMod <- lm(index ~ Retention.Time, data=data)
plot(data$Retention.Time, data$index)
print(linearMod)
summary(linearMod)
#adjusted R^2=0.8252
##y= 0.015246*X + 0.121381


#diagnostics
plot(linearMod)

####diagnostics of model of fixed effect of pg, with rand effect of label
Res <- resid(linearMod) 
Fit <- fitted(linearMod) 


###histogram of residuals to check for normality
qqnorm(Res)
hist(Res)
shapiro.test(Res)


#significant dependence of index on retention time

library(ggplot2)


p1<- ggplot(data, aes(x=Retention.Time, y=index)) + 
  geom_point() + 
  xlab("Median Retention Time of Compound (Minutes since Start)") + 
  ylab("Index of Median Rel. Peak Area") +
  ylim(0.2,0.7) +
  # xlim(0,250) +
  geom_smooth(method=lm, se=FALSE)
p1


#check correlation

cor.test(data$Retention.Time, data$index) 
##variables seem to be correlated, as its different than 0 according to a test
###pearson's correlation, though
#output:
#Pearson's product-moment correlation

#data:  data$Retention.Time and data$Index.of.Medians
#t = 7.5946, df = 12, p-value = 6.377e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.7331699 0.9714509
#sample estimates:
#      cor 
#0.9098235




####index only for those at 12 weeks ####


###prep for of index of medians of relative peak areas of 15 compound classes @ 12 weeks####

#subset gam data for only 12 weeks

datagam_twelve <- subset(datagam, Interval=="Twelve")
datagam_twelve


#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagam_twelve, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=28 colonies for 12-week dataset
sum_data<-summarySE(datagam_twelve, measurevar="TOTALAREA", groupvars="TREATMENT")
sum_data
#n=95 IP gams
#n=21 SP gams
95+21
#n=116 gamergates total @ 12 weeks




#subset data by each treatment

data.incrprop<-subset (datagam_twelve, TREATMENT=="INCRPROP")
data.incrprop




str(data.incrprop)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data.incrprop, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=14 IP colonies
sum_data<-summarySE(data.incrprop, measurevar="TOTALAREA", groupvars="TREATMENT")
sum_data
#n=95 gamergates from IP colonies @ 12 weeks

data.sameprop<-subset (datagam_twelve, TREATMENT=="SAMEPROP")
data.sameprop

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data.sameprop, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=14 SP colonies
sum_data<-summarySE(data.sameprop, measurevar="TOTALAREA", groupvars="TREATMENT")
sum_data
#n=21 gamergates from SP colonies @ 12 week









###calculate index of medians of relative peak areas of 15 compound classes@ 12 weeks data####
##calculate median of each compound class' RPAREA for each treatment

###INCRPROP MEDIANs###


##1. calculate Median of Relative Peak area of c23alkane##

med_c23_incrprop  <- median(data.incrprop$RPAREAc23)
med_c23_incrprop

##2. calculate MEDIAN of Relative Peak area of c25alkane##

med_c25_incrprop  <- median(data.incrprop$RPAREAc25)
med_c25_incrprop

##3. calculate MEDIAN of Relative Peak area of c27alkane##

med_c27_incrprop  <- median(data.incrprop$RPAREAc27)
med_c27_incrprop

##4. calculate MEDIAN of Relative Peak area of c29alkene##

med_c29alkene_incrprop  <- median(data.incrprop$RPAREAc29alkene)
med_c29alkene_incrprop


##5. calculate MEDIAN of Relative Peak area of c29alkane##

med_c29_incrprop  <- median(data.incrprop$RPAREAc29)
med_c29_incrprop


##6. calculate MEDIAN of Relative Peak area of mec29alkane##

med_mec29_incrprop  <- median(data.incrprop$RPAREAmec29)
med_mec29_incrprop

##7. calculate MEDIAN of Relative Peak area of c31alkene##


med_c31alkene_incrprop  <- median(data.incrprop$RPAREAc31alkene)
med_c31alkene_incrprop


##8. calculate MEDIAN of Relative Peak area of mec31##

med_mec31_incrprop  <- median(data.incrprop$RPAREAmec31)
med_mec31_incrprop


##9. calculate MEDIAN of Relative Peak area of c33alkene##

med_c33alkene_incrprop  <- median(data.incrprop$RPAREAc33alkene)
med_c33alkene_incrprop


##10. calculate MEDIAN of Relative Peak area of mec33##

med_mec33_incrprop  <- median(data.incrprop$RPAREAmec33)
med_mec33_incrprop

##11. calculate MEDIAN of Relative Peak area of dimec33##

med_dimec33_incrprop  <- median(data.incrprop$RPAREAdimec33)
med_dimec33_incrprop

##12. calculate MEDIAN of Relative Peak area of c35alkene##


med_c35alkene_incrprop  <- median(data.incrprop$RPAREAc35alkene)
med_c35alkene_incrprop

##13. calculate MEDIAN of Relative Peak area of mec35##

med_mec35_incrprop  <- median(data.incrprop$RPAREAmec35)
med_mec35_incrprop

##14. calculate MEDIAN of Relative Peak area of dimec35##

med_dimec35_incrprop  <- median(data.incrprop$RPAREAdimec35)
med_dimec35_incrprop


##15. calculate MEDIAN of Relative Peak area of 1323Dimec37##

med_1323dimec37_incrprop  <- median(data.incrprop$RPAREA1323dimec37)
med_1323dimec37_incrprop

#SAMEPROP MEDIANs#

##1. calculate Median of Relative Peak area of c23alkane##

med_c23_sameprop <- median(data.sameprop$RPAREAc23)
med_c23_sameprop

##2. calculate MEDIAN of Relative Peak area of c25alkane##

med_c25_sameprop  <- median(data.sameprop$RPAREAc25)
med_c25_sameprop

##3. calculate MEDIAN of Relative Peak area of c27alkane##

med_c27_sameprop  <- median(data.sameprop$RPAREAc27)
med_c27_sameprop

##4. calculate MEDIAN of Relative Peak area of c29alkene##

med_c29alkene_sameprop  <- median(data.sameprop$RPAREAc29alkene)
med_c29alkene_sameprop


##5. calculate MEDIAN of Relative Peak area of c29alkane##

med_c29_sameprop  <- median(data.sameprop$RPAREAc29)
med_c29_sameprop


##6. calculate MEDIAN of Relative Peak area of mec29alkane##

med_mec29_sameprop <- median(data.sameprop$RPAREAmec29)
med_mec29_sameprop

##7. calculate MEDIAN of Relative Peak area of c31alkene##


med_c31alkene_sameprop  <- median(data.sameprop$RPAREAc31alkene)
med_c31alkene_sameprop


##8. calculate MEDIAN of Relative Peak area of mec31##

med_mec31_sameprop  <- median(data.sameprop$RPAREAmec31)
med_mec31_sameprop


##9. calculate MEDIAN of Relative Peak area of c33alkene##

med_c33alkene_sameprop  <- median(data.sameprop$RPAREAc33alkene)
med_c33alkene_sameprop


##10. calculate MEDIAN of Relative Peak area of mec33##

med_mec33_sameprop  <- median(data.sameprop$RPAREAmec33)
med_mec33_sameprop

##11. calculate MEDIAN of Relative Peak area of dimec33##

med_dimec33_sameprop  <- median(data.sameprop$RPAREAdimec33)
med_dimec33_sameprop

##12. calculate MEDIAN of Relative Peak area of c35alkene##


med_c35alkene_sameprop  <- median(data.sameprop$RPAREAc35alkene)
med_c35alkene_sameprop

##13. calculate MEDIAN of Relative Peak area of mec35##

med_mec35_sameprop  <- median(data.sameprop$RPAREAmec35)
med_mec35_sameprop

##14. calculate MEDIAN of Relative Peak area of dimec35##

med_dimec35_sameprop  <- median(data.sameprop$RPAREAdimec35)
med_dimec35_sameprop


##15. calculate MEDIAN of Relative Peak area of 1323Dimec37##

med_1323dimec37_sameprop  <- median(data.sameprop$RPAREA1323dimec37)
med_1323dimec37_sameprop

### calculations for medians DETAILS####
##want to see positive slope as
#I predict that sameprop will have more larger compounds than incrprop ones

##1. 

index_c23 <- med_c23_sameprop / ( med_c23_incrprop + med_c23_sameprop)
index_c23
##answer is NaN since its 0/0+0


##2. 

index_c25 <- med_c25_sameprop / (med_c25_incrprop + med_c25_sameprop)
index_c25

##3. 

index_c27 <- med_c27_sameprop / ( med_c27_incrprop + med_c27_sameprop)
index_c27

##4.

index_c29alkene <- med_c29alkene_sameprop / ( med_c29alkene_incrprop + med_c29alkene_sameprop)
index_c29alkene


##5.


index_c29 <- med_c29_sameprop / ( med_c29_incrprop + med_c29_sameprop)
index_c29


##6. 

index_mec29 <- med_mec29_sameprop / ( med_mec29_incrprop + med_mec29_sameprop)
index_mec29


##7.

index_c31alkene <- med_c31alkene_sameprop / ( med_c31alkene_incrprop + med_c31alkene_sameprop)
index_c31alkene



##8.

index_mec31 <- med_mec31_sameprop / ( med_mec31_incrprop + med_mec31_sameprop)
index_mec31



##9.


index_c33alkene <- med_c33alkene_sameprop / ( med_c33alkene_incrprop + med_c33alkene_sameprop)
index_c33alkene


##10.

index_mec33 <- med_mec33_sameprop / ( med_mec33_incrprop + med_mec33_sameprop)
index_mec33



##11. 


index_dimec33 <- med_dimec33_sameprop / ( med_dimec33_incrprop + med_dimec33_sameprop)
index_dimec33


##12. 

index_c35alkene <- med_c35alkene_sameprop / ( med_c35alkene_incrprop + med_c35alkene_sameprop)
index_c35alkene


##13.

index_mec35 <- med_mec35_sameprop / ( med_mec35_incrprop + med_mec35_sameprop)
index_mec35


##14.

index_dimec35 <- med_dimec35_sameprop / ( med_dimec35_incrprop + med_dimec35_sameprop)
index_dimec35

##15. 

index_1323dimec37 <- med_1323dimec37_sameprop / ( med_1323dimec37_incrprop + med_1323dimec37_sameprop)
index_1323dimec37


###make a table of index

index_data <- matrix(c(
  index_c25,
  index_c27,
  index_c29alkene,
  index_c29,
  index_mec29,
  index_c31alkene,
  index_mec31,
  index_c33alkene,
  index_mec33,
  index_dimec33,
  index_c35alkene,
  index_mec35,
  index_dimec35,
  index_1323dimec37),
  ncol=1,byrow=TRUE)
rownames(index_data) <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)


plot(index_data,
     ylim=c(0,0.7))

df_indexdata<- as.data.frame(index_data)
#export the gene lists with statistics
write.csv(df_indexdata, file="df_indexdata_12wk.csv", row.names=F)







#make linear model and check correlation
##entered data in excel, found median of RTs of compound classes

#import  DATA set 
##copied into a new spreadsheet and saved as .csv
data12<-read.csv("index_summary_2405_12wk.csv", header=TRUE)
data12
str(data12)


View(data12)


linearMod <- lm(index ~ Retention.Time, data=data12)
plot(data$Retention.Time, data$index)
print(linearMod)
summary(linearMod)
#adjusted R^2=0.01223
##y= 0.00206*X + 0.451454
####so basically no relationship here compound size

#diagnostics
plot(linearMod)

####diagnostics of model of fixed effect of pg, with rand effect of label
Res <- resid(linearMod) 
Fit <- fitted(linearMod) 


###histogram of residuals to check for normality
qqnorm(Res)
hist(Res)
shapiro.test(Res)


#significant dependence of index on retention time

library(ggplot2)


p1<- ggplot(data12, aes(x=Retention.Time, y=index)) + 
  geom_point() + 
  xlab("Median Retention Time of Compound (Minutes since Start)") + 
  ylab("Index of Median Rel. Peak Area") +
  ylim(0.2,0.7) +
  # xlim(0,250) +
  geom_smooth(method=lm, se=FALSE)
p1


#check correlation

cor.test(data12$Retention.Time, data12$index) 
##variables seem to be correlated, as its different than 0 according to a test
###pearson's correlation, though
#output:
#Pearson's product-moment correlation

#data:  data$Retention.Time and data$Index.of.Medians
#t = 1.0775, df = 12, p-value = 0.3025
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#-0.2772705  0.7149220
#sample estimates:
#      cor 
#0.2970022




####index only for those at 18 weeks ####


###prep for of index of medians of relative peak areas of 15 compound classes @ 18 weeks####

#subset gam data for only 18 weeks

datagam_eighteen <- subset(datagam, Interval=="Eighteen")
datagam_eighteen
#View(datagam_eighteen)


#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagam_eighteen, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=22 colonies for 18-week dataset
sum_data<-summarySE(datagam_eighteen, measurevar="TOTALAREA", groupvars="TREATMENT")
sum_data
#n=48 IP gams
#n=22 SP gams
48+22
#n=70 gamergates from IP colonies @ 18 week




#subset data by each treatment

data.incrprop<-subset (datagam_eighteen, TREATMENT=="INCRPROP")
data.incrprop

str(data.incrprop)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data.incrprop, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=10 IP colonies


data.sameprop<-subset (datagam_eighteen, TREATMENT=="SAMEPROP")
data.sameprop

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data.sameprop, measurevar="TOTALAREA", groupvars="COLONY")
sum_data
#n=12 SP colonies










###calculate index of medians of relative peak areas of 15 compound classes@ 18 week data####
##calculate median of each compound class' RPAREA for each treatment

###INCRPROP MEDIANs###


##1. calculate Median of Relative Peak area of c23alkane##

med_c23_incrprop  <- median(data.incrprop$RPAREAc23)
med_c23_incrprop

##2. calculate MEDIAN of Relative Peak area of c25alkane##

med_c25_incrprop  <- median(data.incrprop$RPAREAc25)
med_c25_incrprop

##3. calculate MEDIAN of Relative Peak area of c27alkane##

med_c27_incrprop  <- median(data.incrprop$RPAREAc27)
med_c27_incrprop

##4. calculate MEDIAN of Relative Peak area of c29alkene##

med_c29alkene_incrprop  <- median(data.incrprop$RPAREAc29alkene)
med_c29alkene_incrprop


##5. calculate MEDIAN of Relative Peak area of c29alkane##

med_c29_incrprop  <- median(data.incrprop$RPAREAc29)
med_c29_incrprop


##6. calculate MEDIAN of Relative Peak area of mec29alkane##

med_mec29_incrprop  <- median(data.incrprop$RPAREAmec29)
med_mec29_incrprop

##7. calculate MEDIAN of Relative Peak area of c31alkene##


med_c31alkene_incrprop  <- median(data.incrprop$RPAREAc31alkene)
med_c31alkene_incrprop


##8. calculate MEDIAN of Relative Peak area of mec31##

med_mec31_incrprop  <- median(data.incrprop$RPAREAmec31)
med_mec31_incrprop


##9. calculate MEDIAN of Relative Peak area of c33alkene##

med_c33alkene_incrprop  <- median(data.incrprop$RPAREAc33alkene)
med_c33alkene_incrprop


##10. calculate MEDIAN of Relative Peak area of mec33##

med_mec33_incrprop  <- median(data.incrprop$RPAREAmec33)
med_mec33_incrprop

##11. calculate MEDIAN of Relative Peak area of dimec33##

med_dimec33_incrprop  <- median(data.incrprop$RPAREAdimec33)
med_dimec33_incrprop

##12. calculate MEDIAN of Relative Peak area of c35alkene##


med_c35alkene_incrprop  <- median(data.incrprop$RPAREAc35alkene)
med_c35alkene_incrprop

##13. calculate MEDIAN of Relative Peak area of mec35##

med_mec35_incrprop  <- median(data.incrprop$RPAREAmec35)
med_mec35_incrprop

##14. calculate MEDIAN of Relative Peak area of dimec35##

med_dimec35_incrprop  <- median(data.incrprop$RPAREAdimec35)
med_dimec35_incrprop


##15. calculate MEDIAN of Relative Peak area of 1323Dimec37##

med_1323dimec37_incrprop  <- median(data.incrprop$RPAREA1323dimec37)
med_1323dimec37_incrprop

#SAMEPROP MEDIANs#

##1. calculate Median of Relative Peak area of c23alkane##

med_c23_sameprop <- median(data.sameprop$RPAREAc23)
med_c23_sameprop

##2. calculate MEDIAN of Relative Peak area of c25alkane##

med_c25_sameprop  <- median(data.sameprop$RPAREAc25)
med_c25_sameprop

##3. calculate MEDIAN of Relative Peak area of c27alkane##

med_c27_sameprop  <- median(data.sameprop$RPAREAc27)
med_c27_sameprop

##4. calculate MEDIAN of Relative Peak area of c29alkene##

med_c29alkene_sameprop  <- median(data.sameprop$RPAREAc29alkene)
med_c29alkene_sameprop


##5. calculate MEDIAN of Relative Peak area of c29alkane##

med_c29_sameprop  <- median(data.sameprop$RPAREAc29)
med_c29_sameprop


##6. calculate MEDIAN of Relative Peak area of mec29alkane##

med_mec29_sameprop <- median(data.sameprop$RPAREAmec29)
med_mec29_sameprop

##7. calculate MEDIAN of Relative Peak area of c31alkene##


med_c31alkene_sameprop  <- median(data.sameprop$RPAREAc31alkene)
med_c31alkene_sameprop


##8. calculate MEDIAN of Relative Peak area of mec31##

med_mec31_sameprop  <- median(data.sameprop$RPAREAmec31)
med_mec31_sameprop


##9. calculate MEDIAN of Relative Peak area of c33alkene##

med_c33alkene_sameprop  <- median(data.sameprop$RPAREAc33alkene)
med_c33alkene_sameprop


##10. calculate MEDIAN of Relative Peak area of mec33##

med_mec33_sameprop  <- median(data.sameprop$RPAREAmec33)
med_mec33_sameprop

##11. calculate MEDIAN of Relative Peak area of dimec33##

med_dimec33_sameprop  <- median(data.sameprop$RPAREAdimec33)
med_dimec33_sameprop

##12. calculate MEDIAN of Relative Peak area of c35alkene##


med_c35alkene_sameprop  <- median(data.sameprop$RPAREAc35alkene)
med_c35alkene_sameprop

##13. calculate MEDIAN of Relative Peak area of mec35##

med_mec35_sameprop  <- median(data.sameprop$RPAREAmec35)
med_mec35_sameprop

##14. calculate MEDIAN of Relative Peak area of dimec35##

med_dimec35_sameprop  <- median(data.sameprop$RPAREAdimec35)
med_dimec35_sameprop


##15. calculate MEDIAN of Relative Peak area of 1323Dimec37##

med_1323dimec37_sameprop  <- median(data.sameprop$RPAREA1323dimec37)
med_1323dimec37_sameprop

### calculations for medians DETAILS####
##want to see positive slope as
#I predict that sameprop will have more larger compounds than incrprop ones

##1. 

index_c23 <- med_c23_sameprop / ( med_c23_incrprop + med_c23_sameprop)
index_c23
##answer is NaN since its 0/0+0


##2. 

index_c25 <- med_c25_sameprop / (med_c25_incrprop + med_c25_sameprop)
index_c25

##3. 

index_c27 <- med_c27_sameprop / ( med_c27_incrprop + med_c27_sameprop)
index_c27

##4.

index_c29alkene <- med_c29alkene_sameprop / ( med_c29alkene_incrprop + med_c29alkene_sameprop)
index_c29alkene


##5.


index_c29 <- med_c29_sameprop / ( med_c29_incrprop + med_c29_sameprop)
index_c29


##6. 

index_mec29 <- med_mec29_sameprop / ( med_mec29_incrprop + med_mec29_sameprop)
index_mec29


##7.

index_c31alkene <- med_c31alkene_sameprop / ( med_c31alkene_incrprop + med_c31alkene_sameprop)
index_c31alkene



##8.

index_mec31 <- med_mec31_sameprop / ( med_mec31_incrprop + med_mec31_sameprop)
index_mec31



##9.


index_c33alkene <- med_c33alkene_sameprop / ( med_c33alkene_incrprop + med_c33alkene_sameprop)
index_c33alkene


##10.

index_mec33 <- med_mec33_sameprop / ( med_mec33_incrprop + med_mec33_sameprop)
index_mec33



##11. 


index_dimec33 <- med_dimec33_sameprop / ( med_dimec33_incrprop + med_dimec33_sameprop)
index_dimec33


##12. 

index_c35alkene <- med_c35alkene_sameprop / ( med_c35alkene_incrprop + med_c35alkene_sameprop)
index_c35alkene


##13.

index_mec35 <- med_mec35_sameprop / ( med_mec35_incrprop + med_mec35_sameprop)
index_mec35


##14.

index_dimec35 <- med_dimec35_sameprop / ( med_dimec35_incrprop + med_dimec35_sameprop)
index_dimec35

##15. 

index_1323dimec37 <- med_1323dimec37_sameprop / ( med_1323dimec37_incrprop + med_1323dimec37_sameprop)
index_1323dimec37


###make a table of index

index_data <- matrix(c(
  index_c25,
  index_c27,
  index_c29alkene,
  index_c29,
  index_mec29,
  index_c31alkene,
  index_mec31,
  index_c33alkene,
  index_mec33,
  index_dimec33,
  index_c35alkene,
  index_mec35,
  index_dimec35,
  index_1323dimec37),
  ncol=1,byrow=TRUE)
rownames(index_data) <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)


plot(index_data,
     ylim=c(0,0.7))

df_indexdata<- as.data.frame(index_data)
write.csv(df_indexdata, file="df_indexdata_18wk.csv", row.names=F)


#make linear model and check correlation
##entered data in excel, found median of RTs of compound classes

#import  DATA set 
##copied into spreadsheet and saved as .csv
data18<-read.csv("index_summary_2405_18wk.csv", header=TRUE)
data18
str(data18)


View(data18)


linearMod18 <- lm(index ~ Retention.Time, data=data18)
plot(data18$Retention.Time, data18$index)
print(linearMod18)
summary(linearMod18)
#R^2=0.8901
##y= 0.033762*X + -0.324825
####super strong relationship with 18-week data

#diagnostics
plot(linearMod)

####diagnostics of model of fixed effect of pg, with rand effect of label
Res <- resid(linearMod) 
Fit <- fitted(linearMod) 


###histogram of residuals to check for normality
qqnorm(Res)
hist(Res)
shapiro.test(Res)


#significant dependence of index on retention time

library(ggplot2)


p1<- ggplot(data18, aes(x=Retention.Time, y=index)) + 
  geom_point() + 
  xlab("Median Retention Time of Compound (Minutes since Start)") + 
  ylab("Index of Median Rel. Peak Area") +
  ylim(0.1,0.8) +
  # xlim(0,250) +
  geom_smooth(method=lm, se=FALSE)
p1


#check correlation

cor.test(data18$Retention.Time, data18$index) 
##variables seem to be correlated, as its different than 0 according to a test
###pearson's correlation, though
#output:
#Pearson's product-moment correlation

#data:  data$Retention.Time and data$Index.of.Medians
#t = 9.7162, df = 12, p-value = 4.884e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.8223062 0.9818228
#sample estimates:
#      cor 
#0.941925




### plotting CHC indexes both 12 and 18 weeks####




setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/CHCanalysis/FertilitySignal/Index_calculations")
getwd()
#clear all
rm(list=ls())

#import  DATA set 

databoth<-read.csv("index_summary_2405_both.csv", header=TRUE)
databoth
str(databoth)


#reverse order of 12 and 18 s/t it is on the left
#databoth$Interval<- factor(databoth$Interval, levels=rev(levels(databoth$Interval)))



library(ggplot2)
library(dplyr)
library(forcats)



pboth<-
  ggplot(databoth, aes(x=Retention.Time, y=index, shape=fct_rev(Interval))) + 
  geom_point() + 
  xlab("Median Retention Time of Compound (Minutes since Start)") + 
  ylab("Index of Median Rel. Peak Area") +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20),  axis.title.x = element_text(size = 20), legend.text= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  ylim(0,0.8) +
  # xlim(0,250) +
  geom_smooth(method=lm, se=FALSE, aes(color=fct_rev(Interval)))

pboth














#### Fig. 4A. Obs dueling and duels/24 hrs: Behaviors @ end for dueling for gams with 2+ present #####



setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Behavioralanalysis")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Behavior_2405.csv", header=TRUE)
str(data)

#View(data)

#fix variables
data$Colony<-as.factor(data$Colony)
data$Time<-as.factor(data$Time)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Time<- factor(data$Time, levels=rev(levels(data$Time)))



#create propgam

data$propgam<- data$NumGams/data$GroupSize
data$propgam

# using subset function for  final sampling
data_fin <- subset(data, Time=="Final")
str(data_fin)

##sample size of each individual

library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_fin, measurevar="propgam", groupvars=c("Treatment"))
sum_data
#IP gams=183
#SP gams=89
sum_data<-summarySE(data_fin, measurevar="propgam", groupvars=c("Treatment", "Status_Scanobs"))
sum_data
#IP, G=159
#IP, NG3=3
#IP, RG=12
#IP, unclear=3
#IP, W=6
#SP, G=43
#SP, NG1=7
#SP, NG2=4
#SP, RG=1
#SP, unclear=9
#SP, W=25


data_fin$Status_Scanobs
# using subset function for only those were there are  gamergates only
data_gam_only <- subset(data_fin, Status_Scanobs=="G")
str(data_gam_only)
#n=202 rows (i.e. gamergates)
#View(data_multgam)



# using subset function for only those were there are multiple gamergates
data_multgam <- subset(data_gam_only, multigam2==1)
str(data_multgam)
#reduced down to 191 gamergates
#View(data_multgam)







library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_multgam, measurevar="propgam", groupvars=c("Treatment"))
sum_data
#IP gams=159
#SP gams=32

#removed SP ants that were in colonies with SP colonies with solo gams


sum_data<-summarySE(data_multgam, measurevar="propgam", groupvars=c("Treatment", "Colony"))
sum_data
#IP colonies=24
39-24
#SP colonies=15

sum_data<-summarySE(data_multgam, measurevar="propgam", groupvars=c( "Treatment", "Status_Scanobs", "Colony"))
sum_data
#IP, G=159
#SP, n=32






library(ggplot2)
duel <- ggplot(data_multgam, aes(y=Duel, x=factor(Status_Scanobs))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,18, by=3)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Obs Dueling") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

duel


numduel <- ggplot(data_multgam, aes(y=NumDuel, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,250, by=50)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="# duels by a dominant after 12 weeks") +
  expand_limits(y=c(0,250))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

numduel

###there are two colonies, SAFC10 and F30 that had 3 gams, while rest had 2


wilcox.test(data_multgam$Duel~data_multgam$Treatment)
#W = 3117, p-value = 0.03004





####Fig.4B, 4C Behaviors-Paired DIFFERENCES of Initial and Final gamergates--DUELING only#####



setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Behavioralanalysis")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("gam_diff_2405.csv", header=TRUE)
str(data)



#fix variables
data$Colony<-as.factor(data$Colony)
data$Time<-as.factor(data$Time)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Time<- factor(data$Time, levels=rev(levels(data$Time)))



#create propgam

data$propgam<- data$NumGams/data$GroupSize
data$propgam

# using subset function for only those rows with differences between initial and final
data <- subset(data, diffcalc > 0)
str(data)

# using subset function for  those where there are multiple gamergates
data_multigam <- subset(data, multigam2 == 1)
str(data_multigam)
#228 ants

# using subset function for  ONLY gamergates
data_multigam_G <- subset(data_multigam, Status_Scanobs== "G")
str(data_multigam_G)
#206 gamergates

# omit those which changed reproductive status
data_multigam_G2 <- subset(data_multigam_G, omit== 0)
str(data_multigam_G2)
#191 gamergates




sum_data<-summarySE(data_multigam_G2, measurevar="NumDuel", groupvars=c("Treatment"))
sum_data
#159 IP gamergates
#32 SP gamergates
sum_data<-summarySE(data_multigam_G2, measurevar="NumDuel", groupvars=c("Treatment", "Colony"))
sum_data
#159 IP gamergates from 24 IP colonies
#32 SP gamergates from 39-24=15 SP colonies


library(ggplot2)
numduel_diff <- ggplot(data_multigam_G2, aes(y=difference_numduel, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend=F) +
  #geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~status3, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,250, by=50)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # duels by a dominant (Final-Initial)") +
  expand_limits(y=c(-200,200))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

numduel_diff


wilcox.test(data_multigam_G2$difference_numduel~data_multigam_G2$Treatment)
#W = 3587, p-value = 0.0008906

data_multigam_G2$diff_duelpergam <- as.numeric(data_multigam_G2$diff_duelpergam)

library(ggplot2)
duelpergam <- ggplot(data_multigam_G2, aes(y=diff_duelpergam, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~status3, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,250, by=50)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20)  ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # duels by a dominant / # dominants in col") #+
#expand_limits(y=c(-10,60))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

duelpergam


wilcox.test(data_multigam_G2$diff_duelpergam~data_multigam_G2$Treatment)
#W = 3488, p-value = 0.0009196


data_multigam_G2$diff_duelperpropgam <- as.numeric(data_multigam_G2$diff_duelperpropgam)

library(ggplot2)
duelperpropgam <- ggplot(data_multigam_G2, aes(y=diff_duelperpropgam, x=factor(Treatment))) + 
  geom_boxplot() +
  #geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~status3, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,250, by=50)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20)  ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # duels by a dominant / # dominants in col / # colony size") #+
#expand_limits(y=c(-10,60))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

duelperpropgam


wilcox.test(data_multigam_G2$diff_duelperpropgam~data_multigam_G2$Treatment)
#W = 3479.5, p-value = 0.0009229








#Fig. 4

require(gridExtra)
grid.arrange(
  numduel,
  numduel_diff,
  duelpergam,
  ncol=3)












##### Fig. 5.  SSR data####


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/SSR/")
getwd()
#clear all
rm(list=ls())


#import  DATA
data<-read.csv("SSR_2405.csv", header=TRUE)
View(data)
str(data)
#147 sensilla sampled

#subset only those that were analyzed for SSR, excluding several

ssrdata <- subset(data, SSR_ANALYZED == 1)
str(ssrdata)
##140 rows



#fix variables
ssrdata$status<-as.factor(ssrdata$status)
ssrdata$DISSECTOR<-as.factor(ssrdata$DISSECTOR)
ssrdata$Interval<-as.factor(ssrdata$Interval)
ssrdata$TREATMENT<-as.factor(ssrdata$TREATMENT)
ssrdata$GAMMARK<-as.factor(ssrdata$GAMMARK)
ssrdata$COLONY<-as.factor(ssrdata$COLONY)


#rearrange order of levels of sampling
ssrdata$Interval<- factor(ssrdata$Interval, levels=rev(levels(ssrdata$Interval)))


#create propgam

ssrdata$propgam<- ssrdata$GAMSAMPLE/ssrdata$COLSIZESAMPLE
ssrdata$propgam


#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(ssrdata, measurevar="propgam", groupvars=c("TREATMENT", "status"))
sum_data
##n= 85 IP gam sensilla
#n= 51 SP gam sensilla
#n=2 RG ant sensilla
#n= 2 IP unclear ant sensilla

str(ssrdata)

#convert to a number from a character
ssrdata$fourME3hept <- as.numeric(ssrdata$X4.Methyl.3.heptanol)
ssrdata$fourME3hept
class(ssrdata$fourME3hept)

#converting
ssrdata$ga <- as.numeric(ssrdata$geranyl.acetate)
ssrdata$ga
class(ssrdata$ga)



#converting
ssrdata$h1al <- as.numeric(ssrdata$X2.hexen.1.al)
ssrdata$h1al
class(ssrdata$h1al)


#converting
ssrdata$sixME5hept <- as.numeric(ssrdata$X6.methyl.5.hepten.2.one)
ssrdata$sixME5hept
class(ssrdata$sixME5hept)


#converting
ssrdata$C11a <- as.numeric(ssrdata$C11)
ssrdata$C11a
class(ssrdata$C11a)

#converting
ssrdata$C23a <- as.numeric(ssrdata$C23_v2)
ssrdata$C23a
class(ssrdata$C23a)

#converting
ssrdata$C26a <- as.numeric(ssrdata$C26_v2)
ssrdata$C26a
class(ssrdata$C26a)

#converting
ssrdata$C29a <- as.numeric(ssrdata$C29_v2)
ssrdata$C29a
class(ssrdata$C29a)


#converting
ssrdata$C33a <- as.numeric(ssrdata$C33_v2)
ssrdata$C33a
class(ssrdata$C33a)


#converting
ssrdata$pentane_v2 <- as.numeric(ssrdata$pentane_v2)
class(ssrdata$pentane_v2)


#converting
ssrdata$PARAFFINSOLVENT <- as.numeric(ssrdata$PARAFFINSOLVENT)
class(ssrdata$PARAFFINSOLVENT)



str(ssrdata)

#par(mfrow=c(1,3))


###normalize data relative to Parrafin solvents

ssrdata$normFOUR<- ssrdata$fourME3hept - ssrdata$PARAFFINSOLVENT

ssrdata$normGER<- ssrdata$ga - ssrdata$PARAFFINSOLVENT

ssrdata$normtwo<- ssrdata$h1al - ssrdata$PARAFFINSOLVENT

ssrdata$normsix<- ssrdata$sixME5hept - ssrdata$PARAFFINSOLVENT

ssrdata$normC11<- ssrdata$C11a - ssrdata$PARAFFINSOLVENT


#str(data)

###normalize data relative to pentane solvents

ssrdata$normC11_pent<- ssrdata$C11_v3 - ssrdata$pentane_v2

ssrdata$normC23<- ssrdata$C23a - ssrdata$pentane_v2

ssrdata$normC26<- ssrdata$C26a - ssrdata$pentane_v2

ssrdata$normC29<- ssrdata$C29a - ssrdata$pentane_v2

ssrdata$normC33<- ssrdata$C33a - ssrdata$pentane_v2

str(ssrdata)



##### SSR combining C11 from paraffin and pentane solvent corrections into a new column####

str(ssrdata$normC11)
str(ssrdata$normC11_pent)
#numeric columns

library(dplyr)
ssrdata$normC11_all<- coalesce(ssrdata$normC11, ssrdata$normC11_pent)
str(ssrdata$normC11_all)
#also a numeric


##### SSR in response to yolky oocyte and headwidth####



library(ggplot2)

four_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normFOUR)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR: 4-Methyl-3-heptanol") +
  ylim(-200,300) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
four_oocyte

four_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normFOUR)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: 4-Methyl-3-heptanol") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
four_hw


ger_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normGER)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for Geranyl Acetate--normalized") +
  ylim(-100,300) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
ger_oocyte

ger_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normGER)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for Geranyl Acetate--normalized") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
ger_hw



two_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normtwo)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for 2-hexen-1-al--normalized") +
  ylim(-200,300) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
two_oocyte

two_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normtwo)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for 2-hexen-1-al--normalized") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
two_hw


six_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normsix)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for 6-methyl-5-hepten-2-one--normalized") +
  ylim(-100,300) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
six_oocyte

six_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normsix)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for 6-methyl-5-hepten-2-one--normalized") +
  ylim(-100,300) +
  xlim(2.4,3.2) +
    geom_smooth(method=lm, se=FALSE)
six_hw


c11_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normC11_all)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for C11--normalized") +
  ylim(-200,300) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
c11_oocyte

c11_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normC11_all)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for C11--normalized") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c11_hw




c23_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normC23)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for C23--normalized") +
  ylim(-100,200) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
c23_oocyte

c23_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normC23)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for C23--normalized") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c23_hw

c26_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normC26)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for C26--normalized") +
  ylim(-200,200) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
c26_oocyte

c26_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normC26)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for C26--normalized") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c26_hw



c29_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normC29)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for C29--normalized") +
  ylim(-200,200) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
c29_oocyte

c29_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normC29)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for C29--normalized") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c29_hw



c33_oocyte<-  ggplot(ssrdata, aes(x=OOCYTE_PHOTO_BP, y=normC33)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("# Yolky Oocytes") + 
  ylab("SSR for C33--normalized") +
  ylim(-300,200) +
  xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
c33_oocyte

c33_hw<-  ggplot(ssrdata, aes(x=HEADWIDTH, y=normC33)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Headwidth(mm)") + 
  ylab("SSR for C33--normalized") +
  ylim(-300,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c33_hw


#plot all results
require(gridExtra)
grid.arrange(four_oocyte, ger_oocyte, two_oocyte, six_oocyte, c11_oocyte, 
             c23_oocyte, c26_oocyte, c29_oocyte, c33_oocyte, ncol=3)


grid.arrange(four_hw, ger_hw, two_hw, six_hw, c11_hw, 
             c23_hw, c26_hw, c29_hw, c33_hw, ncol=3)



#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(ssrdata, measurevar="propgam", groupvars=c("status", "GAMMARK"))
sum_data
##n=  50 marks, some of which include multiple gams from diff colonies
#n= 1 unclear ant
#n=1 RG ant
sum_data<-summarySE(ssrdata, measurevar="propgam", groupvars=c("status"))
sum_data
##n=  136 sensilla from  gams
#n= 2 sensilla from 2 unclear ants
#n=2 sensilla from a RG

##### SSR gamergate data only####


###select for only those that are gamergates
ssrdatagam <- subset(ssrdata, status == "G")
str(ssrdatagam)
##136 rows for 136 sensilla

sum_data<-summarySE(ssrdatagam, measurevar="propgam", groupvars=c("TREATMENT"))
sum_data
#IP sensilla= 85 sensilla
#SP sensilla= 51 sensilla

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(ssrdatagam, measurevar="propgam", groupvars=c("TREATMENT", "status", "COLONY", "GAMMARK"))
sum_data
#n=71 gams total
##n=  43 gams from IP treatment
#n= (71-43)=28 gams from SP treatment

sum_data<-summarySE(ssrdatagam, measurevar="propgam", groupvars=c("TREATMENT", "COLONY"))
sum_data
##n= 85 IP gam sensilla from 43 gams, from 17 colonies
#n= 51 SP gam sensilla, from (71-43)=28 gams, from (36-17)=19 colonies
36-17

sum_data<-summarySE(ssrdatagam, measurevar="propgam", groupvars=c("TREATMENT", "Interval"))
sum_data
#INCRPROP 12: 51 sensilla
#INCRPROP 18: 34 sensilla
#SAMEPROP 12: 28 sensilla
#SAMEPROP 18: 23 sensilla

#########SSR of all gamergates in relation to headwidth ###############



library(ggplot2)

four_hw_gam <-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normFOUR)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: 4-Methyl-3-heptanol") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
four_hw_gam



ger_hw_gam <-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normGER)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: Geranyl Acetate") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
ger_hw_gam




two_hw_gam <-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normtwo)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: 2-hexen-1-al") +
  ylim(-200,400) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
two_hw_gam



six_hw_gam <-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normsix)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Head width(mm)") + 
  ylab("SSR: 6-methyl-5-hepten-2-one") +
  ylim(-100,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
six_hw_gam


c11_hw_gam<-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normC11_all)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: C11") +
  ylim(-200,300) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c11_hw_gam



c23_hw_gam <-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normC23)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: C23") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c23_hw_gam



c26_hw_gam<-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normC26)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: C26") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c26_hw_gam



c29_hw_gam<-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normC29)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("") + 
  ylab("SSR: C29") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c29_hw_gam


c33_hw_gam<-  ggplot(ssrdatagam, aes(x=HEADWIDTH, y=normC33)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) + 
  xlab("Head width (mm)") + 
  ylab("SSR: C33") +
  ylim(-200,200) +
  xlim(2.4,3.2) +
  geom_smooth(method=lm, se=FALSE)
c33_hw_gam


#plot all results
require(gridExtra)
grid.arrange(four_hw_gam, ger_hw_gam, two_hw_gam, six_hw_gam, c11_hw_gam, 
             c23_hw_gam, c26_hw_gam, c29_hw_gam, c33_hw_gam, ncol=2)

grid.arrange(
             c11_hw_gam, four_hw_gam,
             c23_hw_gam, ger_hw_gam,
             c26_hw_gam, two_hw_gam,
             c29_hw_gam, six_hw_gam,
             c33_hw_gam, ncol=2)





###regressions of gamergate headwidth to SSR responses####

###select for only those that have headwidth data
ssrdatagam_hw <- subset(ssrdatagam, HEADWIDTH > 0)
str(ssrdatagam)
str(ssrdatagam_hw)
##134 rows instead of 136 in larger dataset
#therefore excluded 2 sensilla from the IP gamergate YOY in colony SAFC17A (this colony has data for other sensilla in other gams)
##it is sampled at eighteen weeks




#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(ssrdatagam_hw, measurevar="propgam", groupvars=c("TREATMENT", "Interval"))
sum_data
###
#IP sensilla-12 weeks: n=51
#SP sensilla-12 weeks: n=28
#IP sensilla-18 weeks: n=32
#SP sensilla-18 weeks: n=23


sum_data<-summarySE(ssrdatagam_hw, measurevar="propgam", groupvars=c("TREATMENT", "COLONY"))
sum_data
#n=       17 IP colonies
#n=36-17= 19 SP colonies for sensilla
36-17


sum_data<-summarySE(ssrdatagam_hw, measurevar="propgam", groupvars=c("TREATMENT", "GAMMARK"))
sum_data
#n=35 IP gamergates
#n=61-35= 26 SP gamergates
61-35

sum_data<-summarySE(ssrdatagam_hw, measurevar="propgam", groupvars=c("TREATMENT"))
sum_data
#n=83 IP sensilla
#n=51 SP sensilla


lnormfour <- lm(HEADWIDTH ~ normFOUR, data=ssrdatagam_hw)
lnormfour
summary(lnormfour)
#y= 2.363e-05 *X + 2.762
#Adjusted R-squared:  -0.007442
#t=0.132 
#p-value=0.895


lger <- lm(HEADWIDTH ~ normGER, data=ssrdatagam_hw)
lger
summary(lger)
#y= 0.0001837 *X + 2.7560993
#Adjusted R-squared: -0.00201 
#t=0.856 
#p-value=0.393


ltwo <- lm(HEADWIDTH ~ normtwo, data=ssrdatagam_hw)
ltwo
summary(ltwo)
#y= 2.396e-05  *X + 2.762
#Adjusted R-squared:  -0.007437 
#t=0.135 
#p-value=0.893



lnormsix <- lm(HEADWIDTH ~ normsix, data=ssrdatagam_hw)
lnormsix
summary(lnormsix)
#y= -8.861e-06 *X + 2.765
#Adjusted R-squared:  -0.007558 
#t=-0.049 
#p-value=0.961




lc11 <- lm(HEADWIDTH ~ normC11_all, data=ssrdatagam_hw)
lc11
summary(lc11)
#y= 0.0003558 *X + 2.7587159
#Adjusted R-squared:  0.00803 
#t=1.587 
#p-value=0.115


lc23 <- lm(HEADWIDTH ~ normC23, data=ssrdatagam_hw)
lc23
summary(lc23)
#y= -8.099e-05 *X +  2.767
#Adjusted R-squared: -0.008224 
#t= -0.364 
#p-value=0.716


lc26 <- lm(HEADWIDTH ~ normC26, data=ssrdatagam_hw)
lc26
summary(lc26)
#y=  3.234e-05  *X + 2.764
#Adjusted R-squared:  -0.007397  
#t= 0.153  
#p-value=0.879




lc29 <- lm(HEADWIDTH ~ normC29, data=ssrdatagam_hw)
lc29
summary(lc29)
#y= 0.0001305 *X + 2.7618192
#Adjusted R-squared:  -0.005634
#t=0.505 
#p-value=0.615



lc33 <- lm(HEADWIDTH ~ normC33, data=ssrdatagam_hw)
lc33
summary(lc33)
#y= 0.0002105 *X + 2.7622477
#Adjusted R-squared: -0.0004291
#t=0.971 
#p-value=0.333














#########SSR of all gamergates, analysis by interval and treatment###############



library(ggplot2)

pnorm4 <- ggplot(ssrdatagam, aes(y=normFOUR, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: 4-Methyl-3-heptanol") +
  expand_limits(y=c(-200,300))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnorm4




library(lme4)

m1 <- lmer(log(200+normFOUR) ~ TREATMENT + Interval + TREATMENT*Interval + (1|GAMMARK), data = ssrdatagam)
summary(m1 )


m2 <- lmer(log(200+normFOUR) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2)


m3 <- lmer(log(200+normFOUR) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3)



#random
random <- lmer(log(200+normFOUR) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )

plot(m1)



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#no effect of treatment and interval, p=0.0787


library(car)
#fixed effect
Anova(m1)
#TREATMENT          5.9026  1    0.01512 *
#Interval           0.2513  1    0.61617  
#TREATMENT:Interval 0.7611  1    0.38298  

library(emmeans)
library(multcomp)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)

KRmodcomp(m2, random)
anova(m2, random)
#no effect of interval, p=0.7516


KRmodcomp(m3, random)
anova(m3, random)
# effect of treatment, p=0.01584


#p=0.01164
###so, there's an effect of treatment!


library(ggplot2)

pnormGER <- ggplot(ssrdatagam, aes(y=normGER, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: Geranyl Acetate") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormGER

library(lme4)


m1 <- lmer(log(300+normGER) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(300+normGER) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(300+normGER) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(300+normGER) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p=2.2e-16
###so there's a strong effect of treatment and interval here 


library(car)
Anova(m1)
#TREATMENT          4.7519  1    0.02927 *
#Interval           5.8300  1    0.01576 *
#  TREATMENT:Interval 0.0280  1    0.86700  




library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP Eighteen, group 12
#INCRPROP Twelve, group 2
#SAMEPROP  Eighteen , group 1
#SAMEPROP  Twelve , group 12





KRmodcomp(m2, random)
anova(m2, random)
##there's an effect of interval 
#p=0.01596
KRmodcomp(m3, random)
anova(m3, random)
##there's also an effect of treatment 
#p=0.02237





library(ggplot2)

pnormTWO <- ggplot(ssrdatagam, aes(y=normtwo, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: 2-hexen-1-al") +
  expand_limits(y=c(-200,300))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormTWO




m1 <- lmer(log(400+normtwo) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(400+normtwo) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(400+normtwo) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(400+normtwo) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p=0.05356
###so there's a small effect of treatment and interval here 

library(car)
Anova(m1)
#TREATMENT          6.1009  1    0.01351 *
#Interval           1.8347  1    0.17558  
#TREATMENT:Interval 0.1096  1    0.74058  

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP Eighteen, group 2
#INCRPROP Twelve, group 12
#SAMEPROP  Eighteen , group 12
#SAMEPROP  Twelve , group 1



KRmodcomp(m2, random)
anova(m2, random)
##there's NO effect of interval
#p=0.2174
KRmodcomp(m3, random)
anova(m3, random)
##there's an effect of treatment only
#p=0.0186



library(ggplot2)

pnormSIX <- ggplot(ssrdatagam, aes(y=normsix, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="SSR: 6-methyl-5-hepten-2-one") +
  expand_limits(y=c(-100,300))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormSIX


library(lme4)

m1 <- lmer(log(200+normsix) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(200+normsix) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(200+normsix) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(200+normsix) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p=0.001801
###so there's an effect of treatment and interval here 

library(car)
Anova(m1)
#TREATMENT          13.1530  1  0.0002871 ***
#Interval            3.8916  1  0.0485268 *  
#TREATMENT:Interval  0.1565  1  0.6923680    




library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP Eighteen, group 2
#INCRPROP Twelve, group 2
#SAMEPROP  Eighteen , group 12
#SAMEPROP  Twelve , group 1





KRmodcomp(m2, random)
anova(m2, random)
##there's NO effect of interval only
#p=0.09299
KRmodcomp(m3, random)
anova(m3, random)
##there's an effect of treatment only
#p=0.0007405



#hydrocarbons

pnormC11 <- ggplot(ssrdatagam, aes(y=normC11_all, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: C11") +
  expand_limits(y=c(-200,300))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormC11



#median of each group for subset of 12 week old
twelveweek<-subset(ssrdatagam, Interval=="Twelve")

##RV: ssrdatagam$normC11_all
##IV: TREATMENT
medians <- tapply(twelveweek$normC11_all, twelveweek$TREATMENT, median)
medians
#  IP   SP 
##40.0 27.5
##percent reduction= 13.5/40
13.5/40


#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(ssrdatagam, measurevar="normC11_all", groupvars=c("TREATMENT", "Interval"))
sum_data



library(lme4)

m1 <- lmer(log(200+normC11) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(200+normC11) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(200+normC11) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(200+normC11) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p=2.555e-05
###so there's an effect of treatment and interval here 


library(car)
Anova(m1)

#TREATMENT           6.1497  1    0.01314 *  
#Interval           17.3671  1  3.081e-05 ***
#TREATMENT:Interval  2.0314  1    0.15408  



library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP 12, group 2
#INCRPROP 18, group 1
#SAMEPROP  12 , group 1
#SAMEPROP  18 , group 1




KRmodcomp(m2, random)
anova(m2, random)
##there an effect of interval only
#p=7.133e-05
KRmodcomp(m3, random)
anova(m3, random)
##there's an effect of treatment only
#p=0.01343







pnormC23 <- ggplot(ssrdatagam, aes(y=normC23, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: C23") +
  expand_limits(y=c(-200,200))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormC23

m1 <- lmer(log(200+normC23) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(200+normC23) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(200+normC23) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(200+normC23) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p=3.756e-09
###so there's an effect of treatment and interval here 

library(car)
Anova(m1)

#TREATMENT           0.0540  1     0.8162    
#Interval           47.7368  1  4.874e-12 ***
#TREATMENT:Interval  0.2697  1     0.6035   



library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP 12, group 1
#INCRPROP 18, group 2
#SAMEPROP  12 , group 1
#SAMEPROP  18 , group 2




KRmodcomp(m2, random)
anova(m2, random)
##there an effect of interval only
#p=1.01e-10
KRmodcomp(m3, random)
anova(m3, random)
##there's no effect of  effect of treatment only
#p=0.6901




pnormC26 <- ggplot(ssrdatagam, aes(y=normC26, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: C26") +
  expand_limits(y=c(-200,200))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormC26

m1 <- lmer(log(200+normC26) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(200+normC26) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(200+normC26) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(200+normC26) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p= 1.836e-06 
###so there's an effect of treatment and interval here 

library(car)
Anova(m1)
#TREATMENT           0.2838  1     0.5943    
#Interval           32.3545  1  1.285e-08 ***
#TREATMENT:Interval  1.1387  1     0.2859  



library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP 12, group 1
#INCRPROP 18, group 2
#SAMEPROP  12 , group 1
#SAMEPROP  18 , group 2




KRmodcomp(m2, random)
anova(m2, random)
##there an effect of interval only
#p=1.262e-07 
KRmodcomp(m3, random)
anova(m3, random)
##there's no effect of  effect of treatment only
#p=0.9331



pnormC29 <- ggplot(ssrdatagam, aes(y=normC29, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="SSR: C29") +
  expand_limits(y=c(-200,200))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormC29

m1 <- lmer(log(200+normC29) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(200+normC29) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(200+normC29) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(200+normC29) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p= 4.619e-08
###so there's an effect of treatment and interval here 


library(car)
Anova(m1)
#TREATMENT           2.4907  1     0.1145    
#Interval           39.7678  1   2.86e-10 ***
#TREATMENT:Interval  1.2522  1     0.2631   


library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP 12, group 1
#INCRPROP 18, group 2
#SAMEPROP  12 , group 1
#SAMEPROP  18 , group 2




KRmodcomp(m2, random)
anova(m2, random)
##there no effect of interval only
#p=8.579e-09
KRmodcomp(m3, random)
anova(m3, random)
##there's no effect of  effect of treatment only
#p=0.2296






pnormC33 <- ggplot(ssrdatagam, aes(y=normC33, x=TREATMENT)) + 
  geom_boxplot(aes(fill=Interval), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="SSR: C33") +
  expand_limits(y=c(-200,200))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pnormC33

m1 <- lmer(log(300+normC33) ~ TREATMENT + Interval + Interval*TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m1 )

plot(m1)

m2 <- lmer(log(300+normC33) ~ Interval + (1|GAMMARK), data = ssrdatagam)
summary(m2 )


m3 <- lmer(log(300+normC33) ~ TREATMENT + (1|GAMMARK), data = ssrdatagam)
summary(m3 )

plot(m1)

#random
random <- lmer(log(300+normC33) ~ 1 + (1|GAMMARK), data = ssrdatagam)
summary(random )



library(pbkrtest)
KRmodcomp(m1, random)
anova(m1, random)
#p= 3.012e-09 
###so there's an effect of treatment and interval here 

library(car)
Anova(m1)
#TREATMENT           0.7517  1    0.38593    
#Interval           52.4776  1  4.352e-13 ***
#TREATMENT:Interval  4.5028  1    0.03384 *  



library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)
#INCRPROP 12, group 1
#INCRPROP 18, group 2
#SAMEPROP  12 , group 1
#SAMEPROP  18 , group 2




KRmodcomp(m2, random)
anova(m2, random)
##there no effect of interval only
#p=4.581e-05
KRmodcomp(m3, random)
anova(m3, random)
##there's no effect of  effect of treatment only
#p=0.9912

#####SSR of all gamergates --plots, effect of treatment and interval ####

#plot all results
require(gridExtra)
grid.arrange(pnorm4, pnormGER, pnormTWO, pnormSIX, pnormC11, 
             pnormC23, pnormC26, pnormC29, pnormC33, ncol=3)




#####SSR of all gamergates-analysis and plotting of treatment only####

library(ggplot2)

p1<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normFOUR)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="", y="4-Methyl-3-Heptanol")

p2<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normGER)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="", y="Geranyl Acetate")



p3<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normtwo)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="", y="2-Hexen-1-al")


p4<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normsix)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="", y="6-Methyl-5-Hepten-2-one")


p5<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normC11)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="", y="C11")


p6<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normC23)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="", y="C23")


p7<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normC26)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="C26")


p8<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normC29)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="C29")


p9<- ggplot(ssrdatagam, aes(x=TREATMENT, y=normC33)) + 
  geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 15),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="C33")


require(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3)

p1

require(ggpubr)
ggarrange(pnormC11, pnorm4, 
          pnormC23, pnormGER,
          pnormC26, pnormTWO,
          pnormC29, pnormSIX,
          pnormC33,
          ncol=2, 
          nrow=5,
          common.legend=T,
          legend="bottom")




######Fig. 6. Brains Analysis ############


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Brains")
getwd()
#clear all
rm(list=ls())

#import  DATA set 


######import  DATA set 


data<-read.csv("Brains_2405.csv", header=TRUE)
data
str(data)

library(Rmisc)
library(ggplot2)
library(gg.gap)
library(ggsignif)
library(ggpubr)


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="HEADWIDTH", groupvars=c("TREATMENT", "status"))
sum_data

sum_data<-summarySE(data, measurevar="HEADWIDTH", groupvars=c("TREATMENT", "Interval", "status"))
sum_data
#n=18-week, IP





######Brain size relationships to body size############

library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="HEADWIDTH", groupvars=c("TREATMENT", "status"))
sum_data
#n=42 IP Gams, n=26 SP gams

data_hw <- subset(data, HEADWIDTH > 0)
str(data_hw)

#only keep the gams

data_hw <- subset(data_hw, status=="G")
data_hw

library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_hw, measurevar="HEADWIDTH", groupvars=c("TREATMENT", "status"))
sum_data
#n=40 IP gams with headwidth, n=24 SP gams with headwidth
#n=4 gamergates excluded b/c no headwidth (2 IP and 2 SP)


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_hw, measurevar="HEADWIDTH", groupvars=c("TREATMENT", "status"))
sum_data
#n=40 IP gams with headwidth, n=24 SP gams with headwidth
#n=4 gamergates excluded b/c no headwidth (2 IP and 2 SP)


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_hw, measurevar="HEADWIDTH", groupvars=c("TREATMENT", "COLONY"))
sum_data
#n=19 IP colonies
#n=37-19=18 SP colonies





##compare treatments for OL part of brain by hw
OL_hw <- ggplot(data_hw, aes(x=HEADWIDTH, y=OL_VOL)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) +
  #geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs (x="Head width (mm)", y="Optic Lobe Volume (um^3)") +
  expand_limits(x=c(2.4, 3.2), y=c(900000,20000000)) +
  geom_smooth(method=lm, se=FALSE)
#expand_limits(y=c(0,0.25))
OL_hw


linearMod <- lm(OL_VOL ~ HEADWIDTH, data=data_hw)
print(linearMod)
summary(linearMod)
#adjusted R^2=-0.01611 
#t=-0.038; p=0.97
##y= -132702 *X + 9288342
plot(linearMod)

##compare treatments for CB part of brain by oocyte #
CB_hw <- ggplot(data_hw, aes(x=HEADWIDTH, y=CB_VOL)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) +
  #geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs (x="Head width (mm)", y="Central Complex Volume (um^3)") +
  expand_limits(x=c(2.4, 3.2), y=c(10000000,40000000)) +
  geom_smooth(method=lm, se=FALSE)
#expand_limits(y=c(0,0.25))
CB_hw



linearMod <- lm(CB_VOL ~ HEADWIDTH, data=data_hw)
print(linearMod)
summary(linearMod)
#adjusted R^2=  -0.01357
#t= 0.396; p=0.693
##y= 1871453 *X + 15268225 


##compare treatments for CB part of brain by oocyte #
AL_hw<- ggplot(data_hw, aes(x=HEADWIDTH, y=AL_VOL)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) +
  #geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Head width (mm)", y="Antennal Lobe Volume (um^3)")+
  expand_limits(x=c(2.4, 3.2), y=c(2500000,12500000)) +
  geom_smooth(method=lm, se=FALSE)

#expand_limits(y=c(0,0.25))
AL_hw



linearMod <- lm(AL_VOL ~ HEADWIDTH, data=data_hw)
print(linearMod)
summary(linearMod)
#adjusted R^2= -0.01613 
##y= 10318 *X + 6680448 
#t=0.006; p=0.995

plot(linearMod)


##compare treatments for CB part of brain by oocyte #
totbrain_hw<- ggplot(data_hw, aes(x=HEADWIDTH, y=Total_brain)) + 
  geom_point(aes(colour=TREATMENT), show.legend = F) +
  #geom_boxplot(width=0.5, aes(fill=factor(TREATMENT))) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs (x="Head width (mm)", y="Total Brain Volume (um^3)") +
  expand_limits(x=c(2.4, 3.2), y=c(25000000,125000000)) +
  geom_smooth(method=lm, se=FALSE)
#expand_limits(y=c(0,0.25))
totbrain_hw



linearMod <- lm(Total_brain ~ HEADWIDTH, data=data_hw)
print(linearMod)
summary(linearMod)
#adjusted R^2= -0.01549 
#t=0.197, p=0.844
##y= 3498139 *X + 62474032 

plot(linearMod)





require(gridExtra)
grid.arrange(AL_hw, OL_hw, CB_hw, totbrain_hw,
             ncol=4)











###### Brain Volume--Treatment and Interval effects of gamergates only ############


datagambrain <- subset(data, status=="G")
str(datagambrain)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagambrain, measurevar="GAMSAMPLE", groupvars=c("TREATMENT", "Interval", "status"))
sum_data
#n=12 week, IP=28
#n=12 week, SP=18
#n=18 week, IP=14
#n=18 week, SP=8


#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagambrain, measurevar="GAMSAMPLE", groupvars=c("TREATMENT", "status"))
sum_data
##n= 42 IP gamergates
##n= 26 SP gamergates



#make interval a factor
datagambrain$Interval<- as.factor(datagambrain$Interval)


#reverse order of 12 and 18 s/t it is on the left
datagambrain$Interval<- factor(datagambrain$Interval, levels=rev(levels(datagambrain$Interval)))


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagambrain, measurevar="GAMSAMPLE", groupvars=c("TREATMENT", "status"))
sum_data
##n= 42 IP gamergates
##n= 26 SP gamergates




##compare treatments for AL part of brain
ALvol<- ggplot(datagambrain, aes(x=TREATMENT, y=AL_VOL)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = F) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Antennal Lobe Volume (um^3)")+
  expand_limits(y=c(2500000,12500000))
ALvol


library(lme4)
library(car)

m1 <- lm( AL_VOL~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)



##divide values for all volumes by 1000 to generate in mm3 rather than um3
datagambrain$alvol_mm <- datagambrain$AL_VOL/1000


##compare treatments for AL part of brain
ALvol_mm<- ggplot(datagambrain, aes(x=TREATMENT, y=alvol_mm)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = F) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Antennal Lobe Volume (mm^3)")+
  expand_limits(y=c(0,15000))
ALvol_mm


library(lme4)
library(car)

m1 <- lm( alvol_mm~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)







##compare treatments for AL part of brain
OLvol<- ggplot(datagambrain, aes(x=TREATMENT, y=OL_VOL)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = F) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Optic Lobe Volume (um^3)") +
  expand_limits(y=c(900000,20000000))
OLvol


library(lme4)
library(car)

m1 <- lm( OL_VOL~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)





##divide values for all volumes by 1000 to generate in mm3 rather than um3
datagambrain$olvol_mm <- datagambrain$OL_VOL/1000


##compare treatments for AL part of brain
OLvol_mm<- ggplot(datagambrain, aes(x=TREATMENT, y=olvol_mm)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = F) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Optic Lobe Volume (mm^3)")+
  expand_limits(y=c(0,20000))
OLvol_mm


library(lme4)
library(car)

m1 <- lm( olvol_mm~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)






##compare treatments for CB part of brain
CBvol<- ggplot(datagambrain, aes(x=TREATMENT, y=CB_VOL)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = F) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Central Complex Volume (um^3)") +
  expand_limits(y=c(10000000,40000000))
CBvol



library(lme4)
library(car)

m1 <- lm( CB_VOL~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)



##divide values for all volumes by 1000 to generate in mm3 rather than um3
datagambrain$cbvol_mm <- datagambrain$CB_VOL/1000


##compare treatments for AL part of brain
CBvol_mm<- ggplot(datagambrain, aes(x=TREATMENT, y=cbvol_mm)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = F) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Central Complex Volume (mm^3)")+
  expand_limits(y=c(0,40000))
CBvol_mm


library(lme4)
library(car)

m1 <- lm( cbvol_mm~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)





##compare treatments for CB part of brain
totvol<- ggplot(datagambrain, aes(x=TREATMENT, y=Total_brain)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = T) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Total Brain Volume (um^3)") +
  expand_limits(y=c(25000000,125000000))
totvol



library(lme4)
library(car)

m1 <- lm( Total_brain~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)




##divide values for all volumes by 1000 to generate in mm3 rather than um3
datagambrain$totvol_mm <- datagambrain$Total_brain/1000


##compare treatments for AL part of brain
totalvol_mm<- ggplot(datagambrain, aes(x=TREATMENT, y=totvol_mm)) + 
  geom_boxplot(width=0.5, aes(fill=Interval), show.legend = T) + 
  #geom_signif(comparisons=list(c("Alate", "Dealate")), annotations="", y_position = 0.2, tip_length = 0) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size=10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks=c(0,1,2,3)) +
  labs (x="Treatment", y="Total Brain Volume (mm^3)")+
  expand_limits(y=c(0,160000))
totalvol_mm


library(lme4)
library(car)

m1 <- lm( totvol_mm~ TREATMENT + Interval + TREATMENT*Interval, data = datagambrain)
summary(m1 )
Anova(m1)
#no significant differences

library(emmeans)
e1<-emmeans(m1, pairwise ~ TREATMENT*Interval)
e1
e1$contrasts
multcomp::cld(e1)








require(gridExtra)
grid.arrange(ALvol, OLvol, CBvol, totvol, ncol=4)


require(gridExtra)
grid.arrange(ALvol_mm, OLvol_mm, CBvol_mm, totalvol_mm, ncol=4)




#load package
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(datagambrain, measurevar="GAMSAMPLE", groupvars=c("TREATMENT", "Interval"))
sum_data
##n= 42 IP gamergates
##IP-18: n=14
##IP-12: n=28
##n= 26 SP gamergates
##IP-18: n=8
##IP-12: n=18
##n= 1 IP unclear ant
sum_data<-summarySE(data, measurevar="GAMSAMPLE", groupvars=c("TREATMENT", "Interval"))
sum_data
###same except there's
##n= 1 IP unclear ant @ 18 weeks









####Fig. S1, Colsize, #Gams and Propgam: All Experimental Cols####


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Col_sum_2405.csv", header=TRUE)

View(data)

#fix variables
data$Interval<-as.factor(data$Interval)
data$SourceCol<-as.factor(data$SourceCol)
data$Colony<-as.factor(data$Colony)
data$Time<-as.factor(data$Time)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Time<- factor(data$Time, levels=rev(levels(data$Time)))
str(data)

#create propgam

data$propgam<- data$NumGams/data$GroupSize
data$propgam


#create num nonrepros


data$nonrepros<- data$GroupSize - data$NumGams
data$nonrepros


#how many were reverted
# using subset function for  final sampling
data_fin <- subset(data, Time=="Final")
str(data_fin)

##
library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_fin, measurevar="propgam", groupvars="Treatment")
sum_data
#IP=24
#SP=27

sum_data<-summarySE(data_fin, measurevar="propgam", groupvars=c("Interval", "Treatment"))
sum_data
#IP-eighteen weeks: 10 cols
#SP-eighteen weeks: 13 cols
#IP-twelve weeks: 14 cols
#SP-twelve weeks: 14 cols



##calculate median and range of IP group's propgam
median(data_fin$propgam[data_fin$Treatment=="IP"], na.rm=T)
range(data_fin$propgam[data_fin$Treatment=="IP"], na.rm=T)


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="propgam", groupvars=c("Treatment","Time"), na.rm=TRUE)
sum_data
#n = 24 cols for IP-final
#n= 24 cols for IP-initial
#n= 27 cols for SP-initial
#n= 27 cols for IP-initial

# using subset function for  early sampling
data_init <- subset(data, Time=="Initial")
str(data_init)



#median group size initially 
median(data_init$GroupSize)
#median # gams initially
median(data_init$NumGams)
#median propgam
median(data_init$propgam)

##calculate median and range of all groups propgam
median(data_init$propgam, na.rm=T)
range(data_init$propgam, na.rm=T)



library(ggplot2)
pcolsize <- ggplot(data, aes(y=GroupSize, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Time), show.legend = F) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Colony size (# workers)") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,150))

pcolsize

#nonparametric comparison

#create new variable combination of time and treatment

library(tidyr)
data2 <- data %>% 
  unite(col = "kruskal_fact", c("Time", "Treatment"), sep = "-")
data2

data2$kruskal_fact

kruskal.test(data2$GroupSize~data2$kruskal_fact)
#Kruskal-Wallis chi-squared = 76.821, df = 3, p-value <2.2e-16

pairwise.wilcox.test(data2$GroupSize, g=data2$kruskal_fact, p.adjust.method = "bonferroni")
#             Final-IP Final-SP Initial-IP
#   Final-SP   0.5      -        -         
#  Initial-IP 1.8e-08  6.1e-09  -         
#  Initial-SP 6.0e-09  1.8e-09  1.0     

##therefore, nonparam differences are
#Final and Initial groups, but no differences between them




#distribution
hist(data$GroupSize)

#log-transform
data$log_gpsize<- log(1+data$GroupSize)
data$log_gpsize

hist(data$log_gpsize)

library(lme4)

#simply linear model!
full.m <-lm (log_gpsize ~ 
               Treatment +
               Time +
               Treatment*Time
             ,data)
#excluding subject as a factor since it's already include in Time
summary(full.m)

library(car)
Anova(full.m)
#effects of time and treatment, but not their interaction

#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Time)
e1
e1$contrasts
multcomp::cld(e1)


plot(full.m)



###group sizes don't differ between Treatments!

str(data)

pgam <- ggplot(data, aes(y=NumGams, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=factor(Time)), show.legend = F) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),axis.title.y = element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Dominants") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,20))

pgam




#nonparametric comparison

#create new variable combination of time and treatment

library(tidyr)
data2 <- data %>% 
  unite(col = "kruskal_fact", c("Time", "Treatment"), sep = "-")
data2

data2$kruskal_fact

kruskal.test(data2$NumGams~data2$kruskal_fact)
#Kruskal-Wallis chi-squared = 61.311, df = 3, p-value =3.084e-13

pairwise.wilcox.test(data2$NumGams, g=data2$kruskal_fact, p.adjust.method = "bonferroni")
#             Final-IP Final-SP Initial-IP
#   Final-SP   3.5e-09     -        -         
#  Initial-IP  0.51      3.5e-09  -         
#  Initial-SP   1.00      1.1e-09  1.0     

##therefore, nonparam differences are
#no differences among initial groups as well as final IP
#Final SP is the only one different








#distribution
hist(data$NumGams)



data$log_NumGams<- log(1+data$NumGams)
data$log_NumGams


hist(data$log_NumGams)


#simply linear model!
full.m <-lm (log_NumGams ~ 
               Treatment +
               Time +
               Treatment*Time
             ,data)
#excluding subject as a factor since it's already include in Time
summary(full.m)

library(car)
Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Time)
e1$contrasts
multcomp::cld(e1)
###no diff in treatment in # gams





##comparison of proportion of gamergates




ppropgam <- ggplot(data, aes(y=propgam, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=factor(Time)), show.legend = T) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size=20), axis.title.y = element_text(size=20)) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Dominants / Colony size") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,1))

ppropgam



#nonparametric comparison

#create new variable combination of time and treatment

library(tidyr)
data2 <- data %>% 
  unite(col = "kruskal_fact", c("Time", "Treatment"), sep = "-")
data2

data2$kruskal_fact

kruskal.test(data2$propgam~data2$kruskal_fact)
#Kruskal-Wallis chi-squared = 56.217, df = 3, p-value =3.775e-12

pairwise.wilcox.test(data2$propgam, g=data2$kruskal_fact, p.adjust.method = "bonferroni")
#             Final-IP Final-SP Initial-IP
#   Final-SP   5.5e-09     -        -         
#  Initial-IP  1.8e-09    1.0  -         
#  Initial-SP   6.0e-09   0.59  1.0     

##therefore, nonparam differences are
#no differences among initial groups as well as final SP
#Final IP is the only one different from the rest









#minimum and maximum propgam
min(data$propgam)
max(data$propgam)
#minimum and maximum propgam from initial data
min(data_init$propgam)
max(data_init$propgam)
#minimum and maximum propgam from final data
min(data_fin$propgam)
max(data_fin$propgam)

str(data_fin)
str(data_init)


View(data)


hist(data$propgam)

data$asinpropgam<- asin((data$propgam)^(1/2))
data$asinpropgam


#distribution
hist(data$asinpropgam)
shapiro.test(resid(full.m))

#simply linear model!
full.m <-lm (asinpropgam ~ 
               Treatment +
               Time +
               Treatment*Time
             , data)
#excluding subject as a factor since it's already include in Time
summary(full.m)

Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Time)
e1
e1$contrasts
multcomp::cld(e1)
##weird results as I know that they're different

str(data$Colony)




require(gridExtra)
grid.arrange(pcolsize, pgam, ppropgam, ncol=3)





#boxplot of ovaries from both groups
#boxplot(data$yolky~data$Treatment,
#     ylab="Yolky Oocytes",
#     xlab="Treatment",
#   ylim=c(0,12),
#   main="")


#analysis with K-W test
#wilcox.test(data$yolky~data$Treatment, data=data)
#mann-whitney U-test ranked test, p=0.004


#load package
#library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
#sum_data<-summarySE(data, measurevar="yolky", groupvars="Treatment")
#sum_data

#sample size
#INCRPROP: n=34 gams
#SAMEPROP: n=21 gams





####Fig. S2, Table S1 yolky oocytes from photographs compared to fresh observations####






setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Ovaries")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Oocyte_Gam_Count_2405_bothearlylater.csv", header=TRUE)
str(data)

#View(data)

#fix variables
data$Dissector<-as.factor(data$Dissector)
data$Colony<-as.factor(data$TRUE.GAM.COLONY)
data$Sampling<-as.factor(data$Sampling)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Sampling<- factor(data$Sampling, levels=rev(levels(data$Sampling)))
str(data)

#create propgam

data$propgam<- data$Gams/data$GroupSize
data$propgam






##
#View(data)

#correlations of mating and yb for fresh and photos

#yellow bodies
c2<-cor.test(data$BPYB, data$PHOTO_YB, na.rm=TRUE)
c2
#pearson's rho: 0.8307692, df = 141, p-value < 2.2e-16
c2<-cor.test(data$CABYB, data$PHOTO_YB, na.rm=TRUE)
c2
#pearson's rho: 0.8269803, df = 98, p-value < 2.2e-16



#mating
c2<-cor.test(data$BPMATED, data$PHOTO_MATED, na.rm=TRUE)
c2
#pearson's rho: 0.624692, df = 134, p-value = 4.392e-16
c2<-cor.test(data$CABMATED, data$PHOTO_MATED, na.rm=TRUE)
c2
#pearson's rho: 0.7110391, df = 109, p-value < 2.2e-16



data_yo <- subset(data, PHOTO_YO >= 0)
#View(data_yo)


data_bpyo <- subset(data, BPYO >= 0)
#View(data_bpyo)


c2<-cor.test(data_bpyo$PHOTO_YO, data_bpyo$BPYO, na.rm=TRUE)
c2
#p=2.2e-16, Pearson's rho= 0.9618337, df=140




library(ggplot2)

pbp_yo<- ggplot(data_bpyo, aes(x=BPYO, y=PHOTO_YO)) +
  geom_point() +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Freshly-dissected # YO obs. by BP ", y="Photographed # Yolky Oocytes") +
  #geom_smooth(method=lm, se=FALSE)
  expand_limits(y=c(0,15)) +
  expand_limits(x=c(0,15))

pbp_yo






data_cabyo <- subset(data, CABYO >= 0)
#View(data_cabyo)


c2<-cor.test(data_cabyo$PHOTO_YO, data_cabyo$CABYO, na.rm=TRUE)
c2
#p=2.2e-16, Pearson's rho= 0.9544165, df=109


library(ggplot2)

p_cabyo<- ggplot(data_cabyo, aes(x=CABYO, y=PHOTO_YO)) +
  geom_point() +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Freshly-dissected # YO obs. by CAB ", y="") +
  #geom_smooth(method=lm, se=FALSE)
  expand_limits(y=c(0,15)) +
  expand_limits(x=c(0,15))

p_cabyo


require(gridExtra)
grid.arrange(pbp_yo, p_cabyo, ncol=2)







####Fig. S3A, Table S2: yolky oocytes and other ovary data from photographs compared to behavioral estimates####


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Ovaries")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Oocyte_Gam_Count_2405_bothearlylater.csv", header=TRUE)
str(data)

#fix variables
data$Dissector<-as.factor(data$Dissector)
data$Colony<-as.factor(data$TRUE.GAM.COLONY)
data$Sampling<-as.factor(data$Sampling)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Sampling<- factor(data$Sampling, levels=rev(levels(data$Sampling)))
str(data)

#create propgam

data$propgam<- data$Gams/data$GroupSize
data$propgam


data_yo <- subset(data, PHOTO_YO >= 0)
#View(data_yo)


###create a new variable column for potential dominants
##for NG1 and NG2

data_yo$status2 <- data_yo$status


# replace NG1 and NG2 with PD for potential dominant
data_yo$status2[data_yo$status2 == "NG1"] <- "PD"
data_yo$status2[data_yo$status2 == "NG2"] <- "PD"
data_yo$status2[data_yo$status2 == "NG3"] <- "ND"
data_yo$status2[data_yo$status2 == "RG"] <- "RD"
data_yo$status2[data_yo$status2 == "G"] <- "D"

str(data_yo$status2)

str(data_yo)


library(ggplot2)
pworkers <- ggplot(data_yo, aes(y=PHOTO_YO, x=factor(status2))) + 
  #geom_boxplot(aes(fill=Treatment), show.legend = T) +
  geom_boxplot() +
  #facet_wrap(.~Interval, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,13, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Yolky Oocytes")# +
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pworkers


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_yo, measurevar="PHOTO_YO", groupvars=c("Treatment", "status2"))
sum_data
sum_data<-summarySE(data_yo, measurevar="PHOTO_YO", groupvars=c("status2"))
sum_data


#nonparametric comparison


kruskal.test(data_yo$PHOTO_YO~data_yo$status2)
#Kruskal-Wallis chi-squared = 75.555, df = 5, p-value = 7.127e-15


pairwise.wilcox.test(data_yo$PHOTO_YO, g=data_yo$status2, p.adjust.method = "bonferroni")






#because we only have n=1 ND and n=3 PD, exclude these from analysis
data_yo$status2 <- as.factor (data_yo$status2)

data_yo2 <- subset(data_yo, status2 != "ND")
data_yo3 <- subset(data_yo2, status2 != "PD")

str(data_yo3)
#n=195, excluding the 4 potential and new dominants
#View(data_yo)


pyo2 <- ggplot(data_yo3, aes(y=PHOTO_YO, x=factor(status2))) + 
  #geom_boxplot(aes(fill=Treatment), show.legend = T) +
  geom_boxplot() +
  #facet_wrap(.~Interval, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,13, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Yolky Oocytes")# +
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pyo2






library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_yo3, measurevar="PHOTO_YO", groupvars=c("Treatment", "status2"))
sum_data
sum_data<-summarySE(data_yo3, measurevar="PHOTO_YO", groupvars=c("status2"))
sum_data


hist(data_yo3$PHOTO_YO)

data_yo3$logphotoyo<- log(1+data_yo3$PHOTO_YO)

hist(data_yo3$logphotoyo)


l1 <-lm (logphotoyo ~ 
           factor(status2)
         , data_yo3 )
summary(l1)
##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#F 3, 150=58.197, p<2.2e-16
plot(l1)

#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(l1, pairwise ~ factor(status2))
e1
e1$contrasts
multcomp::cld(e1)
##see that there's a diff in terms of G from all else except ND
##NG and unclear are in a different group than W and RG













library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_yo, measurevar="propgam", groupvars=c("PHOTO_MATED", "status2"))
sum_data
##134 of 162 gamergates are mated, 28 gams are unmated
##4 of 8 W are mated, 4 of 8 are unmated



m1 <- matrix(c(134, 28, 4, 4),
             nrow = 2,
             dimnames = list(groups = c("mated", "unmated"),
                             Treatments = c("gamergates", "workers")))

m1

fisher.test(m1)
#p-value = 0.04205



sum_data<-summarySE(data_yo, measurevar="PHOTO_YO", groupvars=c("PHOTO_YB", "status2"))
sum_data
##162 of 162 G have YB
#17 of 17 RD have YB
#ND and PD do not show YB
##2 of 8 W have YB

m1 <- matrix(c(162, 0, 2, 6),
             nrow = 2,
             dimnames = list(groups = c("yellow bodies", "no yb"),
                             Treatments = c("gamergates", "workers")))

m1

fisher.test(m1)
#p-value = 9.131e-10




####Fig. S3B: analyzing how propensity to kill cricket relates to ovarian activity####



data_cricket <- subset(data, cricket >= 0)
#View(data_cricket)


library(ggplot2)
pcricket <- ggplot(data_cricket, aes(y=PHOTO_YO, x=factor(cricket))) + 
  geom_boxplot(aes(fill=factor(status)), show.legend = T) +
  #geom_boxplot() +
  #facet_wrap(.~Interval, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,13, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Cricket Score", y="# Yolky Oocytes")# +
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pcricket



library(ggplot2)
pcricket2 <- ggplot(data_cricket, aes(y=PHOTO_YO, x=factor(cricket))) + 
  #geom_boxplot(aes(fill=factor(status)), show.legend = T) +
  #geom_boxplot(aes(fill=factor(cricket)), show.legend = F) +
  geom_boxplot() +
  #facet_wrap(.~Interval, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,13, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Cricket Score", y="")# +
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pcricket2


wilcox.test(data_cricket$PHOTO_YO~data_cricket$cricket)
#W = 982, p-value = 7.808e-06


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_cricket, measurevar="propgam", groupvars=c("Treatment", "status"))
sum_data
sum_data<-summarySE(data_cricket, measurevar="propgam", groupvars=c("cricket"))
sum_data

###how many IP compared to SP gamergates killed a cricket?
sum_data<-summarySE(data_cricket, measurevar="PHOTO_YO", groupvars=c("cricket", "status", "Treatment"))
sum_data
#3/36 IP gamergates killed a cricket, whereas none of the SP gamergates did




#plot figure S3

require(gridExtra)
grid.arrange(pworkers, pcricket2, ncol=2)


grid.arrange(pyo2, pcricket2, ncol=2)








####Fig. S4. OOCYTE DATA####


####colony size, # gams, propgam from oocyte data (12 week AND 18 weeks)####




setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Ovaries")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Oocyte_Gam_Count_2405_bothearlylater.csv", header=TRUE)
str(data)

#fix variables
data$Dissector<-as.factor(data$Dissector)
data$Colony<-as.factor(data$TRUE.GAM.COLONY)
data$Sampling<-as.factor(data$Sampling)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Sampling<- factor(data$Sampling, levels=rev(levels(data$Sampling)))
str(data)

#create propgam

data$propgam<- data$Gams/data$GroupSize
data$propgam

View(data)






library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data, measurevar="propgam", groupvars=c("Treatment", "Colony"))
sum_data


sum_data<-summarySE(data, measurevar="propgam", groupvars=c("DeadGams", "Treatment", "Colony"))
sum_data
##16/51 colonies show a dead gamergate
#5 SP colonies, 11 IP colonies

sum_data<-summarySE(data, measurevar="propgam", groupvars=c("Newgams", "Treatment", "Colony"))
sum_data
##9/51 colonies have new gamergates in them
###2 of them are IP, 7 of them are SP
sum_data<-summarySE(data, measurevar="propgam", groupvars=c("Reverts", "Treatment", "Colony"))
sum_data
##11/51 colonies have reverted gamergates
##3 SP colonies, 8 IP colonies


library(ggplot2)
pcolsize <- ggplot(data, aes(y=GroupSize, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Interval), show.legend = T) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Workers") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,150))

pcolsize

#distribution
hist(data$GroupSize)

#log-transform
data$log_gpsize<- log(1+data$GroupSize)
data$log_gpsize

hist(data$log_gpsize)

library(lme4)

#simply linear model!
full.m <-lm (log_gpsize ~ 
               Treatment +
               Interval +
               Treatment*Interval
             ,data)
#excluding subject as a factor since it's already include in Time
summary(full.m)

library(car)
Anova(full.m)
#effects of time and treatment and its interaction

#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Interval)
e1
e1$contrasts
multcomp::cld(e1)


plot(full.m)



###group sizes don't differ between Treatments!

str(data)

pgam <- ggplot(data, aes(y=Gams, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=factor(Interval)), show.legend = T) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Gamergates") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,15))

pgam

#distribution
hist(data$Gams)






#simply linear model!
full.m <-lm (Gams ~ 
               Treatment +
               Sampling +
               Treatment*Sampling
             ,data)
#excluding subject as a factor since it's already include in Time
summary(full.m)

library(car)
Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Sampling)
e1$contrasts
multcomp::cld(e1)
###no diff in treatment in # gams




require(gridExtra)
grid.arrange(pcolsize, pgam, ncol=2)





ppropgam <- ggplot(data, aes(y=propgam, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=factor(Interval)), show.legend = T) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Gamergates / # Workers") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,1))

ppropgam


#minimum and maximum propgam
min(data$propgam)
max(data$propgam)
#minimum and maximum propgam from initial data
min(data_init$propgam)
max(data_init$propgam)
#minimum and maximum propgam from final data
min(data_fin$propgam)
max(data_fin$propgam)

str(data_fin)
str(data_init)





hist(data$prop)

data$asinpropgam<- asin((data$propgam)^(1/2))
data$asinpropgam


#distribution
hist(data$asinpropgam)


#simply linear model!
full.m <-lm (asinpropgam ~ 
               Treatment +
               Sampling +
               Treatment*Sampling
             ,data)
#excluding subject as a factor since it's already include in Time
summary(full.m)

Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Sampling)
e1
e1$contrasts
multcomp::cld(e1)
##weird results as I know that they're different

str(data$Colony)




#boxplot of ovaries from both groups
#boxplot(data$yolky~data$Treatment,
#     ylab="Yolky Oocytes",
#     xlab="Treatment",
#   ylim=c(0,12),
#   main="")


#analysis with K-W test
#wilcox.test(data$yolky~data$Treatment, data=data)
#mann-whitney U-test ranked test, p=0.004


#load package
#library(Rmisc)

#summarize data easily with summarySE, only for sample sizes
#sum_data<-summarySE(data, measurevar="yolky", groupvars="Treatment")
#sum_data

#sample size
#INCRPROP: n=34 gams
#SAMEPROP: n=21 gams




####Fig. S4A. IP/SP on oocytes for 12 and 18 weeks####




setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Ovaries")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Oocyte_Gam_Count_2405_bothearlylater.csv", header=TRUE)
str(data)

#fix variables
data$Dissector<-as.factor(data$Dissector)
data$Colony<-as.factor(data$TRUE.GAM.COLONY)
data$Sampling<-as.factor(data$Sampling)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Sampling<- factor(data$Sampling, levels=rev(levels(data$Sampling)))
str(data)

#create propgam

data$propgam<- data$Gams/data$GroupSize
data$propgam

#View(data)

###compare PhotoYO between different groups for gamergates only



data_gam <- subset(data, status == "G")
str(data_gam)
#View(data_gam)
#n=163 gams


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_gam, measurevar="propgam", groupvars=c("Treatment"))
sum_data

sum_data<-summarySE(data_gam, measurevar="propgam", groupvars=c("Interval", "Treatment"))
sum_data
#n=IP @ 18 weeks= 53
#n=SP @ 18 weeks=23
#n=IP @ 12 weeks= 66
#n=SP @ 12 weeks=21


sum_data<-summarySE(data_gam, measurevar="propgam", groupvars=c("Interval", "Treatment", "TRUE.GAM.COLONY"))
sum_data
#n=10 IP colonies @ 18 weeks
#n=22-10= 12 SP colonies @ 18 weeks


data_gam_only <- subset(data_gam, Treatment != "")
str(data_gam_only)
#n=163 gams, without the nA one


library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_gam_only, measurevar="propgam", groupvars=c("Interval", "Treatment"))
sum_data


str(data_gam_only)
data_gam_only$Interval<- as.factor(data_gam_only$Interval)
str(data_gam_only)


#reverse order of 12 and 18 s/t it is on the left
data_gam_only$Interval<- factor(data_gam_only$Interval, levels=rev(levels(data_gam_only$Interval)))




library(ggplot2)
pyo_diffgroups <- ggplot(data_gam_only, aes(y=PHOTO_YO, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Interval), show.legend = T) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=seq(0,15, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20),
        legend.text= element_text(size=16), legend.title= element_blank()) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Yolky Oocytes") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

pyo_diffgroups







library(car)
#simply linear model!
full.m <-lm (PHOTO_YO ~ 
               Treatment +
               Interval +
               Treatment * Interval
             , data_gam_only)
#excluding subject as a factor since it's already include in Time
summary(full.m)

Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Interval)
e1
e1$contrasts
multcomp::cld(e1)
##weird results as I know that they're different


###log transform results to meet assumptions of normality better

data_gam_only$logphotoyo<- log(1+data_gam_only$PHOTO_YO)

hist(data_gam_only$logphotoyo)

#simply linear model!
full.m <-lm (logphotoyo ~ 
               Treatment +
               Interval +
               Treatment * Interval
             , data_gam_only)
#excluding subject as a factor since it's already include in Time
summary(full.m)

Anova(full.m)
#effects of time and treatment and its interaction
plot(full.m)
#multiple comparisons after wald's chi-square test showed a difference
library(emmeans)
e1<-emmeans(full.m, pairwise ~ Treatment*Interval)
e1
e1$contrasts
multcomp::cld(e1)











#### Fig. S4B. Final Egg data @ 18 weeks####


setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals")
getwd()
#clear all
#rm(list=ls())


#import  DATA set 

data<-read.csv("Col_sum_2405.csv", header=TRUE)

#View(data)

#fix variables
data$Interval<-as.factor(data$Interval)
data$SourceCol<-as.factor(data$SourceCol)
data$Colony<-as.factor(data$Colony)
data$Time<-as.factor(data$Time)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Time<- factor(data$Time, levels=rev(levels(data$Time)))
str(data)

#create propgam

data$propgam<- data$NumGams/data$GroupSize
data$propgam


#create num nonrepros


data$nonrepros<- data$GroupSize - data$NumGams
data$nonrepros


#how many were reverted
# using subset function for  final sampling
data_fin <- subset(data, Time=="Final")
str(data_fin)









#how many were counted for eggs or larvae
# using subset function for  final sampling
data_egg <- subset(data_fin, eggs>=0)
str(data_egg)
#21 colonies




library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_egg, measurevar="propgam", groupvars=c("Treatment"))
sum_data
#9 IP colonies
#12 SP colonies

wilcox.test(data_egg$eggs~data_egg$Treatment)
#W = 76, p-value = 0.1285




egg_p <- ggplot(data_fin, aes(y=eggs, x=factor(Treatment))) +
  #geom_boxplot() +
  geom_boxplot(aes(fill=factor(Treatment)), show.legend = T) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size=20), axis.title.y = element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Eggs") +
  expand_limits(y=c(0,150))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,1))

egg_p

eggs_propgam<-  ggplot(data_fin, aes(x=propgam, y=eggs)) + 
  geom_point() + 
  xlab("# Gamergates/ # Workers") + 
  ylab("# Eggs") +
  #ylim(0.2,0.7) +
  #xlim(0,15) +
  geom_smooth(method=lm, se=FALSE)
eggs_propgam


library(lme4)
library(car)

m1 <- lm( eggs~ propgam, data = data_fin)
summary(m1 )
anova(m1)
#adj r^2=  -0.00558 
#y= 40.94 * X + 40.94



####Fig S4B. continued Eggs per gamergate @ 18 weeks####

data_egg$eggpergam<-data_egg$eggs/data_egg$NumGams


egg_gam <- ggplot(data_egg, aes(y=eggpergam, x=factor(Treatment))) +
  geom_boxplot() +
  #geom_boxplot(aes(fill=factor(Treatment)), show.legend = F) +
  #facet_wrap(.~age + gpsize, ncol=4) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size=20), axis.title.y = element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="# Eggs / # Dominants") +
  expand_limits(y=c(0,40))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,1))

egg_gam

wilcox.test(data_egg$eggpergam~data_egg$Treatment)
#W = 5, p-value = 0.0001293


####Plotting Fig S4A and S4B together ####



require(gridExtra)
grid.arrange(pyo_diffgroups, egg_gam, ncol=2)














####Fig. S5. yolky oocytes of GAMERGATES only in comparison to headwidth####




setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Ovaries")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("Oocyte_Gam_Count_2405_bothearlylater.csv", header=TRUE)
str(data)

#fix variables
data$Dissector<-as.factor(data$Dissector)
data$Colony<-as.factor(data$TRUE.GAM.COLONY)
data$Sampling<-as.factor(data$Sampling)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Sampling<- factor(data$Sampling, levels=rev(levels(data$Sampling)))
str(data)

#create propgam

data$propgam<- data$Gams/data$GroupSize
data$propgam

data_gam <- subset(data, status == "G")
str(data_gam)
#View(data_gam)
#n=163 gams




headwidth_oocytes_treatment<- ggplot(data_gam, aes(x=headwidth_mm, y=PHOTO_YO)) +
  geom_point(aes(colour=Treatment), show.legend = T) + 
  #facet_wrap(.~Treatment, ncol=2) +
  #geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Headwidth (mm)", y="# Yolky Oocytes") #+
#expand_limits(y=c(0,8))

headwidth_oocytes_treatment

View(data_gam)

headwidth_oocytes_treatment2<- ggplot(data_gam, aes(x=headwidth_mm, y=PHOTO_YO)) +
  #geom_point(aes(colour=Treatment), show.legend = T) + 
  geom_point() +
  facet_wrap(.~Treatment, ncol=2) +
  #geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log')), se=F, col="blue") +
  #geom_smooth(method=lm, formula= y~log(1+x), se=F, col="green") +
  geom_smooth(method=lm, formula= y~x, se=F) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
        strip.text = element_text(size = 20)) +
  #scale_y_continuous(breaks=c(0,1,2)) +
  labs (x="Head width (mm)", y="# Yolky Oocytes") +
  expand_limits(y=c(0,15)) +
  expand_limits(x=c(2.0,3.25))

headwidth_oocytes_treatment2






#what is the relationship of oocyte number and headwidth

data_gam_IP <- subset(data_gam, Treatment == "IP")
str(data_gam_IP)
#n=119 gams
data_gam_SP <- subset(data_gam, Treatment == "SP")
str(data_gam_SP)
#n=44 gams


l1 <-lm ( PHOTO_YO~ 
            headwidth_mm 
          , data_gam_IP )
summary(l1)
#y= 0.7037*X + 0.7037
#R^2: 0.005815
#t=0.680, p=0.499

##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#no effect of group size on propgam
#plot(l1)

l1 <-lm ( PHOTO_YO~ 
            headwidth_mm 
          , data_gam_SP )
summary(l1)
#y= 4.765*X + -6.123 
#R^2=0.1246
#t=2.326
#p=0.0254



##using Wald's Chi-square Test (Type II)
library(car)
Anova(l1)
#no effect of group size on propgam
#plot(l1)










#### Fig. S6. Reversion, Death, New Gamergate analysis by individual events for all colonies within 12 weeks of experiment ####



setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals")
getwd()
#clear all
rm(list=ls())


#import  DATA set 
##these are all known events of RG, gam death, and NG1 or NG3 in experiment
#RG1= reversion of original gamergate to NR status
#Gam death= death of original gamergate
#NG1= original NR who shows gamergate like characteristics
#NG3= original NR who shows gamergate like characteristics on 3 successive observations


data<-read.csv("RG_death_NG_analysis.csv", header=TRUE)

#View(data)





library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
#sum_data<-summarySE(data, measurevar="propgam", groupvars=c("NumGams"), na.rm=TRUE)
#sum_data
#sum_data<-summarySE(data, measurevar="eggrate", groupvars=c("Colony"), na.rm=TRUE)
#sum_data
#n = 49 cols total







#only use observations within first 6 observation periods
##therefore pooling 12 and 18 week colonies, but only for observations  of 6 or less
##i.e. those with gamergates in their colonies
data_early <- subset(data, AfterManip<7)
str(data_early)
#n=46 events of NG, RG, and death
##out of 24 IP colonies and 27 SP colonies total






###how many colonies show NG1 by treatment


#only use colonies with previousG>1
##i.e. those with gamergates in their colonies
data_NG1 <- subset(data_early, newNG1>0)
str(data_NG1)
#n=17 events of NG1



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_NG1, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
sum_data<-summarySE(data_NG1, measurevar="previousG", groupvars=c("Treatment"), na.rm=TRUE)
sum_data

#2 IP colonies, each with 2 events
##out of a total of 24 IP colonies
#6 SP colonies, with 1-7 events
##out of a total of 27 SP colonies





m1 <- matrix(c(6, 21, 2, 22),
             nrow = 2,
             dimnames = list(groups = c("instability", "remainder"),
                             Treatments = c("SP", "IP")))

m1

fisher.test(m1)
#p-value = 0.2553





###how many colonies show NG3 by treatment


#only use colonies with previousG>1
##i.e. those with gamergates in their colonies
data_NG3 <- subset(data_early, newNG3>0)
str(data_NG3)
#n=2 events of NG3



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_NG3, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
#1 IP colonies,  with 2 events




###how many colonies show RG1 by treatment


data_RG1 <- subset(data_early, newRG1>0)
str(data_RG1)
#n=18 events of RG1



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_RG1, measurevar="previousG", na.rm=TRUE)
sum_data
sum_data<-summarySE(data_RG1, measurevar="previousG", groupvars=c("Treatment"), na.rm=TRUE)
sum_data
sum_data<-summarySE(data_RG1, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
#9 IP colonies, each with 1-4 events
##total IP colonies= 24 colonies
#3 SP colonies, with 1 event
##total SP colonies= 27 colonies




m1 <- matrix(c(3, 24, 9, 15),
             nrow = 2,
             dimnames = list(groups = c("instability", "remainder"),
                             Treatments = c("SP", "IP")))

m1

fisher.test(m1)
#p-value = 0.04562








###how many colonies show death by treatment


data_death <- subset(data_early, deadG>0)
str(data_death)
#n=13 events of NG1



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_death, measurevar="previousG", na.rm=TRUE)
sum_data
sum_data<-summarySE(data_death, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
sum_data<-summarySE(data_death, measurevar="previousG", groupvars=c("Treatment"), na.rm=TRUE)
sum_data
#10 IP colonies, each with 1-2 events
#out of 24 IP colonies
#1 SP colony, with 1 event
#out of 27 SP colonies
##both IP and SP show similar # previous Gams, suggesting that effect may be just at high # gams





m1 <- matrix(c(1, 26, 10, 14),
             nrow = 2,
             dimnames = list(groups = c("instability", "remainder"),
                             Treatments = c("SP", "IP")))

m1

fisher.test(m1)
#p-value = 0.001438






#### Fig. S6. Reversion, Death, New Gamergate analysis by individual events only for colonies that went 18 weeks of experiment ####



#setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals")
#getwd()
#clear all
#rm(list=ls())


#import  DATA set 
##these are all known events of RG, gam death, and NG1 or NG3 in experiment
#RG1= reversion of original gamergate to NR status
#Gam death= death of original gamergate
#NG1= original NR who shows gamergate like characteristics
#NG3= original NR who shows gamergate like characteristics on 3 successive observations


#data<-read.csv("RG_death_NG_analysis.csv", header=TRUE)

#View(data)








#only use observations within first 6 observation periods
##therefore pooling 12 and 18 week colonies, but only for observations  of 6 or less
##i.e. those with gamergates in their colonies
data_18 <- subset(data, Interval == "Eighteen")
str(data_18)
#n=48 events of NG, RG, and death
##out of 10 IP colonies and 13 SP colonies total





###how many colonies show NG1 by treatment


#only use colonies with previousG>1
##i.e. those with gamergates in their colonies
data_NG1 <- subset(data_18, newNG1>0)
str(data_NG1)
#n=21 events of NG1



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_NG1, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
sum_data<-summarySE(data_NG1, measurevar="previousG", groupvars=c("Treatment"), na.rm=TRUE)
sum_data
#2 IP colonies, each with 2 events
##out of a total of 10 IP colonies
#6 SP colonies, with 1-7 events
##out of a total of 13 SP colonies



m1 <- matrix(c(6, 7, 2, 8),
             nrow = 2,
             dimnames = list(groups = c("instability", "remainder"),
                             Treatments = c("SP", "IP")))

m1

fisher.test(m1)
#p-value = 0.3788




###how many colonies show NG3 by treatment


#only use colonies with previousG>1
##i.e. those with gamergates in their colonies
data_NG3 <- subset(data_18, newNG3>0)
str(data_NG3)
#n=5 events of NG3



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_NG3, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
#1 IP colonies,  with 2 events
#1 SP colony, with 3 events




###how many colonies show RG1 by treatment


data_RG1 <- subset(data_18, newRG1>0)
str(data_RG1)
#n=18 events of NG1



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_RG1, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data
sum_data<-summarySE(data_RG1, measurevar="previousG", groupvars=c("Treatment"), na.rm=TRUE)
sum_data

#7 IP colonies, each with 1-5 events
##out of 10 IP colonies
#2 SP colonies, with 1 event
##out of 13 SP colonies


m1 <- matrix(c(2, 11, 7, 3),
             nrow = 2,
             dimnames = list(groups = c("instability", "remainder"),
                             Treatments = c("SP", "IP")))

m1

fisher.test(m1)
#p-value = 0.01306








###how many colonies show death by treatment


data_death <- subset(data_18, deadG>0)
str(data_death)
#n=10 events of NG1



library(Rmisc)
#summarize data easily with summarySE, only for sample sizes
sum_data<-summarySE(data_death, measurevar="previousG", groupvars=c("Treatment", "Col"), na.rm=TRUE)
sum_data

sum_data<-summarySE(data_death, measurevar="previousG", groupvars=c("Treatment"), na.rm=TRUE)
sum_data

#7 IP colonies, each with 1-2 events
#out of 10 colonies
#1 SP colony (49B), with 1 event, where there were no clear gamergates in colony
#out of 13 colonies
m1 <- matrix(c(1, 12, 7, 3),
             nrow = 2,
             dimnames = list(groups = c("instability", "remainder"),
                             Treatments = c("SP", "IP")))

m1

fisher.test(m1)
#p-value = 0.005898





####Fig. S6--Barcharts--Exp. colonies showing dead gamergates, reverted gamergates, and new gamergates####


#plot this as barchart



setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals")
getwd()
#import  DATA set 

data_barchart <- read.csv("gam_changes_cols_2405.csv", header=TRUE)

#proportions
data_barchart$propRG<- data_barchart$RG/data_barchart$cols
data_barchart$propdead<- data_barchart$Dead/data_barchart$cols
data_barchart$propNG<- data_barchart$NG/data_barchart$cols

library(forcats)

bc_rg <- ggplot(data_barchart, aes(y=propRG, x=factor(Treatment))) +
  #geom_col(aes(colour=Treatment)) +
  geom_col(aes(fill=fct_rev(Interval)), show.legend = F) +
  facet_wrap(.~fct_rev(Interval), ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20),  axis.title.x = element_text(size = 20), strip.text = element_text(size = 20)  ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="# cols with dom. reversion / total # cols") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,1)) 


bc_rg


bc_dg <- ggplot(data_barchart, aes(y=propdead, x=factor(Treatment))) +
  #geom_col(aes(colour=Treatment)) +
  geom_col(aes(fill=fct_rev(Interval)), show.legend = F) +
  facet_wrap(.~fct_rev(Interval), ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20),  axis.title.x = element_text(size = 20), strip.text = element_text(size = 20)  ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="# cols with dom. death / total # cols") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,1)) 


bc_dg


bc_ng <- ggplot(data_barchart, aes(y=propNG, x=factor(Treatment))) +
  #geom_col(aes(colour=Treatment)) +
  geom_col(aes(fill=fct_rev(Interval)), show.legend = F) +
  facet_wrap(.~fct_rev(Interval), ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=c(0,1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20),  axis.title.x = element_text(size = 20), strip.text = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="# cols with potential new dom. / total # cols") +
  #expand_limits(y=c(0,25))
  #scale_colour_manual(values=cbPalette)
  expand_limits(y=c(0,1)) 


bc_ng


require(gridExtra)
grid.arrange(bc_rg, bc_dg, bc_ng, ncol=3)








####Fig. S7. for 5 behaviors Paired DIFFERENCES of Initial and Final gamergates-- Remaining behaviors#####



setwd("~/Google Drive/MacBookPro_Backup/Drive--Documents/ASUGradSchool/Harpegnathos_projects/AddingGams_Ch1/WorkerRemoval/14d_intervals/Behavioralanalysis")
getwd()
#clear all
rm(list=ls())


#import  DATA set 

data<-read.csv("diff_all_2405.csv", header=TRUE)
str(data)



#fix variables
data$Colony<-as.factor(data$Colony)
data$Time<-as.factor(data$Time)
data$Treatment<-as.factor(data$Treatment)
#rearrange order of levels of sampling
data$Time<- factor(data$Time, levels=rev(levels(data$Time)))



#create propgam

data$propgam<- data$NumGams/data$GroupSize
data$propgam

# using subset function for only those rows with differences between initial and final
data <- subset(data, diffcalc > 0)
str(data)

#View(data)
#255 gamergates


# using subset function for  ONLY gamergates
data_G <- subset(data, Status_Scanobs== "G")
str(data_G)
#202 gamergates



library(Rmisc)
sum_data<-summarySE(data_G, measurevar="NumDuel", groupvars=c("Treatment", "Time"))
sum_data
#159 IP gamergates
#43 SP gamergates (-10 SP gamergates if consider those that end up with more than 1 gamergate SP groups @ end)
sum_data<-summarySE(data_G, measurevar="NumDuel", groupvars=c("Treatment", "Time", "Colony"))
sum_data
#24 IP colonies
50-24
#26 SP colonies


library(ggplot2)

##number of antennal drumming observation (y/n) across 14 observationss
antdrum <- ggplot(data_G, aes(y=diffantdrum, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,15, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Change in # Antennal Boxing Events (Final-Initial)") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

antdrum


#cumulative antennal drummings
library(ggplot2)
numantdrum <- ggplot(data_G, aes(y=diffnumantdrum, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=3)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # antennal boxings (Final-Initial)") +
  expand_limits(y=c(-20,30))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

numantdrum

wilcox.test(data_G$diffnumantdrum~data_G$Treatment)
#W = 3261, p-value = 0.6359






#receiving antennal drummings
library(ggplot2)
recantdrum <- ggplot(data_G, aes(y=diffrecantdrum, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in Obs Rec. Antennal Drummings") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

recantdrum






# obs performing dominance
library(ggplot2)
domperfs <- ggplot(data_G, aes(y=diffdomperf, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=1)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Observations out of 14 where dominance bites performed per day") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

domperfs


# sum of performing dominance
library(ggplot2)
sumdomperfs <- ggplot(data_G, aes(y=diffnumdomperf, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # dom. bites (Final-Initial)") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

sumdomperfs


wilcox.test(data_G$diffnumdomperf~data_G$Treatment)
#W = 3053, p-value = 0.1153



# sum of receiving dominance
library(ggplot2)
domrecs <- ggplot(data_G, aes(y=diffdomrec, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in # Dom Rec") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

domrecs


# sum of receiving dominance
library(ggplot2)
sumdomrecs <- ggplot(data_G, aes(y=diffnumdomrec, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in # Sum Dom Rec") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

sumdomrecs

# sum of allogrooming
library(ggplot2)
allogroom <- ggplot(data_G, aes(y=diffallogrooming, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # 5-min obs. allogrooming (Final-Initial)") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

allogroom


wilcox.test(data_G$diffallogrooming~data_G$Treatment)
#W = 3410.5, p-value = 0.9681




# sum of recgrooming
library(ggplot2)
recgroom <- ggplot(data_G, aes(y=diffrecgrooming, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in # Obs Receiving Grooming") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

recgroom



# obs of walkover
library(ggplot2)
walkover <- ggplot(data_G, aes(y=diffwalkover, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in # Obs being walked over") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

walkover



# sum of walkovers
library(ggplot2)
numwalkover <- ggplot(data_G, aes(y=diffnumwalkover, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Diff in Sum of being walked over") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

numwalkover




# sum of handlepupae
library(ggplot2)
handlepup <- ggplot(data_G, aes(y=diffhandlepupae, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in Obs Handling pupae") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

handlepup

# sum of handling larvae
library(ggplot2)
handlelarv <- ggplot(data_G, aes(y=diffhandlelarvae, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="diff in handling larvae") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

handlelarv

# sum of handling eggs
library(ggplot2)
handleeggs <- ggplot(data_G, aes(y=diffhandleeggs, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="diff in handling old eggs") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

handleeggs


# sum of brood handling (pup+larv+eggs)
library(ggplot2)
brood <- ggplot(data_G, aes(y=diffhandlebrood, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #scale_y_continuous(breaks=seq(0,30, by=2)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20), axis.title.x= element_text(size=20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="Treatment", y="Change in # 5-min obs. brood handling (Final-Initial)") +
  expand_limits(y=c(-4,6))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

brood

wilcox.test(data_G$diffhandlebrood~data_G$Treatment)
#W = 3597.5, p-value = 0.5589


# being outside

library(ggplot2)
outside <- ggplot(data_G, aes(y=diffoutside, x=factor(Treatment))) + 
  geom_boxplot(aes(fill=Treatment), show.legend = F) +
  #facet_wrap(.~age, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  # scale_y_continuous(breaks=seq(0,15, by=3)) +
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20) ) +
  theme(legend.title = element_blank()) +
  labs (title="", x="", y="Diff in Obs Outside") #+
#expand_limits(y=c(0,25))
#scale_colour_manual(values=cbPalette)
#expand_limits(y=c(0,150))

outside




require(gridExtra)
grid.arrange( numantdrum, recantdrum, sumdomperfs,
              sumdomrecs, allogroom, recgroom,
              numwalkover, brood, outside,
              ncol=3)


require(gridExtra)
grid.arrange( numantdrum, sumdomperfs,
              allogroom, brood,
              ncol=4)









