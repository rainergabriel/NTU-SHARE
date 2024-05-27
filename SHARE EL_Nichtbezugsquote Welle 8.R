#Loading packages

library(foreign)
#install.packages("reshape2")
library(reshape2)
#install.packages("memisc")
library(memisc)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("questionr")
library(questionr)
#install.packages("htmltools")
library(htmltools)
#install.packages("maditr")
library(maditr)

#setting working directory
setwd("/Users/selinariedo/Desktop/SHARE/Wave 8")

### Part 1: Setting up ###

#Loading datasets

rm(list=ls())
load(file="data_wave8.activities.Rdata")
load(file="data_wave8.assets.Rdata")
load(file="data_wave8.basics.Rdata")
load(file="data_wave8.children.Rdata")
load(file="data_wave8.consumption.Rdata")
load(file="data_wave8.demo.Rdata")
load(file="data_wave8.fin.transfers.Rdata")
load(file="data_wave8.finance.Rdata")
load(file="data_wave8.gv.housing.Rdata")
load(file="data_wave8.healthcare.Rdata")
load(file="data_wave8.hh.Rdata")
load(file="data_wave8.imp.Rdata")
load(file="data_wave8.pensions.Rdata")
load(file="data_wave8.tech.Rdata")
load(file="data_wave8.weights.Rdata")
load(file="data_wave6.imp.Rdata")


#list all the available dataframes and look at the variables
ls()
names(wave8.assets)
names(wave8.basic)
names(wave8.children)
names(wave8.gv.housing)
names(wave8.hh)
names(wave8.imp)
names(wave8.pension)
names(wave8.tech)

# Subset all datasets for switzerland 
wave8.activities <- subset(wave8.activities,country=="Switzerland")
wave8.assets<- subset(wave8.assets,country=="Switzerland")
wave8.basics<- subset(wave8.basic,country=="Switzerland")
wave8.children<- subset(wave8.children,country=="Switzerland")
wave8.consumption<- subset(wave8.consumption,country=="Switzerland")
wave8.demo<- subset(wave8.demo,country=="Switzerland")
wave8.fin_transfers<- subset(wave8.fin_transfers,country=="Switzerland")
wave8.finance<- subset(wave8.finance,country=="Switzerland")
wave8.gv.housing<- subset(wave8.gv.housing,country=="Switzerland")
wave8.health<- subset(wave8.health,country=="Switzerland")
wave8.hh<- subset(wave8.hh,country=="Switzerland")
wave8.imp<- subset(wave8.imp,country=="Switzerland")
wave8.pensions<- subset(wave8.pension,country=="Switzerland")
wave8.tech<- subset(wave8.tech,country=="Switzerland")
wave8.weights<- subset(wave8.weights,country=="Switzerland")

# Merging all these switzerland specific datasets -------------------------
wave8.merged <-  merge(wave8.activities, wave8.assets, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.basics, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.children, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.consumption, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.demo, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.fin_transfers, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.gv.housing, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.health, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.hh, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.pensions, by = "mergeid")
wave8.merged <- merge(wave8.merged, wave8.tech, by = "mergeid")
wave8 <- merge(wave8.merged, wave8.weights, by = "mergeid")

#remove all previous datasets to clear the workspace
rm(wave8.activities)
rm(wave8.assets)
rm(wave8.basic)
rm(wave8.children)
rm(wave8.consumption)
rm(wave8.demo)
rm(wave8.fin_transfers)
rm(wave8.finance)
rm(wave8.gv.housing)
rm(wave8.health)
rm(wave8.hh)
# rm(wave8.imp) #...except for the imputation file cause lots of variables are gonna be taken from it and it can't be merged in long form
rm(wave8.pension)
rm(wave8.tech)
rm(wave8.weights)

# at this stage we have merged all the files into one specific switzerland dataset

# creating key demographic variables  -------------------------------------
#age 
levels(wave8$age2020)
wave8$age2020.char <- as.character(wave8$age2020)
head(wave8$age2020.char)
wave8$age2020.num <- memisc::recode(wave8$age2020.char,
                                    NA<- c( "Don't know","Refusal","Not applicable"), 
                                    otherwise="copy")
head(wave8$age2020.num)
wave8$age2020.num <- as.numeric(as.character(wave8$age2020.num))
summary(wave8$age2020.num)
table(wave8$age2020.num)
is.numeric(wave8$age2020.num)
length(which(wave8$age2020.num>=65))
is.factor(wave8$age2020)

#children
wave8$child1.where.living <- wave8$ch007_1
wave8$child2.where.living <- wave8$ch007_2
wave8$child3.where.living <- wave8$ch007_3
wave8$child4.where.living <- wave8$ch007_4
wave8$child5.where.living <- wave8$ch007_5

#Creating a dummy variable whether a child lives at home 
levels(wave8$child1.where.living)  
wave8$child1.inhh <- ifelse(is.na(wave8$child1.where.living),0,ifelse(as.factor(wave8$child1.where.living)=="In the same household",1,0))
table(wave8$child1.inhh)
wave8$child2.inhh <- ifelse(is.na(wave8$child2.where.living),0,ifelse(as.factor(wave8$child2.where.living)=="In the same household",1,0))
table(wave8$child2.inhh)
wave8$child3.inhh <- ifelse(is.na(wave8$child3.where.living),0,ifelse(as.factor(wave8$child3.where.living)=="In the same household",1,0))
table(wave8$child3.inhh)
wave8$child4.inhh <- ifelse(is.na(wave8$child4.where.living),0,ifelse(as.factor(wave8$child4.where.living)=="In the same household",1,0))
table(wave8$child4.inhh)
wave8$child5.inhh <- ifelse(is.na(wave8$child5.where.living),0,ifelse(as.factor(wave8$child5.where.living)=="In the same household",1,0))
table(wave8$child5.inhh)

#how many children live in hh
wave8$number.child.living.in.hh <- wave8$child1.inhh+wave8$child2.inhh+wave8$child3.inhh+wave8$child4.inhh+wave8$child5.inhh
table(wave8$number.child.living.in.hh)

#some relevant sociodemographic info from the imputation file 

finder <- match(as.character(wave8$mergeid),as.character(wave8.imp$mergeid)) 

wave8$living.in.nursing.home <- wave8.imp$nursinghome[finder]#nursinghome
wave8$household.type <- wave8.imp$htype[finder] #household type

wave8$marital.status <- wave8.imp$mstat[finder] #marital status
table(wave8$marital.status)

table(wave8$marital.status, wave8$household.type)
table(wave8$marital.status, wave8$living.in.nursing.home)


levels(wave8$marital.status)
wave8$married.bn <- memisc::recode(wave8$marital.status, 
                                   0 <- c("Divorced", "Not applicable","Widowed","Never married"), 
                                   1 <- c("Married, living with spouse","Married, not living with spouse","Registered partnership"), 
                                   otherwise="copy")

euro.chf.exrate <- wave8.imp$exrate[1] #getting the exchange rate from the gv/imp file

# fixing the couple-id variable -------------------------------------------
#note: because i'm merging the files, identical variables get overwritten and somehow this messes up the coupleid variable
#that's why i'm getting it once again from the imputation file
#writing a little loop function that checks for each individual and sticks it back into the merged file

wave8$coupleid<-NA
for (i in 1:nrow(wave8)) {
  finder <- match(as.character(wave8$mergeid[i]),as.character(wave8.imp$mergeid))
  wave8$coupleid[i] <- as.character(wave8.imp$coupleid8[finder])
}
head(wave8$coupleid)
wave8$coupleid <- as.factor(wave8$coupleid)
count.no.couple.match <- 0
print(count.no.couple.match)

# Creating a typology variable for the calculation of EL eligibility --------

wave8$el.eligibility.type <- NA

table(wave8$marital.status) #majority married, living with spouse
table(wave8$marital.status,wave8$living.in.nursing.home) #

levels(wave8$marital.status)
filter <- which((wave8$marital.status=="Married, living with spouse" | wave8$marital.status=="Registered partnership")
                & wave8$living.in.nursing.home=="No")
length(filter)
wave8$el.eligibility.type[filter] <- "married.both.home"
summary(as.factor(wave8$el.eligibility.type))

filter <- which((wave8$marital.status=="Married, living with spouse" | wave8$marital.status=="Registered partnership")
                & wave8$living.in.nursing.home=="Yes")
length(filter)
wave8$el.eligibility.type[filter] <- "married.both.nursing.home"
summary(as.factor(wave8$el.eligibility.type))

filter <- which(
  (wave8$marital.status=="Widowed" | 
     wave8$marital.status=="Never married" |
     wave8$marital.status=="Divorced" | 
     wave8$marital.status=="Married, not living with spouse") 
  & wave8$living.in.nursing.home=="No")
wave8$el.eligibility.type[filter] <- "solo.tarif.own.home"

filter <- which(
  (wave8$marital.status=="Widowed" | 
     wave8$marital.status=="Never married" |
     wave8$marital.status=="Divorced" | 
     wave8$marital.status=="Married, not living with spouse") 
  & wave8$living.in.nursing.home=="Yes")                  
wave8$el.eligibility.type[filter] <- "solo.tarif.nursing.home"

wave8$el.eligibility.type <- as.factor(wave8$el.eligibility.type)
summary(wave8$el.eligibility.type)

### Part 2: Wealth ###

#household.net.worth.imputed.euro  
wave8.imp.hnetw.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "hnetw")
names(wave8.imp.hnetw.wide) <- c(
  "mergeid",
  "hnetw.1",
  "hnetw.2",
  "hnetw.3",
  "hnetw.4",
  "hnetw.5"
)
wave8.imp.hnetw.wide$mean.hnetw <- (wave8.imp.hnetw.wide$hnetw.1+ 
                                      wave8.imp.hnetw.wide$hnetw.2+
                                      wave8.imp.hnetw.wide$hnetw.3+
                                      wave8.imp.hnetw.wide$hnetw.4+
                                      wave8.imp.hnetw.wide$hnetw.5)/5
summary(wave8.imp.hnetw.wide$mean.hnetw) 
plot((wave8.imp.hnetw.wide$mean.hnetw))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.hnetw.wide$mergeid))
wave8$imputed.household.net.worth.amount.euro <- wave8.imp.hnetw.wide$mean.hnetw[finder]
summary(wave8$imputed.household.net.worth.amount.euro)
wave8$imputed.household.net.worth.amount <- wave8$imputed.household.net.worth.amount.euro*euro.chf.exrate
summary(wave8$imputed.household.net.worth.amount)


plot(wave8$gender,log(wave8$imputed.household.net.worth.amount))


# adjusting according to marital status and household size ----------------
wave8$adjusted.household.net.worth <- wave8$imputed.household.net.worth.amount
filter <- which(wave8$married.bn==0 & wave8$hhsize==2) #adjusting for 2-people households that are not married 
wave8$adjusted.household.net.worth[filter] <- wave8$adjusted.household.net.worth[filter]/2
filter <- which(wave8$married.bn==1 & wave8$hhsize==3) #adjusting for 3-people households that are married
wave8$adjusted.household.net.worth[filter] <- (wave8$adjusted.household.net.worth[filter]/3)*2
filter <- which(wave8$married.bn==1 & wave8$hhsize==4) #adjusting for 4-people households that are married
wave8$adjusted.household.net.worth[filter] <- wave8$adjusted.household.net.worth[filter]*0.5
filter <- which(wave8$married.bn==1 & wave8$hhsize==5) #adjusting for 5-people households that are married
wave8$adjusted.household.net.worth[filter] <- (wave8$adjusted.household.net.worth[filter]/5)*2

### Part 3: Financial Inflows ("Anrechenbares Einkommen") ###

# AHV Income --------------------------------------------------------------
#Note: Since I wasn't sure if the imputation data was correct, I confirmed this by comparing the 
#regular EP (aka "Pensions) Module with the imputation variable with regards to AHV-Amount. 

#amount for ahv from the "normal" EP module 

wave8$ahv.amount.euro <- wave8$ep078e_1
summary(wave8$ahv.amount.euro)
levels(wave8$ahv.amount.euro)
wave8$ahv.amount.euro <- as.character(wave8$ahv.amount.euro)
wave8$ahv.amount.euro <- as.numeric(wave8$ahv.amount.euro)
table(wave8$ahv.amount.euro)
plot(wave8$ahv.amount.euro)
summary(wave8$ahv.amount.euro)
wave8$ahv.amount.chf <- wave8$ahv.amount.euro*euro.chf.exrate
summary(wave8$ahv.amount.chf)

#standardize to timeframe 
table(wave8$ep074_1)
levels(wave8$ep074_1)
wave8$ahv.amount.chf.month<-ifelse(wave8$ep074_1=="One week",wave8$ahv.amount.chf*4,
                                   ifelse(wave8$ep074_1=="Two weeks",wave8$ahv.amount.chf*2,
                                          ifelse(wave8$ep074_1=="Three months/13 weeks",wave8$ahv.amount.chf/3,
                                                 ifelse(wave8$ep074_1=="Calendar month/4 weeks",wave8$ahv.amount.chf,
                                                        ifelse(wave8$ep074_1=="Six months/26 weeks",wave8$ahv.amount.chf/6,
                                                               ifelse(wave8$ep074_1=="Full year/12 months/52 weeks",wave8$ahv.amount.chf/6, NA))))))
summary(wave8$ahv.amount.chf.month) #this is the AHV-amount variable from the regular EP module, to compare with imputed values

#getting the imputed values for 1st pillar rent
wave8.imp.ypen1.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "ypen1")
names(wave8.imp.ypen1.wide) <- c(
  "mergeid",
  "ypen1.1",
  "ypen1.2",
  "ypen1.3",
  "ypen1.4",
  "ypen1.5"
)
wave8.imp.ypen1.wide$mean.ypen1 <- (wave8.imp.ypen1.wide$ypen1.1+ 
                                      wave8.imp.ypen1.wide$ypen1.2+
                                      wave8.imp.ypen1.wide$ypen1.3+
                                      wave8.imp.ypen1.wide$ypen1.4+
                                      wave8.imp.ypen1.wide$ypen1.5)/5
summary(wave8.imp.ypen1.wide$mean.ypen1) 
plot((wave8.imp.ypen1.wide$mean.ypen1))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.ypen1.wide$mergeid))
wave8$imputed.ahv.amount.year.euro <- wave8.imp.ypen1.wide$mean.ypen1[finder]
summary(wave8$imputed.ahv.amount.year.euro)
wave8$imputed.ahv.amount.month.euro <- wave8$imputed.ahv.amount.year.euro/12
wave8$imputed.ahv.amount.month.chf <- wave8$imputed.ahv.amount.month.euro*euro.chf.exrate
filter <- which(wave8$imputed.ahv.amount.month.chf==0)
filter <- which(wave8$imputed.ahv.amount.month.chf==0 & wave8$age2020.num>65)
length(filter)


#coherence checking between the imputed and the observed values 
print(wave8[,c("mergeid", "ahv.amount.chf.month","imputed.ahv.amount.month.chf")]) 
#Note: relatively high coherence, but I have to find out why there are ~60 who have no AVS rent (survivor or regular)
print(wave8[,c("mergeid", "imputed.ahv.amount.month.chf")]) 
summary(wave8$imputed.ahv.amount.month.chf)
filter <- which(wave8$living.in.nursing.home=="Yes")
check <- wave8[filter,"imputed.ahv.amount.month.chf"]
mean(check)
plot(check)
filter <- which(wave8$living.in.nursing.home=="No")
check <- wave8[filter,"imputed.ahv.amount.month.chf"]
mean(check)

# BVG --------------------------------------------------------------------
# professional pension, 2nd pillar
wave8.imp.ypen2.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "ypen2")
names(wave8.imp.ypen2.wide) <- c(
  "mergeid",
  "ypen2.1",
  "ypen2.2",
  "ypen2.3",
  "ypen2.4",
  "ypen2.5"
)
wave8.imp.ypen2.wide$mean.ypen2 <- (wave8.imp.ypen2.wide$ypen2.1+ 
                                      wave8.imp.ypen2.wide$ypen2.2+
                                      wave8.imp.ypen2.wide$ypen2.3+
                                      wave8.imp.ypen2.wide$ypen2.4+
                                      wave8.imp.ypen2.wide$ypen2.5)/5
summary(wave8.imp.ypen2.wide$mean.ypen2) 
plot((wave8.imp.ypen2.wide$mean.ypen2))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.ypen2.wide$mergeid))
wave8$imputed.bvk.amount.year.euro <- wave8.imp.ypen2.wide$mean.ypen2[finder]
summary(wave8$imputed.bvk.amount.year.euro)
wave8$imputed.bvk.amount.month.euro <- wave8$imputed.bvk.amount.year.euro/12
wave8$imputed.bvk.amount.month.chf <- wave8$imputed.bvk.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.bvk.amount.month.chf)
plot(wave8$imputed.bvk.amount.month.chf)
plot(wave8$gender,wave8$imputed.bvk.amount.month.chf)
hist(wave8$imputed.bvk.amount.month.chf)

# 3rd pillar --------------------------------------------------------------
# private pension / 3rd pillar
wave8.imp.yreg1.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "yreg1")
names(wave8.imp.yreg1.wide) <- c(
  "mergeid",
  "yreg1.1",
  "yreg1.2",
  "yreg1.3",
  "yreg1.4",
  "yreg1.5"
)
wave8.imp.yreg1.wide$mean.yreg1 <- (wave8.imp.yreg1.wide$yreg1.1+ 
                                      wave8.imp.yreg1.wide$yreg1.2+
                                      wave8.imp.yreg1.wide$yreg1.3+
                                      wave8.imp.yreg1.wide$yreg1.4+
                                      wave8.imp.yreg1.wide$yreg1.5)/5
summary(wave8.imp.yreg1.wide$mean.yreg1) 
plot((wave8.imp.yreg1.wide$mean.yreg1))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.yreg1.wide$mergeid))
wave8$imputed.private.pens.amount.year.euro <- wave8.imp.yreg1.wide$mean.yreg1[finder]
summary(wave8$imputed.private.pens.amount.year.euro)
wave8$imputed.private.pens.amount.month.euro <- wave8$imputed.private.pens.amount.year.euro/12
wave8$imputed.private.pens.amount.month.chf <- wave8$imputed.private.pens.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.private.pens.amount.month.chf)
plot(wave8$imputed.private.pens.amount.month.chf)
plot(wave8$gender,wave8$imputed.private.pens.amount.month.chf)
hist(wave8$imputed.private.pens.amount.month.chf)

# Income from work  -------------------------------------------------------

#Salary from employment 
wave8.imp.ydip.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "ydip")
names(wave8.imp.ydip.wide) <- c(
  "mergeid",
  "ydip.1",
  "ydip.2",
  "ydip.3",
  "ydip.4",
  "ydip.5"
)
wave8.imp.ydip.wide$mean.ydip <- (wave8.imp.ydip.wide$ydip.1+ 
                                    wave8.imp.ydip.wide$ydip.2+
                                    wave8.imp.ydip.wide$ydip.3+
                                    wave8.imp.ydip.wide$ydip.4+
                                    wave8.imp.ydip.wide$ydip.5)/5
summary(wave8.imp.ydip.wide$mean.ydip) 
plot((wave8.imp.ydip.wide$mean.ydip))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.ydip.wide$mergeid))
wave8$imputed.salar.employment.amount.year.euro <- wave8.imp.ydip.wide$mean.ydip[finder]
summary(wave8$imputed.salar.employment.amount.year.euro)
wave8$imputed.salar.employment.amount.month.euro <- wave8$imputed.salar.employment.amount.year.euro/12
wave8$imputed.salar.employment.amount.month.chf <- wave8$imputed.salar.employment.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.salar.employment.amount.month.chf)
plot(wave8$imputed.salar.employment.amount.month.chf)

## Salaray from independent activities 
wave8.imp.yind.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "yind")
names(wave8.imp.yind.wide) <- c(
  "mergeid",
  "yind.1",
  "yind.2",
  "yind.3",
  "yind.4",
  "yind.5"
)
wave8.imp.yind.wide$mean.yind <- (wave8.imp.yind.wide$yind.1+ 
                                    wave8.imp.yind.wide$yind.2+
                                    wave8.imp.yind.wide$yind.3+
                                    wave8.imp.yind.wide$yind.4+
                                    wave8.imp.yind.wide$yind.5)/5
summary(wave8.imp.yind.wide$mean.yind) 
plot((wave8.imp.yind.wide$mean.yind))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.yind.wide$mergeid))
wave8$imputed.salar.ind.amount.year.euro <- wave8.imp.yind.wide$mean.yind[finder]
summary(wave8$imputed.salar.ind.amount.year.euro)
wave8$imputed.salar.ind.amount.month.euro <- wave8$imputed.salar.ind.amount.year.euro/12
wave8$imputed.salar.ind.amount.month.chf <- wave8$imputed.salar.ind.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.salar.ind.amount.month.chf)
plot(wave8$imputed.salar.ind.amount.month.chf)

# Adjust income to 66.6% value ----------------------------------------------
#note, only 2/3 (66.6%) of salary counts towards EL calculation 

#calculating variable with all adjusted work-income  
wave8$salary.amount.month.chf <- wave8$imputed.salar.ind.amount.month.chf+wave8$imputed.salar.employment.amount.month.chf
wave8$salary.amount.month.chf.for.el <- wave8$salary.amount.month.chf *0.666
table(  wave8$salary.amount.month.chf.for.el)
summary(  wave8$salary.amount.month.chf.for.el)


# income from rent --------------------------------------------------------
wave8.imp.ysrent.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "ysrent")
names(wave8.imp.ysrent.wide) <- c(
  "mergeid",
  "ysrent.1",
  "ysrent.2",
  "ysrent.3",
  "ysrent.4",
  "ysrent.5"
)
wave8.imp.ysrent.wide$mean.ysrent <- (wave8.imp.ysrent.wide$ysrent.1+ 
                                        wave8.imp.ysrent.wide$ysrent.2+
                                        wave8.imp.ysrent.wide$ysrent.3+
                                        wave8.imp.ysrent.wide$ysrent.4+
                                        wave8.imp.ysrent.wide$ysrent.5)/5
summary(wave8.imp.ysrent.wide$mean.ysrent) 
plot((wave8.imp.ysrent.wide$mean.ysrent))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.ysrent.wide$mergeid))
wave8$imputed.rent.income.amount.year.euro <- wave8.imp.ysrent.wide$mean.ysrent[finder]
summary(wave8$imputed.rent.income.amount.year.euro)
wave8$imputed.rent.income.amount.month.euro <- wave8$imputed.rent.income.amount.year.euro/12
wave8$imputed.rent.income.amount.month.chf <- wave8$imputed.rent.income.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.rent.income.amount.month.chf)
plot(wave8$imputed.rent.income.amount.month.chf)
plot(wave8$gender,wave8$imputed.rent.income.amount.month.chf)
hist(wave8$imputed.rent.income.amount.month.chf)

# # income from interests or dividends ------------------------------------
wave8.imp.ybabsmf.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "ybabsmf")
names(wave8.imp.ybabsmf.wide) <- c(
  "mergeid",
  "ybabsmf.1",
  "ybabsmf.2",
  "ybabsmf.3",
  "ybabsmf.4",
  "ybabsmf.5"
)
wave8.imp.ybabsmf.wide$mean.ybabsmf <- (wave8.imp.ybabsmf.wide$ybabsmf.1+ 
                                          wave8.imp.ybabsmf.wide$ybabsmf.2+
                                          wave8.imp.ybabsmf.wide$ybabsmf.3+
                                          wave8.imp.ybabsmf.wide$ybabsmf.4+
                                          wave8.imp.ybabsmf.wide$ybabsmf.5)/5
summary(wave8.imp.ybabsmf.wide$mean.ybabsmf) 
plot((wave8.imp.ybabsmf.wide$mean.ybabsmf))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.ybabsmf.wide$mergeid))
wave8$imputed.interests.dividends.amount.year.euro <- wave8.imp.ybabsmf.wide$mean.ybabsmf[finder]
summary(wave8$imputed.interests.dividends.amount.year.euro)
wave8$imputed.interests.dividends.amount.month.euro <- wave8$imputed.interests.dividends.amount.year.euro/12
wave8$imputed.interests.dividends.amount.month.chf <- wave8$imputed.interests.dividends.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.interests.dividends.amount.month.chf)
plot(wave8$imputed.interests.dividends.amount.month.chf)
plot(wave8$gender,wave8$imputed.interests.dividends.amount.month.chf)
hist(wave8$imputed.interests.dividends.amount.month.chf)

# Income from non.responding.partner --------------------------------------
wave8.imp.yincnrp.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "yincnrp")
names(wave8.imp.yincnrp.wide) <- c(
  "mergeid",
  "yincnrp.1",
  "yincnrp.2",
  "yincnrp.3",
  "yincnrp.4",
  "yincnrp.5"
)
wave8.imp.yincnrp.wide$mean.yincnrp <- (wave8.imp.yincnrp.wide$yincnrp.1+ 
                                          wave8.imp.yincnrp.wide$yincnrp.2+
                                          wave8.imp.yincnrp.wide$yincnrp.3+
                                          wave8.imp.yincnrp.wide$yincnrp.4+
                                          wave8.imp.yincnrp.wide$yincnrp.5)/5
summary(wave8.imp.yincnrp.wide$mean.yincnrp) 
plot((wave8.imp.yincnrp.wide$mean.yincnrp))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.yincnrp.wide$mergeid))
wave8$imputed.income.non.responding.partner.amount.year.euro <- wave8.imp.yincnrp.wide$mean.yincnrp[finder]
summary(wave8$imputed.income.non.responding.partner.amount.year.euro)
wave8$imputed.income.non.responding.partner.amount.month.euro <- wave8$imputed.income.non.responding.partner.amount.year.euro/12
wave8$imputed.income.non.responding.partner.amount.month.chf <- wave8$imputed.income.non.responding.partner.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.income.non.responding.partner.amount.month.chf)
plot(wave8$imputed.income.non.responding.partner.amount.month.chf)
plot(wave8$gender,wave8$imputed.income.non.responding.partner.amount.month.chf)
hist(wave8$imputed.income.non.responding.partner.amount.month.chf)

# Calculate additional income from "Vermögensverzehr"--------------------------------------
table(wave8$living.in.nursing.home)
table(wave8$el.eligibility.type)
levels(wave8$el.eligibility.type)

wave8$vermoegensverzehr.year. <- NA

filter <- which(wave8$el.eligibility.type=="married.both.home"  & wave8$adjusted.household.net.worth>60000)  
length(filter)
wave8$vermoegensverzehr.year[filter] <- (wave8$adjusted.household.net.worth[filter]-60000)*0.1
hist(wave8$vermoegensverzehr.year)

filter <- which(wave8$el.eligibility.type=="married.both.nursing.home"  & wave8$adjusted.household.net.worth>60000)  
length(filter)
wave8$vermoegensverzehr.year[filter] <- ((wave8$adjusted.household.net.worth[filter]-60000)*0.2)
hist(wave8$vermoegensverzehr.year)

filter <- which(wave8$el.eligibility.type=="solo.tarif.own.home"  & wave8$adjusted.household.net.worth>37500)  
length(filter)
wave8$vermoegensverzehr.year[filter] <- (wave8$adjusted.household.net.worth[filter]-37500)*0.1

filter <- which(wave8$el.eligibility.type=="solo.tarif.nursing.home"  & wave8$adjusted.household.net.worth>37500)  
length(filter)
wave8$vermoegensverzehr.year[filter] <- (wave8$adjusted.household.net.worth[filter]-37500)*0.2

wave8$vermoegensverzehr.year[is.na(wave8$vermoegensverzehr.year)] <- 0

summary(wave8$vermoegensverzehr.year)
hist(wave8$vermoegensverzehr.year)

wave8$vermoegensverzehr.month <- wave8$vermoegensverzehr.year/12
summary(  wave8$vermoegensverzehr.month)

# create unified income variable for each typology ------------------------------------------

#checking if there are any NAs in those composite variables that could mess up the overal calculation
summary( wave8$imputed.ahv.amount.month.chf) #...none
summary( wave8$imputed.bvk.amount.month.chf)#...none
summary(  wave8$imputed.private.pens.amount.month.chf)#...none
summary( wave8$salary.amount.month.chf.for.el)#...none
summary( wave8$imputed.rent.income.amount.month.chf)#...none
summary(  wave8$imputed.interests.dividends.amount.month.chf)#...none
summary(wave8$vermoegensverzehr.month)#...none

levels(wave8$el.eligibility.type)   


filter <- which(wave8$el.eligibility.type == "married.both.home") 
wave8$incomes.married.both.home.chf.month <- NA
wave8$incomes.married.both.home.chf.month[filter] <- 
  wave8$imputed.ahv.amount.month.chf[filter]+
  wave8$imputed.bvk.amount.month.chf[filter]+
  wave8$imputed.private.pens.amount.month.chf[filter]+
  wave8$salary.amount.month.chf.for.el[filter]+
  wave8$imputed.rent.income.amount.month.chf[filter]+
  wave8$imputed.interests.dividends.amount.month.chf[filter]+
  wave8$vermoegensverzehr.month[filter] +
  wave8$imputed.income.non.responding.partner.amount.month.chf[filter] 
summary(wave8$incomes.married.both.home.chf.month)


filter<- which(wave8$el.eligibility.type == "married.both.nursing.home") 
wave8$incomes.married.both.nursing.home.chf.month <- NA
wave8$incomes.married.both.nursing.home.chf.month[filter] <- 
  wave8$imputed.ahv.amount.month.chf[filter]+
  wave8$imputed.bvk.amount.month.chf[filter]+
  wave8$imputed.private.pens.amount.month.chf[filter]+
  wave8$salary.amount.month.chf.for.el[filter]+
  wave8$imputed.rent.income.amount.month.chf[filter]+
  wave8$imputed.interests.dividends.amount.month.chf[filter]+
  wave8$vermoegensverzehr.month[filter] #note here we don't consider the partner's income
summary(wave8$incomes.married.both.nursing.home.chf.month)


filter<- which(wave8$el.eligibility.type == "solo.tarif.nursing.home") 
wave8$incomes.solo.tarif.nursing.home.chf.month <- NA
wave8$incomes.solo.tarif.nursing.home.chf.month[filter] <- 
  wave8$imputed.ahv.amount.month.chf[filter]+
  wave8$imputed.bvk.amount.month.chf[filter]+
  wave8$imputed.private.pens.amount.month.chf[filter]+
  wave8$salary.amount.month.chf.for.el[filter]+
  wave8$imputed.rent.income.amount.month.chf[filter]+
  wave8$imputed.interests.dividends.amount.month.chf[filter]+
  wave8$vermoegensverzehr.month[filter] #note, this is already considering the special nursing home verzehr 
summary(wave8$incomes.solo.tarif.nursing.home.chf.month)

filter<- which(wave8$el.eligibility.type == "solo.tarif.own.home") 
wave8$incomes.solo.tarif.own.home.chf.month <- NA
wave8$incomes.solo.tarif.own.home.chf.month[filter] <- 
  wave8$imputed.ahv.amount.month.chf[filter]+
  wave8$imputed.bvk.amount.month.chf[filter]+
  wave8$imputed.private.pens.amount.month.chf[filter]+
  wave8$salary.amount.month.chf.for.el[filter]+
  wave8$imputed.rent.income.amount.month.chf[filter]+
  wave8$imputed.interests.dividends.amount.month.chf[filter]+
  wave8$vermoegensverzehr.month[filter] #10% verzehr as usual when living at home 
summary(wave8$incomes.solo.tarif.own.home.chf.month)

#NOTE: AT THIS STAGE WE HAVE TYPOLOGY-SPECIFIC VARIABLES (WHICH ARE BASICALLY MARITAL-STATUS-SPECIFIC) INCOME LEVELS THAT HAVE TO BE TREATED SPECIFICALLY AS WELL! 

# Coherence checking ------------------------------------------------------
#first, we compare the retained income variable with the aggregated variable from the imputation file 
#the one we calculated should be lower, as we only retain 70% of work incomes and only count other people's or partners income for married couples
# one-shot question from imputation file  ---------------------------------
#here, we compare the retained income variables with the one-shot question for household income 
#aggregated hhinc variable 
wave8.imp.thinc.wide <-
  dcast(wave8.imp, mergeid ~ implicat, value.var = "thinc")
names(wave8.imp.thinc.wide) <- c(
  "mergeid",
  "thinc.1",
  "thinc.2",
  "thinc.3",
  "thinc.4",
  "thinc.5"
)
wave8.imp.thinc.wide$mean.thinc <- (wave8.imp.thinc.wide$thinc.1+ 
                                      wave8.imp.thinc.wide$thinc.2+
                                      wave8.imp.thinc.wide$thinc.3+
                                      wave8.imp.thinc.wide$thinc.4+
                                      wave8.imp.thinc.wide$thinc.5)/5
summary(wave8.imp.thinc.wide$mean.thinc) 
plot((wave8.imp.thinc.wide$mean.thinc))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.thinc.wide$mergeid))
wave8$imputed.household.net.worth.imputed.euro.amount.year.euro <- wave8.imp.thinc.wide$mean.thinc[finder]
summary(wave8$imputed.household.net.worth.imputed.euro.amount.year.euro)
wave8$imputed.household.net.worth.imputed.euro.amount.month.euro <- wave8$imputed.household.net.worth.imputed.euro.amount.year.euro/12
wave8$aggregated.total.hhincome <- wave8$imputed.household.net.worth.imputed.euro.amount.month.euro*euro.chf.exrate
summary(wave8$aggregated.total.hhincome)
plot(wave8$aggregated.total.hhincome)
plot(wave8$gender,wave8$aggregated.total.hhincome)
hist(wave8$aggregated.total.hhincome)
#as expected, our variable is quite lower, but this is justified and thus coherent  
 

### Part 4: Expenses (aka "Anrechenbare Ausgaben") ###

# inpatient-care / stationär costs  ---------------------------------------------
# care.costs.station

#The following variables are not available for wave 8, therefore, I take the values from wave 6 (the imputation file from wave 7 doesn't seem correct)

wave8.imp.inpat8.wide <-
  dcast(wave6.imp, mergeid ~ implicat, value.var = "inpat6")
names(wave8.imp.inpat8.wide) <- c(
  "mergeid",
  "inpat8.1",
  "inpat8.2",
  "inpat8.3",
  "inpat8.4",
  "inpat8.5"
)
wave8.imp.inpat8.wide$mean.inpat8 <- (wave8.imp.inpat8.wide$inpat8.1+ 
                                        wave8.imp.inpat8.wide$inpat8.2+
                                        wave8.imp.inpat8.wide$inpat8.3+
                                        wave8.imp.inpat8.wide$inpat8.4+
                                        wave8.imp.inpat8.wide$inpat8.5)/5
summary(wave8.imp.inpat8.wide$mean.inpat8) 
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.inpat8.wide$mergeid))
wave8$imputed.care.costs.station.amount.year.euro <- wave8.imp.inpat8.wide$mean.inpat8[finder]
summary(wave8$imputed.care.costs.station.amount.year.euro)
wave8$imputed.care.costs.station.amount.month.euro <- wave8$imputed.care.costs.station.amount.year.euro/12
wave8$imputed.care.costs.station.amount.month.chf <- wave8$imputed.care.costs.station.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.care.costs.station.amount.month.chf)
plot(wave8$imputed.care.costs.station.amount.month.chf)
plot(wave8$gender,wave8$imputed.care.costs.station.amount.month.chf)
hist(wave8$imputed.care.costs.station.amount.month.chf)  


wave8$imputed.care.costs.station.amount.month.chf[is.na(wave8$imputed.care.costs.station.amount.month.chf)] <- 0 
summary(wave8$imputed.care.costs.station.amount.month.chf)

# care.costs.ambulant-----------------------------------------------------
wave8.imp.outpa8.wide <-
  dcast(wave6.imp, mergeid ~ implicat, value.var = "outpa6")
names(wave8.imp.outpa8.wide) <- c(
  "mergeid",
  "outpa8.1",
  "outpa8.2",
  "outpa8.3",
  "outpa8.4",
  "outpa8.5"
)
wave8.imp.outpa8.wide$mean.outpa8 <- (wave8.imp.outpa8.wide$outpa8.1+ 
                                        wave8.imp.outpa8.wide$outpa8.2+
                                        wave8.imp.outpa8.wide$outpa8.3+
                                        wave8.imp.outpa8.wide$outpa8.4+
                                        wave8.imp.outpa8.wide$outpa8.5)/5
summary(wave8.imp.outpa8.wide$mean.outpa8) 
plot((wave8.imp.outpa8.wide$mean.outpa8))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.outpa8.wide$mergeid))
wave8$imputed.care.costs.ambulant.amount.year.euro <- wave8.imp.outpa8.wide$mean.outpa8[finder]
summary(wave8$imputed.care.costs.ambulant.amount.year.euro)
wave8$imputed.care.costs.ambulant.amount.month.euro <- wave8$imputed.care.costs.ambulant.amount.year.euro/12
wave8$imputed.care.costs.ambulant.amount.month.chf <- wave8$imputed.care.costs.ambulant.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.care.costs.ambulant.amount.month.chf)
plot(wave8$imputed.care.costs.ambulant.amount.month.chf)
plot(wave8$gender,wave8$imputed.care.costs.ambulant.amount.month.chf)
hist(wave8$imputed.care.costs.ambulant.amount.month.chf)  

wave8$imputed.care.costs.ambulant.amount.month.chf[is.na(wave8$imputed.care.costs.ambulant.amount.month.chf)] <- 0 
summary(wave8$imputed.care.costs.ambulant.amount.month.chf)

# drug.costs
wave8.imp.drugs8.wide <-
  dcast(wave6.imp, mergeid ~ implicat, value.var = "drugs6")
names(wave8.imp.drugs8.wide) <- c(
  "mergeid",
  "drugs8.1",
  "drugs8.2",
  "drugs8.3",
  "drugs8.4",
  "drugs8.5"
)
wave8.imp.drugs8.wide$mean.drugs8 <- (wave8.imp.drugs8.wide$drugs8.1+ 
                                        wave8.imp.drugs8.wide$drugs8.2+
                                        wave8.imp.drugs8.wide$drugs8.3+
                                        wave8.imp.drugs8.wide$drugs8.4+
                                        wave8.imp.drugs8.wide$drugs8.5)/5

summary(wave8.imp.drugs8.wide$mean.drugs8) 
plot((wave8.imp.drugs8.wide$mean.drugs8))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.drugs8.wide$mergeid))
wave8$imputed.drug.costs.amount.year.euro <- wave8.imp.drugs8.wide$mean.drugs8[finder]
summary(wave8$imputed.drug.costs.amount.year.euro)
wave8$imputed.drug.costs.amount.month.euro <- wave8$imputed.drug.costs.amount.year.euro/12
wave8$imputed.drug.costs.amount.month.chf <- wave8$imputed.drug.costs.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.drug.costs.amount.month.chf)
plot(wave8$imputed.drug.costs.amount.month.chf)
plot(wave8$gender,wave8$imputed.drug.costs.amount.month.chf)
hist(wave8$imputed.drug.costs.amount.month.chf)  

wave8$imputed.drug.costs.amount.month.chf[is.na(wave8$imputed.drug.costs.amount.month.chf)] <- 0 
summary(wave8$imputed.drug.costs.amount.month.chf)

# nursinghome.costs -------------------------------------------------------
wave8.imp.nurs8.wide <-
  dcast(wave6.imp, mergeid ~ implicat, value.var = "nurs6")
names(wave8.imp.nurs8.wide) <- c(
  "mergeid",
  "nurs8.1",
  "nurs8.2",
  "nurs8.3",
  "nurs8.4",
  "nurs8.5"
)

wave8.imp.nurs8.wide$mean.nurs8 <- (wave8.imp.nurs8.wide$nurs8.1+ 
                                      wave8.imp.nurs8.wide$nurs8.2+
                                      wave8.imp.nurs8.wide$nurs8.3+
                                      wave8.imp.nurs8.wide$nurs8.4+
                                      wave8.imp.nurs8.wide$nurs8.5)/5
summary(wave8.imp.nurs8.wide$mean.nurs8) 
plot((wave8.imp.nurs8.wide$mean.nurs8))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.nurs8.wide$mergeid))
wave8$imputed.nursinghome.costs.amount.year.euro <- wave8.imp.nurs8.wide$mean.nurs8[finder]
summary(wave8$imputed.nursinghome.costs.amount.year.euro)
wave8$imputed.nursinghome.costs.amount.month.euro <- wave8$imputed.nursinghome.costs.amount.year.euro/12
wave8$imputed.nursinghome.costs.amount.month.chf <- wave8$imputed.nursinghome.costs.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.nursinghome.costs.amount.month.chf)
plot(wave8$imputed.nursinghome.costs.amount.month.chf)
plot(wave8$gender,wave8$imputed.nursinghome.costs.amount.month.chf)
hist(wave8$imputed.nursinghome.costs.amount.month.chf)  
plot(wave8$living.in.nursing.home, log(wave8$imputed.nursinghome.costs.amount.month.chf))

wave8$imputed.nursinghome.costs.amount.month.chf[is.na(wave8$imputed.nursinghome.costs.amount.month.chf)] <- 0 
summary(wave8$imputed.nursinghome.costs.amount.month.chf)


# other.aid.costs ---------------------------------------------------------
wave8.imp.aapt8.wide <-
  dcast(wave6.imp, mergeid ~ implicat, value.var = "aapt6")
names(wave8.imp.aapt8.wide) <- c(
  "mergeid",
  "aapt8.1",
  "aapt8.2",
  "aapt8.3",
  "aapt8.4",
  "aapt8.5"
)
wave8.imp.aapt8.wide$mean.aapt8 <- (wave8.imp.aapt8.wide$aapt8.1+ 
                                      wave8.imp.aapt8.wide$aapt8.2+
                                      wave8.imp.aapt8.wide$aapt8.3+
                                      wave8.imp.aapt8.wide$aapt8.4+
                                      wave8.imp.aapt8.wide$aapt8.5)/5
summary(wave8.imp.aapt8.wide$mean.aapt8) 
plot((wave8.imp.aapt8.wide$mean.aapt8))
finder <- match(as.character(wave8$mergeid),as.character(wave8.imp.aapt8.wide$mergeid))
wave8$imputed.other.aid.costs.amount.year.euro <- wave8.imp.aapt8.wide$mean.aapt8[finder]
summary(wave8$imputed.other.aid.costs.amount.year.euro)
wave8$imputed.other.aid.costs.amount.month.euro <- wave8$imputed.other.aid.costs.amount.year.euro/12
wave8$imputed.other.aid.costs.amount.month.chf <- wave8$imputed.other.aid.costs.amount.month.euro*euro.chf.exrate
summary(wave8$imputed.other.aid.costs.amount.month.chf)
plot(wave8$imputed.other.aid.costs.amount.month.chf)
plot(wave8$gender,wave8$imputed.other.aid.costs.amount.month.chf)
hist(wave8$imputed.other.aid.costs.amount.month.chf)  

wave8$imputed.other.aid.costs.amount.month.chf[is.na(wave8$imputed.other.aid.costs.amount.month.chf)] <- 0 
summary(wave8$imputed.other.aid.costs.amount.month.chf)

# calculate overall care and treatment cost variable -----------------------------------------
wave8$total.care.treatment.costs <-  wave8$imputed.care.costs.station.amount.month.chf+
  wave8$imputed.care.costs.ambulant.amount.month.chf+
  wave8$imputed.drug.costs.amount.month.chf+
  wave8$imputed.nursinghome.costs.amount.month.chf+
  wave8$imputed.other.aid.costs.amount.month.chf

plot(wave8$total.care.treatment.costs)

#korrektur gemäss heim- oder zuhause 

# corrected variable for couples ------------------------------------------
#note, here I'm solving the problem that for couples (reminder: for whom EL is calculated at the couple-level) all expenses count towards EL
#hence, i have to find the partner in the sample, and take the expenses into consideration (if possible - for nonresponding partners not possible obvs )
wave8$adjusted.total.care.treatment.costs <- NA
count.no.couple.match <- 0
no.couple.match.index <- NA
for (i in 1:nrow(wave8)) {
  if (wave8$married.bn[i]==1) {
    print(i)
    print("is couple")
    z <- wave8$mergeid[i] #the individual in question 
    z
    x <- wave8$coupleid[i] # the couple identifier
    x
    filter <- which(wave8$coupleid==x) #look for all individuals with the same couple indentifier in the main (wave8) file (therefore finding the partner)
    temp <- as.data.frame(wave8[filter,c("mergeid", "coupleid", "total.care.treatment.costs")]) #stick the necessary infos in a temp df
    temp
    if (nrow(temp)<2) #for those, where there is no matching person with the same couple id in the file, we just keep the individual amount  
    {print("no match")
      count.no.couple.match<-count.no.couple.match+1
      no.couple.match.index <- c(no.couple.match.index,i)
      wave8$adjusted.total.care.treatment.costs[i]<- wave8$total.care.treatment.costs[i]}else {
        filter2 <- which(as.character(temp$mergeid) != as.character(z))
        wave8$adjusted.total.care.treatment.costs[i] <- temp[filter2,"total.care.treatment.costs"]+wave8$total.care.treatment.costs[i]}
  } else {print("No couple")
    wave8$adjusted.total.care.treatment.costs[i]<- wave8$total.care.treatment.costs[i]
  }
}

#check those who have no couple id 
wave8[ no.couple.match.index, c("mergeid", "coupleid", "age2020","total.care.treatment.costs", "adjusted.total.care.treatment.costs")]
summary(wave8$adjusted.total.care.treatment.costs)

# variable with fixed expenses relative to EL-typologie (nursing home or not, etc.) ----------------------------------------------

levels(wave8$el.eligibility.type)

wave8$fixed.expenses.married.both.home <- NA
filter <- which(wave8$el.eligibility.type == "married.both.home") 
wave8$fixed.expenses.married.both.home[filter] <- 
  15000 / 12 + #mietkosten
  29175 / 12 + #grundbedarf
  2 * (224.8) #durchschnittliche KVG prämie 2020)
summary(wave8$fixed.expenses.married.both.home)

wave8$fixed.expenses.married.both.nursing.home <- NA
filter<- which(wave8$el.eligibility.type == "married.both.nursing.home") #de facto solo-tarif 
wave8$fixed.expenses.married.both.nursing.home[filter] <- 
  ((190*365)/12)  + #Anrechenbare Heimtaxe 
  370 + #persönliche auslagen 
  (224.8) #durchschnittliche KVG prämie 2020)
summary(wave8$fixed.expenses.married.both.nursing.home)

wave8$fixed.expenses.solo.tarif.nursing.home<-NA
filter<- which(wave8$el.eligibility.type == "solo.tarif.nursing.home") 
wave8$fixed.expenses.solo.tarif.nursing.home[filter] <- 
  (190)*30.5  + #Anrechenbare Heimtaxe 
  370 + #persönliche auslagen 
  (224.8) #durchschnittliche KVG prämie 2020)
summary(wave8$fixed.expenses.solo.tarif.nursing.home)

wave8$fixed.expenses.solo.tarif.own.home<-NA
filter <- which(wave8$el.eligibility.type == "solo.tarif.own.home") 
wave8$fixed.expenses.solo.tarif.own.home[filter] <- 
  13200 / 12 + #mietkosten
  19450 / 12 + #grundbedarf
  224.8 #durchschnittliche KVG prämie 2020)
summary(wave8$fixed.expenses.solo.tarif.own.home)


# Overall expense variable ("Anrechenbare Ausgaben")------------------------------------------------


levels(wave8$el.eligibility.type)

wave8$total.expenses.married.both.home <- NA
filter <- which(wave8$el.eligibility.type == "married.both.home") 
wave8$total.expenses.married.both.home[filter] <- 
  wave8$fixed.expenses.married.both.home[filter]+
  wave8$adjusted.total.care.treatment.costs[filter]
summary(wave8$total.expenses.married.both.home)

wave8$total.expenses.married.both.nursing.home <- NA
filter<- which(wave8$el.eligibility.type == "married.both.nursing.home") 
wave8$total.expenses.married.both.nursing.home[filter] <- 
  wave8$fixed.expenses.married.both.nursing.home[filter]+
  wave8$imputed.care.costs.ambulant.amount.month.chf[filter]+
  wave8$imputed.drug.costs.amount.month.chf[filter]+
  wave8$imputed.other.aid.costs.amount.month.chf[filter]
summary(wave8$total.expenses.married.both.nursing.home)

wave8$total.expenses.solo.tarif.nursing.home<-NA
filter<- which(wave8$el.eligibility.type == "solo.tarif.nursing.home") 
wave8$total.expenses.solo.tarif.nursing.home[filter] <- 
  wave8$fixed.expenses.solo.tarif.nursing.home[filter]+
  wave8$imputed.care.costs.ambulant.amount.month.chf[filter]+
  wave8$imputed.drug.costs.amount.month.chf[filter]+
  wave8$imputed.other.aid.costs.amount.month.chf[filter]  
summary(wave8$total.expenses.solo.tarif.nursing.home)

wave8$total.expenses.solo.tarif.own.home<- NA
filter <- which(wave8$el.eligibility.type == "solo.tarif.own.home") 
wave8$total.expenses.solo.tarif.own.home[filter] <- 
  wave8$fixed.expenses.solo.tarif.own.home[filter]+
  wave8$total.care.treatment.costs[filter]
summary(wave8$total.expenses.solo.tarif.own.home)

### Part 5: Comparison between income and expenses ###

wave8$difference.incomes.expenses <- NA

filter <- which(wave8$el.eligibility.type == "married.both.home") 
wave8$difference.incomes.expenses[filter] <- wave8$incomes.married.both.home.chf.month[filter]-wave8$total.expenses.married.both.home[filter]

filter<- which(wave8$el.eligibility.type == "married.both.nursing.home") 
wave8$difference.incomes.expenses[filter] <- wave8$incomes.married.both.nursing.home.chf.month[filter] -wave8$total.expenses.married.both.nursing.home[filter]

filter<- which(wave8$el.eligibility.type == "solo.tarif.nursing.home") 
wave8$difference.incomes.expenses[filter] <- wave8$incomes.solo.tarif.nursing.home.chf.month[filter]-wave8$total.expenses.solo.tarif.nursing.home[filter]

filter <- which(wave8$el.eligibility.type == "solo.tarif.own.home") 
wave8$difference.incomes.expenses[filter] <- wave8$incomes.solo.tarif.own.home.chf.month[filter]-wave8$total.expenses.solo.tarif.own.home[filter]

summary(wave8$difference.incomes.expenses)
plot(wave8$difference.incomes.expenses)

wave8$expenses.bigger.than.incomes <- ifelse(wave8$difference.incomes.expenses<0, 1,0)
table(  wave8$expenses.bigger.than.incomes)

#=> this has to be done specifically for el typology (which factually is a differentiation between solo and couple-tarif) 

### PART 6: Income Sources ###

#income sources from first pillar
#avs
levels(wave8$ep671d1)
table(wave8$ep671d1)
wave8$has.reg.ahv.pension <- memisc::recode(wave8$ep671d1, 
                                            1 <- "Selected", 
                                            0 <- c("Refusal","Don't know","Not selected"), 
                                            otherwise=NA)
table(wave8$has.reg.ahv.pension)
plot( wave8$age2020, wave8$has.reg.ahv.pension)
prop.table(table(wave8$has.reg.ahv.pension))#coherence check


#el
table(wave8$ep671d2)
wave8$has.EL <- memisc::recode(wave8$ep671d2, 
                               1 <- "Selected", 
                               0 <- c("Refusal","Don't know","Not selected"), 
                               otherwise=NA)
table(wave8$has.EL)
table(wave8$living.in.nursing.home,wave8$has.EL)
prop.table(table(wave8$has.EL))

prop.table(table(wave8$living.in.nursing.home,wave8$has.EL),1)

prop.table(table(wave8$has.EL))

prop.table(wtd.table(x = wave8$has.EL, weights = wave8$cciw_w8_main))

plot(wave8$living.in.nursing.home,wave8$has.EL)

#witwenrente
table(wave8$ep671d9)
wave8$has.survivors.pension <- memisc::recode(wave8$ep671d9, 
                                              1 <- "Selected", 
                                              0 <- c("Refusal","Don't know","Not selected"), 
                                              otherwise=NA)
table(wave8$has.survivors.pension )

wave8$income.from.1st.pillar <- ifelse(wave8$has.survivors.pension==1 | wave8$has.reg.ahv.pension==1, 1,0)
prop.table(table(wave8$income.from.1st.pillar))#coherence check
prop.table(table(wave8$income.from.1st.pillar, wave8$gender),2)#coherence check

#sozialleistung
table(wave8$ep671d13)
wave8$has.social.welfare<- memisc::recode(wave8$ep671d13, 
                                          1 <- "Selected", 
                                          0 <- c("Refusal","Don't know","Not selected"), 
                                          otherwise=NA)
table(wave8$has.social.welfare )

### PART 7: Final NTU variable ###

# subsetting for only pensioners ------------------------------------------

wave8 <- subset(wave8, wave8$age2020.num>64)



# little fix regarding people who are 65 but no ahv -----------------------

summary(wave8$has.reg.ahv.pension)
prop.table(table(  wave8$has.reg.ahv.pension)) #in our sample it is at 91.7%
filter <- which(wave8$has.reg.ahv.pension==0)
wave8[filter,c("int_month","mobirth","salary.amount.month.chf")] 

filter <- which(wave8$has.reg.ahv.pension==0)
length(filter)

wave8 <- wave8[-filter,]

# calculate eligibility ---------------------------------------------------

wave8$eligible.for.EL <- ifelse(wave8$expenses.bigger.than.incomes==1,1,0)
prop.table(table(wave8$eligible.for.EL)) 

wave8$NTU.EL <- ifelse(wave8$eligible.for.EL==1 & wave8$has.EL==0,1,0)

prop.table(table(wave8$NTU.EL)) #unweighted values 

prop.table(table(wave8$gender, wave8$NTU.EL),1) 
prop.table(table(wave8$living.in.nursing.home, wave8$NTU.EL),1) 

plot(wave8$age2020, wave8$NTU.EL)


# Create the new weighting variable ---------------------------------------

# adjusting the provided sample weights using the methodology provided to me by the SHARE Team at FORS Switzerland 
# Accordingly, for each person the new weight is calculated by dividing the old weight by the sum of all old weights in the new sample and multiplying it with the new sample size.


sum.old.weights.new.sample <- sum(wave8$cciw_w8_main)
sum.old.weights.new.sample 

new.population.size <- 1524315


wave8$adjusted.cciw_w8 <- wave8$cciw_w8_main/sum.old.weights.new.sample*new.population.size


#coherence checking 
print(wave8$cciw_w8_main[1:3])
print(wave8$adjusted.cciw_w8[1:3])

sum(wave8$adjusted.cciw_w8)


wave8NTU <- as.vector(prop.table(wtd.table(x = wave8$NTU.EL, weights = wave8$cciw_w8_main, useNA = NULL))) # this is the final weighted estimation 
save(wave8NTU, file = "wave8_NTU.Rdata")


#NTU gender
wave8NTU_gender <- as.vector(prop.table(wtd.table(x = wave8$NTU.EL , y= wave8$gender, weights = wave8$cciw_w8_main, useNA = NULL),2))
save(wave8NTU_gender, file = "wave8_NTU_gender.Rdata")

#EL-eligibility gender

wave8NTU_eligibility <- as.vector(prop.table(wtd.table(x = wave8$eligible.for.EL, y = wave8$gender, weights = wave8$cciw_w8_main, useNA = NULL),2))
save(wave8NTU_eligibility, file = "wave8_NTU_eligibility.Rdata")

#NTU home v. nursing home 
wave8NTU_hometype <- as.vector(prop.table(wtd.table(x = wave8$NTU.EL, y = wave8$el.eligibility.type, weights = wave8$cciw_w8_main, useNA = NULL),2))
save(wave8NTU_hometype, file = "wave8_NTU_hometype.Rdata")

prop.table(wtd.table(x = wave8$NTU.EL , y= wave8$living.in.nursing.home, weights = wave8$cciw_w8_main, useNA = NULL),2)

wave8_mutated <- wave8

save(wave8_mutated, file = "wave8_mutated.Rdata")

