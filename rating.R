##################################################
#Rim Weighting, written by Yifei Liu 11/1/2016
#Load the library and raw data
##################################################

library(anesrake)

setwd('C:/Users/yifei.liu/Desktop/XXXXX/')
read.data <- "Final Data_all_countries_unweighted.csv"
output.data <- "C:/Users/yifei.liu/Desktop/weighting.csv"

raw.data <- read.csv(file=read.data, sep=",", dec=".")

#data cleaning step
for(i in 1:nrow(raw.data)){
  if (is.na(raw.data$vCategoryFinalc2[i]&&raw.data$vCategoryFinalc2[i] < 9)){
    raw.data$product[i] = raw.data$vCategoryFinalc1[i]
   } else {
   raw.data$product[i]=raw.data$vCategoryFinalc2[i]
   }}
for(i in 1:nrow(raw.data)){
  if (raw.data$vAgeGroup[i]<5){
   raw.data$agegroup[i] = raw.data$vAgeGroup[i]
  } else {
    raw.data$agegroup[i] = 5
  }}

chn.data<-as.data.frame(raw.data[raw.data$S3 == 30,])

#specify the varlist
varlist <- c("product", "agegroup", "S2")

#specify the target marginal distribution 
AGEGROUPtarg<- c("1"=0.14, "2"=0.2, "3"=0.21, "4"=0.19, "5"=0.27)
S2targ<- c("male"=.56, "female"=.44)
PRODUCTtarg<- c("1"=0.133, "2"=0.112, "3"=0.119,  "4"=0.123, "5"=0.124, "6"=.125, "7"=0.124, "8"=0.084, "9"=0.057)

#before rim weighting, coerce variables to be factor data type
for (i in 1:length(varlist)){
   chn.data[varlist[i]] <- as.factor(chn.data[[varlist[i]]])
}

#make the target and raw data variable having same level label
levels(chn.data$S2) <- names(S2targ)
levels(chn.data$product)<- names(PRODUCTtarg)
levels(chn.data$agegroup)<- names(AGEGROUPtarg)
#create target list, name of target variable should be same to varlist
targets <- list(PRODUCTtarg, AGEGROUPtarg, S2targ)
names(targets) <- varlist

#perform rim weighting, change verbose to close iteration history, change maxit to change maximum iteration number
#change convcrit to specify the converge criterion
#call weight are forced to be less than 5 by default, change the default using option cap=
rakeout <- anesrake(targets, chn.data, caseid = chn.data$uuid, verbose = T, maxit = 50, convcrit = 0.001, cap = 50)

#summarize and plot weight
summary(rakeout)
plot(rakeout$weightvec)

#output result
output<- cbind(rakeout$dataframe$UUID, rakeout$weightvec)
write.csv(output, output.data)








