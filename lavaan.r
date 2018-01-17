#########################################################################################
#Set up model, install package "lavaan", include lavaan_function.r as source            #
#Please check whether source directory is correct                                       #
#########################################################################################

library("lavaan.survey")
library("lavaan")
library("survey")
source("C:/Users/yifei.liu/Desktop/R code/lavaan_function.r")


#########################################################################################
#Please modify following line to specify data file                                      #
#Read the data that is ready for SEM model                                              #
#clean raw data and make sure that sampling weight is in the datafile and the variable  #
#label is "weight"                                                                      #
#########################################################################################

read.data <- "C:/Users/yifei.liu/Desktop/halo_SEM.csv"
halo<- read.csv(file=read.data, sep=",", dec=".")

forza<- subset(forza, is.na(forza$q10_1) == F)

#########################################################################################
#Specify SEM model structure and call function to perform SEM                           #
# Please don't change model name                                                        #
# Syntax:                                                                               #
# dv ~ lv                                                                               #
# lv =~ ov                                                                              #
#########################################################################################

model <- "      q10_1 ~ f
                f=~ f1 + f2 + f3
                f1=~ q18_1 + q18_4 + q18_5 + q18_8 + q18_12 + q18_13 + q18_15 + q20_1 +q20_2 + q20_5 + q20_6 +q20_7 +q20_8 +q20_11
                f2=~ q18_6 + q18_7 + q18_9 + q18_10 + q18_14 + q19_1 + q19_2 + q19_4 +q19_5 + q19_6 + q20_9
                f3=~ q18_2 + q18_3 + q18_11 + q19_3 + q20_3 +q20_4 +q20_10"

model<- " reachcompdv ~f
         f =~ f1 + f2+ f3 
         f1=~ vQ23r1c1 + vQ23r2c1 + vQ23r3c1 + vQ23r5c1 + vQ23r6c1 + vQ23r7c1+ vQ23r17c1 + vQ23r18c1 + vQ23r22c1 + vQ23r24c1
         f2=~ vQ23r4c1 + vQ23r8c1 + vQ23r9c1 + vQ23r10c1 + vQ23r11c1 + vQ23r12c1 + vQ23r15c1 + vQ23r21c1 + vQ23r23c1
         f3=~ vQ23r13c1 + vQ23r14c1 + vQ23r16c1 + vQ23r19c1 + vQ23r20c1 + vQ23r25c1 + vQ23r26c1 "


forza.weight <- forza$WEIGHT
weight<- rep(1, nrow(halo))

sem.analysis(model, raw.data=halo, samp.weight = weight)
