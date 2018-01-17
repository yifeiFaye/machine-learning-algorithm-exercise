library(foreign)
library(rpart)
library(CHAID)


setwd("C:/Users/yifei.liu/Desktop/rid-x positioning CHAID/")

raw<- read.spss("Confidential_DI - Septic Tank Study_Client_SPSS_20_12_2012.sav", use.value.labels = F, to.data.frame = T)
attach(raw)
for (i in 1:nrow(raw)){
  if (qs16[i] == 1){raw$rqs16[i] = 1}
  if (qs16[i] == 2||3||4){raw$rqs16[i] = 2}
  if (qs16[i] == 5||6||7){raw$rqs16[i] = 3}
  if (qs16[i] == 8||9||10){raw$rqs16[i] = 4}
  if (qs16[i] == 11||12||13||14){raw$rqs16[i] = 5}
  if (qs16[i] == 15){raw$rqs16[i] = 6}
  
  if (q12_1[i]== 1 || q12_2[i] == 1){raw$rq12_N[i] = 1}else{raw$rq12_N[i] = 0}
  if (q12_3[i] == 1 || q12_4[i] == 1 || q12_5[i] == 1 || q12_6[i] == 1){
      raw$rq12_W[i]=1}else{raw$rq12_W[i]=0}
  if (q12_7[i] == 1 || q12_8[i] == 1 || q12_9[i] == 1){raw$rq12_A[i]=1}else{raw$rq12_A[i]=0}
  if (q12_10[i] == 1 || q12_11[i] == 1 || q12_12[i] == 1 || q12_13[i] == 1 || q12_14[i] == 1){
      raw$rq12_NS[i]=1}else{raw$rq12_NS[i]=0}
  if (q12_15[i] == 1){raw$rq12_none[i]=1}else{raw$rq12_none[i]=0}
  
  if (is.na(q19[i])){q19[i]=1}
  
  raw$rq20b_prof[i]=0
  if (is.na(q30b_9[i])==F && q30b_9[i] == 1){
    raw$rq20b_prof[i]=1} else {
      if (is.na(q30b_10[i])==F && q30b_10[i] == 1){
        raw$rq20b_prof=1} else {
          if (is.na(q30b_12[i])==F && q30b_12[i] == 1){
            raw$rq20b_prof=1}
       }
    }
}
  
q21<- raw[,seq(from=match('q21_1', colnames(raw)),to=match('q21_27', colnames(raw)))]
q22<- raw[,seq(from=match('q22_1', colnames(raw)),to=match('q22_27', colnames(raw)))]

for (i in 1:nrow(raw)){
  raw$q21_22_cnt[i]=1
  for (j in 1:ncol(q21)){
    if (q21[i,j]==1 && q22[i,j]==1){
      raw$q21_22_cnt[i]=raw$q21_22_cnt[i]+1
    }
  }
}

raw$bath_cnt = q8_1 + 0.5*q8_2

raw$risk_cnt=0
for (i in 1:nrow(raw)){
  if (q6[i]==1){raw$risk_cnt[i]=1}
  if (q7[i]==1){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (raw$bath_cnt[i] >= 3){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (q9[i] >=3){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (q10[i]>1){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (q16[i]==1 || q16[i]==13){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (q16[i]==2 ||q16[i]==3){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (q17[i]==2){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (q18[i]==2 || q18[i]==3){raw$risk_cnt[i]=raw$risk_cnt[i]+2}
  if (q19[i]>=5){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  if (raw$q21_22_cnt[i]>=3){raw$risk_cnt[i]=raw$risk_cnt[i]+2}
  if (raw$q21_22_cnt[i]==2){raw$risk_cnt[i]=raw$risk_cnt[i]+1}
  
  if (raw$risk_cnt[i]<4){raw$risk2[i]=1}else{
    if (raw$risk_cnt[i]<6){raw$risk2[i]=2}else{
      raw$risk2[i]=3
    }
  }
}

q33<- raw[,seq(from=match('q33_1', colnames(raw)),to=match('q33_35', colnames(raw)))]
s.q33<- apply(q33, 1, sum)

for (i in 1:nrow(raw)){
  a<- is.na(q30a_6[i])==F && q30a_6[i]==1
  b<- is.na(q30a_9[i])==F && q30a_9[i]==1
  c<- is.na(q30b_7[i])==F && q30b_7[i]==1
  d<- is.na(q30b_8[i])==F && q30b_8[i]==1
  e<- s.q33[i]>2
  f<- a||b||c||d
  if (f || e){raw$green[i]=1}else{raw$green[i]=0}
  if (q23[i]==1 || q23[i]==9){raw$disaster[i]=0}else{raw$disaster[i]=1}
  if (is.na(q29b_5[i]) && qs9[i]==1){q29b_5[i]=0}
  if (q19[i]==9){q19[i]=='NA'}
  if (q20[i]==10){q20[i]=='NA'}
  if (q23[i]==9){q23[i]=='NA'}
}

attach(raw)
chaid.data<- data.frame( s2_1, qs3, s8_1, qs12, qs15, rqs16, q1, q4, q5, q11, rq12_N, rq12_W, rq12_NS, rq12_A, rq12_none, 
                         q15, q18, q19, q20, q23, q25_9, risk2, green)
varlist<- names(chaid.data)
dv<- ifelse(qs9<2,'No', 'Yes')
chaid.data<- data.frame(chaid.data, dv)

#rpart
tree.rpart<- rpart(dv ~ . -dv, data = chaid.data, method = 'class', control = rpart.control(minbucket = 0))
summary(tree1)
plot(tree1)
text(tree1, pretty = 0)

#tree
tree3<- tree(dv~ ., data = chaid.data)
summary(tree3)
plot(tree3)
text(tree3, pretty=0)
set.seed(2)
train=sample(1:nrow(chaid.data),800)
chaid.test<- chaid.data[-train, ]
dv.test<- dv[-train]
tree.pred<- predict(tree3, chaid.test, type="class")
table(tree.pred, dv.test)

set.seed(3)
cv.chaid <- cv.tree(tree3,FUN = prune.misclass)
names(cv.chaid)
cv.chaid
par(mfrow=c(1,2))
plot(cv.chaid$size, cv.chaid$dev, type = "b")
plot(cv.chaid$k, cv.chaid$dev, type="b")
prune.chaid<- prune.misclass(tree3, best = 4)
plot(prune.chaid)
text(prune.chaid, pretty=0)
cv.predict<- predict(prune.chaid, chaid.test, type="class")
table(cv.predict, dv.test)



#bagging and randome forest
chaid.bag<- randomForest(dv~., data = chaid.data, subset = train, mtry=23, ntrees= 1000, importance=T, na.action = na.omit)
summary(chaid.bag)
chaid.bag.test<- predict(chaid.bag, chaid.data[-train, ], type = "class")
dv.test.numeric<- ifelse(dv.test=="Yes", 1, 0)
chaid.bag.test<- as.numeric(chaid.bag.test)
mean((chaid.bag.test-dv.test.numeric)^2)
plot(chaid.bag)
importance(chaid.bag)

chaid.cv.rf<- rfcv(chaid.data, chaid.data$dv, cv.fold = 3)
with(chaid.cv.rf, plot(n.var, error.cv, log = "x", type = "o", lwd=2))


#boosting
chaid.boost<- gbm(dv~., data=chaid.data[train,], distribution="gaussian", n.trees = 5000, interaction.depth = 4,
                  shrinkage = 0.2, verbose = T)
summary(chaid.boost)
yhat.boost<- predict.gbm(chaid.boost, newdata = chaid.data[-train,], n.trees = 5000)
mean((yhat.boost-dv.test.numeric)^2)

#tree
library(ISLR)
library(tree)
attach(Carseats)
High=ifelse(Sales<=8, 'No', 'Yes')
Carseats=data.frame(Carseats, High)
tree.carseats<- tree(High ~ . -Sales, Carseats)
# . means every variables except sales
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats
#instead of only computing training error, need to estimate the test error, build training set and testing set
set.seed(2)
train= sample(1:nrow(Carseats),200)
Carseats.test= Carseats[-train, ]
High.test=High[-train]
tree.carseats<- tree(High~.-Sales, Carseats, subset=train)
tree.pred<- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(86+57)/200

set.seed(3)
cv.carseats <- cv.tree(tree.carseats,FUN = prune.misclass)
names(cv.carseats)
cv.carseats
#see alpha=3, dev=mindev, which is 52, so use 3 as cost of complexity parameter
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
#number of terminal node = 5, K = 3, give us the lowest error
prune.carseats<- prune.misclass(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty=0)
#this tree plot is different from tree.carseats plot, let's now look at how prune.carseats perform in terms of error rate
tree.pred2<- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred2, High.test)
(94+60)/200

library(MASS)
set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~., Boston, subset = train)
summary(tree.boston)

#regression tree:
plot(tree.boston)
text(tree.boston, pretty = 0)
cv.boston<- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
prune.boston<- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
yhat<- predict(tree.boston, newdata = Boston[-train,])
boston.test<- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
#MSE calculation
mean((yhat-boston.test)^2)


#bagging and random forest using randomForest package
library(randomForest)
set.seed(1)
bag.boston<- randomForest(medv~., data=Boston, subset = train, mtry= 13, importance = T)
bag.boston
yhat.bag<- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

#change the number of tree growned 
bag.boston<- randomForest(medv~., data = Boston, subset = train, mtry=13, ntree= 25)
bag.boston
yhat.bag<- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

#growing randome forest is the same procedure with bagging except from changing mtry 
set.seed(1)
rf.boston<- randomForest(medv~., data = Boston, subset = train, mtry=6, importance=T)
rf.boston
yhat.rf<- predict(rf.boston, newdata = Boston[-train,])
plot(yhat.rf, boston.test)
abline(0,1)
mean((yhat.rf-boston.test)^2)

#variable importance: two measusres of variable importance are predicted. the former is based on the mean decrease of accurary in 
#prediction on the out of bag sample when a given variable is excluded from the model.
#the latter is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all tree. 
importance(rf.boston)
varImpPlot(rf.boston)

#boosting 
library(gbm)
set.seed(1)
boost.boston<- gbm(medv~., data = Boston[train,], distribution="gaussian", n.trees = 5000, interaction.depth = 4,
                   shrinkage = 0.2, verbose = T)
summary(boost.boston)
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')
yhat.boost<- predict.gbm(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost-boston.test)^2)

