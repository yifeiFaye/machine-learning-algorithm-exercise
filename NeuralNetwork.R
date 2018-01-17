#example one, can be found in Evernote/R/Neural Network
set.seed(1234567890)
library("neuralnet")
dataset<- read.csv("C:/Users/yifei.liu/Desktop/R code/creditset.csv")
#extract a set to train the NN
trainset<- dataset[1:800, ]
#select the test set
testset<- dataset[801:2000,]

#build the neural network, with 4 hidden nodes. The number for nodes is chosen here without a clear method, however there are some rules of thumb.
#the lifesign option refers to the verbosity. use threshold value of 10%. 
creditnet<- neuralnet(default10yr ~ LTI + age, trainset, hidden=4, threshold = 0.1, stepmax = 1e+05, rep = 1,
                      startweights=NULL, lifesign="minimal", linear.output=F)
#plot the NN
plot(creditnet, rep = "best")
#test the resulting output
temp_test<- subset(testset, select= c("LTI", "age"))
creditnet.result<- compute(creditnet, temp_test)
results<- data.frame(actual=testset$default10yr, prediction = creditnet.result$net.result)
results[100:125,]
#round to the nearest integer to improve readability
results$prediction <- round(results$prediction)
results$error <- ifelse(results$actual != results$prediction, 1,0)


#############################################################################################################################################
#############################################################################################################################################


#example two, can be found in Evernote/R/Neural Network
set.seed(500)
library("MASS")
data<- Boston
#first check that no datapoint is missing, otherwise we need to fix the dataset
apply(data,2,function(x) sum(is.na(x)))
#there is no missing data, then randomly split the data into trainning and testing set. 
index<- sample(1:nrow(data), round(0.75*nrow(data)))
train<- data[index, ]
test<- data[-index, ]
lm.fit<- glm(medv~., data = train)
summary(lm.fit)
pr.lm<- predict(lm.fit, test)
MSE.lm<- sum((pr.lm-test$medv)^2)/nrow(test)
#before fitting the neural network model, first need to scale the data
maxs<- apply(data, 2, max)
mins<- apply(data, 2, min)
scaled<- as.data.frame(scale(data, center=mins, scale=maxs-mins))#scale result need to be coerced into data.frame
train_<- scaled[index, ]
test_<- scaled[-index, ]
n<- names(train_)
f<- as.formula(paste("medv~", paste(n[!n %in% "medv"], collapse = "+")))
nn<- neuralnet(f, data = train_, hidden=c(5,3), linear.output = T)
#no fixed rule as to how many layers and neurons to use although there are several more or less accepted rules of thumb. 
#usually one hidden layer is enough for a vast numbers of application
#as for as the number of neurons is concerned, it should be between the input layer size and output layer size, usually 2/3 input size.
#this example is just a toy example so we are thinking of 2 hidden layers 5 neurons in one layer and 3 neurons in another layer
plot(nn)
#The black lines show the connections between each layer and the weights on each connection while the blue lines show the bias term added
#in each step. This bias can be thought as the intercept of a linear model.
pr.nn<- compute(nn, test_[,1:13])
pr.nn_<- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r<- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn<- sum((test.r-pr.nn_)^2)/nrow(test)
#compare the two MSEs
print(paste(MSE.lm, MSE.nn))
#plot the performance of the network and the linear model on the test set
par(mfrow=c(1,2))
plot(test$medv, pr.nn_, col='red', main='Real vs Predicted NN', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18, col='red', bty='n')
plot(test$medv, pr.lm, col='blue', main = 'Real vs Predicted lm', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend='LM', pch=18, col='blue', bty='n', cex=.95)
#another plot compare these two results
plot(test$medv, pr.nn_,col='red', main = 'Real vs predicted NN', pch=18, cex=0.7)
points(test$medv, pr.lm, col='blue', pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright', legend=c('NN','LM'), pch=18, col=c('red','blue'))
#cross validation
library(boot)
set.seed(200)
lm.fit<- glm(medv~., data=data)
cv.glm(data, lm.fit, K=10)$delta[1]
set.seed(450)
cv.error<- NULL
k<- 10
library(plyr)
pbar<- create_progress_bar('text')#create the progress bars give feedback on how apply step is proceeding. this is mainly useful for long
#running functions, as for short functions, the time taken up by splitting and combining may be on the same order(or longer) as the apply step.
pbar$init(k)
for(i in 1:k){
  index<- sample(1:nrow(data), round(0.9*nrow(data)))
  train.cv<- scaled[index,]
  test.cv<- scaled[-index,]
  nn<- neuralnet(f, data=train.cv, hidden=c(5,2), linear.output=T)
  pr.nn<- compute(nn, test.cv[,1:13])
  pr.nn<- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  test.cv.r<- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  cv.error[i]<- sum((test.cv.r-pr.nn)^2)/nrow(test.cv)
  pbar$step()
}
mean(cv.error)
boxplot(cv.error, xlab='MSE CV', col='cyan', border='blue', names='CV error(MSE)', main='CV error(MSE) for NN', horizontal = T)
