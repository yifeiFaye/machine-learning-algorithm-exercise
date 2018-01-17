#######################################################################################################
# This file performs Structuaral Equation Model based on maximum likelihood                           #
# Function include reading data file, perform SEM, output model summary, write latent variable socre  #
# in CSV file.                                                                                        #
# return coefficient matrix, model structure, covariance matrix, predicted value, etc                 #
# call coef.matrix cov.matrix predict.value model.structure para.estimate                             #
#######################################################################################################

sem.analysis <-function(model, raw.data, samp.weight) {
  #fit the model 
  lavaan.fit<- sem(model, raw.data, estimator="ML", mimic = "Mplus", test= "standard", sample.cov = T)
  #lavaan.fit<- lavaan(model, raw.data, estimator="ML",model.type="SEM",test="standard",
  #                   auto.fix.first = T, auto.var=T, int.ov.free = T, int.lv.free = T)
  
  lavaan.des<- svydesign(id=~1, weights = samp.weight, data=raw.data)
  
  survey.fit<- lavaan.survey(lavaan.fit, lavaan.des, estimator = c("ML"), estimator.gamma = c("default"))
  
  #output summary, check the variance, need to be positive!
  print("Please check estimated variance, need to be positive!")
  summary(survey.fit, fit.measures=TRUE)
  
  #coefficient estimates
  coef.matrix <- as.data.frame(coef(survey.fit))
  
  #fitted data
  data.fitted<-as.data.frame(fitted(survey.fit))
  
  #fitted residual
  resid.fitted<-as.data.frame(resid(survey.fit))
  
  #covarince matrix for everything
  cov.matrix<- as.data.frame(vcov(survey.fit))
  
  #latent variable value across all respondents
  predict.value<- as.data.frame(predict(survey.fit, newdata=raw.data))
  
  #coefficients estimates and statistics 
  para.estimate<-as.data.frame(parameterEstimates(survey.fit, ci=FALSE, standardized=TRUE))
  para.estimate<-subset(para.estimate, op=="=~")
  
  #model structure
  model.structure<-inspect(survey.fit, what= "start")
  
  #observed variable list
  ov.list<-lavNames(survey.fit, "ov")
  
  
  #latent variable score matrix
  predict.matrix <- lavPredict(survey.fit, type="lv", newdata = raw.data, label = T, fsm = T)
  lv.score.matrix <- attributes(predict.matrix)$fsm[[1]]
  lv<-as.data.frame(head(lavPredict(survey.fit, type="lv", newdata = raw.data)))
  ov<-as.data.frame(head(lavPredict(survey.fit, type="ov", newdata = raw.data)))
  lv.label<-attributes(lv)$names
  ov.label<-attributes(ov)$names
  
  #delete last row which shows value for dependent variable 
  lv.score.matrix<- lv.score.matrix[-1,]
  f<-dim(lv.score.matrix)
  if ( is.null(f) ){
    lv.score.matrix<-rbind(ov.label, lv.score.matrix)}
  if ( is.null(f)==FALSE){
    rownames(lv.score.matrix)<-lv.label 
    colnames(lv.score.matrix)<-ov.label}
  lv.score.matrix<-t(lv.score.matrix)
  
  print("Latent variable scores have been writen in CSV file and saved in directory specified by outfile.directory")
  
  write.csv(lv.score.matrix, "C:/Users/yifei.liu/Desktop/latent variable score.csv")
}

