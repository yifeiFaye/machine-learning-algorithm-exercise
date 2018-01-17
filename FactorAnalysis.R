####################################################
# Factor Analysis in R
# Written by Yifei
####################################################

#confirmatory factor anlaysis
library("lavaan")
library("mi")
library("foreign")
HS.model<- 'visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
summary(fit, fit.measures = T)

#Halo SEM case
setwd("C:/Users/yifei.liu/Desktop/Halo SEM/")
raw.data<- read.spss("merged overall.sav", to.data.frame = T)
attach(raw.data)

#data cleaning
X <- cbind(Q22Ar1c1, Q22Br1c1, Q22Cr1c1)
reachsatlkrr<- rowMeans(X, na.rm = T)
X <- cbind(Q25Ar1c1, Q25Br1c1, Q25Cr1c1)
raw.data$blopssatlkrr<- rowMeans(X, na.rm = T)
X <- cbind(Q12r1c1, Q22Ar1c1, Q22Br1c1, Q22Cr1c1, Q22Dr1c1, Q22Er1c1, Q22Fr1c1)
reachcompdv<- rowMeans(X, na.rm = T)
X<- cbind(Q25A1r1c1, Q25Ar1c1, Q25Br1c1, Q25Gr1c1, Q25Er1c1, Q25Fr1c1)
blopscompdv<- rowMeans(X, na.rm = T)

A <- data.frame(uuid,S1, S11Ar14, reachsatlkrr, reachcompdv, Q12r1c1, Q22Ar1c1, Q22Br1c1, 
             Q22Cr1c1, Q22Dr1c1, Q22Er1c1, Q22Fr1c1)
B<- data.frame(raw.data[seq(from= match("vQ23r1c1", colnames(raw.data)), to=match("vQ23r26c1", 
               colnames(raw.data)) )])
halo<- cbind(A, B)
halo<- subset(halo, is.na(Q22Ar1c1)==F)
halo<- subset(halo, uuid!="pcchbab3nvrg3akw" || uuid!="cc89jjx0sj5zwu63")

#imputation for halo
haloim<- halo[,6:38]
haloimp<- missing_data.frame(haloim, subclass= "allcategorical")
show(haloimp)

haloimp<- change(haloimp, y=c("Q12r1c1", "Q22Ar1c1"), what = "type", to=c("un", "un"))
summary(haloimp)
image(haloimp)

imp.halo<- mi(haloimp,seed=1)
summary(imp.halo)
show(imp.halo)

imputed.data<-complete(imp.halo)
c1<-imputed.data$`chain:1`[,1:33]
c2<-imputed.data$`chain:2`[,1:33]
c3<-imputed.data$`chain:3`[,1:33]
c4<-imputed.data$`chain:4`[,1:33]

#why there are different chains?? use which data result?? it seems that these chains having same results
pca.data<- data.matrix(c1, rownames.force = NA)

#prcomp using singular value decomposition SVD
pca1<-prcomp(pca.data, center = T, scale = T)
plot(pca1)
summary(pca1)

#princomp using eigen on the correlation or covariance matrix
pca2<- princomp(pca.data, cor=T, scores = T)
plot(pca2)
summary(pca2)

Fanalysis1<- factanal(pca.data, factors=3, scores= "regression", rotation="varimax")
summary(Fanalysis1)
f.loadings<- Fanalysis1$loadings
f.scores<- data.frame(Fanalysis1$scores)


library('psych')
describe(pca.data)
pairs.panels(pca.data)
lowerCor(pca.data)

Fanalysis2<- fa(pca.data, nfactors = 3, scores = "regression", residuals = F)
summary(Fanalysis2)
vss<- vss(pca.data)
