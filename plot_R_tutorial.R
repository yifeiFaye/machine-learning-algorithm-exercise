
data<- read.table(file.choose(), header = T, sep=",")
#setting up working directories
dim(data)
names(data)
class(data$Gender)
count<-table(data$Gender)
percent<-count/725
barplot(percent, main="title", xlab="Gender", ylab="%", las=1)
names.arg=c("female", "male")
pie(count)

boxplot(data)
quantile(data)
quantile(data$LungCap, probs=c(0,0.25,0.5,0.75,1))
boxplot(data$LungCap, main="Boxplot", ylab="Lung Capacity", ylim=c(0, 16), las=1)
boxplot(data$LungCap~data$Gender)
boxplot(data$LungCap[data$Gender=="female"])

attach(data)
AgeGroups <- cut(data$Age, breaks = c(0, 13, 15, 17, 25), labels=c("<13", "14/15", "16/17", "18+"))
Age[1:5]
AgeGroups[1:5]

boxplot(LungCap~Smoke, ylab="LungCapacity", main="LungCap vs Smoke", las=1)

boxplot(LungCap[Age>=18]~Smoke[Age>=18], ylab="LungCapacity", main="LungCap vs Smoke for 18+", las=1) #use [] to subset data

Age[1:5]
levels(AgeGroups)

boxplot(LungCap~Smoke*AgeGroups, ylab="LungCapacity", main="LungCap vs Smoke by age group", las=2, col=c(4,2))
boxplot(LungCap~Smoke*AgeGroups, xlab="AgeStrap", ylab="LungCapacity", main="LungCap vs Smoke by age group", las=2, col=c(4,2), axes=F)
box()
axis(2, at=seq(0,20,2), seq(0,20,2),las=1)
axis(1, at=c(1.5,3.5,5.5,7.5), labels=c("<13", "14-15", "16-17", "18+"))
legend(x=5.5,y=4.5,legend=c("Non-smoke", "Smoke"), col=c(4,2), pch=12, cex=0.8)

hist(LungCap)
hist(LungCap, freq = F)
hist(LungCap, prob=T)
hist(LungCap, prob=T, ylim=c(0,0.2))
hist(LungCap, prob=T, ylim=c(0,0.2), breaks = 7)
hist(LungCap, prob=T, ylim=c(0,0.2), breaks = seq(from=0, to=16, by=2), main="boxplot", xlab="Lung Capacity", las=1)
lines(density(LungCap), col=2, lwd=3)

female<-LungCap[Gender=="female"]
stem(female)
stem(female, scale = 2)

table1<-table(Smoke, Gender)
barplot(table1)
barplot(table1, beside=T)
barplot(table1, beside=T, legend.text = T)
barplot(table1, beside=T, legend.text = c("Non-smoke", "Smoke"), main="Gender and Smoking", col=c(4,2),xlab="Gender", las=1)
mosaicplot(table1)

#pearson correlation
cor(Age, Height)
plot(Age, Height)
plot(Age, Height, main="Scatterplot", xlab="Age", ylab="Height", las=1)
plot(Age, Height, main="Scatterplot", xlab="Age", ylab="Height", las=1, xlim=c(0, 25), cex=0.5)
plot(Age, Height, main="Scatterplot", xlab="Age", ylab="Height", las=1, xlim=c(0, 25), cex=0.5, pch=8, col=2)

abline(lm(Height~Age))
abline(lm(Height~Age), col=4)

lines(smooth.spline(Age, Height), lty=2, lwd=5)

#produce numeric summary

