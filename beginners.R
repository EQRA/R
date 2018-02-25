data(iris)
attach(iris)
detach(iris)

head(iris)
tail(iris)
head(iris,2)

dim(iris)
ncol(iris)
nrow(iris)

names(iris)
attributes(iris)
class(iris)

summary(iris)
table(iris$Species)

median(iris$Sepal.Width)
IQR(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)

aggregate(.~Species, data=iris,median)

iris$Petal.Width
iris[,3]
iris[,"Petal.Width"]

iris[10:12,]
iris[10:12,"Petal.Width"]


myi <- iris

getwd()

plot(iris)
barplot(table(iris$Species))
boxplot(Sepal.Length~Species, data=iris)
aggregate(Sepal.Length~Species, data=iris, summary)

hist(iris$Sepal.Length)
aggregate()


mtcars
class(mtcars)
write.csv(mtcars,"mtcars.csv")

dim(mtcars)
attributes(mtcars)
mtcars$gear

summary(mtcars)
mode(mtcars)
names(mtcars)
str(mtcars)
head(mtcars,2)

mtcars$am<-as.factor(mtcars$am)
mtcars$vs<-as.factor(mtcars$vs)

mtcars["Volvo 142E",]
mtcars[which.max(mtcars$mpg),]
mtcars[which.min(mtcars$wt),]
minwt<-which.min(mtcars$wt)
mtcars[minwt,]

aggregate(mpg~gear, data=mtcars, summary)
aggregate(mpg~am,mtcars,summary)

plot(mtcars)
attach(mtcars)
plot(hp~disp)
plot(cyl~mpg)

pie(table(gear))
pie(gear)

pie(table(am))
boxplot(hp~cyl)
barplot(table(gear))

hist(hp)
attach(iris)
cor.test(Sepal.Length,Sepal.Width)
plot(Sepal.Length~Sepal.Width)


library(MASS)
attach(anorexia)
cor.test(Prewt,Postwt)

InsectSprays
summary(aov(count~spray, InsectSprays))

plot(InsectSprays)

mydata<-read.csv("pain.csv")
mydata
summary(mydata)
class(mydata)
names(mydata)
str(mydata)
attach(mydata)
myvec<-c(med_1,med_2,med_3)
class(myvec)
myhed<-c(rep("med_1",9),rep("med_2",9),rep("med_3",9))
class(myhed)
mydf<-data.frame(myvec,myhed)
class(mydf)
summary(mydf)
aggregate(myvec~myhed,mydf,summary)
mydf
boxplot(myvec~myhed)
summary(aov(myvec~myhed, mydf))


HairEyeColor
data("HairEyeColor")
mydata<-data.frame((HairEyeColor))
mydata
attach(mydata)
chisq.test(Hair,Eye)
str(mydata)

plot(mydata)

summary(anorexia)
str(anorexia)
aggregate(Prewt~Treat,anorexia,summary)
t.test(Prewt, Postwt, paired = TRUE)
t.test(Prewt,mu=3)
t.test(Prewt,Postwt)

install.packages("pastecs")
library(pastecs)
stat.desc(anorexia)
hist(Prewt)
anorexia[anorexia$Prewt>90,]

str(anorexia)

cor(anorexia[unlist(lapply(anorexia,is.numeric))])
cor.test(anorexia$Prewt,anorexia$Postwt)
fit<-aov(anorexia$Prewt~anorexia$Treat)
fit
summary(fit)

x<-rnorm(10)
y<-rnorm(10)
t.test(x,y)
t.test(x,x)
ttest<-t.test(x,y)
names(ttest)
ttest
ttest$method
ttest$statistic
ts <- replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)
ts                
mean(ts)
range(ts)

pts<-seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')

lines(density(ts))

