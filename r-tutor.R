library(MASS)
school<- painters$School
school.freq<-table(school)
school.freq
cbind(school.freq)
str(painters)
class(school)
mode(school)
summary(school)

cbind(table(painters$Composition))

school.relfreq <- school.freq/nrow(painters)
school.relfreq
nrow(painters)
nrow(school)

getOption("digits")
options(digits = 1)

barplot(school.freq, col=c("orange","cyan","green","red","blue","violet","yellow"))
pie(school.relfreq)
school.relfreq
tapply(painters$Composition,painters$School,mean)

mean(painters$Composition[which(painters$School=="C")])

painters$Composition[(painters$School=="C")]

head(faithful)
summary(faithful)
plot(faithful)
breaks=seq(1.5,5.5,by=0.5)
breaks
options(digits=2)

cbind(table(cut(faithful$eruptions,breaks,right=F)))

hist(faithful$waiting)      
cumfreq0=c(0,(cumsum(table(cut(faithful$eruptions,seq(1.5,5.5,by=0.5),right=F)))))
cumfreq0
plot(breaks,cumfreq0)
lines(breaks,cumfreq0)


