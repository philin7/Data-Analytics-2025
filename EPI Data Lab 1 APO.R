setwd("C:/Users/Nyah/Downloads/Data Analytics/Lab 1/")
EPI_data <- read.csv("epi2024results06022024.csv") 

View(EPI_data)
#tips
attach(EPI_data) # sets the ‘default’ object 

EPI.new # prints out values EPI_data$EPI.new

NAs <- is.na(EPI.new) # records True values if the value is NA 

EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array 

#exercise 1

summary(EPI.new) # stats 
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new) #Use help(<command>), e.g. > help(stem) 
boxplot(EPI.new, APO.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TR,bw=1.)) 
rug(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ")) 
rug(EPI.new) 
x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

#exercise 2 
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#exercise 2a
ECO.new # prints out values EPI_data$EPI.new

NAs <- is.na(ECO.new) # records True values if the value is NA 

ECO.new.noNAs <- ECO.new[!NAs] # filters out NA values, new array 

#exercise 1

summary(ECO.new) # stats 
fivenum(ECO.new,na.rm=TRUE) 
stem(ECO.new) # stem and leaf plot 
hist(ECO.new) 
hist(ECO.new, seq(20., 84., 1.0), prob=TRUE) 
lines(density(ECO.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(ECO.new) #Use help(<command>), e.g. > help(stem) 
boxplot(ECO.new, APO.new) 
hist(ECO.new, seq(20., 84., 1.0), prob=TRUE)
lines (density(ECO.new,na.rm=TR,bw=1.)) 
rug(ECO.new) 
hist(ECO.new, seq(20., 84., 1.0), prob=TRUE)
lines (density(ECO.new,na.rm=TRUE,bw="SJ")) 
rug(ECO.new) 
x<-seq(20,84,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 


plot(ecdf(ECO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(ECO.new); qqline(ECO.new) 
qqplot(rnorm(250), ECO.new, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.new)

qqplot(rt(250, df = 5), ECO.new, xlab = "Q-Q plot for t dsn") 
qqline(ECO.new)

#exercise 2b
APO.new # prints out values ECO_data$APO.new

NAs <- is.na(APO.new) # records True values if the value is NA 

APO.new.noNAs <- APO.new[!NAs] # filters out NA values, new array 

#exercise 1

summary(APO.new) # stats 
fivenum(APO.new,na.rm=TRUE) 
stem(APO.new) # stem and leaf plot 
hist(APO.new) 
hist(APO.new, seq(5., 100., 1.0), prob=TRUE) 
lines(density(APO.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(APO.new) #Use help(<command>), e.g. > help(stem) 
boxplot(APO.new, APO.new) 
hist(APO.new, seq(5., 100., 1.0), prob=TRUE)
lines (density(APO.new,na.rm=TR,bw=1.)) 
rug(APO.new) 
hist(APO.new, seq(5., 100., 1.0), prob=TRUE)
lines (density(APO.new,na.rm=TRUE,bw="SJ")) 
rug(APO.new) 
x<-seq(5,100,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

#exercise 2 
plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) 
qqnorm(APO.new); qqline(APO.new) 
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn") 
qqline(APO.new)

qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn") 
qqline(APO.new)