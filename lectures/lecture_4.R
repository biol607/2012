################################################
### R Code for Lecture 4 - Estimating Population Properties
###   
###
### Last Modified 9/13/2012
###
### Changelog:
### 9/13/2012 - Cut file to where we reached by the end of lecture.  Rest moved to lecture 5.
###
### Notes:
### 9/13/2012 - Got through end of descriptions of samples
###           - next year make this all loops - cumsum to fib to population
################################################


## @knitr lecture4prep
set.seed(5000)
population<-runif(n = 400, min = 1, max = 100)

library(sn, quietly=TRUE)
library(ggplot2)


## @knitr lecture4-title
apop<-rnorm(5000, 15,5)
samp<-sapply(rep(1:100,5), function(x) mean(sample(apop, x)))

#two panels
par(mfrow=c(1,2))
plot(rep(1:100,5), samp, pch=19, xlab="Sample Size", ylab="Estimated Mean")
abline(mean(apop), 0, col="blue", lwd=5)

#boostrapped standard error
est<-sapply(rep(1:100), function(x) mean(sample(apop, 15)))

#draw the distributions of means with a bootstrapped 95% CI
hist(est, main="Esimated Means from 100 simulations", xlab="Mean with n=15", col="grey")
lines(rep(mean(est)+2*sd(est), 2), c(0,40), col="red", lwd=4)
lines(rep(mean(est)-2*sd(est), 2), c(0,40), col="red", lwd=4)

#set everything back to normal
par(mfrow=c(1,1))


## @knitr bootMean
#first create a vector of sample sizes
n<-rep(1:400, times = 4)

# next create an empty vector
m<-rep(NA, times = length(n))

#now a loop
for(i in 1:length(n)){
  m[i]<-mean(sample(population, size=n[i]))
}

#plot the result
plot(n, m, xlab="size", ylab="mean")


######For stepping through piece by piece
## @knitr bootMean1
n<-rep(1:400, times = 4)

## @knitr bootMean2
m<-rep(NA, times = length(n))

## @knitr bootMean3
for(i in 1:length(n)){
  m[i]<-mean(sample(population, size=n[i]))
}

## @knitr bootMean4
plot(n, m, xlab="size", ylab="mean")

## @knitr fib
#start with a blank vector with some 1's
fibVec<-c(1,1,rep(NA, 13))

#now loop
for(i in 3:15){
  fibVec[i] <- fibVec[i-1] + fibVec[i-2]  
}

fibVec

## @knitr fibBlank
#start with a blank vector, use ifelse to fill in first two
fibVec2 <- rep(NA, 15)
for(i in 1:15)  {
  fibVec[i] <- ifelse(i<3, 1, fibVec[i-1] + fibVec[i-2])
}
fibVec


## @knitr skew
samp<-seq(-5,5,.01)
plot(samp, dsn(samp), type="l", col="black", lwd=4, ylim=c(0,0.9), ylab="frequency", xlab="measure")
matplot(samp, dsn(samp, shape=4), type="l", col="red", add=T, lwd=4)

## @knitr kurtosis
plot(samp, dunif(samp, -5, 5), type="l", col="red", lwd=4, ylim=c(0,0.9), ylab="frequency", xlab="measure")
matplot(samp, dsn(samp), type="l", col="black", lwd=4, add=T)
matplot(samp, dsn(samp, scale=.5), type="l", col="blue", lwd=4, add=T)


## @knitr load-bird
bird<-read.csv("./data/02e1bDesertBirdCensus.csv")
birdDistPlot <- qplot(Count, data=bird, geom="histogram",fill=I("darkblue"), binwidth=20)+theme_bw()

## @knitr mode
birdDistPlot + annotate("text",100,20,label="Mode", color="red", size=8)


## @knitr median
birdDistPlot + 
  geom_vline(xintercept=median(bird$Count), lwd=3, colour="red") + 
  annotate("text", 195,15, label=paste("Median = ", median(bird$Count)), size=8)

## @knitr median2
sort(bird$Count)

nrow(bird) #this is the # of rows in the data frame

sort(bird$Count)[22]

## @knitr quantiles
sort(bird$Count)

## @knitr quartiles
boxplot(bird$Count, horizontal=T)
