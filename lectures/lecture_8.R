################################################
### R Code for Lecture 8 - Hypothesis Testing
###   
###
### Last Modified 9/27/2012
###
### Changelog:
### 
################################################

## @knitr lecture8prep
library(ggplot2)


## @knitr normal
vals<-seq(-3,3,.01)
normPlot<-ggplot(mapping = aes(x=vals, y=dnorm(vals))) +
  geom_line() +
  xlab("Y") +
  ylab("Proability Density\n") +
  theme_bw()

normPlot 

## @knitr normalTest
normPlot <- normPlot + geom_vline(aes(xintercept=-2), color="red")
normPlot

## @knitr normalFill
normPlot <- normPlot + geom_area(mapping = (aes(x=seq(-3,-2,.01), y=dnorm(seq(-3,-2,.01)))), fill="red", alpha=0.4)
normPlot

## @knitr normalFill2
normPlot <- normPlot + 
            geom_area(mapping = (aes(x=seq(2,3,.01), y=dnorm(seq(2,3,.01)))), fill="red", alpha=0.4)
          + geom_vline(aes(xintercept=2), color="red")
normPlot


## @knitr r-pois-p
2*ppois(40, 54)

#OR!
p<-0
for(i in 1:40){
  p<-p+dpois(i, 54)
}
p*2

## @knitr rpois-challenge
p<-0
for(i in 0:54){
  p[i+1]<-2*pnorm(i, 54)
}

for(i in 55:108){
  p[i+1]<-2*pnorm(i, 54, lower.tail=F)
}

plot(0:108, p)


## @knitr powerSim1
set.seed(09262012)
n<-rep(1:10, 50)
simSD<-6
vec<-sapply(n, function(x) mean(rnorm(x, 15, simSD)))
plot(vec ~ n, ylab="Estimated Effect", main="50 Simulated Values per Sample Size")

## @knitr powerSim2
pvec<-pnorm(abs(vec), sd=simSD, lower.tail=FALSE)*2
plot(pvec ~ n, ylab="p")

## @knitr powerSim3
power<-rep(NA, 10)
for(i in 1:10){
  subPvec <- pvec[which(n==i)]
  power[i] <- 1 - sum(subPvec > 0.05) / length(subPvec)
}

plot(power ~ I(1:10), xlab="n", ylab="power", type="b")


## @knitr lizardPower
lizards <- rep(10:60,50)
perched<-rep(NA, length(lizards))
for(i in 1:length(perched)){
  perched[i]<-rbinom(1, size=lizards[i], prob=0.2)
}


p<-pbinom(perched, lizards, prob=0.5)

#correct for tails
p[which(p>0.5)] <- 1-p[which(p>0.5)]

#two tailed test
p<-2*p

plot(lizards, p, xlab="Number of Lizards Sampled", ylab="P")

## @knitr lizardPower2

#power by alpha
powDF<-data.frame(expand.grid(n=10:60, alpha=c(0.01, 0.05, 0.1, 0.15)))
powDF$Power <- rep(NA, nrow(powDF))

for(i in 1:nrow(powDF)){
 idx<-which(lizards==powDF$n[i])
 powDF$Power[i]<- 1 - sum(p[idx] > powDF$alpha[i]) / length(idx)
}

qplot(n, Power, data=powDF,  geom=c("point", "line"), color=alpha, group=alpha) + theme_bw()