################################################
### R Code for Lecture 7 - Probability Distributions
###   
###
### Last Modified 9/21/2012
###
### Changelog:
### 
################################################

## @knitr lecture7prep
library(ggplot2)
bird<-read.csv("./data/02e1bDesertBirdCensus.csv")

########
### Probabilty Distributions
########

## @knitr freqDist1

bplot<-ggplot(data=bird, aes(x=Count))+xlab("# of Birds") + theme_bw()
bplot+geom_histogram(binwidth=20)  + ylab("# of Times Observed") 


## @knitr freqDist2
bplot+geom_histogram(aes(y=..density..*20), binwidth=20)  + ylab("Probability") 


## @knitr freqDist3
bplot+geom_histogram(binwidth=1) + ylab("# of Times Observed") 


## @knitr freqDist4
bplot+geom_histogram(aes(y=..density..), binwidth=1)  + ylab("Probability") 


## @knitr birdDensity1
bplot+geom_density() + ylab("Probability Density") 

## @knitr birdDensityP
#area with density approach gakked from http://codealamode.blogspot.com/2012/04/density-plots-and-histograms-in-ggplot2.html
dd <- with(density(bird$Count),data.frame(x,y))

bplot+geom_density() + ylab("Probability Density") + 
  geom_area(data=dd, aes(x=ifelse(x>100 & x<200, x, 200), y=y), fill="red") +
  ylim(c(0,0.0082))


## @knitr normal
vals<-seq(-3,3,.01)
normPlot<-ggplot(mapping = aes(x=vals, y=dnorm(vals))) +
  geom_line() +
  xlab("Y") +
  ylab("Proability Density\n") +
  theme_bw() + annotate(geom="text", x=-1.8, y= 0.4, label="mean=0, sd=1")

normPlot 

## @knitr normalSD1
normPlot + geom_area(mapping = (aes(x=seq(-1,1,.01), y=dnorm(seq(-1,1,.01)))), fill="red", alpha=0.4)

## @knitr normalSD2
normPlot + geom_area(mapping = (aes(x=seq(-1,1,.01), y=dnorm(seq(-1,1,.01)))), fill="red", alpha=0.4) +
  geom_area(mapping = (aes(x=seq(-2,-1,.01), y=dnorm(seq(-2,-1,.01)))), fill="blue", alpha=0.4) +
  geom_area(mapping = (aes(x=seq(1,2,.01), y=dnorm(seq(1,2,.01)))), fill="blue", alpha=0.4) 


## @knitr pnormPlot
normPlot + geom_area(mapping = (aes(x=seq(-3,-1,.01), y=dnorm(seq(-3,-1,.01)))), fill="purple", alpha=0.4)

## @knitr qnormPlot
ggplot(mapping = aes(y=seq(0,1,.01), x=qnorm(seq(0,1,.01)))) + 
  geom_line() + 
  ylim(0,1) + 
  theme_bw() +
  xlab("Y") +
  ylab("Quantile\n")


## @knitr pnormVqnorm
pnorm(-1)
qnorm(pnorm(-1))
qnorm(0.025)

## @knitr logNorm
ggplot(mapping = aes(x=exp(vals), y=dlnorm(exp(vals)))) +
  geom_line() +
  xlab("Y") +
  ylab("Proability Density\n") +
  theme_bw()

## @knitr gamma1
valsG<-seq(0,15,.01)

gammaPlot<-ggplot(mapping=aes(x=valsG, y=dgamma(valsG, shape = 2, scale=2))) +
  geom_line()+
  xlab("Y") +
  ylab("Proability Density\n") +
  theme_bw()

gammaPlot

## @knitr gamma2
gammaPlot + geom_line(aes(y=dgamma(valsG, shape = 5, scale=2)), color="red") +
  annotate("text", x=10, y=0.2, label="Shape = 2, scale = 2")+
  annotate("text", x=10, y=0.15, label="Shape = 5, scale = 2", color="red")

## @knitr gamma3
gammaPlot + geom_line(aes(y=dgamma(valsG, shape = 2, scale=3)), color="red")+
  annotate("text", x=10, y=0.2, label="Shape = 2, scale = 2")+
  annotate("text", x=10, y=0.15, label="Shape = 2, scale = 3", color="red")

## @knitr poissonPlot
valsP<-0:30
poisPlot1<-ggplot(mapping=aes(x=factor(valsP), y=dpois(valsP, 5))) +
  geom_bar(width=0.1)+
  scale_x_discrete("Y", breaks=seq(0,30,3))+
  ylab("Proability Density") +
  theme_bw()

poisPlot1

## @knitr poisson2
poisPlot1 + geom_bar(mapping=aes(y=dpois(0:30, 15)), fill="red", width=0.1)


## @knitr poisson3
poisPlot1 +  geom_line(mapping=(aes(x=seq(0,15,.01), y=dnorm(seq(0,15,.01), 5.5,sqrt(5)))), color="blue")



## @knitr binomPlot
valsB<-0:20
binB<-dbinom(valsB, 20, 0.3)
binB2<-dbinom(valsB, 20, 0.8)
binomPlot1<-ggplot(mapping=aes(x=factor(valsB), y=binB)) +
  geom_bar(width=0.1)+
  scale_x_discrete("Y", breaks=seq(0,20,2))+
  ylab("Proability Density") +
  theme_bw()

binomPlot1

## @knitr binomPlot2
binomPlot1 + geom_bar(aes(y=binB2), fill="red", width=0.1)

## @knitr dbinomPlot
nbin1<-dnbinom(0:34, 5, 0.3)
nbPlot<-ggplot(mapping=aes(x=factor(0:34), y=nbin1)) +
  geom_bar(width=0.1)+
  scale_x_discrete("Y", breaks=seq(0,36,3))+
  ylab("Proability Density") +
  theme_bw()

nbPlot