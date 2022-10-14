## @knitr lecture13prep
library(emdbook)
library(bbmle)

## @knitr vectorize1
1:10 + 1


## @knitr Timevectorize1
system.time(1:100000+1)

system.time({
  x<-1:100000
  for(i in x) x[i]<-x[i] +1	
})

## @knitr vectorize2
dnorm(5, mean=1:10, sd=1)

## @knitr vectorizationBad
sampMean <- function (vec, size) mean(sample(vec, size))

sampMean(vec=1:5, size=1:5)

## @knitr vectorizationGood
sampMeanV <- Vectorize(sampMean, 
                       vectorize.args="size")

sampMeanV(vec=1:5, size=1:5)

## @knitr vectorizationGuts
sampMeanV


## @knitr sapply1
f <- function(x) x+1
#
sapply(1:5,f)


## @knitr sapply2
sapply(1:5, function(x) x+1)


## @knitr sapply3
sapply(1:5, function(x) sampMean(1:5, x))

## @knitr sapplyMatrix
sapply(1:5, function(x) return( c(x+2, x^2) ) )

## @knitr sapplyTime
system.time({
  x<-1:100000
  for(i in x) x[i]<-x[i] +1  
})

system.time(sapply(1:100000, f))

## @knitr apply1
m <- matrix( c(1, 2,
               3, 4), ncol=2)

## @knitr apply1a
apply(m, 1, sum)

## @knitr apply1b
apply(m, 2, sum)

## @knitr apply2
apply(m, c(1,2), sum)

## @knitr lapply
x <- list(a = 1:10, 
          beta = exp(-3:3), 
          logic = c(TRUE,FALSE,FALSE,TRUE))

# compute the list mean for each list element
lapply(x,mean)

## @knitr exercise1
set.seed(697)
vec <- sapply(1:500, function(x) mean(rnorm(x, mean=5, sd=3)))
plot(1:500, vec, pch=19, xlab="n", ylab="mean")


## @knitr exercise2
seVec <- sapply(1:500, function(x){
    mvec<- sapply(1:50, function (i) mean(rnorm(x, mean=5, sd=3)))
    return(sd(mvec))
})

plot(1:500, seVec, pch=19, xlab="n", ylab="SE")


## @knitr exercise2a
library(bootstrap)
seVec2 <- sapply(1:500, function(x) 
  sd(bootstrap(rnorm(x, mean=5, sd=3), 50, mean)$thetastar))

plot(1:500, seVec2, pch=19, xlab="n", ylab="SE")


## @knitr exercise3
n.sims <- 500
power <- sapply(seq(0.01, 0.1, .01), function(alpha){
  p <- sapply(1:500, function(y){
    pop <- rnorm(5, 5, 3)
    t.test(pop)$p.value
  })
  1-sum(p>alpha)
})

plot(seq(0.01, 0.1, .01), power, xlab="alpha", ylab="power")

## @knitr breakbreak
####################################################
## likelihood
####################################################

## @knitr likelihoodSapply
count <- 10
#
l <- sapply(0:20, function(x) dpois(count, lambda=x) ) 
#
plot(0:20, l, ylab="Likelihood", xlab="lambda", pch=19)
abline(v=10, col="red", lwd=2, lty=2)

## @knitr LoglikelihoodSapply
plot(0:20, log(l), ylab="Log-Likelihood", xlab="lambda", pch=19)


## @knitr likelihoodPrep
set.seed(697)
counts <- rpois(10, 15)
lambdaVals <- 0:50

## @knitr likelihoodDemo1
poisCurve10 <- dpois(lambdaVals, 10)
barplot(poisCurve10,  ylab="Probability Density", xlab="Lambda", col="white", width=1, xlim=c(0,50), main="Density Function at Lambda = 10")

## @knitr likelihoodDemo2
colvec<-rep("white", 50)
colvec[counts]<-"red"
barplot(poisCurve10,  ylab="Probability Density", xlab="Lambda", col=colvec, width=1, xlim=c(0,50), main="Density Function at Lambda = 10")


## @knitr likelihoodDemo3
poisCurve15 <- dpois(lambdaVals, 15)

par(mfrow=c(1,2))
barplot(poisCurve10,  ylab="Probability Density", xlab="Lambda", col=colvec, width=1, xlim=c(0,50), main="Lambda = 10")
barplot(poisCurve15,  ylab="Probability Density", xlab="Lambda", col=colvec, width=1, xlim=c(0,50), main="Lambda = 15")
par(mfrow=c(1,1))




## @knitr likelihoodSapply1
lik <- sapply(lambdaVals, 
             function(x) prod( dpois(counts, lambda=x) ) )
#
ll <- sapply(lambdaVals, 
             function(x) sum( dpois(counts, lambda=x, log=TRUE) ) )

## @knitr likelihoodSapply2
max(lik)

lambdaVals[which(lik==max(lik))]

## @knitr likelihoodSapply1Plot
par(mfrow=c(1,2))
plot(lambdaVals, lik, ylab="Likelihood", xlab="lambda", pch=19)
plot(lambdaVals, ll, ylab="Log-Likelihood", xlab="lambda", pch=19)
par(mfrow=c(1,1))

## @knitr beeExample
bees <- read.csv("./data/20q18BeeLifespans.csv")
hist(bees$hours)

## @knitr beeExampleCode
scaleVals <- seq(0.2, 80, 0.2)
#
beeD <- function(x) sum(dgamma(bees$hours, shape=1, 
                               scale=x, log=TRUE))
mll <- sapply(scaleVals, beeD)
#
scaleVals[which(mll==max(mll))]

## @knitr beeExamplePlot
par(mfrow=c(2,2), mar=c(5,4,1,1))
plot(exp(mll) ~ scaleVals, xlab="Scale", ylab="Likelihood")
plot(mll ~ scaleVals, xlab="Scale", ylab="Log-Likelihood")
plot(mll ~ scaleVals, ylim=c(-150,-140), xlab="Scale", ylab="Log-Likelihood")
par(mfrow=c(1,1))


## @knitr break2
################################################################################

## @knitr likelihoodConfPlot
plot(lambdaVals, ll, ylab="Log-Likelihood", xlab="lambda", pch=19, ylim=c(-60,-25))
abline(h=max(ll)-1.92, col="red", lwd=2, lty=2)

## @knitr likelihoodSapply3
lConf <- max(ll) - 1.92
lConf

confIDX <- which(ll >= lConf)
confIDX <- c(confIDX[1], confIDX[length(confIDX)])

#95% confidence interval of Lambda
lambdaVals[confIDX]

#compare our estimated fit to the hypothesis that lambda = 12
G <- 2*(ll[16] - ll[13])

pchisq(G, 1, lower.tail=F)