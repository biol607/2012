## @knitr lecture14prep
library(emdbook)
library(bbmle)


## @knitr likelihoodPrep
set.seed(697)
counts <- rpois(10, 15)
lambdaVals <- 0:50


lik <- sapply(lambdaVals, 
              function(x) prod( dpois(counts, lambda=x) ) )
#


## @knitr likelihoodMLE
colvec<-rep("white", 50)
colvec[counts]<-"red"

mle<-lambdaVals[which(lik==max(lik))]

poisCurvemle <- dpois(lambdaVals, mle)

barplot(poisCurvemle,  ylab="Probability Density", xlab="lambda", col=colvec, width=1, xlim=c(0,50), main=paste("MLE of Lambda = ", mle, sep=""))


## @knitr LoglikelihoodPlot
lambdaVals<-lambdaVals[-1]

lik <- sapply(lambdaVals, 
              function(x) prod( dpois(counts, lambda=x) ) )
#
ll <- sapply(lambdaVals, 
             function(x) sum( dpois(counts, lambda=x, log=TRUE) ) )

plot(lambdaVals, ll, ylab="Log-Likelihood", xlab="lambda", pch=19, ylim=c(-60,-25))

## @knitr LoglikelihoodConfPlot
plot(lambdaVals, ll, ylab="Log-Likelihood", xlab="lambda", pch=19, ylim=c(-60,-25))
abline(h=max(ll)-1.92, col="red", lwd=2, lty=2)

## @knitr likelihoodCI
lConf <- max(ll) - 1.92
lConf

#Get which values of lambda  that have LL >= 95% cutoff
confVals <- lambdaVals[which(ll >= lConf)]

confVals[c(1, length(confVals))]

## @knitr LRTest
#compare our estimated fit to the hypothesis that lambda = 12
G12 <- 2*(ll[17] - ll[12])
pchisq(G12, 1, lower.tail=F)


#compare our estimated fit to the hypothesis that lambda = 15
G15 <- 2*(ll[17] - ll[15])
pchisq(G15, 1, lower.tail=F)


## @knitr beeExampleStart
bees <- read.csv("./data/20q18BeeLifespans.csv")

#find the mle
scaleVals <- seq(0.2, 80, 0.2)

beeD <- function(x) sum(dgamma(bees$hours, shape=1, 
                               scale=x, log=TRUE))
mll <- sapply(scaleVals, beeD)

## @knitr beeExamplePlot
#plot it with it's CIs
plot(mll ~ scaleVals, ylim=c(-150,-140), xlab="Scale", ylab="Log-Likelihood")
abline(h=max(mll), col="red", lwd=2, lty=2)
abline(h=max(mll)-1.92, col="red", lwd=2, lty=2)

## @knitr beeExampleCI
#a function to get a CI given values and their log-likelihood 
mllCI <- function(values, logl) {
  ci <- values[which(logl > max(logl) - 1.92)]
  ci[c(1, length(ci))]
}

mllCI(scaleVals, mll)

## @knitr beeExampleCIPlot
plot(mll ~ scaleVals, ylim=c(-150,-140), xlab="Scale", ylab="Log-Likelihood")
abline(h=max(mll), col="red", lwd=2, lty=2)
abline(h=max(mll)-1.92, col="red", lwd=2, lty=2)
abline(v=mllCI(scaleVals, mll), col="blue", lty=3, lwd=2)

## @knitr beeExampleLR
G <- 2 * max(mll - beeD(10))
pchisq(G, df=1, lower.tail=F)


## @knitr mleBeesBruteForce
shape = seq(1,2.5,.01)
scale1 = seq(10,35,.01)
llmat <- matrix(rep(NA, length(shape) * length(scale1)), nrow=length(shape))
for (i in 1:length(shape)) { 
  for(j in 1:length(scale1)) {
    llmat[i,j] <- - sum(dgamma(bees$hours, shape=shape[i], scale=scale1[j], log=TRUE))
 }
}

#plot the contours
contour(shape, scale1, llmat, levels=c(140.66, 140.7, 140.75, 141, 142, 143, 144, 145, 150, 155), xlab="Shape", ylab="Scale", main="- Log Likelihood")


## @knitr profileBrute
profileFromMat <- function(rowCol) data.frame(t(apply(llmat, rowCol, function(x) c(ll = min(x), idx = which(x == min(x))))))

shapeProfile <- profileFromMat(1)
shapeProfile$scale <- scale1[shapeProfile$idx]

#show the shape profile on the plot
contour(shape, scale1, llmat, levels=c(140.66, 140.7, 140.75, 141, 142, 143, 144, 145, 150, 155), xlab="Shape", ylab="Scale")
lines(shape,  shapeProfile$scale, lwd=2, col="red")

## @knitr profileBrute2
scaleProfile <- profileFromMat(2)
scaleProfile$shape <- shape[scaleProfile$idx]

#show the shape profile on the plot
contour(shape, scale1, llmat, levels=c(140.66, 140.7, 140.75, 141, 142, 143, 144, 145, 150, 155), xlab="Shape", ylab="Scale")
lines(shape,  shapeProfile$scale, lwd=2, col="red")
lines(scaleProfile$shape, scale1, lwd=2, col="blue", lty=2)

####

## @knitr profileBruteMLE
#see http://www.math.mcmaster.ca/~bolker/emdbook/lab6.html for inspiration

#need a function to make mle2 work properly
beeLL <- function(shape, scale) -sum(dgamma(bees$hours, 
                                            shape=shape, 
                                            scale=scale, log=TRUE))

scaleProfile <- sapply(shape, function(x){
  mleBeesScale <- mle2(beeLL, data=bees, start=list(scale=4), fixed=list(shape=x))
  return(coef(mleBeesScale)[2])
})

#get the profile for shape
shapeProfile <- sapply(scale1, function(x){
  mleBeesShape <- mle2(beeLL, data=bees, start=list(shape=1), fixed=list(scale=x))
  return(coef(mleBeesShape)[1])
  
})

contour(shape, scale1, llmat, levels=c(140.66, 140.7, 140.75, 141, 142, 143, 144, 145, 150, 155), xlab="Shape", ylab="Scale")
lines(shape, scaleProfile, lwd=2, col="red")

## @knitr profileBruteMLE2
contour(shape, scale1, llmat, levels=c(140.66, 140.7, 140.75, 141, 142, 143, 144, 145, 150, 155), xlab="Shape", ylab="Scale")
lines(shape, scaleProfile, lwd=2, col="red")
lines(shapeProfile, scale1, lwd=2, lty=2, col="blue")

## @knitr mleBeesFunction
#first write a function that you want to minimize
beeLL <- function(shape, scale) -sum(dgamma(hours,
										shape=shape, scale=scale, 
										log=TRUE))

#now feed the function to an optimizer
beeLL_Fit <- mle2(beeLL, data=bees, start=list(shape=1, scale=4))

## @knitr mleBeesOutput
summary(beeLL_Fit)


## @knitr mleBeesProfile
plot(profile(beeLL_Fit))

## @knitr mleBeesCI
confint(beeLL_Fit)

# Wald CIs assume quadratic relationship at peak
confint(beeLL_Fit, method="quad")


## @knitr mleBees
mleBees <- mle2(hours ~ dgamma(shape=shape, scale=scale), 
                data=bees, start=list(shape=1,scale=4))

## @knitr mleBeesAnova
mleBeesOrig <- mle2(hours ~ dgamma(shape=1, scale=scale), 
                data=bees, start=list(scale=4))

anova(mleBees, mleBeesOrig)


## @knitr mleAnything
#You can represent many relationships with a function
#or just using mle2 - so, poisson regression
#you could write a function
flowerLL <- function(b, int){
  fittedFlowers = b * nitrogen + int
  
  -sum(dpois(Flowers, lambda = fittedFlowers, log=T))
}

mleFlowers <- mle2(flowersLL, data=flowers, 
            start=list(b=2, int = 4))

## @knitr mleAnything2
#Or...
mleFlowers <- mle2(Flowers ~ dpois(lambda = b * nitrogen + Int), 
                data=flowers, start=list(b=2, Int = 4))

## @knitr regressionMLE
wolves <- read.csv("./data/16e2InbreedingWolves.csv")

wolfFun <- function(intercept, inbreeding, wolves_sd){
  pupsHat <- intercept + inbreeding*wolves$inbreeding.coefficient
  -sum(dnorm(wolves$pups, mean = pupsHat, sd = wolves_sd, log=TRUE))
}

wolf_mle<-mle2(wolfFun, 
               start=list(intercept = 0, inbreeding=0, wolves_sd=3))


## @knitr regressionMLE2

wolf_mle2<-mle2(pups ~ dnorm(mean = intercept + 
                            inbreeding*inbreeding.coefficient, 
                             sd = wolves_sd), 
                data=wolves, 
                start=list(intercept = 0, inbreeding=0, wolves_sd=3))

## @knitr regressionMLEInfo
summary(wolf_mle2)

## @knitr regressionMLECI
confint(wolf_mle2)


## @knitr regressionMLEProfile
plot(profile(wolf_mle2))

## @knitr regressionNull
wolf_mleNull<-mle2(pups ~ dnorm(intercept, wolves_sd), 
                data=wolves, start=list(intercept = 0, wolves_sd=3))

anova(wolf_mle2, wolf_mleNull)

## @knitr MLEtoLM
wolf_lm <- lm(pups ~ inbreeding.coefficient, data=wolves)

summary(wolf_lm)