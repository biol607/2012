## @knitr lecture27prep
library(ggplot2)
library(MCMCglmm)
library(coda)


## @knitr wolfPlot
plot(pups ~ inbreeding.coefficient, data=wolves, pch=19)
plot(jitter(pups) ~ jitter(inbreeding.coefficient), data=wolves, pch=19)

## @knitr prior
bayesParams <- function(samp, priormean=0, priorvar=1){
  wo <- 1/priorvar
  wn <- length(samp)/var(samp)
  ybar <- mean(samp)
  
  m <- (wo*priormean + wn*ybar)/(wo+wn)
  v <- 1/(wo+wn)
  
  return(c(m = m, v=v))
  
}

set.seed(55)
samp3 <- rnorm(3, 1,0.5)
samp20 <- rnorm(20, 1,0.5)

p1 <- bayesParams(samp3, 0,0.1)
p2 <- bayesParams(samp20, 0,0.1)

w1 <- bayesParams(samp3, 0,1000)
w2 <- bayesParams(samp20, 0,1000)

samp3 <- rnorm(5)
z1 <- bayesParams(samp3, 0,1000)
z2 <- bayesParams(samp3, 0,0.1)

x<-seq(-3,3,.01)
plot(x, dnorm(x, z1[1], sqrt(z1[2])), col="red", 
     lty=2,  type="l", xlim=c(-2,2), main=("Mean = 0"),
     ylim=c(0,2), lwd=1.5,ylab="density", xlab="Estimate of the Mean")
lines(x, dnorm(x, z2[1], sqrt(z2[2])), col="blue", lty=2)
legend(-1.7,1.5, col=c("red", "blue"), lty=1:2, legend=c("Weak Prior", "Strong Prior"), cex=1.5, lwd=1.5)

## @knitr priorDataStrong
x<-seq(-3,3,.01)
plot(x, dnorm(x,0,0.1), main="Strong Prior: N(0,0.1), True Mean = 1", type="l", ylim=c(0,4), lwd=1.5,ylab="density", xlab="Estimate of the Mean")
lines(x, dnorm(x, p1[1], sqrt(p1[2])), col="red", lty=2, lwd=1.5)
lines(x, dnorm(x, p2[1], sqrt(p2[2])), col="blue", lty=3, lwd=1.5)
legend(-2,3, col=c("black", "red", "blue"), lty=1:3, legend=c("Prior", "n=3", "n=20"), cex=1.5, lwd=1.5)

## @knitr weakPrior
plot(x, dnorm(x,0,sqrt(1000)), main="Weak Prior: N(0,1000), True Mean=1", type="l", ylim=c(0,4))
lines(x, dnorm(x, w1[1], sqrt(w1[2])), col="red", lty=2)
lines(x, dnorm(x, w2[1], sqrt(w2[2])), col="blue", lty=3)
legend(-2,3, col=c("black", "red", "blue"), lty=1:3, legend=c("Prior", "n=3", "n=20"), cex=1.5, lwd=1.5)


## @knitr bayesBruteForce
num <- sapply(x, function(i) prod(dnorm(samp20, i, 1))*dnorm(i, 0, 0.1))

posterior <- num/sum(num)

plot(x, posterior, type="l")
lines(x, dnorm(x, 0, 0.1), col="blue", lty=3, lwd=1.5)

## @knitr mcmcgraphic
set.seed(100)
mod <- MCMCglmm(x ~ 1, data=data.frame(x=rnorm(50)), verbose=FALSE)
plot(mod$Sol)


## @knitr rikz_example
rikz <- read.csv("./data/rikz.csv")
rikz$Beach <- factor(rikz$Beach)
#
library(MCMCglmm)
NAPMod <- MCMCglmm(Richness ~ NAP, data=rikz, verbose=F)

## @knitr rikz_summary
summary(NAPMod)

## @knitr rikz_solution
plot(NAPMod$Sol)

## @knitr rikz_solution_VCV
plot(NAPMod$VCV)

## @knitr rikz_autocor
autocorr(NAPMod$Sol)

## @knitr rikz_hpd
HPDinterval(NAPMod$Sol)

## @knitr other_chains
NAPMod2 <- MCMCglmm(Richness ~ NAP, data=rikz, verbose=F)
NAPMod3 <- MCMCglmm(Richness ~ NAP, data=rikz, verbose=F)
#
library(coda)
chainList <- mcmc.list(NAPMod$Sol, NAPMod2$Sol, NAPMod3$Sol)

## @knitr other_chains_plot
plot(chainList)

## @knitr gelman_rubin
gelman.diag(chainList)

## @knitr DIC
NAPMod$DIC

## @knitr setPrior
prior<-list(B=list(mu=c(0,-3),V=diag(c(1e+10, 1))))
#
NAPMod_Prior <- MCMCglmm(Richness ~ NAP, 
                         data=rikz, verbose=F, prior=prior)


## @knitr nap_prior
summary(NAPMod)$solutions
summary(NAPMod_Prior)$solutions

## @knitr nap_bad
ranModBad <- MCMCglmm(Richness ~ NAP, random=~ Beach + NAP:Beach, data=rikz, burnin=0, nitt=1000, verbose=F, thin=1 )
plot(ranModBad$VCV)

## @knitr lab
ranMod1 <- MCMCglmm(Richness ~ NAP*angle1, random=~ Beach + NAP:Beach, data=rikz, verbose=F )
ranMod2 <- MCMCglmm(Richness ~ NAP+angle1, random=~ Beach +NAP:Beach, data=rikz, verbose=F )
ranMod3 <- MCMCglmm(Richness ~ NAP*angle1, random=~ Beach , data=rikz, verbose=F )
ranMod4 <- MCMCglmm(Richness ~ NAP+angle1, random=~ Beach, data=rikz, verbose=F )

ran_prior<-list(B=list(mu=c(0,-10,0,-5),V=diag(c(1e+10, 1,1e+10,3))))
ranModPrior <- MCMCglmm(Richness ~ NAP*angle1, random=~ Beach, data=rikz, verbose=F, prior=ran_prior )
summary(ranModPrior)

summary(ranMod4)$solutions
summary(ranModPrior)$solutions