## @knitr lecture16prep
library(ggplot2)
crypto <- read.csv("./data/cryptoDATA.csv")
crypto.glm <- glm(cbind(Y, N-Y) ~ Dose, data=crypto, family=binomial)


## @knitr logitPlot
x<-seq(-4,4,.01)
plot(x, 1/(1+exp(-x)), type="l", ylab="Probability", xlab="X")


## @knitr crypto_glms
# 1) using Heads, Tails 
glm(cbind(Y, N-Y) ~ Dose, data=crypto, family=binomial)
#
#
# 2) using weights as size parameter for Binomial
glm(Y/N ~ Dose, weights=N, data=crypto, family=binomial)

## @knitr crypto_ggplot
cryptoPlot <- qplot(Dose, Y/N, data=crypto) +
  theme_bw() +
  ylab("Fraction of Mice Infected")
cryptoPlot

## @knitr crypto_ggplot_fit
cryptoPlot+stat_smooth(method="glm", aes(weight=N), family="binomial", col="red", lwd=2) 
  
## @knitr crypto_diagnostics
par(mfrow=c(2,2))
plot(crypto.glm)
par(mfrow=c(1,1))

## @knitr crypto_summary
summary(crypto.glm)

## @knitr load_seeds
seeds <- read.csv("./data/seedbank.csv", na.strings="-")


## @knitr seeds_trans
seeds <- within(seeds, {
  log.seed.weight <- log(seed.weight)
})

## @knitr seeds_plot
qplot(log.seed.weight, Predation, data=seeds,position= position_jitter(w = 0.1, h=0.01)) +
  theme_bw()


## @knitr seeds_glm
seed.glm <- glm(Predation ~ log.seed.weight, 
                data=seeds, family=binomial)



## @knitr seed_plot_fitted
qplot(log.seed.weight, Predation, data=seeds,position= position_jitter(w = 0.2, h=0.02)) +
  theme_bw() +
  stat_smooth(method="glm", family=binomial, color="red", lwd=2)

## @knitr seed_diagnostics
par(mfrow=c(2,2))
plot(seed.glm)
par(mfrow=c(1,1))

## @knitr seeds_binned_resids
binplot <- function(x,y,nclass=10, ...){
  idx <- sort(x, index.return=T)$ix
  x<-x[idx]
  y<-y[idx]
  bins <- seq(1, length(x), floor(length(x)/nclass))
  q <- ifelse(range(bins)[2] != length(idx),  bins <- c(bins,length(x)), bins)
  bins <- cbind(bins[-length(bins)], bins[-1])
  
  ret<- apply(bins, 1, function(b) return(c(x=mean(x[b[1]:b[2]]), y=mean(y[b[1]:b[2]])))) 
  
  plot(ret[1,], ret[2,], xlab="Fitted", ylab="Residual", pch=19, main=paste(nclass, " Bins", sep=""), ...)
  
}

binplot( x=fitted(seed.glm), y=residuals(seed.glm), nclass=200)

