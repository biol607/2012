## @knitr lecture12prep
library(mvtnorm)
library(ggplot2)

#load the pufferfish data
puffer <- read.csv("./data/16q11PufferfishMimicry Caley & Schluter 2003.csv")

#load the wolf data
wolves <- read.csv("./data/16e2InbreedingWolves.csv")

#create a data frame to show a multivariate normal distribution
sigma <- matrix(c(3,2,2,4), ncol=2)
vals <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)

nums<-seq(-5,5,.2)
data_mvnorm<-expand.grid(x1=nums, x2=nums)
data_mvnorm$freq<-dmvnorm(data_mvnorm, sigma=sigma)

#make up some fake data
set.seed(697)
data_rmnorm<-as.data.frame(rmvnorm(400, sigma=sigma))
names(data_rmnorm)=c("x", "y")
data_rmnorm$y<-data_rmnorm$y + 3

## @knitr linefit
set.seed(697)
x<-1:10
y<-rnorm(10, mean=x,sd=2)
a<-lm(y~x)
plot(x,y,pch=19, cex=1.5)
abline(a, lwd=2)
segments(x,fitted(a),x,y, col="red", lwd=2)

## @knitr wolf_lm
wolf_lm <- lm(pups ~ inbreeding.coefficient, data=wolves)

## @knitr wolf_lm_plot
par(mfrow=c(2,2))
plot(wolf_lm)
par(mfrow=c(1,1))

## @knitr wolf_lm_plot_all
par(mfrow=c(2,3))
plot(wolf_lm, which=1:5, cex.axis=1.4)
par(mfrow=c(1,1))

## @knitr wolf_lm_plot_1
plot(wolf_lm, which=1, cex.axis=1.4)
## @knitr wolf_lm_plot_2
plot(wolf_lm, which=2, cex.axis=1.4)
## @knitr wolf_lm_plot_3
plot(wolf_lm, which=3, cex.axis=1.4)
## @knitr wolf_lm_plot_4
plot(wolf_lm, which=4, cex.axis=1.4)

## @knitr wolf_lm_plot_5
plot(wolf_lm, which=5, cex.axis=1.4)

## @knitr wolf_lm_delete
summary(wolf_lm)$coef

summary(lm(pups ~ inbreeding.coefficient, 
        data=wolves, subset=-c(6,7,3)))$coef

## @knitr wolf_lm_residualfit
par(mar=c(5,5,2,2))
plot(pups ~ inbreeding.coefficient, data=wolves, cex.lab=1.8, pch=19, mar=c(5,4,4,2))
abline(wolf_lm, lwd=2)
with(wolves, segments(inbreeding.coefficient, fitted(wolf_lm),
                      inbreeding.coefficient, pups, col="red", lwd=1.5))
par(mar=c(5,4,2,2))

## @knitr df
x<-seq(0,6,.01)
qplot(x,df(x,1,25), geom="line",  xlab="Y", ylab="df(Y)") + theme_bw()

## @knitr anova_wolf
anova(wolf_lm)

## @knitr summary_wolf
summary(wolf_lm)

## @knitr puffer_lm
puffer_lm <- lm(predators ~ resemblance, data=puffer)

## @knitr puffer_lm_plot
par(mfrow=c(2,3))
plot(puffer_lm, which=1:5)
par(mfrow=c(1,1))

## @knitr puffer_anova
anova(puffer_lm)

## @knitr puffer_summary
summary(puffer_lm)

## @knitr ObeseN
set.seed(1001)
par(mfrow=c(2,2))
p<-sapply(seq(10,400, 100), function(n) {
  x<-rnorm(n)
  y<-rnorm(n)
  alm<-lm(y~x)
  p<-anova(alm)[1,5]
  r2<-summary(alm)$r.squared
  plot(y~x, main=paste("p=", round(p,2), " r^2=", round(r2,2), sep=""))
  abline(alm)
})
par(mfrow=c(1,1))

## @knitr ObeseN2
set.seed(10201)
par(mfrow=c(2,2))
p<-sapply(seq(20,325, 100), function(n) {
  x<-rnorm(n)
  y<-rnorm(n, 0.2*x,1)
  alm<-lm(y~x)
  p<-anova(alm)[1,5]
  r2<-summary(alm)$r.squared
  plot(y~x, main=paste("p=", round(p,2), " r^2=", round(r2,2), sep=""))
  abline(alm)
})
par(mfrow=c(1,1))

## @knitr wolf_fitConf
plot(pups ~ inbreeding.coefficient, data=wolves, pch=19)
abline(wolf_lm, col="red", lwd=2)

predFrame <- data.frame(inbreeding.coefficient=seq(0,0.4,.01))
predFitConf <-  predict(wolf_lm, newdata=predFrame, 
                        interval="confidence")


matlines(predFrame, predFitConf[,2:3], type="l", lty=2, col="black")



## @knitr wolf_fitPred
plot(pups ~ inbreeding.coefficient, data=wolves, pch=19, main='interval = "prediction"')
abline(wolf_lm, col="red")

predFrame <- data.frame(inbreeding.coefficient=seq(0,0.4,.01))
predFitPred <-  predict(wolf_lm, newdata=predFrame, 
                        interval="prediction")

matlines(predFrame, predFitPred[,2:3], type="l", lty=2, col="black")

## @knitr fitConf2
qplot(inbreeding.coefficient, pups, data=wolves) + stat_smooth(method="lm") + theme_bw()


## @knitr wolf_lm_delete
wolf_lm_sub <- lm(pups ~ inbreeding.coefficient, 
           data=wolves, subset=-c(6,7,3))

#another way
wolf_lm_sub <- update(wolf_lm, subset=-c(6,7,3))

## @knitr compareSlopes1
#get anova tables for later extraction of MSE
a1 <- anova(wolf_lm)
a2 <- anova(wolf_lm_sub)

#We'll need Sums of Squres from each set of X's
with(wolves, {
  ss1 <<- sum((inbreeding.coefficient - 
              mean(inbreeding.coefficient))^2)
  
  ss2 <<- sum((inbreeding.coefficient[-c(6,7,3)] - 
              mean(inbreeding.coefficient[-c(6,7,3)]))^2)
})

## @knitr compareSlopes2
#calculate the DF
df<-nrow(wolves)*2 -3 -4

#calcaulate the mean square pooled error
msp <- (a1[2,3] + a2[2,3])/(df)
  
#calculate the SE of the difference
sep<-sqrt( msp/ss1 + msp/ss2)

#calculate t
t <- (coef(wolf_lm)[2] - coef(wolf_lm_sub)[2]) /sep

#get the p value
pt(t, df)*2