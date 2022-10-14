## @knitr lecture11prep
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

## @knitr rnormPlot_cov
plot(y~x, pch=19, data=data_rmnorm)
text(-4,8, paste("cov(x,y) = ",round(cov(data_rmnorm)[1,2],3), sep=""))

## @knitr rnormPlot_cor
plot((y-mean(y))/sd(y)~I(x/sd(x)), pch=19, data=data_rmnorm, xlab="x/sd(x)")
text(-2,2, paste("cor(x,y) = ",round(cor(data_rmnorm)[1,2],3), sep=""))

## @knitr corLevels
set.seed(1001)
par(mfrow=c(2,2))
for(i in c(0.2, 0.4, 0.6, 0.8)){
  xy<-as.data.frame(rmvnorm(200, sigma=matrix(c(1, i, i, 1), byrow=T, nrow=2)))
  names(xy)<-c("x", "y")
  plot(y~x, data=xy, mar=c(3,1,1,2), main=paste("r = ", round(cor(xy)[1,2],2), sep=""))
  
}
par(mfrow=c(1,1))


## @knitr mvnorm_ggplot
qplot(x1, x2, data=data_mvnorm, color=freq, alpha=freq, size=I(5)) + 
  scale_color_continuous("Frequency", low="blue", high="red") +
  scale_alpha_continuous(guide="none") +
  theme_bw()

## @knitr mvnorm_persp
facetCols<-heat.colors(length(unique(data_mvnorm$freq)))
data_mvnorm$fCols<-as.numeric(as.factor(data_mvnorm$freq))
with(data_mvnorm, 
     persp(nums, nums, matrix(freq, nrow=length(nums)),
           xlab="x", ylab="y", zlab="Frequency",
           theta = -90, phi = 25,  ltheta=145, border=gray(0.2),
           col="lightblue", cex.axis=2
           ))


## @knitr wolf_scatterplot
plot(pups ~ inbreeding.coefficient, data=wolves, pch=19, xlab="Inbreeding Coefficient", ylab="# of Pups")

## @knitr wolf_cor_cov
cov(wolves)
cor(wolves)

## @knitr wolves_cor_test
with(wolves, cor.test(pups, inbreeding.coefficient))

## @knitr pufferplot
plot(predators ~ resemblance, data=puffer, pch=19)
cor(puffer)

## @knitr puffer_ha
#get the correlation and se
puff_cor = cor(puffer)[1,2]
se_puff_cor = sqrt((1-puff_cor)/(nrow(puffer)-2))

#t-test with difference from 1
t_puff<-(puff_cor-1)/se_puff_cor
t_puff

#1 tailed, as > 1 is not possible
pt(t_puff, nrow(puffer)-2)


## @knitr break
############################################################

## @knitr regPlot
rm.lm <- lm(y~x, data=data_rmnorm)
plot(y ~ x, data=data_rmnorm, pch=19, col="grey")
abline(rm.lm, col="black", lwd=4)

preds <- predict(rm.lm, se.fit=T, newdata=data_rmnorm, interval="prediction")


## @knitr regPlot_addDist
i<-c(200, 200)

drawError<-function(i){
  x<-data_rmnorm$x[i]
  yTest<-seq(preds$fit[i,2], preds$fit[i,3], .01)
  dens<-dnorm(yTest, preds$fit[i], preds$se.fit[i])
  dens<-dens/max(dens)
  len<-length(dens)
  matplot(rep(x, len)+dens, yTest, add=T, type="l", col="red", lwd=2)
}

drawError(100)
drawError(200)
drawError(300)
drawError(400)

## @knitr cor_and_reg
set.seed(1001)
sampdf <- data.frame(x=1:50)
sampdf <- within(sampdf, {
       y1 <- rnorm(50, 3*x, 10)
       y2 <- rnorm(50, 3*x, 40)
       y3 <- y2/3
})

#cor(sampdf)
#lines and slopes
par(mfrow=c(1,3))
plot(y1 ~ x, data=sampdf, main="Slope = 3, r = 0.98", ylim=c(-60, 180))
abline(lm(y1~x, data=sampdf), lwd=2, col="red")
plot(y2 ~ x, data=sampdf, main="Slope = 3, r = 0.72", ylim=c(-60, 180))
abline(lm(y2~x, data=sampdf), lwd=2, col="red")
plot(y3 ~ x, data=sampdf, main="Slope = 1, r = 0.72", ylim=c(-60, 180))
abline(lm(y3~x, data=sampdf), lwd=2, col="red")
par(mfrow=c(1,1))


## @knitr basic_lm1
wolf_lm <- lm(pups ~ inbreeding.coefficient, data=wolves)
## @knitr basic_lm2
wolf_lm

## @knitr coefs_lm
coef(wolf_lm)

coef(wolf_lm)[1]

## @knitr yhat_lm
fitted(wolf_lm)

coef(wolf_lm)[1] + coef(wolf_lm)[2]*0.25

## @knitr plot_lm
plot(pups ~ inbreeding.coefficient, data=wolves, pch=19)

abline(wolf_lm, col="red", lwd=2)

## @knitr plot_lm2
plot(pups ~ inbreeding.coefficient, data=wolves, pch=19)

matplot(wolves$inbreeding.coefficient, fitted(wolf_lm), 
        add=T, lwd=2, col="red", type="l")

## @knitr ggplot_lm
ggplot(data=wolves, aes(y=pups, x=inbreeding.coefficient)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method="lm", color="red")

## @knitr residual_lm
par(mfrow=c(1,2))
plot(fitted(wolf_lm), residuals(wolf_lm))
#
hist(residuals(wolf_lm), main="Residuals")
par(mfrow=c(1,1))