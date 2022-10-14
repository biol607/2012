## @knitr lecture24prep
library(nlme)
library(ggplot2)
library(reshape2)
library(car)
library(plyr)
library(contrast)


plants <- read.csv("./data/nestedGrowth.csv")
rikz <- read.csv("./data/rikz.csv")
rikz$Beach <- factor(rikz$Beach)

## @knitr plot_plantData
plantPlot <- ggplot(data=plants, aes(x=Pot, y=Growth,color=Treatment, group=Pot)) + theme_bw()

plantPlot + geom_point(size=3)


## @knitr plot_plantData_means
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  shape=17, geom=geom, size = 6, ...)
}

plantPlot+ geom_point(alpha=0.3) + stat_sum_single(mean)

## @knitr lm_plants_means
meanPlants <- ddply(plants, .(Pot, Treatment), summarise, Growth = mean(Growth))

plantGrowth = lm(Growth ~ Treatment, data=meanPlants)

Anova(plantGrowth)

## @knitr lm_plants_Error
plantAOV <- aov(Growth ~ Treatment + Error(Pot), data=plants)

## @knitr lm_plants_Error_anova
summary(plantAOV)

## @knitr plant_GLS
library(nlme)
PotVar <- corCompSymm( form =~ 1 | Pot)
plantGLS <- gls(Growth ~ Treatment, data=plants, correlation=PotVar)
anova(plantGLS, type="m")

## @knitr plant_lme
library(nlme)
plantLME <- lme(Growth ~ Treatment, random = ~ 1|Pot, data=plants)

## @knitr plant_lme_summary
summary(plantLME)

## @knitr plant_lme_anova
anova(plantLME, type="marginal")

## @knitr errorCompare
par(mfrow=c(2,1))
plot(density(ranef(plantLME)[,1]), xlim=c(-6,6), main="Among Pot Variation")
plot(density(residuals(plantLME)), xlim=c(-6,6), main="Within Pot Variation")
par(mfrow=c(1,1))

## @knitr fittedCompare
fitted(plantLME, level=0)
fitted(plantLME, level=1)

## @knitr residualsCompare
residuals(plantLME, level=0)
residuals(plantLME, level=1)

## @knitr diagnostic1
plot(fitted(plantLME, level=1), residuals(plantLME, level=1))
abline(a=0, b=0,col="red")

## @knitr qqresiduals
qqnorm(residuals(plantLME,  type="pearson"))
abline(a=0,b=1, col="red", lty=2)

## @knitr diagnostic0
plot(fitted(plantLME, level=0), residuals(plantLME, level=0))
abline(a=0, b=0,col="red")

## @knitr ranef_plants
ranef(plantLME)

## @knitr qqranef
r<-ranef(plantLME)[,1]
qqnorm(r/sd(r))
abline(a=0,b=1, col="red", lty=2)

## @knitr visualizeFit0
plantLME2 <- lme(Growth ~ Treatment-1, random = ~ 1|Pot, data=plants)

fixDF <- data.frame(summary(plantLME2)$tTable)
fixDF$Treatment <- gsub("Treatment", "", rownames(fixDF))

ggplot(fixDF, aes(x=Treatment, y=Value, 
                  ymin=Value - Std.Error*2, 
                  ymax=Value+Std.Error*2, color=Treatment)) +
  theme_bw() +
  ylab("Growth") +
  geom_point(size=4) +
  geom_linerange() +
  ggtitle("Fixed Effects")



## @knitr visualizeFit1
fixRanDF <- data.frame(Treatment = rep(plants$Treatment, 2), 
           level= factor(c(rep(0,36), rep(1, 36)), levels=c(1,0)),
           Growth = c(fitted(plantLME, level=0), fitted(plantLME, level=1)))

qplot(Treatment, Growth, alpha=level, color=Treatment, data=fixRanDF, size=I(4)) +
  theme_bw()


## @knitr contrastPlants
library(contrast)
contrast(plantLME,
         list(Treatment = c("Add C", "Add N")),
         list(Treatment = "Control"))

## @knitr plant_lme_ml
plantLME.ml <- lme(Growth ~ Treatment, random = ~ 1|Pot, data=plants, method="ML")
anova(plantLME.ml)

## @knitr shrinkage_plants
library(reshape2)
shrinker <- plants
shrinker$fitted <- fitted(plantLME, level=1)
shrinker = ddply(shrinker, .(Pot, Treatment), summarise, Growth=mean(Growth), fitted = mean(fitted))
shrinker$id <- 1:nrow(shrinker)

shrinker <- melt(shrinker, c("Pot", "Treatment", "id"), c("Growth", "fitted"))
qplot(variable, value, color=Treatment, group=id, data=shrinker, size=I(6)) + geom_line(size=1) + xlab("") + ylab("Estimate")

## @knitr fixef_coef
fixef(plantLME)
coef(plantLME)

## @knitr breakbreak
####################################################################################################################

## @knitr rikzFit
rikzInt <- lme(Richness ~ 1, random = ~1|Beach, data=rikz)
#
rikzNoBeach <- gls(Richness ~ 1,  data=rikz)

## @knitr rikz_evalRanef
anova(rikzInt, rikzNoBeach)

## @knitr rikz_diag
plot(rikzInt)

## @knitr rikz_diag1
rr <- ranef(rikzInt)[,1] / sd(ranef(rikzInt)[,1])
qqnorm(rr)
abline(0,1,col="red")

## @knitr rikz_ranef_dist
qplot(ranef(rikzInt)[,1], geom="density", fill=I("red"), alpha=0.2) + theme_bw()

## @knitr rikz_ranef_with_error
se.ranef <- function(obj) ranef(obj, standardized=T)/sapply(ranef(obj), sd)

ggplot(mapping=aes(x=1:9, y=ranef(rikzInt)[,1], 
                   ymin=(ranef(rikzInt)-2*se.ranef(rikzInt))[,1],
                   ymax=(ranef(rikzInt)+2*se.ranef(rikzInt))[,1])) +
  geom_point() + geom_linerange() + xlab("Beach") + ylab("Beach Effect") +
  geom_hline(yintercept=0, lty=2) + theme_bw() + coord_flip() 

## @knitr se_ranef
se.ranef <- function(obj) 
  ranef(obj, standardized=T)/sapply(ranef(obj), sd)

## @knitr show_types
set.seed(697)
a<-rnorm(5)
par(mfrow=c(1,3), cex.main=2, cex.lab=1.5, cex.axis=1.5)
matplot(rbind(a, a+1), type="l", xlab="x", ylab="y", main="Variable Intercept")
matplot(rbind(rep(0,5), a), type="l", xlab="x", ylab="y", main="Variable Slope")
matplot(rbind(a, a+rnorm(5)*1), type="l", xlab="x", ylab="y", main="Variable Slope &Intercept")
par(mfrow=c(1,1))

## @knitr variable-slope-intercept
qplot(NAP, Richness, group=Beach, color=Beach, data=rikz) + stat_smooth(method="lm")


## @knitr others

qplot(Treatment, Growth, color=Pot, data=plants)

avgplants <- ddply(plants, .(Pot, Treatment), summarise, Growth=mean(Growth))

PotVar <- varIdent(~Pot)

badLM <- lm(Growth ~ Treatment + Pot, data=plants)
badLM2 <- lm(Growth ~ Treatment, data=plants)
avgLM <- lm(Growth ~ Treatment, data=avgplants)
tradAOV <- aov(Growth ~ Treatment + Error(Pot), data=plants)
tradLME <- lme(Growth ~ Treatment, random =~ 1|Pot, data=plants)
tradLMEML <- lme(Growth ~ Treatment, random =~ 1|Pot, data=plants, method="ML")
tradGLS <- gls(Growth ~ Treatment, data=plants, weights=PotVar)

summary(tradAOV)
anova(avgLM)
anova(tradLME)


#show shrinkage

#shrink ranef
ranLME <- lme(Growth ~ 1, random=~1|Pot, data=plants)
shrink2 <- plants
shrink2$fitted <- fitted(ranLME)
shrunkenMeans <- ddply(shrink2, .(Pot), summarise, Raw = mean(Growth), Fit = mean(fitted))
#shrink2 <- melt(shrink2, c("Pot", "Treatment", "id"))
#qplot(variable, value, color=Pot, group=id, data=shrink2) + 
#  geom_line(size=1) + 
#  geom_hline(yintercept=mean(plants$Growth)) +
#  geom_hline(yintercept=ddply(plants, .(Pot), summarise, g = mean(Growth))$g)

shrunkenMeansMelted <- melt(shrunkenMeans, "Pot")
qplot(variable, value, group=Pot, geom=c("point", "line"), data=shrunkenMeansMelted) +
  theme_bw()



intModel <- lme(Richness ~ 1 +exposure, random =~1| Beach, data=RIKZ)

slopeIntModel <- lme(Richness ~ 1 + NAP, random =~ 1+NAP| Beach, data=RIKZ)

anova(intModel)
anova(slopeIntModel)
anova(plantLME)
