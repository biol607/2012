## @knitr lecture25prep
library(nlme)
library(ggplot2)
library(reshape2)
library(car)
library(plyr)
library(contrast)
library(lme4)


rikz <- read.csv("./data/rikz.csv")
rikz$Beach <- factor(rikz$Beach)

#birds <- read.table("./data/Hawaii.txt", header=T)
#birds <- birds[!is.na(birds$Moorhen.Kauai),]
#allbirds <- melt(birds, c("Year", "Rainfall"))
#names(allbirds)[3:4]<- c("Site", "Birds")
#allbirds <- na.omit(allbirds)
#write.csv(allbirds, "./data/allbirds.csv")
allbirds <- read.csv("./data/allbirds.csv")

boreal <- read.table("./data/Boreality.txt", header=T)
# 
# 
# plankton <- read.table("./data/RIKZDATAEnv.txt", header=T)
# plank <- ddply(plankton, .(Year, Month, Station), summarise, 
#                DIN = mean(DIN), DIP = mean(DIP), CHLFa = mean(CHLFa),
#                SIL = mean(SIL), TN = mean(TN), TP = mean(TP), SAL = mean(SAL), T = mean(T), ZS = mean(ZS))
# plank <- na.omit(plank)
# write.csv(plank, "./data/planktonSummary.csv")
# plankton <- read.csv("./data/planktonSummary.csv")
# plank <- ddply(plankton, .(Year, Station), summarise, 
#                DIN = mean(DIN), DIP = mean(DIP), CHLFa = mean(CHLFa),
#                SIL = mean(SIL), TN = mean(TN), TP = mean(TP), SAL = mean(SAL), T = mean(T), ZS = mean(ZS))
# write.csv(plank, "./data/planktonMeans.csv")
plankton <- read.csv("./data/planktonMeans.csv")

## @knitr showRikz
head(rikz)[,c(6:9, 16)]

## @knitr varInt
varIntExposure<- lme(Richness ~ exposure, 
                     random =~ 1 | 
                       Beach, data=rikz)
anova(varIntExposure, type="m")

## @knitr exposure_plot
p <- qplot(exposure, Richness, data=rikz, alpha=I(1), 
           color=Beach, size=I(1.4)) + 
             geom_jitter(position = position_jitter(width = .1, height=0)) +
             theme_bw()
p

## @knitr varInt_plot
adf <- as.data.frame(t(fixef(varIntExposure)))
#
names(adf)[1] <- "intercept"
#
p+  geom_abline(data=adf, 
              mapping=aes(intercept=intercept, slope=exposure), 
              lwd=2)
## @knitr varInt_plot2
adf2 <- as.data.frame(coef(varIntExposure))
#
names(adf2)[1] <- "intercept"
#
p+ geom_abline(data=adf2, 
               mapping=aes(intercept=intercept, slope=exposure), 
               color="grey") 

## @knitr NAP_plot
napPlot <- qplot(NAP, Richness, group=Beach, color=Beach, data=rikz, size=I(4)) + theme_bw()
napPlot

## @knitr variable-slope-intercept_raw
napPlot + stat_smooth(method="lm")


## @knitr variable-slope-intercept_1
varSlope <- lme(Richness ~ NAP, 
                random =~ 0 + NAP | Beach, data=rikz)
#
varInt<- lme(Richness ~ NAP, 
             random =~ 1 | Beach, data=rikz)
#
varSlopeInt <- lme(Richness ~ NAP, 
                   random =~ 1 + NAP | Beach, data=rikz)

## @knitr variable-slope-ranef
ranef(varSlope)

## @knitr variable-slope-summary
summary(varSlope)

## @knitr variable-slope-intercept-ranef
ranef(varSlopeInt)

## @knitr compare_ranef_vsi
anova(varSlope, varSlopeInt)
anova(varSlopeInt, varInt)

## @knitr eval_fixed_vsi
anova(varSlopeInt, type="m")

## @knitr variable-slope-intercept_plot
napCoef <- coef(varSlopeInt)
names(napCoef)[1] <- "intercept"

napFixef <- as.data.frame(t(fixef(varSlopeInt)))
names(napFixef)[1] <- "intercept"
napPlot + 
  geom_abline(data=napCoef, mapping=aes(intercept=intercept, slope=NAP), color="grey", alpha=0.9) +
  geom_abline(data=napFixef, mapping=aes(intercept=intercept, slope=NAP), color="black", lwd=2)
  


## @knitr angle_mods
angI <- lme(Richness ~ angle1*NAP, random =~ 1  | Beach, data=rikz)
angS <- lme(Richness ~ angle1*NAP, random =~ 0 + angle1 | Beach, data=rikz)
angIS <- lme(Richness ~ angle1*NAP, random =~ 1 +  angle1 | Beach, data=rikz)


## @knitr angle_compare
anova(angI, angIS)
anova(angI, angS)

## @knitr angle_anova
anova(angI, type="m")

angI2 <- lme(Richness ~ angle1+NAP, random =~ 1  | Beach, data=rikz)
anova(angI2, type="m")

## @knitr lmer_ang
library(lme4)
angI_lmer <- lmer(Richness ~ angle1*NAP + (1  | Beach), 
                  data=rikz, family=poisson(link="log"))
Anova(angI_lmer)


#nestNest
data(Oats)
Oats$nitro <- ordered(Oats$nitro)
olme <- lme(yield ~ Variety*nitro, random =~ 1 | Block/Variety, data=Oats)
olme2 <- lme(yield ~ Variety*nitro, random =~ 1 | Block, data=Oats)

## @knitr olme_summary
summary(olme)

## @knitr olme_anovas
anova(olme)
anova(olme2)

anova(olme, olme2)

## @knitr lmer
library(lme4)
olme_lmer <- lmer(yield ~ Variety*nitro + (1 | Block/Variety), data=Oats)

## @knitr lmer_anova
Anova(olme_lmer)
anova(olme_lmer)

## @knitr lmer_summary
summary(olme_lmer)

## @knitr lmer_ranef_compare
olme_lmer2 <- lmer(yield ~ Variety*nitro + (1 | Block), data=Oats)
anova(olme_lmer, olme_lmer2)

## @knitr lmer_glm
olme_glmer <- lmer(yield ~ Variety*nitro + (1 | Block/Variety), data=Oats, family=gaussian(link="log"))

anova(olme_glmer, olme_lmer)

## @knitr bird_ts_plot
birdPlot <- qplot(Year, Birds, data=allbirds, geom=c("line", "point"), color=Rainfall) +
  theme_bw() +
  facet_wrap(~Site)
birdPlot

## @knitr bird_repeated
allbirds_repeated <- lme(Birds ~ Rainfall, random= ~1|Site, 
                         data=allbirds)

## @knitr bird_ts_fit
allbirds_lme <- lme(Birds ~ Rainfall + Year, random= ~1|Site, 
                    data=allbirds)

## @knitr bird_ts_fit_resid
qplot(Year, residuals(allbirds_lme), facets=~Site, 
      data=allbirds) + theme_bw()

## @knitr bird_ts_fit_acf
acf(residuals(allbirds_lme))

## @knitr bird_ts_fit_AR1
birds_corAR <- corAR1 (form =~ Year)
#
allbirds_lme_ar <- lme(Birds ~ Rainfall + Year, random= ~1|Site, 
                       data=allbirds, correlation=birds_corAR)

## @knitr bird_ts_fit_compare1
anova(allbirds_lme_ar, allbirds_lme)

## @knitr bird_ts_fit_compare2
anova(allbirds_lme_ar, type="m")
anova(allbirds_lme, type="m")

## @knitr bird_fit_plot
allbirds$fit <- predict(allbirds_lme_ar)
birdPlot + geom_line(color="red", data=allbirds, mapping=aes(y=fit))
r2 <- 1-sum(residuals(allbirds_lme_ar)^2)/sum((allbirds$Birds - mean(allbirds$Birds))^2)
paste("R^2 = 1 - RSS/TSS = ",round(r2,2), sep="")

## @knitr boreal_gls_residualPlot
qplot(x, y, data=boreal, size=Wet, color=NDVI) +
  theme_bw() + 
  scale_size_continuous("Index of Wetness", range=c(0,10)) + 
  scale_color_gradient("NDVI", low="lightgreen", high="darkgreen")

## @knitr boreal_gls
bor_gls <- gls(NDVI ~ Wet, data=boreal)

## @knitr boreal_gls_residualPlot
qplot(x, y, data=boreal, size=abs(residuals(bor_gls, type="normalized")), color=factor((residuals(bor_gls)>0))) +
  theme_bw() + scale_size_continuous("Absolute Value of Residual", range=c(0,20)) + scale_color_discrete("Residual > 0?")

## @knitr boreal_gls_variogram
plot(Variogram(bor_gls, form=~x+y, robust=T, maxDist=2000, resType="normalized"))

## @knitr boreal_spatial
spaceCor <- corExp(form =~ x+y, nugget=T)
bor_gls_space <- gls(NDVI ~ Wet, data=boreal, correlation=spaceCor)

## @knitr boreal_gls_residualPlot
qplot(x, y, data=boreal, size=abs(residuals(bor_gls_space, type="normalized")), color=factor((residuals(bor_gls)>0))) +
  theme_bw() + scale_size_continuous("Absolute Value of Residual", range=c(0,5)) + scale_color_discrete("Residual > 0?")

## @knitr boreal_spatial_variogram
plot(Variogram(bor_gls_space, form=~x+y, robust=T, maxDist=2000, resType="normalized"))

summary(bor_gls_space)
## @knitr plankton_plot
plankPlot <- qplot(Year, DIN, facets=~Station, data=plankton, scale="free", color=T, size=SAL) + theme_bw() +
  scale_color_gradient(low="blue", high="red") + theme(axis.text.x=element_text(angle=45, hjust=0.5, vjust=0.5))
plankPlot

## @knitr Plankton_models
plankLME_nocorr<-lme(DIN ~ Year+SAL + T, 
                     random =~ 1|Station, data=plankton)
#
plankLME<-lme(DIN ~ Year+SAL + T, random =~ 1|Station, 
              correlation=corAR1(form=~Year), data=plankton)
#
plankLME_IS<-lme(DIN ~ Year+SAL + T, random =~ 1+T|Station, 
                 correlation=corAR1(form=~Year), data=plankton)

## @knitr Plankton_modselect1
anova(plankLME_nocorr, plankLME)
anova(plankLME, plankLME_IS)

## @knitr Plankton_fixTest
anova(plankLME_IS, type="m")

## @knitr Plankton_evaluate
summary(plankLME_IS)

## @knitr Plankton_r2
1-sum(residuals(plankLME_IS)^2) / sum((plankton$DIN - mean(plankton$DIN))^2)

## @knitr plankton_plot_fit
plankton$fit <- predict(plankLME_IS)
plankPlot + geom_line(data=plankton, mapping=aes(y=fit), color="black", lwd=1.5)

## @knitr show_types
set.seed(697)
a<-rnorm(5)
par(mfrow=c(1,3), cex.main=2, cex.lab=1.5, cex.axis=1.5)
matplot(rbind(a, a+1), type="l", xlab="x", ylab="y", main="Variable Intercept")
matplot(rbind(rep(0,5), a), type="l", xlab="x", ylab="y", main="Variable Slope")
matplot(rbind(a, a+rnorm(5)*1), type="l", xlab="x", ylab="y", main="Variable Slope &Intercept")
par(mfrow=c(1,1))

