## @knitr lecture20prep
library(contrast)
library(plyr)
library(ggplot2)
library(multcomp)
library(car)
library(QuantPsyc)
wnv <- read.csv("./data/SwaddleWestNile2002NCEAS_shortnames.csv")
keeley <- read.csv("./data/Keeley_rawdata_select4.csv")
neand <- read.csv("./data/18q09NeanderthalBrainSize.csv")


## @knitr zoop_prep
zoop <- read.csv("./data/18e2ZooplanktonDepredation.csv")
zoop$block <- factor(zoop$block)
zoop_lm <- lm(zooplankton ~ treatment + block, data=zoop)


## @knitr zoop_cr
crPlots(zoop_lm)


## @knitr zoop_contrast
library(contrast)
contrast(zoop_lm, 
         list(treatment="low", block=levels(zoop$block)), 
         list(treatment="high", block=levels(zoop$block)), 
         type="average")


## @knitr keeley_pairs
pairs(keeley)

## @knitr klm
klm <- lm(rich ~ cover + firesev + hetero, data=keeley)


## @knitr klm_diag
par(mfrow=c(2,3))
plot(klm, which=1:5)
par(mfrow=c(1,1))

## @knitr klm_diag2
residualPlots(klm)

## @knitr klm_cor
with(keeley, cor(cbind(cover, firesev, hetero)))

## @knitr klm_vif
vif(klm)

## @knitr keeley_anova
Anova(klm)

## @knitr keeley_coef
summary(klm)$coef
cat(paste("R^2 = ", round(summary(klm)$r.squared, 2), sep=""))

## @knitr keeley_std
library(QuantPsyc)
lm.beta(klm)


## @knitr klm_crplot
crPlots(klm)

## @knitr klm_avplot
avPlots(klm)


## @knitr klm_see_effects

qplot(cover, rich, data=keeley, colour=firesev, size=firesev^2) +
  theme_bw() + 
  scale_color_gradient(low="yellow", high="purple") +
  scale_size_continuous(range=c(1,10))

## @knitr klm_leaveOneOut
dfbetaPlots(klm)

## @knitr wnv_pairs
pairs(wnv[,c(3:8)])

## @knitr wnv_viflm
wnv_lm_vif <- lm(Species.Richness ~ Corvids + 
                                    Sparrows + 
                                    Robins + 
                                    Thrushes , data=wnv)

## @knitr wnv_vif
vif(wnv_lm_vif)

## @knitr wnv_vif_cor
cor(wnv[,c(3:8)])

## @knitr wnv_vif__summary
summary(wnv_lm_vif)

## @knitr wnv_lm
wnv_lm <- lm(Species.Richness ~ Corvids + 
                                Sparrows + 
                                Robins, data=wnv)

## @knitr wnv_lm_vif
vif(wnv_lm)

## @knitr wnv_lm_anova
Anova(wnv_lm)

## @knitr wnv_lm_summary
summary(wnv_lm)

## @knitr wnv_lm_crplot
crPlots(wnv_lm)

