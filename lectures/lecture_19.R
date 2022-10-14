## @knitr lecture19prep
library(contrast)
library(plyr)
library(ggplot2)
library(multcomp)
library(car)


zoop <- read.csv("./data/18e2ZooplanktonDepredation.csv")
algae <- read.csv("./data/18e3IntertidalAlgae.csv")
neand <- read.csv("./data/18q09NeanderthalBrainSize.csv")

## @knitr zoop_factor
zoop$block <- factor(zoop$block)


## @knitr zooplankton_boxplot
par(mfrow=c(1,2))
boxplot(zooplankton ~ treatment, data=zoop, xlab="Treatment")
boxplot(zooplankton ~ block, data=zoop, xlab="Block")
par(mfrow=c(1,1))

## @knitr zooplankton_intplot1
with(zooplankton, interaction.plot(treatment, block, zooplankton))

## @knitr zooplankton_intplot2
qplot(treatment, zooplankton, data=zoop, color=factor(block), geom="line", group=factor(block)) + theme_bw() 


## @knitr zoop_lm
zoop_lm <- lm(zooplankton ~ treatment + block, data=zoop)

## @knitr zoop_diag
par(mfrow=c(2,3))
plot(zoop_lm, which=1:5)
par(mfrow=c(1,1))

## @knitr zoop_diag2
library(car)
residualPlots(zoop_lm)


## @knitr zoop_anova
anova(zoop_lm)


## @knitr zoop_summary
summary(zoop_lm)$coef

## @knitr zoop_summary2
zoop_noint <- update(zoop_lm, . ~ .-1)
summary(zoop_noint)

## @knitr zoop_cr
crPlots(zoop_lm)

## @knitr zoop_contrast
contrast(zoop_lm, 
         list(treatment="control", block=levels(zoop$block)), 
         list(treatment="high", block=levels(zoop$block)), 
         type="average")

## @knitr load_bees
bees <- read.csv("./data/18q07BeeGeneExpression.csv")
bees$colony <- factor(bees$colony)

bee_lm <- lm(Expression ~ type + colony, data=bees)

## @bee_diag1
residualPlots(bee_lm)

## @bee_diag2
par(mfrow=c(2,3))
plot(bee_lm)
par(mfrow=c(1,1))

## @knitr bee_anova
anova(bee_lm)

## @knitr bee_crplot
crPlots(bee_lm)

## @knitr unbalance
zoop_u <- zoop[-c(1,2),]

## @knitr unbalanced_anova
zoop_u_lm <- update(zoop_lm, data=zoop_u)
anova(zoop_u_lm)

## @knitr unbalanced_Anova
Anova(zoop_u_lm)


## @knitr ss_explained
zoop_intOnly <- lm(zooplankton ~ 1, data=zoop)
zoop_treatment <- lm(zooplankton ~ treatment , data=zoop)

anova(zoop_intOnly, zoop_treatment)


## @knitr ss_explained2
anova(zoop_treatment, zoop_lm)

## @knitr ss_explained2_unbalanced
zoop_u_lm1 <- lm(zooplankton ~ treatment + block, data=zoop_u)
zoop_u_lm2 <- lm(zooplankton ~ block + treatment, data=zoop_u)

## @knitr ss_explained2_unbalanced2
anova(zoop_u_lm1)
anova(zoop_u_lm2)

## @knitr unbalanced_Anova
Anova(zoop_u_lm1)
