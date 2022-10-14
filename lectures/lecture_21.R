## @knitr lecture21prep
library(contrast)
library(plyr)
library(ggplot2)
library(multcomp)
library(car)
library(QuantPsyc)
keeley <- read.csv("./data/Keeley_rawdata_select4.csv")
neand <- read.csv("./data/18q09NeanderthalBrainSize.csv")
algae <- read.csv("./data/18e3IntertidalAlgae.csv")

## @knitr neand_plot
neand_plot <- qplot(lnmass, lnbrain, data=neand, color=species, size=I(3))  + theme_bw()
neand_plot


## @knitr neand_withlines
mass <- ddply(neand, .(species), summarise, lnmass=c(min(lnmass), max(lnmass)))
neand_plot + geom_vline(data=mass, mapping=aes(xintercept=lnmass, color=species), lty=2)

## @knitr algae_plot
algae_plot <- qplot(height, sqrtarea,  data=algae, geom="boxplot", fill=herbivores) + theme_bw()
algae_plot

## @knitr keeley_int_plot
keeley$egroup <- keeley$elev<600
k_plot <- qplot(age, firesev, data=keeley, color=elev, size=elev)  + theme_bw() +
  scale_color_continuous(low="blue", high="red")
k_plot 

## @knitr keeley_int_plot2
k_plot + stat_smooth(method="lm", aes(group=egroup))


## @knitr neand_boxplot
neand_plot_box <- qplot(species, lnbrain, data=neand, fill=species, geom="boxplot")  + theme_bw()
neand_plot_box



## @knitr neand_boxplot2
neand_plot_box2 <- qplot(species, lnmass, data=neand, fill=species, geom="boxplot")  + theme_bw()
neand_plot_box2
## @knitr neand_xyplot
neand_summarise <- ddply(neand, .(species), summarise,
                         lnmass = mean(lnmass), lwr.mass = mean(lnmass) - 1.96*sd(lnmass)/sqrt(length(lnmass)),
                         upr.mass = mean(lnmass) + 1.96*sd(lnmass)/sqrt(length(lnmass)),
                         lnbrain = mean(lnbrain), lwr.brain = mean(lnbrain) - 1.96*sd(lnbrain)/sqrt(length(lnbrain)),
                         upr.brain = mean(lnbrain) + 1.96*sd(lnbrain)/sqrt(length(lnbrain))
                         )
#
neand_plot_xy <- ggplot(data=neand_summarise, mapping=aes(x=lnmass, y=lnbrain, 
                                                          ymin=lwr.brain, ymax=upr.brain,
                                                          xmin=lwr.mass, xmax=upr.mass)) +
                geom_point(size=3) + geom_errorbar(width=0) + geom_errorbarh(height=0)
  
neand_plot_xy

## @knitr neand_model
neand_lm <- lm(lnbrain ~ species + lnmass, data=neand)

## @knitr anova_compare
anova(neand_lm)
Anova(neand_lm)

## @knitr neand_plot_fit
neand <- cbind(neand, predict(neand_lm, interval="confidence"))

neand_plot +
  geom_line(data=neand, aes(y=fit)) + 
  geom_ribbon(data=neand, aes(ymin=lwr, 
                              ymax=upr), 
                              fill="lightgrey", 
                              alpha=0.5) 


## @knitr neand_cr
crPlots(neand_lm)

## @knitr neand_coef
summary(neand_lm)$coefficients
summary(neand_lm)$r.squared

## @knitr neand_contrast
contrast(neand_lm, 
         list(species="neanderthal", lnmass=mean(neand$lnmass)), 
         list(species="recent", lnmass=mean(neand$lnmass)), 
         type="average")

## @knitr graze_linear
graze_linear <- lm(sqrtarea ~ height + herbivores, data=algae)
Anova(graze_linear)

## @knitr graze_linear_diagnostics
par(mfrow=c(2,2))
plot(graze_linear)
par(mfrow=c(1,1))

## @knitr graze_linear_tukey
residualPlots(graze_linear, plot=F)


## @knitr graze_interaction
graze_int <- lm(sqrtarea ~ height + herbivores + herbivores:height, 
                data=algae)

## @knitr graze_interaction2
#Or, more compact syntax
graze_int <- lm(sqrtarea ~ height*herbivores, data=algae)


## @knitr graze_int_resplot
residualPlot(graze_int)

## @knitr graze_interaction_anova
Anova(graze_int)


## @knitr graze_interaction_coefs
summary(graze_int)$coefficients


## @knitr graze_posthoc
algae$int <- with(algae, interaction(height, herbivores))
graze_int2 <- lm(sqrtarea ~ int, data=algae)
#
library(multcomp)
summary(glht(graze_int2, linfct=mcp(int = "Tukey")))


## @knitr keeley_model
keeley_lm <- lm(firesev ~ age*elev, data=keeley)
Anova(keeley_lm)

## @knitr keeley_coef
summary(keeley_lm)$coef
summary(keeley_lm)$r.squared

## @knitr keeley_vif
vif(keeley_lm2)

## @knitr keeley_meanCenter
keeley$int <- with(keeley, meanCenter(age)* meanCenter(elev))
keeley_lm2 <- lm(firesev ~ age + elev + int, data=keeley)

## @knitr keeley_meanCenter_anova
Anova(keeley_lm2)

## @knitr keeley_meanCenter_coef
summary(keeley_lm2)$coef


## @knitr keeley_predict_df
pred.df <- expand.grid(age = quantile(keeley$age),
                       elev = quantile(keeley$elev))
pred.df <- cbind(pred.df, 
                 predict(keeley_lm, pred.df, interval="confidence"))
#
pred.df$firesev <- pred.df$fit


## @knitr keeley_predict_lines
keeley_fit <- ggplot(data=pred.df, aes(x=age, y=firesev, 
                                       ymin=lwr, ymax=upr, 
                                       group=elev)) +
  geom_line(mapping=aes(color=elev)) +
  scale_color_continuous(low="blue", high="red") + theme_bw()
#
keeley_fit


## @knitr keeley_predict_error
keeley_fit+geom_ribbon(alpha=0.1)


## @knitr keeley_predict_lines_data
k_plot2 <- k_plot+geom_line(data=pred.df, aes(x=age, y=firesev, 
                                              ymin=lwr, ymax=upr, 
                                              group=elev), size=1) 
k_plot2

## @knitr keeley_predict_lines_data_error
k_plot2 + geom_ribbon(data=pred.df, aes(x=age, y=firesev, 
                                        ymin=lwr, ymax=upr, group=elev), 
                      color="grey", size=0, alpha=0.1)


## @knitr keeley_surf
kelev <- seq(min(keeley$elev), max(keeley$elev), 1)
kage <- seq(min(keeley$age), max(keeley$age), .1)
#
firesevMat <- outer(kelev, kage, 
                    function(x,y) predict(keeley_lm, 
                                          data.frame(elev=x, age=y)))
#
filled.contour(kelev, kage, firesevMat,  
               color.palette=heat.colors, 
               xlab="Elevation", ylab="Age", 
               key.title=title(main="Fire\nSeverity"))
