## @knitr lecture22prep
library(contrast)
library(plyr)
library(ggplot2)
library(multcomp)
library(car)
library(QuantPsyc)
keeley <- read.csv("./data/Keeley_rawdata_select4.csv")


## @knitr keeley_int_plot
keeley$egroup <- keeley$elev<600
k_plot <- qplot(age, firesev, data=keeley, color=elev, size=elev)  + theme_bw() +
  scale_color_continuous(low="blue", high="red")
k_plot 

## @knitr keeley_int_plot2
k_plot + stat_smooth(method="lm", aes(group=egroup))



## @knitr keeley_model
keeley_lm <- lm(firesev ~ age*elev, data=keeley)
Anova(keeley_lm)

## @knitr keeley_III
Anova(keeley_lm)
Anova(keeley_lm, type="III")

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
              scale_color_continuous(low="blue", high="red") + 
              theme_bw()
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
