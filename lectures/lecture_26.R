## @knitr lecture26prep
rm(AICc)
library(AICcmodavg)
library(glmulti)
library(car)
library(plyr)
library(MASS)
library(mnormt)
library(reshape2)
library(ggplot2)

plankton <- read.csv("./data/hampton.5.1-Baikal_74_97_moAvg_plankton.csv", skip=1, na.strings=c("", ".", "NA", " NA"))
plankton$Month <- factor(plankton$Month)

## @knitr copepod_plot
cop_plot <- qplot(PDO.jisao, Copepod.total, data=plankton) + theme_bw()
cop_plot

## @knitr copepod_plot_linear
cop_plot <- cop_plot+ stat_smooth(method="lm", formula=y ~ x, color="red", lwd=2, fill=NA)
cop_plot

## @knitr copepod_plot_square
cop_plot <- cop_plot+ stat_smooth(method="lm", formula=y ~ poly(x,2), color="orange", lwd=2, fill=NA, lty=2)
cop_plot

## @knitr copepod_plot_log
cop_plot <- cop_plot+ stat_smooth(method="glm", formula=y ~ x, color="blue", lwd=2, fill=NA, lty=3, family=Gamma(link="log"))
cop_plot

## @knitr truth

c <- ggplot(mtcars, aes(qsec, wt)) + theme_bw()+ stat_smooth(method="loess", fill=NA, color="black", lwd=2) +
  xlab("X") + ylab("Y")

c 


## @knitr truth_curve
c <- c+ stat_smooth(method="loess", color="orange", fill=NA, lwd=2, lty=3, degree=1)
c

## @knitr truth_line
c <- c+ stat_smooth(method="lm", color="red", fill=NA, lwd=2, lty=2)
c

## @knitr general_specific
set.seed(15)
x<-rnorm(50)
y<-rnorm(50, x - x^2+2)

qplot(x,y, color=I("orange"), size=I(2.3)) + 
  geom_line(color="grey", size=1.5) + 
  theme_bw() +geom_point(size=2) +
  stat_smooth(method="lm", formula=y~poly(x,2), color="red", fill=NA, size=2, lty=4)

## @knitr copepod_models
cop_linear <- glm(Copepod.total ~ PDO.jisao , data=plankton)
#
cop_square <- glm(Copepod.total ~ poly(PDO.jisao,2), data=plankton)
#
cop_glm <- glm(Copepod.total ~ PDO.jisao , data=plankton, 
               family=Gamma(link="log"))


## @knitr cop_linear_compare
AIC(cop_linear)
AIC(cop_square)

## @knitr cop_nonlinear_compare
AIC(cop_square)
AIC(cop_glm)


## @knitr cop_linear_compare_aicc
library(AICcmodavg)
aicc_by_n <- sapply(10:nrow(plankton), function(x) {
  mod <- lm(Copepod.total ~ poly(ENSO.jisao,2), data=plankton[1:x,])
  AICc(mod) - AIC(mod)
  
})
plot(10:nrow(plankton), aicc_by_n, xlab="n", ylab="Difference between AICc and AIC", type="l", lwd=2)

## @knitr cop_modlist
copepodList <- list(cop_linear, cop_square, cop_glm)
names(copepodList) <- c("linear", "square", "Gamma-log")
#
aictab(cand.set = copepodList, 
       modnames = names(copepodList))

## @knitr cop_aictab
aictab(cand.set = copepodList, 
       modnames = names(copepodList),
       second.ord=F)

## @knitr show_vars
copepod.data <- with(plankton, melt(as.data.frame(cbind(Copepod.total, Watertemp0.50, PDO.jisao, ENSO.jisao)), 
                                    id.var="Copepod.total"))

varPlot <- qplot(value, Copepod.total, data=copepod.data, size=I(1.5)) +
  facet_wrap(facets=~variable, scale="free_x") + theme_bw() 
varPlot

## @knitr show_vars_linear
varPlot + stat_smooth(method="lm", formula=y ~ x, color="red", lwd=2)


## @knitr show_vars_square
varPlot + stat_smooth(method="lm", formula=y ~ poly(x,2), color="orange", lwd=2)

## @knitr show_vars_logGamma
varPlot+  stat_smooth(method="glm", family=Gamma(link="log"), color="blue", lwd=2)

## @knitr fullmod0
full_lm0 <- lm(Copepod.total ~ Watertemp0.50 + 
                  I(Watertemp0.50^2) +
                 PDO.jisao + I(PDO.jisao^2) + 
                 ENSO.jisao+ I( ENSO.jisao^2), 
               data=plankton) 

## @knitr vif1
vif(full_lm0)


## @knitr center_func
cent <- function(x) x-mean(x, na.rm=T)

## @knitr full_lm
full_lm <- lm(Copepod.total ~ Watertemp0.50 + 
                I(cent(Watertemp0.50)^2) + 
                PDO.jisao + I(cent(PDO.jisao)^2)+  
                ENSO.jisao+ I( cent(ENSO.jisao)^2), 
              data=plankton)

## @knitr full_lm_info

vif(full_lm)

## @knitr full_lm_summary
summary(full_lm)


## @knitr models1
#Two predictor models
noEnso_lm <- lm(Copepod.total ~ Watertemp0.50 + I(cent(Watertemp0.50)^2) + 
                PDO.jisao + I(cent(PDO.jisao)^2),  
              data=plankton)

noPDO_lm <- lm(Copepod.total ~ Watertemp0.50 + I(cent(Watertemp0.50)^2) + 
                ENSO.jisao+ I( cent(ENSO.jisao)^2), 
              data=plankton)

noTemp_lm <- lm(Copepod.total ~ PDO.jisao + I(cent(PDO.jisao)^2)+  
                ENSO.jisao+ I( cent(ENSO.jisao)^2), 
              data=plankton)

## @knitr models2
#One predictor models
temp_lm <- lm(Copepod.total ~  Watertemp0.50 + I(cent(Watertemp0.50)^2), 
                data=plankton)

pdo_lm <- lm(Copepod.total ~  PDO.jisao + I(cent(PDO.jisao)^2), 
              data=plankton)

enso_lm <- lm(Copepod.total ~  ENSO.jisao+ I( cent(ENSO.jisao)^2), 
              data=plankton)

null_lm <- lm(Copepod.total ~  1, 
              data=plankton)

## @knitr show_AIC
AIC(full_lm)
AIC(noPDO_lm)

## @knitr show_AICc
library(AICcmodavg)
#
AICc(full_lm)
AICc(noPDO_lm)

## @knitr show_BIC
BIC(full_lm)
BIC(noPDO_lm)


## @knitr full_lm_linear
full_lm_linear <- lm(Copepod.total ~ Watertemp0.50  + 
                       PDO.jisao +  
                       ENSO.jisao, 
                     data=plankton)

AIC(full_lm)
AIC(full_lm_linear)


## @knitr full_lm_linear
full_glm <- glm(Copepod.total ~ Watertemp0.50  + 
                       PDO.jisao +  
                       ENSO.jisao, 
                     data=plankton, family=Gamma(link="log"))

AIC(full_lm)
AIC(full_glm)



## @knitr modelList
modList <- list(full_lm, noEnso_lm, 
                noPDO_lm, noTemp_lm, 
                temp_lm, pdo_lm, 
                enso_lm, null_lm)

names(modList) <- c("Full Model", "No ENSO",
                    "No PDO", "No Temperature", 
                    "Temperature Only", "PDO Only", 
                    "ENSO Only", "Null")




## @knitr aicctab
aictab(modList, modnames=names(modList))

## @knitr aictab
aictab(modList, modnames=names(modList), second.ord=F)


## @knitr confSet
confset(modList, modnames=names(modList))


## @knitr importance
importance(modList, parm="Watertemp0.50", modnames=names(modList))

## @knitr importance2
importance(modList, parm="ENSO.jisao", modnames=names(modList))

## @knitr varEst
modavg(modList, parm="ENSO.jisao",  modnames=names(modList), 
       exclude=list("cent(ENSO.jisao)^2"))

## @knitr Predict
newData <- data.frame(Watertemp0.50 = 3, 
                      PDO.jisao=0.2, 
                      ENSO.jisao=25)
#
modavgpred(modList, modnames=names(modList), newdata = newData)

## @knitr comment
#predVar <- with(plankton, cov(cbind(Watertemp0.50, PDO.jisao, ENSO.jisao), use="pairwise.complete.obs"))
#predMean <- with(plankton, colwise(function(x) mean(x, na.rm=T))(as.data.frame(cbind(Watertemp0.50, PDO.jisao, ENSO.jisao))))


## @knitr glmulti
library(glmulti)
full_glmulti <- glmulti(full_lm, level=1, plot=F)

## @knitr glmulti_coef
coef(full_glmulti)


## @knitr compare_pred
as.data.frame(predict(full_lm, newdata=newData, se.fit=T))
modavgpred(modList, modnames=names(modList), newdata = newData)
as.data.frame(predict(full_glmulti, newdata=newData, se.fit=T))


## @knitr diatom_mod
diatom_lm <- lm(diatom ~ Copepod.total * Bosmina...Daphnia * 
                  Watertemp0.50, data=plankton)

## @knitr diatom_mod_summary
summary(diatom_lm)$coef

## @knitr diatom_glmulti
diatom_glmulti <- glmulti(diatom_lm)

## @knitr diatom_glmulti_coef
coef(diatom_glmulti)
