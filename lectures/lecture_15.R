## @knitr lecture15prep
primates <- read.csv("./data/17q16PrimateMassMetabolicRate.csv")
fungus <- read.csv("./data/17q22DaphniaParasiteLongevity.csv")



## @knitr fungus_fit_linear
fungus.lm <- lm (Spores ~ longevity, data=fungus)


## @knitr fungus_lienar_diagnostic
par(mfrow=c(1,2))
plot(fungus.lm, which=1:2)
par(mfrow=c(1,1))


## @knitr fungus_plot
plot(Spores ~ longevity, data=fungus, pch=19)

## @knitr  fungus_nonlinear_lm
fungus.lmsq <- lm(Spores ~  longevity +  I(longevity^2), data=fungus)

## @knitr  fungus_nonlinear_summary
summary(fungus.lmsq)

## @knitr fungus_nonlinear_plot
plot(Spores ~ longevity, data=fungus, pch=19)
#
fungusFun <- function(x) coef(fungus.lmsq)[1] + 
  coef(fungus.lmsq)[2]*x + 
  coef(fungus.lmsq)[3]*x^2
#
curve(fungusFun, add=T, col="red", lwd=2)




## @knitr primate_bad_fit
primate.lm <- lm(bmr.watts ~ mass.g, data=primates) 
par(mfrow=c(1,2))
plot(primate.lm, which=1:2)
par(mfrow=c(1,1))


## @knitr primate_plot
plot(bmr.watts ~ mass.g, data=primates, pch=19)


## @knitr primate_plot_trans_log
plot(log(bmr.watts) ~ mass.g, data=primates, pch=19)

## @knitr primate_plot_trans_log_log
plot(log(bmr.watts) ~ log(mass.g), data=primates, pch=19)

## @knitr jensen0
x<-seq(0,10,.01)
plot(x, 2^x, type="l")

## @knitr jensen1
x<-seq(0,10,.01)
plot(x, 2^x, type="l")
points(2, 4, col="red", pch=19, cex=4)
points(8, 2^8, col="red", pch=19, cex=4)


## @knitr jensen2
x<-seq(0,10,.01)
plot(x, 2^x, type="l")
points(2, 4, col="red", pch=19, cex=4)
points(8, 2^8, col="red", pch=19, cex=4)
points(6, 2^6, col="blue", pch=15, cex=4)
text(7, 2^6, label=expression(f(bar(x))), cex=2)



## @knitr jensen3
x<-seq(0,10,.01)
plot(x, 2^x, type="l")
points(2, 4, col="red", pch=19, cex=4)
points(8, 2^8, col="red", pch=19, cex=4)
points(6, 2^6, col="blue", pch=15, cex=4)
text(7, 2^6, label=expression(f(bar(x))), cex=2)
points(6, mean(c(4, 2^8)), col="purple", cex=3, pch=13)
text(5.5, mean(c(5, 2^8))+80, label=expression(bar(f(x))), cex=2)


## @knitr primate_loglog_lm
primateLogLog.lm <- lm(log(bmr.watts) ~ I(log(mass.g)), data=primates)

## @knitr primate_loglog_lm_diagnostic
par(mfrow=c(2,2))
plot(primateLogLog.lm)
par(mfrow=c(1,1))


## @knitr primate_nls
primate.nls <- nls(bmr.watts ~ a*mass.g^b, data=primates, 
                   start=list(a = 0.0172858, b = 0.74160))

## @knitr primate_nls_summaru
summary(primate.nls)

## @knitr primate_nls_diagnostic
par(mfrow=c(1,2))
plot(fitted(primateLogLog.lm), residuals(primateLogLog.lm))
plot(fitted(primate.nls), residuals(primate.nls))
par(mfrow=c(1,1))

## @knitr primate_plot_nls
plot(bmr.watts ~ mass.g, data=primates)

primateFun <- function(x) coef(primate.nls)[1]*x^coef(primate.nls)[2]
primateFun2 <- function(x) exp(coef(primateLogLog.lm)[1])*x^coef(primateLogLog.lm)[2]

curve(primateFun, lwd=2, add=T, col="red")
curve(primateFun2, lwd=2, add=T, col="blue", lty=2)
legend(0,80, legend=c("NLS", "log-log"), col=c("red", "blue"), lty=1:2)


## @knitr kelpLoad
kelp<-read.csv("./data/kelp_holdfast.csv")

## @knitr kelp_lm
kelp.lm <- lm(log(FRONDS) ~ HLD_DIAM, data=kelp)

## @knitr kelp_nls
kelp.nls <- nls(FRONDS ~ a*exp(b*HLD_DIAM), data=kelp, start=list(a=exp(1.27), b=0.028))

## @knitr kelp_lm_diagnostic
par(mfrow=c(1,2))
plot(kelp.lm, which=1:2)
par(mfrow=c(1,1))

## @knitr kelp_nls_diagnostic
plot(residuals(kelp.nls) ~ fitted(kelp.nls))


## @knitr kelp_plot
plot(FRONDS ~ HLD_DIAM, data=kelp, pch=19)

## @knitr kelp_glm
kelp.glm <- glm(FRONDS ~ HLD_DIAM, data=kelp, 
                family=poisson(link="log"))


## @knitr kelp_glm_summary
summary(kelp.glm)

## @knitr kelp_glm_diagnostics
par(mfrow=c(2,3))
plot(kelp.glm, which=c(1:5))
par(mfrow=c(1,1))

## @knitr kelp_glm_residuals
residuals(kelp.glm, type="deviance")
residuals(kelp.glm, type="pearson")
residuals(kelp.glm, type="response")

## @knitr kelp_glm_plot_residuals
par(mfrow=c(2,2))

plot(residuals(kelp.glm) ~ predict(kelp.glm, type="link"), ylab="Deviance Residuals", xlab="Fitted Link")
plot(residuals(kelp.glm) ~ predict(kelp.glm, type="response"), ylab="Deviance Residuals", xlab="Fitted Response")

plot(residuals(kelp.glm, type="response") ~ predict(kelp.glm, type="link"), ylab="Response Residuals", xlab="Fitted Link")
plot(residuals(kelp.glm, type="response") ~ predict(kelp.glm, type="response"), ylab="Response Resdiuals", xlab="Fitted Response")
par(mfrow=c(1,1))


## @knitr kelp_glm_predict
predict(kelp.glm, type="response")
predict(kelp.glm, type="link")

## @knitr kelp_ggplot_fitted
library(ggplot2)
kelp.ggplot <- ggplot() + 
  geom_point(data=kelp, mapping=aes(x=HLD_DIAM, y=FRONDS), size=4) +
  stat_smooth(method="glm", family=poisson(link="log"), colour="red", lwd=2, data=kelp, mapping=aes(x=HLD_DIAM, y=FRONDS)) +
  theme_bw()
kelp.ggplot

## @knitr kelp_ggplot_prediction_intervals
upperCI <- qpois(0.975, lambda = round(fitted(kelp.glm)))
lowerCI <-  qpois(0.025, lambda = round(fitted(kelp.glm)))
HLD <- na.omit(kelp)$HLD_DIAM
#
kelp.ggplot + 
  geom_line(mapping=aes(x=HLD,  y=upperCI), lty=2, col="blue") +
  geom_line(mapping=aes(x=HLD,  y=lowerCI), lty=2, col="blue") 

## @knitr kelp_glm_rsq
cor(fitted(kelp.glm), 
    fitted(kelp.glm) + residuals(kelp.glm, type="response"))^2
summary(kelp.lm)$r.squared


## @knitr kelp_glm2
kelp.glm2 <- glm(FRONDS ~ HLD_DIAM, data=kelp, 
                 family=quasipoisson(link="log"))

## @knitr kelp_glm2_summary
summary(kelp.glm2)

## @knitr kelp_gglm2_fitted
kelp.ggplot2 <- ggplot() + 
  geom_point(data=kelp, mapping=aes(x=HLD_DIAM, y=FRONDS), size=4) +
  stat_smooth(method="glm",  colour="red", lwd=2, data=kelp, mapping=aes(x=HLD_DIAM, y=FRONDS), family=quasipoisson, link="log") +
  theme_bw()
kelp.ggplot2

## @knitr kelp_gglm2_fitted_error
disp <- summary(kelp.glm2)$dispersion
upperCI.qp <- qnorm(0.975, mean = fitted(kelp.glm2), sd=sqrt(fitted(kelp.glm2) * disp))
lowerCI.qp <-  qnorm(0.025, mean = fitted(kelp.glm2), sd=sqrt(fitted(kelp.glm2) * disp))
HLD <- na.omit(kelp)$HLD_DIAM
#
kelp.ggplot2 + geom_line(mapping=aes(x=HLD,  y=upperCI.qp), lty=2, col="blue") +
  geom_line(mapping=aes(x=HLD,  y=lowerCI.qp), lty=2, col="blue")



## @knitr kelp_glm_nb
library(MASS)
#
kelp.glm.nb <- glm.nb(FRONDS ~ HLD_DIAM, data=kelp)

## @knitr kelp_glm_anodev
anova(kelp.glm, kelp.glm.nb)

## @knitr kelp_ggplot_fitted2
kelp.ggplot.nb <- ggplot() + 
  geom_point(data=kelp, mapping=aes(x=HLD_DIAM, y=FRONDS), size=4) +
  stat_smooth(method="glm.nb",  colour="red", lwd=2, data=kelp, mapping=aes(x=HLD_DIAM, y=FRONDS)) +
  theme_bw()
kelp.ggplot.nb

## @knitr kelp_glm2_rsq
cor(fitted(kelp.glm), fitted(kelp.glm) + residuals(kelp.glm, type="response"))^2
cor(fitted(kelp.glm2), fitted(kelp.glm2) + residuals(kelp.glm2, type="response"))^2

## @knitr kelp_ggplot_fitted2_error

upperCI.nb <- qnbinom(0.975, mu = round(fitted(kelp.glm.nb)), size=summary(kelp.glm.nb)$theta)
lowerCI.nb <-  qnbinom(0.025, mu = round(fitted(kelp.glm.nb)), size=summary(kelp.glm.nb)$theta)
HLD <- na.omit(kelp)$HLD_DIAM
#
kelp.ggplot.nb + geom_line(mapping=aes(x=HLD,  y=upperCI.nb), lty=2, col="blue") +
  geom_line(mapping=aes(x=HLD,  y=lowerCI.nb), lty=2, col="blue")



## @knitr load_wolves
wolves<-read.csv("./data/16e2InbreedingWolves.csv")

## @knitr wolf_models
a<-glm(pups ~ inbreeding.coefficient, data=wolves, family=poisson(link="log"))
b<-glm(pups ~ inbreeding.coefficient, data=wolves, family=gaussian(link="identity"))
d<-glm(pups ~ inbreeding.coefficient, data=wolves, family=poisson(link="identity"))

## @knitr wolf_anodev
anova(a,b,d)

## @knitr wolf_diagnostics
par(mfrow=c(3,4))
plot(a)
plot(b)
plot(d)
par(mfrow=c(1,1))

## @knitr_wolf_ggplot
qplot(inbreeding.coefficient, pups, data=wolves) +
  stat_smooth(method="glm", family=poisson(link="identity"), lwd=2) +
  theme_bw()

