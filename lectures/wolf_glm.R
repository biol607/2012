clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
clot<-(glm(lot1 ~ log(u), data=clotting, family=Gamma))

summary(clot)
plot(clot)

residuals(clot)
residuals(clot, type="pearson")
residuals(clot, type="response")

predict(clot, type="response")
predict(clot, type="link")


?plot.lm


par(mfrow=c(2,2))

plot(residuals(wolf.glm) ~ predict(wolf.glm, type="link"))
plot(residuals(wolf.glm) ~ predict(wolf.glm, type="response"))

plot(residuals(wolf.glm, type="response") ~ predict(wolf.glm, type="link"))
plot(residuals(wolf.glm, type="response") ~ predict(wolf.glm, type="response"))
par(mfrow=c(1,1))

plot(residuals(clot, type="deviance") ~ predict(clot, type="response"))

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
