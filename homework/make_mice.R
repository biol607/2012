library(energy)

set.seed(697)
activityDF <- data.frame(julian_day =1:365)
activityDF <- within(activityDF, {
           temperature<-rnorm(365, 10*sin(julian_day/45)+5, sd=5)
           activity <- (rnorm(365, exp(temperature/7), 3))+10
           food <- rnorm(365, 3+activity*5, 40)+50
           
           })


write.table(activityDF, "./seasonal_mouse_activity.csv", sep=",", row.names=F)


pairs(activityDF)



alm<-lm(food ~ activity, data=activityDF)

par(mfrow=c(2,3))
plot(alm, which=1:5)
par(mfrow=c(1,1))