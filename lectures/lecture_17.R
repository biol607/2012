## @knitr lecture17prep
library(plyr)
library(ggplot2)
kelp <- read.csv("./data/giant_kelp_all_years_20110921.csv", na.strings="-99999")
kelp <-with(kelp,  kelp[which(!is.na(FRONDS) & !is.na(HLD_DIAM)),])

s<-paste(kelp$YEAR, kelp$SITE)
len <- function(x, string) length(x[which(x %in% string)])
lenV<-Vectorize(len, "string")
kelp <- kelp[ which(lenV(s,s)>2),]
ddply(kelp, .(YEAR, SITE), summarize, length(FRONDS))
#write.csv(kelp, "./data/giant_kelp_all_years_20110921_clean.csv")

## @knitr kelpHead
head(kelp)[,-c(10:11)]

## @knitr kelpAggregateFor
# number of groups
k <- length(levels(kelp$SITE))

#blank means vector
means <- rep(NA, k)

#the loop
for(i in 1:k) {
  #split the data first
  subdata <- subset(kelp, kelp$SITE == levels(kelp$SITE)[i])
  
  #apply the means function, 
  #combine with previous means
  means[i] <- mean(subdata$FRONDS, na.rm=T)
}


## @knitr kelpMean
library(plyr)
#
kelpMeans <- ddply(kelp, .(SITE), summarize, 
                   mean.FRONDS = mean(FRONDS, na.rm=T))

## @knitr kelpMean2
kelpMeans

## @knitr kelpMean3
kelpMeans2 <- ddply(kelp, .(YEAR, SITE), summarize, 
                    mean.FRONDS = mean(FRONDS, na.rm=T))


## @knitr kelpMean4
qplot(YEAR, mean.FRONDS, color=SITE, data=kelpMeans2)


## @knitr kelpMeanComplex
kelpMeans3 <- ddply(kelp, .(YEAR, SITE), function(aFrame){
  #calculate metrics for a 1-sample T test comparison against
  #grand mean of 10 fronds/m^2
  m <- mean(aFrame$FRONDS, na.rm=T)
  n<-length(na.omit(aFrame$FRONDS))
  se <- sd(aFrame$FRONDS, na.rm=T)/sqrt(n)
  t <- (m-10)/se
  p <- 2*pt(abs(t), df=n-1, lower.tail=F)

  # return everything  
  return(c(mean.FRONDS=m, n.FRONDS=n,
           se.FRONDS=se, t.FRONDS=t,
           p.FRONDS = p))
})

## @knitr kelpMeanComplexPlot
qplot(YEAR, mean.FRONDS, color=SITE, data=kelpMeans3, size=p.FRONDS) +
  geom_linerange(mapping=aes(ymin=mean.FRONDS-2*se.FRONDS, ymax=mean.FRONDS+2*se.FRONDS), size=1)

## @knitr kelpMeanComplexPlot2
qplot(YEAR, mean.FRONDS, color=SITE, data=kelpMeans3, size=p.FRONDS) +
  geom_linerange(mapping=aes(ymin=mean.FRONDS-2*se.FRONDS, ymax=mean.FRONDS+2*se.FRONDS), size=1) +
  facet_wrap(~SITE, scale="free_y") 

## @knitr kelpCor
kelpCor <- ddply(kelp, .(YEAR, SITE), function(adf){
  #first get the correlation
  cors <- cor(adf$FROND, adf$HLD_DIAM)

  #use this to calculate it's SE
  seCor <- sqrt((1-cors^2) / (nrow(adf)-2))

  #return both
  return(c(rho = cors, seRho = seCor)) 
  
})

## @knitr kelpCor_plot
qplot(YEAR, rho, color=SITE,  data=kelpCor, size=seRho) +
  theme_bw()


## @knitr load_anova_data
brainGene <- read.csv("./data/15q06DisordersAndGeneExpression.csv")

## @knitr boxplotBrain
boxplot(PLP1.expression ~ group, data=brainGene)

## @knitr brainReorder
brainGene$group <- factor(brainGene$group, 
                          levels=c("control", "bipolar", "schizo"))


## @knitr brainGene_plot
bg <- ddply(brainGene, .(group), summarize, mean.Expression = mean(PLP1.expression), 
            se = sd(PLP1.expression) / length(PLP1.expression), 
            n=length(PLP1.expression))

bg$group <- factor(bg$group, levels=c("control", "bipolar", "schizo"))


ggplot(bg, aes(x=group, y=mean.Expression, 
               ymin=mean.Expression-se,
               ymax=mean.Expression+se)) +
              geom_pointrange(size=1.5) +
                 theme_bw() 
              

## @knitr brainGene_points
bgsub1 <- subset(brainGene, brainGene$group != "schizo")
bgPoints <- ggplot(bgsub1, aes(x=group, y=PLP1.expression)) +
                 geom_point(size=1.5) +
                 theme_bw() 
                   
bgPoints

## @knitr brainGene_points_fit
bgPoints + stat_smooth(method="lm", aes(group=1), color="red", lwd=2, group=1)

## @knitr brainGene_points_fit_01
bgPoints + stat_smooth(method="lm", aes(group=1), color="red", lwd=2, group=1) +
scale_x_discrete(labels=c("0", "1"))

## @knitr brainGene_points_fit_2
bgsub2 <- subset(brainGene, brainGene$group != "bipolar")
bgPoints2 <- ggplot(bgsub2, aes(x=group, y=PLP1.expression)) +
  geom_point(size=1.5) +
  theme_bw() + 
  stat_smooth(method="lm", aes(group=1), color="red", lwd=2, group=1)

bgPoints2




## @knitr brainGene_lm
bg.sub.lm <- lm(PLP1.expression ~ group, data=brainGene)

## @knitr brainGene_lm_summary
summary(bg.sub.lm)

## @knitr brainGene_contrasts
contrasts(brainGene$group)

## @knitr brainGene_anova
anova(bg.sub.lm)
      
## @knitr brainGene_assumptions
par(mfrow=c(2,3))
plot(bg.sub.lm, which=1:5 )
par(mfrow=c(1,1))

## @knitr brainGene_levene
library(car)
leveneTest(PLP1.expression ~ group, data=brainGene)

## @knitr brainGene_kruskal
kruskal.test(PLP1.expression ~ group, data=brainGene)


## @knitr daphnia_load
daphnia <- read.csv("./data/15q13DaphniaResistance.csv")
daphnia$cyandensity <- factor(daphnia$cyandensity, levels=c("low", "med", "high"))

## @knitr daphnia_plot_1
qplot(cyandensity, resistance, data=daphnia) +theme_bw()

## @knitr daphnia_plot_2
#first use plyr to get means and SE
dsummary <- ddply(daphnia, .(cyandensity), summarize, 
                  mean_resistance = mean(resistance), 
                  se = sd(resistance) / sqrt(length(resistance)))
#
ggplot(dsummary, aes(x=cyandensity, y=mean_resistance, 
                     ymin=mean_resistance-se, 
                     ymax=mean_resistance+se)) +
  geom_pointrange() + theme_bw()

## @knitr daphnia_levene
leveneTest(resistance ~ cyandensity, data=daphnia)

## @knitr daphnia_anova
daphniaLM <- lm(resistance ~ cyandensity, data=daphnia)
anova(daphniaLM)

## @knitr daphnia_kruskal
kruskal.test(resistance ~ cyandensity, data=daphnia)


## @knitr daphnia_glm
daphniaGLM <- glm(resistance ~ cyandensity, data=daphnia, family=Gamma(link="identity"))
anova(daphniaGLM)

## @knitr daphnia_diagnostics
par(mfrow=c(2,3))
plot(daphniaLM, which=1:5 )
par(mfrow=c(1,1))