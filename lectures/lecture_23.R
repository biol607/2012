## @knitr lecture23prep
library(ggplot2)
library(car)
library(plyr)
library(reshape2)


## @knitr simp_plot
set.seed(697)
adf <- data.frame(expand.grid(int = 1:5, x = rnorm(10)))

adf$y <- with(adf, rnorm(nrow(adf), mean=int - x))

adf$x <- adf$x+adf$int*2
adf$int <- factor(letters[adf$int])

simp1 <- qplot(x, y, data=adf) + theme_bw()
simp1

## @knitr simp_line
simp1+ stat_smooth(method="lm", colour="red") + 
  xlab("# Lethal Heat Days") +
  ylab("Plant Growth")

## @knitr simp_plots
simp2 <- qplot(x, y, data=adf, colour=int, group=int) + theme_bw() + 
  xlab("# Lethal Heat Days") +
  ylab("Plant Growth") +
  scale_color_discrete(name="Group", labels=c("Low", "Med-Low", "Med", "Med-High", "High"))
simp2

## @knitr simp_lines
simp2 +
  stat_smooth(method="lm")


## @knitr relationship

getY <- function(x) rnorm(length(x), x , 10)

#two approaches
x<-1:24
xAnova<-rep(seq(1,24,length.out=6),4)

## @knitr power_func
powFunc <- function(predictor, n.sims=500, a=F, fun=getY){
  pvec <- sapply(1:n.sims, function(i) {
    y <-fun(predictor)
    
    #run either a regression or categorical model
    if(a){
      alm <- lm(y~I(factor(predictor)))
    }else{
      alm <- lm(y~predictor)
    }
    
    #get p from an f test
    anova(alm)[1,5]
  } )

  #power
  1 - sum(pvec > 0.05)/n.sims
}


## @knitr power_compare
set.seed(100)
powFunc(x)
powFunc(xAnova, a=T)

## @knitr power_compareNonlinear
getYSat <- function(x) rnorm(length(x), -2/x, 0.7)
#
powFunc(x, fun=getYSat)
powFunc(xAnova, a=T, fun=getYSat)


## @knitr power_func_nls
powFuncNonlinear <- function(predictor, n.sims=500, a=F, fun=getYSat){
  pvec <- sapply(1:n.sims, function(i) {
    y <-fun(predictor)
    
    #run either a regression or categorical model
    if(a){
      alm <- lm(y~I(factor(predictor)))
      return(anova(alm)[1,5])
    }else{
      alns <- nls(y ~ -a/predictor + b, start=list(a=2, b=0))
      alns1 <- lm(y~1)
      return(anova(alns, alns1)[2,6])
    }
  } )
  
  #power
  1 - sum(pvec > 0.05)/n.sims
}

## @knitr power_compareNonlinear2
set.seed(100)
powFuncNonlinear(x, fun=getYSat)
powFuncNonlinear(xAnova, a=T, fun=getYSat)


## @knitr load_exam
exam <- read.csv("../tests/e1.csv")
exam<-na.omit(exam)

exam <- ddply(exam, .(Question), function(x) colwise(sum)(x))

exam <- melt(exam, ("Question"))
exam$value <- exam$value*2

totals <- ddply(exam, .(variable), summarise, grade = sum(value))

## @knitr exam_totals
qplot(grade, geom="density", data=totals, fill=I("red"), alpha=I(0.2)) + theme_bw()

## @knitr exam_q
qplot(value, data=exam, facets=~Question, fill=Question, geom="density")

