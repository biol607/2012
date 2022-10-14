
## @knitr start
##First I need to simulate a number of samples
set.seed(09262012)


#my sample size vector
n<-rep(1:10, 50) 

#the Standard Deviation for the whole simulation
simSD<-6 
meanEffect<-15

## @knitr createSims
#a For loop to simulate the experiments where we get means
vec<-rep(NA, length(n))

for(i in 1:length(n)){
  
  #ok, for each simulation, calculate the mean of a bunch of random 
  # variables drawn from a population
  vec[i]<-mean(rnorm(n[i], mean=meanEffect, sd=simSD))

}

## @knitr plotSims
#plot the result
plot(vec ~ n, ylab="Estimated Effect", 
     main="50 Simulated Values per Sample Size")

## @knitr calculateP
##
# Now, calculate the p value for each mean.
# We're using abs and lower.tail=F to deal with both 
# positive and negative values. Note, the mean of 
# the null distribution is 0, and this is a 2-tailed test
pvec<-pnorm(abs(vec), sd=simSD, lower.tail=FALSE)*2

## @knitr plotP
#plot the p values
plot(pvec ~ n, ylab="p")

## @knitr powerLoop
##
# Now let's look at how sample size influences power

#first, we need a blank vector of power values
power<-rep(NA, max(n))

#now loop over all possible sample sizes
for( i in 1:( max(n) ) ){
  
  # pull out the relevant p values based on their 
  # matching up with the sample size we're interested 
  # in using indices derived from the vector of sample 
  # sizes
  subPvec <- pvec[which(n==i)]
  
  #ok, now that we have all of the p values, see what fraction of
  #simulations contained a type II error.  1-p(error) = power
  power[i] <- 1 - sum(subPvec > 0.05) / length(subPvec)
}

## @knitr plotPower
#plot the relationship between sample size and power
plot(power ~ I(1:10), xlab="n", ylab="power", type="b")