## @knitr test-distributions
#playing with transforming between distributions
plot(-2:2, dnorm(-2:2))


dnorm(log(1))
dlnorm(1)

dnorm(log(2))/2 
dlnorm(2)

#remember - a transformed dist = dist(transform(x))* deriv of transform function
plot(0:4, dnorm(log(0:4))/0:4)
matplot(0:4, dlnorm(0:4), add=T, col="red")


#note, the reason there is no 2 in the denominator is b/c of the doubling
#that happens with the chisq due to negatives becoming positive
plot(0:4, dnorm(sqrt(0:4))/sqrt(0:4))
matplot(0:4, dchisq(0:4,1), add=T, col="red")

