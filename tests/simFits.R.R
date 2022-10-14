n.sims<-1000


a <- rnorm(n.sims)
b <- rnorm(n.sims, a*5, 15)
plot(a,b)

alm <- lm(b ~ a)

abline(alm, lwd=3, col="red")

int <- rnorm(n.sims, coef(alm)[1], sqrt(vcov(alm)[1,1]))
slope <- rnorm(n.sims, coef(alm)[2], sqrt(vcov(alm)[2,2]))

#for(i in 1:n.sims) abline(a=int[i], b=slope[i], col="grey")



n<-n.sims
df <- n-1
est.se <- summary(alm)$sigma
	X <- rchisq(n.sims, df=df)
	ses <- est.se * sqrt(df/X)
	e<-rnorm(n.sims, 0, ses)
for(i in 1:n.sims) abline(a=int[i]+e[i], b=slope[i], col="black")
for(i in 1:n.sims) abline(a=int[i], b=slope[i], col="blue")

abline(alm, lwd=3, col="red")
