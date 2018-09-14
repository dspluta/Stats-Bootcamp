#-------------------------------------------------------------------------------
#
## Central Limit Theorem Example
### Brian Vegetabile - 2016 UCI Statsboot Camp
#
#-------------------------------------------------------------------------------

# Central Limit Theorem For Exponential Random Variables


# sample.sizes <- c(20, 50, 100, 1000)
n.sims <- 10000
ss <- 50
lamb <- 0.5
draws <- matrix(NA, ncol=ss, nrow=n.sims)
for(i in 1:n.sims){
  draws[i, ] <- rexp(ss, rate=lamb)
}
sample.means <- rowMeans(draws)
sample.density <- density(sample.means)
x.min <- min(sample.density$x)
x.max <- max(sample.density$x)
x.seq <- seq(x.min, x.max, 0.001)

approx.norm <- dnorm(x.seq, mean=(1/lamb), sd=sqrt(1/(ss*lamb^2)))

x2.seq <- seq(0, x.max, 0.001)
underlying.dist <- dexp(x2.seq, rate=lamb)

true.dist <- dgamma(x.seq, shape = ss, rate = ss*lamb)

y.limits <- c(0,max(max(sample.density$y), 
                    max(approx.norm), 
                    max(underlying.dist)))

plot(x2.seq, underlying.dist, 
     ylim=y.limits, type='l',
     xlab='X - Values',
     ylab='Density')
lines(sample.density, col='blue')
abline(v=mean(sample.means), col='blue', lty=3)
lines(x.seq, approx.norm, col='red')
lines(x.seq, true.dist, col='green')
abline(v=(1/lamb), col='red', lty=3)


#-------------------------------------------------------------------------------
#
# Normal Approximation to the Binomial

n <- 20
p <- 0.5

true.mean <- n*p
true.var <- n*p*(1-p)
x.upper <- true.mean + 3*true.var
x.seq <- 0:x.upper
binom.dist <- dbinom(x.seq, size = n, prob = p)
approx.norm <- dnorm(x.seq, mean=true.mean, sd=sqrt(true.var))

plot(x.seq, binom.dist, pch=19)
abline(v=floor((n+1)*p))
lines(x.seq, approx.norm)
