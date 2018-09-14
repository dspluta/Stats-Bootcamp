#-------------------------------------------------------------------------------
#
## Weak Law of Large Numbers Example
### Brian Vegetabile - 2016 UCI Statsboot Camp
#
#-------------------------------------------------------------------------------

sample.sizes <- 10:5000
n.sims <- length(sample.sizes)
results <- rep(NA, n.sims)
for(i in 1:n.sims){
  ss <- sample.sizes[i]
  random.sample <- rbinom(ss, 1, 0.25)
  # random.sample <- rexp(ss, rate=0.2)
  sample.mean <- mean(random.sample)
  results[i] <- sample.mean
}

plot(sample.sizes, results, type = 'l',
     xlab='Sample Size',
     ylab='Sample Mean')
abline(h = 0.25, col='red', lty=3, lwd=3)
