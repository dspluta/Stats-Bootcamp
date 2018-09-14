
curve(dnorm, from = -3, to = 3)

n <- 20
p <- 0.5
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

curve(dbinom(x, n, p), from = 0, to = n, type = "h", lwd = 2)
curve(dnorm(x, mu, sigma), from = 0, to = n, ty = "l", add = TRUE, col = "blue")
curve(dpois(x, mu), from = 0, to = n, ty = "h", add = TRUE, col = "red")

plot_approximations <- function(n, p, xmin = 0, xmax = n) {
  mu <- n * p
  sigma <- sqrt(n * p * (1 - p))
  
  curve(dbinom(x, n, p), from = 0, to = n, type = "h", lwd = 2, xlim = c(xmin, xmax))
  curve(dnorm(x, mu, sigma), from = 0, to = n, ty = "l", add = TRUE, col = "blue")
  curve(dpois(x, mu), from = 0, to = n, ty = "h", add = TRUE, col = "red")
}

plot_approximations(100, 0.1, 0, 20)
plot_approximations(100, 0.01, 0, 20)
