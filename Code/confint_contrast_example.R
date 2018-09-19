n <- 100
p <- 2

set.seed(1234)
Sigma <- matrix(c(1, -0.6, -0.6, 1), nrow = 2)
X <- MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = Sigma)

plot(X[, 1], X[, 2])

beta <- c(1, -1)

sigma_e <- 1
eps <- rnorm(n, 0, sigma_e)
Y <- X %*% beta + eps

plot(X[, 2], Y)

plot(X[, 1], X[, 2])

dat <- tibble(X1 = X[, 1], X2 = X[, 2], Y = Y[, 1])

ggplot(dat) +
  geom_point(aes(x = X1, y = X2, color = Y))


fit <- lm(Y ~ X1 + X2, data = dat)

alpha <- 0.05

summary(fit)$coefficients[2, 1] + c(-1, 1) * summary(fit)$coefficients[2, 2] * qt(alpha / 2, 97)
confint(fit)
summary(fit)$sigma
names(summary(fit))

X_new <- as.matrix(tibble(X0 = 1, X1 = X[, 1], X2 = X[, 2]))
solve(t(X_new) %*% X_new)
summary(fit)$cov.unscaled

confint_contrast(c(0, 1, 0),  fit)
summary(fit)$coefficients[2, 1] + c(-1, 1) * summary(fit)$coefficients[2, 2] * qt(alpha / 2, 97)

confint_contrast <- function(contrast, fit, alpha = 0.05) {
  beta <- summary(fit)$coefficients[, 1]
  s2 <- summary(fit)$sigma^2
  var_beta <- summary(fit)$cov.unscaled
  se <- sqrt(s2 * t(contrast) %*% var_beta %*% contrast)
  return(t(contrast) %*% beta + c(-1, 1) * se * qnorm(alpha / 2))
}


confint_contrast(c(0, 1, 0),  fit)
summary(fit)$coefficients[2, 1] + c(-1, 1) * summary(fit)$coefficients[2, 2] * qt(alpha / 2, 97)

confint_contrast(c(0, 1, 1), fit)

plot(fit)
summary(fit)

fit <- lm(Y ~ X1 + X2 - 1, data = dat)
summary(fit)



