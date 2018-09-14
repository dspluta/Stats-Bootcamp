B <- 10000
Z <- purrr::rdunif(B, b = 2, a = 0)

X <- Z
X[Z == 0] <- rnorm(sum(Z == 0), 0, 1)
X[Z == 1] <- rnorm(sum(Z == 1), -5, sqrt(2))
X[Z == 2] <- rnorm(sum(Z == 2), 10, 2)

hist(X, breaks = 30)

X <- purrr::map_dbl(Z, function(z) {
  if(z == 0) {return(rnorm(1, 0, 1))}
  else if (z == 1) {return(rnorm(1, 1, sqrt(2)))}
  else {return(rnorm(1, 2, 2))}
})
hist(X, breaks = 30)

