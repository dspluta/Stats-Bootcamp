# Set parameters and random seed.
B <- 1e6
p <- 0.5
lambda <- 4
set.seed(1234)

# Generate Zero indicator variables
Z <- rbinom(B, 1, p)

# Generate X ~ ZIP(p, lambda)
## Method 1: purrr::map_dbl 
X <- map_dbl(Z, ~ifelse(.x == 0, 0, rpois(1, lambda)))
hist(X, breaks = 30)

## Method 2: unlist and purrr::map
X <- unlist(map(Z, function(z) ifelse(z == 0, 0, rpois(1, lambda))))

## Method 3: Set X to poisson draw for elements of Z == 1
X <- Z
X[Z == 1] <- rpois(sum(X), lambda)

## Method 4:
X <- Z * rpois(B, lambda)
hist(X, breaks = 30)
