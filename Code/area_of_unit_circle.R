B <- 1e4
x <- runif(B, 0, 1)
y <- runif(B, 0, 1)

4 * mean((x^2 + y^2) < 1)

plot(1:B, 4 * cumsum((x^2 + y^2 < 1)) / 1:B, ty =  "l")
plot(1:B, abs(4 * cumsum((x^2 + y^2 < 1)) / 1:B - pi), ty =  "l")
