data("PlantGrowth")
dat <- PlantGrowth

ggplot(dat) +
  geom_boxplot(aes(x = group, y = weight))

fit <- lm(weight ~ group, data = dat)
summary(fit)
plot(fit)

confint(fit)
5.0320 + c(-1, 1) * qt(0.025, 27) * 0.1971

fit2 <- lm(weight ~ group - 1, data = dat %>% filter(group != "ctrl"))
summary(fit2)

names(summary(fit2))
summary(fit2)$cov.unscaled
summary(fit2)$sigma

contrast <- c(1, -1)
test_contrast <- function(contrast, fit) {
  T_stat <- c(contrast %*% fit$coefficients / (summary(fit)$sigma * sqrt(t(contrast) %*% summary(fit)$cov.unscaled %*% contrast)))
  P <- 2 * pt(abs(T_stat), fit$df.residual, lower.tail = FALSE)
  return(tibble(T_stat = T_stat, P = P, df = fit$df.residual))
}

test_contrast(contrast, fit)
