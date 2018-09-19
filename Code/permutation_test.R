library(tidyverse)
library(magrittr)

data("PlantGrowth")
head(PlantGrowth)

ggplot(PlantGrowth) +
  geom_histogram(aes(x = weight, fill = group), color = "black", alpha = 0.5)

ggplot(PlantGrowth) +
  geom_density(aes(x = weight, fill = group), color = "black", alpha = 0.5)

dat <- PlantGrowth %>% filter(group != "trt2")
colnames(dat) <- c("x", "group")
B <- 10
perm_test <- function(dat, B = 1000) {
  results <- tibble(b = 1:B, delta = NA)
  delta <- dat %>% group_by(group) %>% summarize(grp_mean = mean(x)) %>%
    ungroup() %>% summarize(delta = diff(grp_mean)) %$% delta

  results$delta <- purrr::map_dbl(1:B, function(b) {
    set.seed(b)
    perm <- sample(n)
    mean(dat[perm[1:(n / 2)], 1]) - mean(dat[perm[(n / 2 + 1):n], 1])
  })
  print(mean(results$delta > delta))
  return(results)
}

dat <- PlantGrowth %>% filter(group != "trt2")
colnames(dat) <- c("x", "group")
out <- perm_test(dat)

dat <- PlantGrowth %>% filter(group != "trt1")
colnames(dat) <- c("x", "group")
out <- perm_test(dat)

dat <- PlantGrowth %>% filter(group != "trt2")
colnames(dat) <- c("x", "group")
out <- perm_test(dat)



