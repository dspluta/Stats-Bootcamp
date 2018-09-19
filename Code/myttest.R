my.ttest <- function(grp1, grp2) {
  delta <- mean(grp1) - mean(grp2)
  t.diff <- delta / sqrt(var(grp1) / length(grp1) + var(grp2) / length(grp2))
  df <- length(grp1) + length(grp2) - 2
  P <- 2 * pt(abs(t.diff), df = df, lower.tail = FALSE)
  return(data.frame(t.diff, P, df))
}

grp1 <- PlantGrowth %>% filter(group == "trt1") %$% weight
grp2 <- PlantGrowth %>% filter(group == "trt2") %$% weight

my.ttest(grp1, grp2)
t.test(grp1, grp2, var.equal = TRUE)
