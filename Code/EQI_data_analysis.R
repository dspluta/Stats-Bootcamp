dat <- read_csv("~/Downloads/EQIDATA_ALL_DOMAINS_2014MARCH11.CSV")
dat <- read_csv("https://edg.epa.gov/data/Public/ORD/NHEERL/EQI/Eqidata_all_domains_2014March11.csv")
dat <- dat %>% select(-c("countyname", "state_name"))
cor_mat <- cor(dat[, 10:ncol(dat)])
which(colnames(dat[, 10:ncol(dat)]) == "violent_log_rate")
hist(cor_mat[, 216])

colnames(dat)
head(dat)
which(colnames(dat) == "violent_rate_log")

dat <- dat[, c(190:199, 227)]

hist(exp(dat$violent_rate_log), breaks = 40)
hist(dat[, 11][[1]])

ggplot(dat) +
  geom_histogram(aes(x = pct_unemp))

ggplot(dat) +
  geom_histogram(aes(x = mean_pb_ln))

cor(dat)
heatmap(cor(dat), symm = TRUE)

fit <- lm(violent_rate_log ~ pct_unemp + pct_hs_more + mean_pb_ln, data = dat)
summary(fit)

plot(fit)


ggplot(dat) +
  geom_point(aes(y = violent_rate_log, x = mean_pb_ln)) +
  geom_smooth(aes(y = violent_rate_log, x = mean_pb_ln), method = "lm")

ggplot(dat, aes(y = violent_rate_log, x = pct_hs_more)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(dat %>% filter(state == "CA" | state == "NY")) +
  geom_point(aes(y = violent_rate_log, x = mean_pb_ln, color = state)) +
  geom_smooth(aes(y = violent_rate_log, x = mean_pb_ln))

ggplot(dat) +
  geom_point(aes(y = violent_rate_log, x = pct_hs_more)) +
  geom_smooth(aes(y = violent_rate_log, x = pct_hs_more), method = "lm")
