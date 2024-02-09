


### Creating data for ch 03 causal inference ###
set.seed(12)
# A randomized experiment for running speeds

# 12 min avg time at baseline
avg_time <- 12

ms <- 3000 / (avg_time * 60)


## n athletes
n <- 38

pre <- rnorm(n, ms, ms * 0.2)

treat <- sample(rep(c("N", "K"), length.out = n), 38, replace = FALSE)

post <- ifelse(treat == "N", pre * 1.15, pre * 1.1) + rnorm(n, 0, pre * 0.1)

dat <- data.frame(athlete = seq(1:n), 
           treat, pre, post)


library(tidyverse)

dat %>%
  pivot_longer(names_to = "time", values_to = "speed", cols = pre:post) %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, speed, color = treat)) + geom_point() + 
  geom_line(aes(group = athlete))


dat %>%
  mutate(change = post - pre) %>%
  with(., t.test(change ~ treat))

















