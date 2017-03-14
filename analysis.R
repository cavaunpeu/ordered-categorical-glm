# analysis

library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(rethinking)

# simulate data
N <- 50
probabilities <- c(.1, .2, .3, .3, .1)
feedback <- rmultinom(n=N, size=1, prob=probabilities) %>% t %>% max.col %>% as.data.frame %>% set_colnames("obs")
ggplot(feedback, aes(x=obs)) + geom_bar()

# plot observed cumulative probabilities
cumulative_proportions <- feedback %>% table %>% "/"(N) %>% cumsum %>% as.data.frame %>% set_colnames("observed") %>% mutate(outcome=1:length(probabilities))
ggplot(cumulative_proportions, aes(x=outcome, y=observed)) + geom_point()

# fit model
model <- map2stan(
  alist(
    obs ~ dordlogit( phi, cutpoints ),
    phi <- 0,
    cutpoints ~ dnorm(0, 10)
  ),
  data=feedback, start=list(cutpoints=c(-2, -1, 1, 2)), chains=2
)

# examine estimates
sigmoid <- function(z) 1 / (1 + exp(-z))
sigmoid( coef(model) ) %>% plot

cutpoint_samples <- extract(model@stanfit)$cutpoints %>% sigmoid
cutpoint_mu <- apply(X = cutpoint_samples, MARGIN = 2, FUN = mean)
cutpoint_PI <- apply(X = cutpoint_samples, MARGIN = 2, FUN = quantile, c(.04, .96))

data.frame(
    mu = cutpoint_mu,
    mu_lower_bound = cutpoint_PI[1,],
    mu_upper_bound = cutpoint_PI[2,]
  ) %>%
  bind_cols(cumulative_proportions[1:nrow(cumulative_proportions)-1,]) %>%
  ggplot(., aes(x=outcome)) +
    geom_point(size=2, aes(y=observed, color="Observed")) +
    geom_line(aes(y=mu, color="Estimated"), linetype="dashed") +
    geom_ribbon(aes(ymax=mu_upper_bound, ymin=mu_lower_bound), alpha=.05, fill="red")

