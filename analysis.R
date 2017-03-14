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
cumulative_proportions <- feedback %>% table %>% "/"(N) %>% cumsum %>% as.data.frame %>% set_colnames("prop") %>% mutate(outcome=1:length(probabilities))
ggplot(cumulative_proportions, aes(x=outcome, y=prop)) + geom_point()

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
