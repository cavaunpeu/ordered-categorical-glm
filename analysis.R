# analysis

library(tidyr)
library(magrittr)
library(ggplot2)

# simulate data
N <- 50
feedback <- rmultinom(n = N, size = 1, prob = c(.1, .2, .3, .3, .1)) %>% t %>% max.col %>% as.data.frame %>% set_colnames("obs")
ggplot(feedback, aes(x=obs)) + geom_bar()

# plot observed cumulative probabilities
feedback %>% table %>% "/"(N) %>% cumsum %>% plot(type="b")
