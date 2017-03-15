# analysis

library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(rethinking)
library(MCMCpack)
library(reshape2)

# simulate data
N <- 50
probabilities <- c(.1, .2, .3, .3, .1)
outcomes <- 1:length(probabilities)

feedback <- rmultinom(n = N, size = 1, prob = probabilities) %>% t %>% max.col %>% as.data.frame %>% set_colnames("obs")
ggplot(feedback, aes(x = obs)) + geom_bar()

# plot observed cumulative probabilities
cumulative_proportions <- feedback %>% table %>% "/"(N) %>% cumsum %>% as.data.frame %>% set_colnames("observed") %>% mutate(outcome = outcomes)
ggplot(cumulative_proportions, aes(x = outcome, y = observed)) + geom_point()

# fit model
model <- map2stan(
  alist(
    obs ~ dordlogit( phi, cutpoints ),
    phi <- 0,
    cutpoints ~ dnorm(0, 10)
  ),
  data = feedback, start = list(cutpoints = c(-2, -1, 1, 2)), chains = 2
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
  ggplot(aes(x = outcome)) +
  geom_point(size = 2, aes(y = observed, color = "Observed")) +
  geom_line(aes(y = mu, color = "Estimated"), linetype = "dashed") +
  geom_ribbon(aes(ymax = mu_upper_bound, ymin = mu_lower_bound), alpha = .05, fill = "red")

# simulate new observations
compute_empirical_multinomial_expected_value <- function(prob, size = 500) {
  rmultinom(n = 1, size = size, prob = prob) %>%
    "/"(size) %>%
    "*"(1:length(prob)) %>%
    sum
}

simulated_probabilities <- cbind(cutpoint_samples, 1) - cbind(0, cutpoint_samples)
simulated_observations <- apply(X = simulated_probabilities, MARGIN = 1, FUN = compute_empirical_multinomial_expected_value) %>%
  as.data.frame %>%
  set_colnames("weighted_average")
simulated_observations$model <- "ordered"

simulated_observations$weighted_average %>%
  qplot(bins = 30, color = I("white"), fill = I("deepskyblue3"), alpha = I(.8))

# compare with multinomial model
simulated_probabilities_multinomial <- rdirichlet(n = nrow(simulated_probabilities), alpha = 1 + table(feedback$obs))
simulated_observations_multinomial <- apply(X = simulated_probabilities_multinomial, MARGIN = 1, FUN = compute_empirical_multinomial_expected_value) %>%
  as.data.frame %>%
  set_colnames("weighted_average")
simulated_observations_multinomial$model <- "multinomial"

# compare histograms
simulated_observations %>%
  rbind(simulated_observations_multinomial) %>%
  ggplot(aes(x = weighted_average, fill = model)) +
  geom_histogram(alpha = .7, color = "white")

# compare density plots
simulated_observations %>%
  rbind(simulated_observations_multinomial) %>%
  ggplot(aes(x = weighted_average, fill = model)) +
  geom_density(alpha = .7, color = "white")

# compare CDF distributions
simulated_probabilities_cumsum <- apply(X = simulated_probabilities, MARGIN = 1, FUN = cumsum) %>%
  t %>%
  as.data.frame %>%
  set_colnames(outcomes)
simulated_probabilities_cumsum$model <- "ordered"

simulated_probabilities_multinomial_cumsum <- apply(X = simulated_probabilities_multinomial, MARGIN = 1, FUN = cumsum) %>%
  t %>%
  as.data.frame %>%
  set_colnames(outcomes)
simulated_probabilities_multinomial_cumsum$model <- "multinomial"

cumsum_df <- rbind(simulated_probabilities_cumsum, simulated_probabilities_multinomial_cumsum)
cumsum_df$trial <- 1:nrow(cumsum_df)
cumsum_df %>%
  melt(id.vars=c("model", "trial")) %>%
  ggplot(aes(x = variable, y = value, group = factor(trial))) +
  geom_line(aes(color = model), alpha = .2)
