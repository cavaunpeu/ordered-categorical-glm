# analysis

packages <- c("dplyr", "tidyr", "magrittr", "ggplot2", "rethinking", "MCMCpack", "reshape2", "ggthemes")
lapply(packages, require, character.only = TRUE)

# simulate data
N <- 50
probabilities <- c(.1, .2, .3, .3, .1)
outcomes <- 1:length(probabilities)
feedback <- rmultinom(n = N, size = 1, prob = probabilities) %>% t %>% max.col %>% as.data.frame %>% set_colnames("obs")
ggplot(feedback, aes(x = obs)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Empirical Counts of Explicit Feedback Scores",
    x = "Score",
    y = "Count"
  )
ggsave("figures/empirical_distribution_explicit_feedback_scores.png", scale = .75, dpi = 300)

# plot observed cumulative probabilities
cumulative_proportions <- feedback %>% table %>% "/"(N) %>% cumsum %>% as.data.frame %>% set_colnames("observed") %>% mutate(outcome = outcomes)
ggplot(cumulative_proportions, aes(x = outcome, y = observed)) +
  geom_point(size = 2) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Empirical CDF of Explicit Feedback Scores",
    x = "Score",
    y = "Cumulative Probability"
  )

# fit model
data <- list(obs = feedback$obs, N = N)
cutpoint_init_values <- list(cutpoints = c(-2, -1, 1, 2))
model <- stan(
  file = "model.stan",
  data = data,
  init = list(cutpoint_init_values, cutpoint_init_values),
  chains = 2,
  cores = 2,
  verbose = TRUE
)

# examine estimates
sigmoid <- function(z) 1 / (1 + exp(-z))
sigmoid( coef(model) ) %>% plot

# cutpoint_samples <- extract(model@stanfit)$cutpoints %>% sigmoid
cutpoint_samples <- extract(model)$cutpoints %>% sigmoid
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
  geom_ribbon(aes(ymax = mu_upper_bound, ymin = mu_lower_bound), alpha = .05, fill = "red") +
  theme_minimal() +
  labs(
    title = "Mean Posterior Cumulative Distribution Function",
    x = "Score",
    y = "Cumulative Probability"
  )
ggsave("figures/mean_posterior_cumulative_distribution.png", scale = .75, dpi = 300)

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
  qplot(bins = 30, color = I("white"), fill = I("deepskyblue3"),
        alpha = I(.8), main = "Distribution of Simulated Weighted Averages, Ordered Categorical GLM",
        xlab = "Weighted Average", ylab = "Count")

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
  geom_histogram(alpha = .7, color = "white", bins = 50) +
  theme_minimal() +
  labs(
    title = "Posterior Predictive Histogram of Weighted Average Throws",
    x = "Weighted Average",
    y = "Count"
  )
ggsave("figures/comparative_posterior_predictive_histograms.png", scale = .75, dpi = 300)

# compare density plots
simulated_observations %>%
  rbind(simulated_observations_multinomial) %>%
  ggplot(aes(x = weighted_average, fill = model)) +
  geom_density(alpha = .7, color = "white") +
  theme_minimal() +
  labs(
    title = "Posterior Predictive Density Plot of Weighted Average Throws",
    x = "Weighted Average",
    y = "Count"
  )
ggsave("figures/comparative_posterior_predictive_density_plots.png", scale = .75, dpi = 300)

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
  geom_line(aes(color = model), alpha = .05) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  labs(
    title = "Posterior Distribution of the CDF",
    x = "Score",
    y = "Cumulative Probability"
  )
ggsave("figures/comparative_posterior_cumulative_distributions.png", scale = .75, dpi = 300)
