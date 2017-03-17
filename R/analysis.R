# analysis

source("./R/functions.R")
packages <- c("magrittr", "ggplot2", "dplyr", "OrderedCategoricalGLM", "reshape2")
lapply(packages, require, character.only = TRUE)

# simulate data
N <- 50
probabilities <- c(.1, .2, .3, .3, .1)
outcomes <- 1:length(probabilities)
feedback <- rmultinom(n = N, size = 1, prob = probabilities) %>% t %>% max.col %>% as.data.frame %>% set_colnames("obs")

# plot empirical counts
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

# fit ordered categorical linear model
model <- OrderedCategoricalGLM::buildModel(feedback = feedback$obs, outcomes = outcomes, chains = 2, cores = 2, warmup = 500, iter = 1500)

# extract cutpoint estimates, convert to cdf scale
cutpoint_samples <- model %>% extract_cutpoint_samples %>% sigmoid
cutpoint_mu <- apply(X = cutpoint_samples, MARGIN = 2, FUN = mean)
cutpoint_PI <- apply(X = cutpoint_samples, MARGIN = 2, FUN = quantile, c(.04, .96))

# plot mean and 92% interval of cdf vs. observed proportions
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

# simulate a posterior predictive distribution of weighted averages with original model
simulated_observations <- compute_posterior_weighted_average_histogram(model, model_name = "ordered")
simulated_observations$weighted_average %>%
  qplot(bins = 30, color = I("white"), fill = I("deepskyblue3"),
        alpha = I(.8), main = "Distribution of Simulated Weighted Averages, Ordered Categorical GLM",
        xlab = "Weighted Average", ylab = "Count")

# simulate a posterior predictive distribution of weighted averages with multinomial model
simulated_observations_multinomial <- rdirichlet(n = nrow(simulated_observations), alpha = 1 + table(feedback$obs)) %>%
  generate_weighted_average_posterior_predictions(model_name = "multinomial")

# compare histograms
simulated_observations %>%
  rbind(simulated_observations_multinomial) %>%
  ggplot(aes(x = weighted_average, fill = model)) +
  geom_histogram(alpha = .7, color = "white", bins = 50, position = "identity") +
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
n_samples <- 200
cdf_samples_ordered <- model %>% extract_cdf_samples %>% cbind(model = "ordered") %>% head(n_samples)
cdf_samples_multinomial <- rdirichlet(n = n_samples, alpha = 1 + table(feedback$obs)) %>%
  apply(MARGIN = 1, FUN = cumsum) %>%
  t %>%
  as.data.frame %>%
  set_colnames(outcomes) %>%
  cbind(model = "multinomial")

rbind(cdf_samples_ordered, cdf_samples_multinomial) %>%
  mutate(trial = 1:nrow(.)) %>%
  melt(id.vars=c("model", "trial")) %>%
  ggplot(aes(x = variable, y = value, group = factor(trial))) +
  geom_line(aes(color = model), alpha = .15) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  labs(
    title = "Posterior Distribution of the CDF",
    x = "Score",
    y = "Cumulative Probability"
  )
ggsave("figures/comparative_posterior_cumulative_distributions.png", scale = .75, dpi = 300)
