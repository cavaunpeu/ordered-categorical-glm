# functions

packages <- c("rstan", "magrittr", "MCMCpack")
lapply(packages, require, character.only = TRUE)


sigmoid <- function(z) 1 / (1 + exp(-z))
extract_cutpoint_samples <- function(model) rstan::extract(model)$cutpoints

extract_cdf_samples <- function(model) {
  model %>%
    extract_cutpoint_samples %>%
    sigmoid %>%
    as.data.frame %>%
    cbind(1) %>%
    set_colnames(1:ncol(.))
}

convert_cdf_to_pmf <- function(cdf_samples) cdf_samples - cbind(0, cdf_samples[ , -ncol(cdf_samples) ])

compute_empirical_multinomial_expected_value <- function(prob, size = 500) {
  rmultinom(n = 1, size = size, prob = prob) %>%
    "/"(size) %>%
    "*"(1:length(prob)) %>%
    sum
}

generate_weighted_average_posterior_predictions <- function(pmf_samples, model_name) {
  apply(X = pmf_samples, MARGIN = 1, FUN = compute_empirical_multinomial_expected_value) %>%
    as.data.frame %>%
    set_colnames("weighted_average") %>%
    cbind(model = model_name)
}

compute_posterior_weighted_average_histogram <- function(model, model_name) {
  model %>%
    extract_cdf_samples %>%
    convert_cdf_to_pmf %>%
    generate_weighted_average_posterior_predictions(model_name)
}
