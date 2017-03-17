# plot

sigmoid <- function(z) 1 / (1 + exp(-z))
extract_cutpoint_samples <- function(model) extract(model)$cutpoints %>% sigmoid
convert_cdf_to_pmf <- function(cutpoint_samples) cbind(cutpoint_samples, 1) - cbind(0, cutpoint_samples)

compute_empirical_multinomial_expected_value <- function(prob, size = 500) {
  rmultinom(n = 1, size = size, prob = prob) %>%
    "/"(size) %>%
    "*"(1:length(prob)) %>%
    sum
}

generate_weighted_average_posterior_predictions <- function(pmf_samples, model_name) {
  predictions <- apply(X = pmf_samples, MARGIN = 1, FUN = compute_empirical_multinomial_expected_value) %>%
    as.data.frame %>%
    set_colnames("weighted_average")
  predictions$model <- model_name
  return( predictions )
}

compute_posterior_weighted_average_density <- function(model, model_name) {
  model %>%
    extract_cutpoint_samples %>%
    convert_cdf_to_pmf %>%
    generate_weighted_average_posterior_predictions(model_name)
}

generate_comparative_density_plot <- function(first_model, second_model, first_model_name = "A", second_model_name = "B") {
  compute_posterior_weighted_average_density(model = first_model, model_name = first_model_name) %>%
    rbind( compute_posterior_weighted_average_density(model = second_model, model_name = second_model_name) ) %>%
    ggplot(aes(x = weighted_average, fill = model)) +
    geom_density(alpha = .5, color = "white") +
    theme_minimal() +
    guides(fill = guide_legend(title="Population")) +
    labs(
      title = "Posterior Predictive Density Plot of Weighted Average Scores",
      subtitle = "(If one is distinctly to the right of the other, then it's probably better)",
      x = "Weighted Average",
      y = "Count"
    )
}
