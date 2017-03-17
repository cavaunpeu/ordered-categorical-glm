# plot

source("functions.R")
packages <- c("ggplot2", "reshape2", "dplyr", "ggthemes", "gridExtra")
lapply(packages, require, character.only = TRUE)

generate_comparative_density_plot <- function(first_model, second_model, first_model_name = "A", second_model_name = "B") {
  compute_posterior_weighted_average_histogram(model = first_model, model_name = first_model_name) %>%
    rbind( compute_posterior_weighted_average_histogram(model = second_model, model_name = second_model_name) ) %>%
    ggplot(aes(x = weighted_average, fill = model)) +
    geom_density(alpha = .5, color = "white") +
    theme_minimal() +
    guides(fill = guide_legend(title="Population")) +
    labs(
      title = "Posterior Predictive Density Plot of Weighted Average Scores",
      subtitle = "(If one is distinctly to the right of the other, it's probably better)",
      x = "Weighted Average",
      y = "Count"
    ) +
    theme(
      plot.title = element_text(size=18),
      plot.subtitle = element_text(size=14),
      axis.title = element_text(size=14),
      plot.margin = unit( c(3, 3, 5, 3), "mm" )
    )
}

generate_comparative_cdf_distribution_plot <- function(first_model, second_model, first_model_name = "A", second_model_name = "B") {
  cdf_samples_first <- first_model %>% extract_cdf_samples %>% cbind(model = first_model_name) %>% head(200)
  cdf_samples_second <- second_model %>% extract_cdf_samples %>% cbind(model = second_model_name) %>% head(200)

  rbind(cdf_samples_first, cdf_samples_second) %>%
    dplyr::mutate(trial = 1:nrow(.)) %>%
    melt(id.vars=c("model", "trial")) %>%
    ggplot(aes(x = variable, y = value, group = factor(trial))) +
    geom_line(aes(color = model), alpha = .15) +
    guides(colour = guide_legend(override.aes = list(alpha = 1), title = "Population")) +
    theme_minimal() +
    labs(
      title = "Posterior CDF Distribution",
      subtitle = "(If one is distinctly \"bent towards the bottom-right\" of the other, it's probably better)",
      x = "Score",
      y = "Cumulative Probability"
    ) +
    theme(
      plot.title = element_text(size=18),
      plot.subtitle = element_text(size=14),
      axis.title = element_text(size=14),
      plot.margin = unit( c(3, 3, 5, 3), "mm" )
    )
}

generate_plot <- function(first_model, second_model) {
  density_plot <- generate_comparative_density_plot(first_model, second_model)
  cdf_distribution_plot <- generate_comparative_cdf_distribution_plot(first_model, second_model)
  grid.arrange(density_plot, cdf_distribution_plot, nrow = 2, ncol = 1)
}
