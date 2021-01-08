library(ggridges)
library(scales)
library(tidyverse)

theme_set(theme_bw())
# function to make joyplot of distributions by condition for an outcome with labelled means
plot_distributions_with_means <- function(df, xlab, ylab, my_pallete, xbreaks = NA, scale = 0.6, xlim_percentile = 0.90, label_format = '$%.0fk', vertical_line = -1) {
  # reverse order of factor for plotting
  # this isn't working for some reason
  df <- df %>%
    mutate(condtion = fct_rev(condition))

  # compute summary stats
  # first average outcome within worker
  # then average across workers and compute SEs
  # plot_data <- df %>%
  #   group_by(worker_id, condition) %>%
  #   summarize(mean_worker_outcome = mean(outcome)) %>%
  #   group_by(condition) %>%
  #   summarize(mean_outcome = mean(mean_worker_outcome),
  #             se_outcome = sd(mean_worker_outcome) / sqrt(n())) %>%
  #   ungroup() %>%
  #   mutate(y = 1:n()) %>%
  #   mutate(label = sprintf(label_format, mean_outcome))

  # compute summary stats from mixed-effects model
  # first fit model, then extract mean and standard errors for each condition
  # finally convert back to same factor levels for condition as in original data frame
  hlm_model <- lmer(outcome ~ condition + (1|worker_id), data = df)
  means <- emmeans(hlm_model, c("condition"))
  condition_levels <- levels(df$condition)
  plot_data <- means %>%
    as.data.frame() %>%
    mutate(condition = factor(condition, levels = condition_levels)) %>%
    arrange(condition) %>%
    rename(mean_outcome = emmean, se_outcome = SE) %>%
    ungroup() %>%
    mutate(y = 1:n()) %>%
    mutate(label = sprintf(label_format, mean_outcome))

  # set limits and colors
  x_lim <- c(0, quantile(df$outcome, xlim_percentile))
  y_lim <- c(1.5, max(plot_data$y + 0.25))

  # make the plot
  p <- df %>%
    group_by(worker_id, condition) %>%
    summarize(outcome = mean(outcome)) %>%
    ggplot(aes(x = outcome, color = condition, fill = condition, y = condition)) +
    geom_density_ridges(alpha = 0.5, scale = scale) +
    geom_errorbarh(data = plot_data, aes(x = mean_outcome, xmin = mean_outcome - se_outcome, xmax = mean_outcome + se_outcome, y = y + 0.65 ), height = 0.05) +
    geom_text(data = plot_data, size = 4, aes(x = mean_outcome, y = y + scale + 0.18, label = label), color = "black", size = 2) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_colour_manual(guide = FALSE, values = my_pallete) +
    scale_fill_manual(guide = FALSE, values = my_pallete) +
    theme_minimal() +
    labs(x = xlab, y = ylab)

  # change breaks if requested
  # and format x axis
  if (all(!is.na(xbreaks))) {
    p <- p + scale_x_continuous(breaks = xbreaks, label = function(x) sprintf(label_format, x))
  } else {
    p <- p + scale_x_continuous(label = function(x) sprintf(label_format, x))
  }

  # if prediction error plots, add the horizontal model error
  if(vertical_line >= 0){

    p <- p + geom_vline(xintercept=vertical_line, linetype="dashed", size = 1, alpha = .5)
  }

  # return the plot
  p
}
