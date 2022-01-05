draw_cancer_plot <- function() {
  cancer <- read_saved_data("cancer_cv_50_2021.RData")
  reduced <- replicate(length(cancer), NA, simplify = FALSE)
  for (i in seq_along(reduced))
    reduced[[i]] <-
      list(means = apply(cancer[[i]], 1, mean),
           sds   = apply(cancer[[i]], 1, sd))
  tib <-
    unnest_wider(enframe(reduced), col = value) %>%
    unnest_longer(col = means) %>%
    unnest_longer(col = sds) %>%
    filter(means_id == sds_id) %>%
    rename(folds = means_id) %>%
    select(-sds_id) %>%
    relocate(name, folds, means, sds) %>%
    mutate(folds = factor(folds, levels = paste("k =", c(2, 5, 10, 20, -1)),
                          ordered = TRUE))
  ggplot(tib, aes(folds, means, group = name, color = name)) +
    geom_line()
}
