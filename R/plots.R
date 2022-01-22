draw_cancer_plot <- function() {
  tib <-
    tibble(dataset = rep(c("cancer", "chess", "hypothyroid", "mushroom"),
                         each = 5 * 7),
           folds   = rep(c(2, 5, 10, 20, -1), each = 7, times = 4),
           algo    = rep(c("C4.5", "naive Bayes", "random forest", "XGBoost",
                           "SVM", "logistic regression", "kNN"), 4 * 5),
           mean    = rep(NA, 4 * 5 * 7),
           sd      = rep(NA, 4 * 5 * 7))

  for (f in c(2, 5, 10, 20, -1)) {
    rows <- tib$folds == f & tib$dataset == "cancer"
    if (f == -1) f <- 50
    d <- read_saved_data(glue("cv_cancer_{f}_50_2021.RData"))
    means <- lapply(d, mean) %>% unlist(use.names = F)
    sds   <- lapply(d, sd)   %>% unlist(use.names = F)
    tib[rows, "mean"] <- means
    tib[rows, "sd"]   <- sds
  }

  for (f in c(2, 5, 10, 20, -1)) {
    rows <- tib$folds == f & tib$dataset == "chess"
    if (f == -1) f <- 200
    d <- read_saved_data(glue("cv_chess_{f}_50_2021.RData"))
    means <- lapply(d, mean) %>% unlist(use.names = F)
    sds   <- lapply(d, sd)   %>% unlist(use.names = F)
    tib[rows, "mean"] <- means
    tib[rows, "sd"]   <- sds
  }

  for (f in c(2, 5, 10, 20, -1)) {
    rows <- tib$folds == f & tib$dataset == "hypothyroid"
    if (f == -1) f <- 200
    d <- read_saved_data(glue("cv_hypothyroid_{f}_50_2021.RData"))
    means <- lapply(d, mean) %>% unlist(use.names = F)
    sds   <- lapply(d, sd)   %>% unlist(use.names = F)
    tib[rows, "mean"] <- means
    tib[rows, "sd"]   <- sds
  }

  for (f in c(2, 5, 10, 20, -1)) {
    rows <- tib$folds == f & tib$dataset == "mushroom"
    seed <- 2021
    if (f == -1) {
      f <- 200
      seed <- 2023
    }
    d <- read_saved_data(glue("cv_mushroom_{f}_50_{seed}.RData"))
    means <- lapply(d, mean) %>% unlist(use.names = F)
    sds   <- lapply(d, sd)   %>% unlist(use.names = F)
    tib[rows, "mean"] <- means
    tib[rows, "sd"]   <- sds
  }

  tib %>%
    filter(algo %in% c("C4.5", "XGBoost", "random forest")) %>%
    mutate(folds = ordered(folds, levels = c(2, 5, 10, 20, -1))) %>%
    ggplot(aes(folds, mean, group = algo, color = algo)) +
    geom_line() +
    facet_wrap(~dataset, scale = "free_y") +
    scale_color_brewer(name = "Algorithm", palette = "Dark2") +
    labs(x = "Number of folds (-1 = leave-one-out)", y = "Sample mean",
         title = "Average accuracy of different algorithms on UCI data sets")
  # ggsave("~/Documents/paper/tex/kohavi_mean.pdf", height=3, width=7)

  tib %>%
    mutate(folds = ordered(folds, levels = c(2, 5, 10, 20, -1))) %>%
    ggplot(aes(folds, sd, group = algo, color = algo)) +
    geom_line() +
    facet_wrap(~dataset, scale = "free_y") +
    scale_color_brewer(name = "Algorithm", palette = "Dark2") +
    labs(x = "Number of folds (-1 = leave-one-out)",
         y = "Sample standard deviation",
         title = "Variation in accuracy of different algorithms on UCI data sets")
  # ggsave("~/Documents/paper/tex/kohavi_sd.pdf", height=3, width=7)

  tib %>%
    filter(algo %in% c("C4.5", "XGBoost", "random forest")) %>%
    rename(SD = sd) %>%
    pivot_longer(c(mean, SD)) %>%
    mutate(folds = ordered(folds, levels = c(2, 5, 10, 20, -1))) %>%
    ggplot(aes(folds, value, group = algo, color = algo)) +
    geom_line() +
    facet_grid(name ~ dataset, scale = "free_y") +
    scale_color_brewer(name = "Algorithm", palette = "Dark2") +
    labs(x = "Number of folds (-1 = leave-one-out)",
         y = "Estimate",
         title = "Accuracy sample mean and standard deviation (trees only)")
  # ggsave("~/Documents/paper/tex/kohavi_trees.pdf", height=3, width=7)
}

draw_bic_plot <- function() {
  d <- replicate(4, NA, simplify = FALSE)
  name_list <- c("cancer", "chess", "hypothyroid", "mushroom")
  for (i in seq_along(name_list)) {
    name <- name_list[[i]]
    d[[i]] <- read_saved_data(glue("bic_{name}_100_2021.RData"))[[i]]
    d[[i]]$set <- name
  }
  tib <- rbind(d[[1]], d[[2]], d[[3]], d[[4]])

  tib_stats <-
    tib %>%
    pivot_longer(ends_with("best_rank"), names_to = "ranking") %>%
    group_by(ranking, set) %>%
    summarize(median = median(value))

  tib %>%
    pivot_longer(ends_with("best_rank"), names_to = "ranking") %>%
    ggplot(aes(value, fill = ranking)) +
    geom_histogram(alpha = 0.8, position = "identity") +
    geom_vline(data = tib_stats,
               aes(xintercept = median, color = ranking),
               size = 1.5) +
    facet_wrap(~set, scales = "free_y") +
    labs(title = "Accuracy rank of best model",
         x = "Accuracy rank (lower is more accurate)",
         y = "Count") +
    scale_fill_brewer(name = "Best according to",
                      labels = c("regular BIC", "BIC with extra term"),
                      palette = "Set1")
}

draw_bic_plot_2 <- function() {
  d <- replicate(4, NA, simplify = FALSE)
  name_list <- c("cancer", "chess", "hypothyroid", "mushroom")
  for (i in seq_along(name_list)) {
    name <- name_list[[i]]
    d[[i]] <- read_saved_data(glue("bic_{name}_100_2021.RData"))[[i]]
    d[[i]]$set <- name
  }
  tib <- rbind(d[[1]], d[[2]], d[[3]], d[[4]])

  tib_stats <-
    tib %>%
    pivot_longer(ends_with("inversions"), names_to = "ranking") %>%
    group_by(ranking, set) %>%
    summarize(median = median(value))

  tib %>%
    pivot_longer(ends_with("inversions"), names_to = "ranking") %>%
    ggplot(aes(value / 1e3, fill = ranking)) +
    geom_histogram(alpha = 0.8, position = "identity") +
    geom_vline(data = tib_stats,
               aes(xintercept = median/1e3, color = ranking),
               size = 1.5) +
    facet_wrap(~set, scales = "free_y") +
    labs(title = "Number of transpositions in accuracy",
         x = "Thousands of transpositions (lower is better)",
         y = "Count") +
    scale_fill_brewer(name = "Sorting by",
                      labels = c("regular BIC", "BIC with extra term"),
                      palette = "Set1")
}

draw_bic_plot_3 <- function() {
  d <- replicate(4, NA, simplify = FALSE)
  name_list <- c("cancer", "chess", "hypothyroid", "mushroom")
  for (i in seq_along(name_list)) {
    name <- name_list[[i]]
    d[[i]] <- read_saved_data(glue("bic_{name}_100_2021.RData"))[[i]]
    d[[i]]$set <- name
  }
  tib <- rbind(d[[1]], d[[2]], d[[3]], d[[4]])

  tib_stats <-
    tib %>%
    pivot_longer(starts_with("acc_best"), names_to = "ranking") %>%
    group_by(ranking, set) %>%
    summarize(mean = median(value))

  tib %>%
    pivot_longer(starts_with("acc_best"), names_to = "ranking") %>%
    ggplot(aes(value, fill = ranking)) +
    geom_histogram(alpha = 0.8, position = "identity") +
    geom_vline(data = tib_stats,
               aes(xintercept = mean, color = ranking),
               size = 1.5) +
    facet_wrap(~set, scales = "free_y") +
    labs(title = "Rank of most accurate model",
         x = "Rank (lower is better)",
         y = "Count") +
    scale_fill_brewer(name = "Ranking by",
                      labels = c("regular BIC", "BIC with extra term"),
                      palette = "Set1")
}

draw_sim_data_ex <- function() {
  d1 <- simulate_data_type_1(1e4, d = 2, bayes_error = 0) %>% filter(bit == 0)
  d1$be <- 0
  d2 <- simulate_data_type_1(1e4, d = 2, bayes_error = 0.1) %>% filter(bit == 0)
  d2$be <- 0.1
  d3 <- simulate_data_type_1(1e4, d = 2, bayes_error = 0.2) %>% filter(bit == 0)
  d3$be <- 0.2
  d <- rbind(d1, d2, d3) %>% rename(`Bayes error` = be)
  ggplot(d, aes(V1, V2, color = class)) +
    geom_point() +
    facet_wrap(~`Bayes error`, labeller = label_both) +
    labs(x = "x", y = "y", title = "Simulated dataset",
         subtitle = "Two-dimensional examples, with different choices of Bayes error") +
    guides(color = "none") +
    coord_fixed()
}
