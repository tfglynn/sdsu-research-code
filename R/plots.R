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
    pivot_longer(c(mean, sd)) %>%
    mutate(folds = ordered(folds, levels = c(2, 5, 10, 20, -1))) %>%
    ggplot(aes(folds, value, group = algo, color = algo)) +
    geom_line() +
    facet_grid(name~dataset, scale = "free_y") +
    scale_color_brewer(name = "Algorithm", palette = "Dark2") +
    labs(x = "Number of folds (-1 = leave-one-out)",
         y = "Sample standard deviation",
         title = "Accuracy sample mean and standard deviation")
  #ggsave("~/Documents/paper/tex/kohavi_both.pdf", height=3, width=7)

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

draw_lr_plot <- function() {
  dataset_names <- c("chess", "mushroom")
  folds_values <- c(2, 5, 10, 20, 200)
  param_values <- c(0, 0.01, 0.1, 1, 10)
  tib <- tibble(name = rep(dataset_names, each = 5 * 5),
                folds = rep(folds_values, each = 5, times = 2),
                param = rep(param_values, times = 5 * 2),
                mean = NA,
                sd   = NA)
  for (k in seq_along(dataset_names)) {
    seed <- c(2021, 2023)[[k]]
    name <- dataset_names[[k]]
    for (i in seq_along(folds_values)) {
      folds <- folds_values[[i]]
      ds <- read_saved_data(glue("cv_lr_{name}_{folds}_50_{seed}.RData"))
      for (j in seq_along(param_values)) {
        param <- param_values[[j]]
        m <- mean(ds[[as.character(param)]])
        s <- sd(ds[[as.character(param)]])
        tib[25 * (k - 1) + 5 * (i - 1) + j, "mean"] <- m
        tib[25 * (k - 1) + 5 * (i - 1) + j, "sd"]   <- s
      }
    }
  }

  # Making an adjustment so 1 and 10 don't totally overlap
  tib2 <- tib
  tib2[tib2$param == 1, "mean"] <- tib2[tib2$param == 1, "mean"] + 0.008
  tib2[tib2$param == 1, "sd"] <- tib2[tib2$param == 1, "sd"] + 0.002

  tib2 %>%
    rename(set = name, SD = sd, Parameter = param) %>%
    pivot_longer(c(mean, SD)) %>%
    mutate(Parameter = ordered(Parameter),
           folds = ordered(folds)) %>%
    ggplot(aes(folds, value, group = Parameter, color = Parameter)) +
    geom_line() +
    facet_grid(name ~ set, scale = "free_y") +
    labs(title = "The effect of regularization",
         x = "Number of folds",
         y = "Estimate")
  ggsave("~/Documents/paper/tex/lr_both.pdf", height=3, width=7)
}

draw_ncv_plot <- function() {
  dataset_names <- c("cancer", "chess", "hypothyroid", "mushroom")
  algo_names <- c("naive_bayes", "rand_forest")

  tib <-
    tibble(set = rep(dataset_names, each = 100),
           trial = rep(1:50, each = 2, times = 4),
           algo = rep(algo_names, times = 200),
           lo = NA, mid = NA, hi = NA, actual = NA, success = NA)

  for (i in seq_along(dataset_names)) {
    set <- dataset_names[[i]]
    for (j in seq_along(algo_names)) {
      algo <- algo_names[[j]]
      d <- read_saved_data(glue("ncv_{algo}_{set}_2021.RData"))
      rows <- which(tib$algo == algo & tib$set == set)
      tib[rows, "lo" ] <- sapply(d, function(x) x$ci_lo)
      tib[rows, "mid"] <- sapply(d, function(x) x$err_hat)
      tib[rows, "hi" ] <- sapply(d, function(x) x$ci_hi)
      tib[rows, "actual"] <- sapply(d, function(x) x$actual)
      tib[rows, "success"] <- sapply(d, function(x) x$actual >= x$ci_lo & x$actual <= x$ci_hi)
    }
  }

  tib %>%
    mutate(algo = fct_recode(algo,
                             "naive Bayes" = "naive_bayes",
                             "random forest" = "rand_forest")) %>%
    ggplot() +
    geom_point(aes(trial, 1 - mid, color = success), size = 0.5) +
    geom_errorbar(aes(x = trial, ymin = 1 - lo, ymax = 1 - hi, color = success),
                  width = 0, size = 0.5) +
    geom_point(aes(trial, 1 - actual), size = 0.5) +
    facet_grid(algo~set, scale = "free_y") +
    labs(title = "Confidence intervals via nested cross-validation",
         x = "Trial", y = "Accuracy") +
    scale_color_hue(name = "Contains holdout estimate?", labels = c("No", "Yes"))
  ggsave("~/Documents/paper/tex/ncv.pdf", height=3, width=7)
}

draw_knn_plot <- function() {
  d <- read_saved_data("knn.RData")
  tib <- tibble(dims = rep(1:3, each = 6 * 3),
                bayes_error = rep(c(0, 0.1, 0.2), each = 6, times = 3),
                additions = rep(1:6, times = 3 * 3),
                mean = NA,
                sd = NA)
  for (i in 1:3) {
    for (j in 1:3) {
      xs <- d[[(i - 1) * 3 + j]]
      for (k in 1:6) {
        ix <- (i - 1) * 6 * 3 + (j - 1) * 6 + k
        tib[ix, "mean"] <- mean(xs[[k]])
        tib[ix, "sd"]   <- sd(xs[[k]])
      }
    }
  }

  gam <- optimize(function(x) 2 * x * pnorm(x, lower.tail = FALSE), 0:1, maximum = TRUE)$objective

  tib %>%
    mutate(asymptote = 1 - bayes_error * 1.5209669,
           upper_bound = 1 - bayes_error,
           bayes_error = ordered(bayes_error),
           dims = ordered(2 * dims),
           size = additions * 500) %>%
    rename(`Bayes error` = bayes_error, Dimension = dims) %>%
    ggplot(aes(size, mean, color = Dimension)) +
    geom_line() +
    geom_hline(aes(yintercept = upper_bound)) +
    geom_hline(aes(yintercept = asymptote), linetype = "dotted") +
    facet_wrap(~`Bayes error`, labeller = label_both) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Asymptotic accuracy of 5\uadnearest neighbor classifiers", x = "Training set size", y = "Sample mean accuracy")
  ggsave("~/Documents/paper/tex/knn.pdf", height=3, width=7)
}
