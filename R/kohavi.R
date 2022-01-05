###################################################
# C4.5 code
###################################################

tmppath <- function(p = ".") here::here("tmp", p)

# Create a stem.names file for C4.5
c4.5_write_names <- function(filestem, tib) {
  class_names <- tib[["class"]] %>% unique %>% as.character
  class_line <- paste0(class_names, collapse = ", ")
  predictor_names <- setdiff(colnames(tib), "class")
  predictor_lines <- rep(NA, length(predictor_names))
  for (i in seq_along(predictor_names)) {
    name <- predictor_names[[i]]
    if (is.numeric(tib[[name]])) {
      vals <- "continuous"
    } else {
      vals <-
        Filter(. %>% is.na %>% `!`, tib[[name]]) %>%
        unique %>%
        as.character
    }
    vals_string <- paste0(vals, collapse = ", ")
    predictor_lines[[i]] <- paste0(name, ": ", vals_string)
  }
  path <- tmppath(paste0(filestem, ".names"))
  writeLines(c(class_line, predictor_lines), path)
}

# Create stem.data and stem.test files for C4.5
# We set this option so `write_csv` doesn't clobber our own progress bar.
options(readr.show_progress = FALSE)
c4.5_write_data <- function(filestem, train, test) {
  write_csv(relocate(train, "class", .after = last_col()),
            tmppath(paste0(filestem, ".data")),
            na = "?",
            col_names = FALSE)
  write_csv(relocate(test, "class", .after = last_col()),
            tmppath(paste0(filestem, ".test")),
            na = "?",
            col_names = FALSE)
}

ensure_dir_exists <- function(p) if (!dir.exists(p)) dir.create(p)

#' Get confusion matrix of C4.5 model
#' @export
c4.5 <- function(filestem, tib, train, test) {
  wd <- getwd()
  on.exit(setwd(wd))
  path <- tmppath()
  ensure_dir_exists(path)
  setwd(path)

  c4.5_write_names(filestem, tib)
  c4.5_write_data(filestem, train, test)
  output <- system2("c4.5", c("-f", filestem, "-u"), stdout = TRUE)

  # C4.5 produces a lot of output, ending with a confusion matrix.
  # This code parses the matrix and returns it.
  pos <- Position(. %>% str_detect("classified as"), output)
  rows <- head(tail(output, -(pos + 1)), -1)
  ncols <- str_count(output[pos], "\\(.\\)")
  r <- paste(c("\\t", rep(" (.{4})", ncols), "\\t\\(.\\): class (.*)"),
             collapse = "")
  r <- str_match(rows, r)[, -1]
  apply(r[, -ncol(r)], c(1, 2), as.numeric) %>% replace_na(0)
}

#' Get accuracy of C4.5 model
#' @export
c4.5_accuracy <- function(filestem, tib, train, test) {
  confusion <- c4.5(filestem, tib, train, test)
  sum(diag(confusion)) / sum(confusion)
}

###################################################
# Other models
###################################################

naive_bayes_accuracy <- function(name, tib, train, test) {
  model <- naive_bayes(class ~ ., train, laplace = 1)
  predictions <- predict(model, select(test, -class))
  mean(predictions == test$class)
}

rf_spec <- rand_forest(mode = "classification") %>% set_engine("ranger")
rand_forest_accuracy <- function(name, tib, train, test) {
  model <- fit(rf_spec, class ~ ., train)
  predictions <- predict(model, test)$.pred_class
  mean(predictions == test$class)
}

boost_spec <- boost_tree(mode = "classification")
boost_tree_accuracy <- function(name, tib, train, test) {
  rec <-
    recipe(class ~ ., train) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep
  train_baked <- bake(rec, train)
  test_baked  <- bake(rec, test)
  # WARNING: ... changed from 'merror' to 'mlogloss' ... blah blah
  sink("/dev/null")
  model <- fit(boost_spec, class ~ ., train_baked)
  sink()
  predictions <- predict(model, test_baked)$.pred_class
  mean(predictions == test_baked$class)
}

svm_spec <- svm_linear(mode = "classification")
svm_accuracy <- function(name, tib, train, test) {
  rec <-
    recipe(class ~ ., train) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    prep
  train_baked <- bake(rec, train)
  test_baked  <- bake(rec, test)
  model <- fit(svm_spec, class ~ ., train_baked)
  predictions <- predict(model, test_baked)$.pred_class
  mean(predictions == test_baked$class)
}

lr_spec <- logistic_reg(penalty = 1, engine = "glmnet")
logistic_reg_accuracy <- function(name, tib, train, test) {
  rec <-
    recipe(class ~ ., train) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep
  train_baked <- bake(rec, train)
  test_baked  <- bake(rec, test)
  model <- fit(lr_spec, class ~ ., train_baked)
  predictions <- predict(model, test_baked)$.pred_class
  mean(predictions == test_baked$class)
}

knn_spec <- nearest_neighbor(mode = "classification", weight_func = "rectangular")
nearest_neighbor_accuracy <- function(name, tib, train, test) {
  # 5 neighbors by default
  rec <-
    recipe(class ~ ., train) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    prep
  train_baked <- bake(rec, train)
  test_baked  <- bake(rec, test)
  model <- fit(knn_spec, class ~ ., train_baked)
  predictions <- predict(model, test_baked)$.pred_class
  mean(predictions == test_baked$class)
}

#' Models used in the Kohavi replication experiments
#' @export
# NOTE: We only run logistic_reg on the binary classification problems,
# i.e. not soybean and vehicle.
my_model_list <-
  list(list(name = "c4.5",
            fn = c4.5_accuracy,
            multiclass = TRUE),
       list(name = "naive_bayes",
            fn = naive_bayes_accuracy,
            multiclass = TRUE),
       list(name = "rand_forest",
            fn = rand_forest_accuracy,
            multiclass = TRUE),
       list(name = "boost_tree",
            fn = boost_tree_accuracy,
            multiclass = TRUE),
       list(name = "svm",
            fn = svm_accuracy,
            multiclass = TRUE),
       list(name = "logistic_reg",
            fn = logistic_reg_accuracy,
            multiclass = FALSE),
       list(name = "nearest_neighbor",
            fn = nearest_neighbor_accuracy,
            multiclass = TRUE))

###################################################
# "True" accuracy estimation
###################################################

#' Estimate the accuracy of models on a dataset
#'
#' \code{estimate_accuracies} repeatedly trains and evaluates models on a given
#' dataset.
#'
#' @param dataset_name a unique string identifying the dataset
#' @param dataset the data set to use
#' @param nsamples how large the training set should be
#' @param times how many times to train and test the models, default 500
#' @param models the models to train
#' @param seed random seed
#' @return a matrix of accuracies, where each row corresponds to a model
#' @export
estimate_accuracies <-
  function(dataset_name, dataset, nsamples,
           times = 500, models = my_model_list, seed = 2021) {
    multiclass <- length(unique(dataset$class)) > 2
    accuracies <-
      replicate(length(models), rep(NA, times), simplify = FALSE)
    names(accuracies) <- sapply(models, . %>% `$`("name"))

    runs <- sum(sapply(accuracies, length))
    bar <- new_bar(runs)

    set.seed(seed)
    samples <- replicate(times, sample(1:nrow(dataset), nsamples))
    for (i in seq_len(times)) {
      train <- dataset[samples[, i], ]
      test  <- dataset[-samples[, i], ]
      for (j in seq_along(models)) {
        model <- models[[j]]
        if (!multiclass || model$multiclass) {
          accuracies[[model$name]][[i]] <-
            model$fn(dataset_name, dataset, train, test)
        }
        tick(bar)
      }
    }
    accuracies
  }

###################################################
# Cross-validation
###################################################

#' Fold counts used in the Kohavi replication experiments
#' @export
my_fold_counts <- c(2, 5, 10, 20, -1) # -1 = LOOCV

#' Estimate the accuracy of models on a dataset using cross-validation
#'
#' \code{cv_experiment} tests the effectiveness of cross-validation for
#' estimating the accuracy of different models.
#'
#' @param dataset_name a unique string identifying the dataset
#' @param dataset the data set to use
#' @param nsamples how large the training set should be
#' @param times how many times to train and test the models, default 500
#' @param fold_counts folds to use, with a -1 meaning leave-one-out
#' @param models the models to train
#' @param seed random seed
#' @return a matrix of accuracies, where each row corresponds to a model
#' @export
cv_experiment <-
  function(dataset_name, dataset, nsamples, folds,
           times = 50, models = my_model_list,
           seed = 2021) {
    multiclass <- length(unique(dataset$class)) > 2
    accuracies <-
      replicate(length(models), rep(NA, times), simplify = FALSE)
    names(accuracies) <- sapply(models, . %>% `$`("name"))

    runs <- length(accuracies) * times * folds
    bar  <- new_bar(runs)
    set.seed(seed)
    for (j in seq_len(times)) {
      if (identical(dataset, hypothyroid_dataset)) {
        classes <- unique(dataset$class)
        class1 <- dataset$class == classes[[1]]
        class2 <- !class1
        nclass1 <- round(nsamples * length(which(class1)) / nrow(dataset))
        nclass2 <- nsamples - nclass1
        class1_subset <- sample(which(class1), nclass1)
        class2_subset <- sample(which(class2), nclass2)
        d_subset <- dataset[c(class1_subset, class2_subset), ]
        cv <- vfold_cv(d_subset, folds, strata = "class")
      } else {
        d_subset <- dataset[sample(1:nrow(dataset), nsamples), ]
        cv <- vfold_cv(d_subset, folds)
      }

      acc_cv <- replicate(length(accuracies), rep(NA, folds), simplify = FALSE)
      names(acc_cv) <- names(accuracies)
      for (i in seq_along(cv$splits)) {
        split <- cv$splits[[i]]
        train <- analysis(split)
        test  <- assessment(split)
        for (m in seq_along(models)) {
          model <- models[[m]]
          if (!multiclass || model$multiclass) {
            acc_cv[[model$name]][[i]] <-
              model$fn(dataset_name, dataset, train, test)
          }
          tick(bar)
        }
      }

      for (label in names(accuracies))
        accuracies[[label]][[j]] <- mean(acc_cv[[label]])
    }
    cat("\nDone!\n")
    accuracies
}

###################################################
# Running the experiments
###################################################

##### "True" accuracy #####

#' @export
run_accuracy_estimation <- function(datasets = my_dataset_list,
                                    times = 500,
                                    seed = 2021) {
  for (i in seq_along(datasets)) {
    name  <- datasets[[i]]$name
    base  <- paste(name, "accuracy", times, seed, sep = "_")
    fname <- paste0(base, ".RData")
    path  <- savepath(fname)
    if (!readable(path)) {
      cat("Testing on dataset `", name, "` ...\n", sep = "")
      tib  <- datasets[[i]]$tibble
      size <- datasets[[i]]$sample_size
      results <-
        suppressWarnings(estimate_accuracies(base, tib, size,
                                             times = times, seed = seed))
      save_data(results, fname)
    } else {
      cat("Dataset `", base, "` has saved RData file, skipping\n", sep = "")
    }
  }
}

##### CV experiments #####

#' @export
run_cv_experiments <-
  function(datasets = my_dataset_list,
           fold_counts = my_fold_counts,
           times = 50, seed = 2021) {
  for (i in seq_along(datasets)) {
    name <- datasets[[i]]$name
    tib  <- datasets[[i]]$tibble
    size <- datasets[[i]]$sample_size
    for (j in seq_along(fold_counts)) {
      k <- fold_counts[[j]]
      if (k == -1) k <- size
      base  <- paste("cv", name, k, times, seed, sep = "_")
      fname <- paste0(base, ".RData")
      path  <- savepath(fname)
      if (!readable(path)) {
        cat("Testing dataset `", name, "` with ", k, " folds ...\n", sep = "")
        results <-
          suppressWarnings(cv_experiment(base, tib, size,
                                         times = times,
                                         folds = k,
                                         seed  = seed))
        save_data(results, fname)
      } else {
        cat("Dataset `", base, "` with ", k,
            " folds has saved RData file, skipping\n", sep = "")
      }
    }
  }
}

## Plan for hypothesis stability:
## 1. Take a sample of the data.
## 2. Split the sample into train/test sets.
## 2. Train algo on the full training data and the full data minus one point.
## 3. Compare the two algos on the test set.
## 4. Repeat these splits to get a sample of "distances" for density estimation.
## 5. Use KDE to estimate the distribution of the "distance".
## 6. Find the minimum value of \beta_1 + \beta_2.
## 7. Plug into Theorem 3.1.
#
##distances <- rep(NA, 50)
##for (i in seq_along(distances)) {
##  rows <- sample(1:nrow(cancer_dataset), 50)
##  train <- cancer_dataset[rows, ]
##  loo_train <- head(train, -1)
##  test  <- cancer_dataset[-rows, ]
##  spec <- logistic_reg(penalty = 1, engine = "glmnet")
##  rec <-
##    recipe(class ~ ., train) %>%
##    step_dummy(all_nominal_predictors()) %>%
##    prep
##  train_baked <- bake(rec, train)
##  loo_train_baked <- bake(rec, loo_train)
##  test_baked <- bake(rec, test)
##  model <- fit(spec, class ~ ., train_baked)
##  loo_model <- fit(spec, class ~ ., loo_train_baked)
##  predictions <- predict(model, test_baked)$.pred_class
##  loo_predictions <- predict(loo_model, test_baked)$.pred_class
##  distances[[i]] <- mean(predictions != loo_predictions)
##}
##
### It looks like KDE is out of the question, so try a beta distribution.
### We can use this: https://stats.stackexchange.com/questions/67443/does-the-beta-distribution-have-a-conjugate-prior
##
##
##if (bw.nrd(distances) > 0) {
##  dens <- density(distances, from=0, to=1, bw = "nrd", n = 1e3) # "nrd" seems to work best on beta distributions
##} else {
##  dens <- dbeta(
##}
##
### This does the integral we need for the density
##integral <- function(dens, low_x) {
##  if (low_x == 0) return(1)
##  if (low_x == 1) return(0)
##  indices <- dens$x >= low_x
##  initial <- first(which(indices))
##  len <- length(dens$x)
##  dx <- 1 / (len - 1)
##  if (dens$x[[initial]] > low_x) {
##    xl <- dens$x[[initial - 1]]
##    xr <- dens$x[[initial]]
##    dx1 <- xr - xl
##    yl <- dens$y[[initial - 1]]
##    yr <- dens$y[[initial]]
##    y1 <- (yl * (low_x - xl) + yr * (xr - low_x)) / dx1
##    area <- dx1 / 2 * (y1 + yr) + dx / 2 * sum(dens$y[indices] * c(1, rep(2, sum(indices) - 2), 1))
##  } else {
##    area <- dx / 2 * sum(dens$y[indices] * c(1, rep(2, sum(indices) - 2), 1))
##  }
##  area
##}
##
### Here is a test of how well it works.  Seems like the code is OK, but ...
##xs <- rbeta(50, 2, 5000) # (note the extreme distribution)
##dens <- density(xs, from=0, to=1, bw="nrd", n=1000)
##f <- function(x) x + integral(dens, x)
##opt <- optimize(f, c(0, 1))
##best_sum <- opt$objective
##(upper_bounds <- sqrt((1 / (2*c(50, 100, 500, 1000)) + 3 * best_sum) / 0.1))
##
### We can compare my results with the correct ones using this:
##g <- function(x) x + pbeta(x, 2, 5000, lower.tail = FALSE)
##opt <- optimize(g, c(0, 1))
##best_sum <- opt$objective
##(upper_bounds <- sqrt((1 / (2*c(50, 100, 500, 1000)) + 3 * best_sum) / 0.1))
### It ends up being close.
##
### It turns out the upper bound is usually useless, sadly.
