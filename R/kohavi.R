##################################################
# C4.5 code
##################################################

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

##################################################
# Other models
##################################################

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

# XXX: This penalty is too high.  Future experiments should use something else.
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

logistic_reg_accuracy_with_penalty <- function(name, tib, train, test, pen) {
  this_lr_spec <- logistic_reg(penalty = pen, engine = "glmnet")
  rec <-
    recipe(class ~ ., train) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep
  train_baked <- bake(rec, train)
  test_baked  <- bake(rec, test)
  model <- fit(this_lr_spec, class ~ ., train_baked)
  predictions <- predict(model, test_baked)$.pred_class
  mean(predictions == test_baked$class)
}

knn_spec <- nearest_neighbor(mode = "classification",
                             weight_func = "rectangular")
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
my_model_list <-
  list(list(name = "c4.5",
            fn = c4.5_accuracy),
       list(name = "naive_bayes",
            fn = naive_bayes_accuracy),
       list(name = "rand_forest",
            fn = rand_forest_accuracy),
       list(name = "boost_tree",
            fn = boost_tree_accuracy),
       list(name = "svm",
            fn = svm_accuracy),
       list(name = "logistic_reg",
            fn = logistic_reg_accuracy),
       list(name = "nearest_neighbor",
            fn = nearest_neighbor_accuracy))

##################################################
# "True" accuracy estimation
##################################################

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

##################################################
# Cross-validation
##################################################

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
#' @param folds number of folds to use, with a -1 meaning leave-one-out
#' @param models the models to train
#' @param seed random seed
#' @return a matrix of accuracies, where each row corresponds to a model
#' @export
cv_experiment <-
  function(dataset_name, dataset, nsamples, folds,
           times = 50, models = my_model_list,
           seed = 2021) {
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
        if (folds < nsamples) {
          cv <-
            # We suppress the warning about pool = 0
            suppressWarnings(vfold_cv(d_subset, folds, strata = class, pool = 0))
        } else {
          cv <- vfold_cv(d_subset, folds)
        }
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
          acc_cv[[model$name]][[i]] <-
            model$fn(dataset_name, dataset, train, test)
          tick(bar)
        }
      }

      for (label in names(accuracies))
        accuracies[[label]][[j]] <- mean(acc_cv[[label]])

      save_data(accuracies, paste0(seed, "_part.RData"))
    }
    cat("\nDone!\n")
    accuracies
}

my_penalty_list <- c(0, 0.01, 0.1, 1, 10)

#' Repeat the CV experiment for logistic regression with different penalties
#'
#' \code{cv_lr_experiment}.
#'
#' @param dataset_name a unique string identifying the dataset
#' @param dataset the data set to use
#' @param nsamples how large the training set should be
#' @param times how many times to train and test the models, default 500
#' @param folds number of folds to use, with a -1 meaning leave-one-out
#' @param seed random seed
#' @return a matrix of accuracies, where each row corresponds to a model
#' @export
cv_lr_experiment <-
  function(dataset_name, dataset, nsamples, folds,
           times = 50, penalties = my_penalty_list,
           seed = 2021) {
    accuracies <-
      replicate(length(penalties), rep(NA, times), simplify = FALSE)
    names(accuracies) <- sapply(penalties, as.character)

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
        if (folds < nsamples) {
          cv <-
            # We suppress the warning about pool = 0
            suppressWarnings(vfold_cv(d_subset, folds, strata = class, pool = 0))
        } else {
          cv <- vfold_cv(d_subset, folds)
        }
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
        for (p in penalties) {
          acc_cv[[as.character(p)]][[i]] <-
            logistic_reg_accuracy_with_penalty(
              dataset_name, dataset, train, test, p
            )
          tick(bar)
        }
      }

      for (label in names(accuracies))
        accuracies[[label]][[j]] <- mean(acc_cv[[label]])
    }
    cat("\nDone!\n")
    accuracies
}

##################################################
# Running the experiments
##################################################

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
      cat("Dataset `", name, "` has saved RData file, skipping\n", sep = "")
    }
  }
}

##### CV experiments #####

#' @export
run_cv_experiments <-
  function(datasets = my_dataset_list,
           fold_counts = my_fold_counts,
           times = 50, seed = 2021) {
    if (is.atomic(datasets)) datasets <- list(datasets)
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
            suppressWarnings(cv_experiment(name, tib, size, k,
                                           times = times,
                                           seed  = seed))
          save_data(results, fname)
        } else {
          cat("Dataset `", name, "` with ", k,
              " folds has saved RData file, skipping\n", sep = "")
        }
      }
    }
  }

#' @export
run_lr_experiments <-
  function(datasets = my_dataset_list[c(2, 4)], # chess and mushroom
           fold_counts = my_fold_counts,
           times = 50, penalties = my_penalty_list, seed = 2021) {
    if (is.atomic(datasets)) datasets <- list(datasets)
    for (i in seq_along(datasets)) {
      name <- datasets[[i]]$name
      tib  <- datasets[[i]]$tibble
      size <- datasets[[i]]$sample_size
      for (j in seq_along(fold_counts)) {
        k <- fold_counts[[j]]
        if (k == -1) k <- size
        base  <- paste("cv_lr", name, k, times, seed, sep = "_")
        fname <- paste0(base, ".RData")
        path  <- savepath(fname)
        if (!readable(path)) {
          cat("Testing dataset `", name, "` with ", k, " folds ...\n", sep = "")
          results <-
            suppressWarnings(cv_lr_experiment(name, tib, size, k,
                                              times = times,
                                              penalties = penalties,
                                              seed  = seed))
          save_data(results, fname)
        } else {
          cat("Dataset `", name, "` with ", k,
              " folds has saved RData file, skipping\n", sep = "")
        }
      }
    }
  }
