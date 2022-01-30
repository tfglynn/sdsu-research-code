d <- chess_dataset
misclass_loss <- function(y1, y2, funcs_params = NA) y1 != y2

fitter_naive_bayes <-
  function(X, Y, idx = NA, funcs_params) {
    if (any(is.na(idx))) idx <- 1:nrow(X)
    classes <- funcs_params$classes
    rf_spec <- rand_forest(mode = "classification")
    XY <- cbind(X[idx, ], class = Y[idx])
    naive_bayes(class ~ ., XY, laplace = 1)
  }

fitter_rand_forest <-
  function(X, Y, idx = NA, funcs_params) {
    if (any(is.na(idx))) idx <- 1:nrow(X)
    classes <- funcs_params$classes
    rf_spec <- rand_forest(mode = "classification")
    XY <- cbind(X[idx, ], class = Y[idx])
    fit(rf_spec, class ~ ., XY)
  }

predictor_naive_bayes <-
  function(model, X_new, funcs_params = NA) {
    predict(model, X_new)
  }

predictor_rand_forest <-
  function(model, X_new, funcs_params = NA) {
    predict(model, X_new)$.pred_class
  }

naive_bayes_fns <-
  list(fitter = fitter_rand_forest,
       predictor = predictor_rand_forest,
       loss = misclass_loss,
       name = "naive_bayes")

rand_forest_fns <-
  list(fitter = fitter_rand_forest,
       predictor = predictor_rand_forest,
       loss = misclass_loss,
       name = "rand_forest")

ncv_experiment <- function(datasets = my_dataset_list, reps = 50, seed = 2021) {
  bar <- new_bar(2 * length(datasets) * reps)
  set.seed(seed)
  for (i in seq_along(datasets)) {
    results_list <-
      list(rand_forest = replicate(reps, NA, simplify = FALSE),
           naive_bayes = replicate(reps, NA, simplify = FALSE))
    d <- datasets[[i]]
    name <- d$name
    tib <- d$tibble
    X <- select(tib, -class)
    Y <- tib$class

    cat(paste("\nUsing dataset", name, "...\n"))
    for (j in seq_len(reps)) {
      train_indices <- sample(1:nrow(tib), d$sample_size)
      X_train <- X[ train_indices, ]
      X_test  <- X[-train_indices, ]
      Y_train <- Y[ train_indices]
      Y_test  <- Y[-train_indices]

      results_list$rand_forest[[j]] <-
        nested_cv(X_train, Y_train, rand_forest_fns, n_folds = 5, reps = 20,
                  funcs_params = list(classes = levels(Y)))
      model <- fitter_rand_forest(X_train, Y_train,
                                  funcs_params = list(classes = levels(Y)))
      results_list$rand_forest[[j]]$actual <-
        1 - mean(predictor_rand_forest(model, X_test) == Y_test)
      tick(bar)

      results_list$naive_bayes[[j]] <-
        nested_cv(X_train, Y_train, naive_bayes_fns, n_folds = 5, reps = 20,
                  funcs_params = list(classes = levels(Y)))
      model <- fitter_naive_bayes(X_train, Y_train,
                                  funcs_params = list(classes = levels(Y)))
      results_list$naive_bayes[[j]]$actual <-
        1 - mean(predictor_naive_bayes(model, X_test) == Y_test)
      tick(bar)
    }

    save_data(results_list$rand_forest,
              paste0("ncv_rand_forest_", name, "_", seed, ".RData"))
    save_data(results_list$naive_bayes,
              paste0("ncv_naive_bayes_", name, "_", seed, ".RData"))
  }
}
