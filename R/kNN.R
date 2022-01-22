knn_spec <-
  nearest_neighbor(mode = "classification", weight_func = "rectangular")
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

#' Investigate how quickly a k-nearest-neighbor classifier reaches capacity
#'
#' @param dims the number of dimensions used to determine the class
#' @param be the Bayes optimal classifier error
#' @param reps the number of times to repeat the experiment
#' @param seed random seed
#' @return a list where the nth item is a vector of accuracies of models given
#'         n * 500 training examples
#' @export
nn_experiment <- function(dims, be, reps = 50, seed = 2021) {
  set.seed(seed)
  holdout <- simulate_data_type_1(10000, dims, be)
  additions <- 6
  accs <- replicate(additions, rep(NA, reps), simplify = FALSE)
  bar <- new_bar(additions * reps)
  for (i in 1:reps) {
    train <- NULL
    for (j in 1:additions) {
      train <- rbind(train, simulate_data_type_1(500, dims, be))
      accs[[j]][[i]] <- nearest_neighbor_accuracy("sim", train, train, holdout)
      tick(bar)
    }
  }
  accs
}

#' Repeat the k-nearest-neighbor experiment with various parameters
#' @export
run_nn_experiment <-
  function(dims = 1:3, bayes_errors = c(0, 0.1, 0.2), seed = 2021) {
    accs <- replicate(9, NA, simplify = FALSE)
    test_dims <- 1:3
    test_be   <- c(0, 0.1, 0.2)
    for (i in seq_along(test_dims)) {
      for (j in seq_along(test_be)) {
        k <- (i - 1) * 3 + j
        accs[[k]] <- nn_experiment(test_dims[[i]], test_be[[j]])
      }
    }
    accs
  }
