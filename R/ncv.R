#d <- cancer_dataset
#
#X <- select(d, -class)
#Y <- d$class
#
#misclass_loss <- function(y1, y2, funcs_params = NA) y1 != y2
#
#fitter_rand_forest <-
#  function(X, Y, idx = NA, funcs_params) {
#    if (any(is.na(idx))) idx <- 1:nrow(X)
#    classes <- funcs_params$classes
#    rf_spec <- rand_forest(mode = "classification")
#    XY <- cbind(X[idx, ], class = Y[idx])
#    fit(rf_spec, class ~ ., XY)
#  }
#
#predictor_rand_forest <-
#  function(model, X_new, funcs_params = NA) {
#    predict(model, X_new)$.pred_class
#  }
#
#rand_forest_fns <-
#  list(fitter = fitter_rand_forest,
#       predictor = predictor_rand_forest,
#       loss = misclass_loss,
#       name = "rand_forest")
#
#nested_cv_results <-
#  nested_cv(X, Y, rand_forest_fns, n_folds = 5, reps = 10,
#            funcs_params = list(classes = levels(d$class)),
#            verbose = TRUE)
#
#naive_cv_results <-
#  naive_cv(X, Y, rand_forest_fns, n_folds = 5,
#            funcs_params = list(classes = levels(d$class)))
#
#print(paste0("Nested CV interval: (", 
#             1-nested_cv_results$ci_lo, ", ",
#             1-nested_cv_results$ci_hi, ")"))
#print(paste0("Standard CV interval: (", 
#             1-naive_cv_results$ci_lo, ", ",
#             1-naive_cv_results$ci_hi, ")"))
