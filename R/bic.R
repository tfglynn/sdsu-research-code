#' Count order inversions
#'
#' This function uses an insertion sort to count the number of transpositions
#' necessary to sort the list \code{xs}.
#'
#' @export
inversions <- function(xs, decreasing = FALSE) {
  cmp <- ifelse(decreasing, `>=`, `<=`)
  N <- length(xs)
  if (N == 0) return(0)
  total <- 0
  for (i in 1:(N-1)) {
    if (cmp(xs[[i]], xs[[i+1]])) next
    tmp <- xs[[i+1]]
    cur <- i
    total <- total + 1
    while (cur > 1) {
      if (cmp(xs[[cur - 1]], tmp)) break
      total <- total + 1
      cur <- cur - 1
    }
    for (k in i:cur)
      xs[[k+1]] <- xs[[k]]
    xs[[cur]] <- tmp
  }
  total
}

###################################################
# Experiments
###################################################

accuracy <- function(model, test) {
  classes <- levels(test$class)
  probs <- predict(model, newdata = test)
  predictions <- if_else(probs >= 0, classes[[2]], classes[[1]])
  mean(predictions == test$class)
}

run_bic_experiment <- function(datasets = my_dataset_list,
                               reps = 50, seed = 2021) {
  if (is.atomic(datasets)) datasets <- list(datasets)
  results <-
    replicate(length(datasets),
              tibble(# How do BIC and BICc rank the "best" (accuracy) model?
                     BIC_best_rank  = rep(NA, reps),
                     BICc_best_rank = rep(NA, reps),
                     # How sorted are the model accuracies when ranked by BIC(c)?
                     BIC_inversions  = rep(NA, reps),
                     BICc_inversions = rep(NA, reps),
                     # How does the "best" (BIC/BICc) model rank in accuracy?
                     acc_best_BIC  = rep(NA, reps),
                     acc_best_BICc = rep(NA, reps),
                     # What is the (dis)advantage of using BICc for stepwise?
                     step_BICc_advantage = rep(NA, reps)),
              simplify = FALSE)

  set.seed(seed)
  for (i in seq_along(datasets)) {
    name <- datasets[[i]]$name
    fname <- paste0("bic", "_", name, "_", reps, "_", seed, ".RData")
    if (readable(savepath(fname))) {
      cat("Dataset `", name, "` has saved BIC RData file, skipping\n", sep = "")
      next
    } else {
      cat("Testing dataset `", name, "` ...\n")
    }
    bar <- new_bar(reps)
    tib <- datasets[[i]]$tibble
    rec <-
      recipe(class ~ ., tib) %>%
      step_dummy(all_nominal_predictors()) %>%
      prep
    baked <- bake(rec, tib)
    predictors <- setdiff(names(baked), "class")
    trunc_predictors <- predictors[1:9] # 512 possible models
    models <- replicate(2 ^ length(trunc_predictors), NA, simplify = FALSE)
    combos <-
      replicate(length(trunc_predictors), c(TRUE, FALSE), simplify = FALSE) %>%
      expand.grid %>%
      as.matrix

    for (j in seq_len(reps)) {
      rows <- sample(1:nrow(tib), datasets[[i]]$sample_size)
      train <- bake(rec, tib[ rows, ])
      test  <- bake(rec, tib[-rows, ])

      for (k in seq_along(models)) {
        using  <- combos[k, ]
        chosen <- trunc_predictors[which(using)]
        if (length(chosen) == 0) {
          form <- class ~ 1
        } else {
          form <- reformulate(chosen, "class")
        }
        models[[k]] <- glm(form, data = train, family = binomial())
      }

      BICcs <- sapply(models, . %>% { AIC(., k = log(nobs(.) / (2 * pi))) })
      BICs  <- sapply(models, BIC)
      accs  <- sapply(models, . %>% { accuracy(., test) })

      acc_order  <- order(accs, decreasing = TRUE)
      BIC_order  <- order(BICs)
      BICc_order <- order(BICcs)
      best <- first(acc_order)

      results[[i]][j, "BIC_inversions"] <-
        inversions(accs[BIC_order ], decreasing = TRUE)
      results[[i]][j, "BICc_inversions"] <-
        inversions(accs[BICc_order], decreasing = TRUE)

      results[[i]][j, "BIC_best_rank" ] <- which(BIC_order  == best)
      results[[i]][j, "BICc_best_rank"] <- which(BICc_order == best)

      results[[i]][j, "acc_best_BIC" ] <- which(acc_order == first(BIC_order))
      results[[i]][j, "acc_best_BICc"] <- which(acc_order == first(BICc_order))

      full  <- suppressWarnings(glm(form, data = train, family = binomial()))
      scope <- list(upper = class ~ ., lower = class ~ 1)
      mBIC <-
        stats::step(full, scope, k = log(nobs(full)), trace = 0)
      mBICc <-
        stats::step(full, scope, k = log(nobs(full) / (2 * pi)), trace = 0)

      different <- !identical(formula(mBIC), formula(mBICc))
      if (different) {
        predBIC <-
          if_else(predict(mBIC, newdata = test) >= 0,
                  classes[[1]], classes[[2]])
        accBIC <- mean(predBIC == test$class)

        predBICc <-
          if_else(predict(mBICc, newdata = test) >= 0,
                  classes[[1]], classes[[2]])
        accBICc <- mean(predBICc == test$class)

        results[[i]][j, "step_BICc_advantage"] <- accBICc - accBIC
      }
      tick(bar)
    }
    save_data(results, fname)
    cat("\nDone!\n")
  }
  results
}
