##################################################
# Simulated data
##################################################

# Here we generate a random plane in R^4 embedded
# in R^8, use it to generate two classes of points,
# then use logistic regression to try to recover
# the original plane.
# We use BIC and BICc to compare models.

# The idea now is to see how out of order accs[order(BICs)]
# is, where we want a perfect descending order.
# We do this by counting inversions.
# Since we don't have many list items and they're already
# mostly sorted, we use an algorithm inspired by insertion
# sort to do the counting.

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
# E.g. inversions(accs[order(BICs)], decreasing = TRUE)

#full_dim <- 30
#true_dim <- 20
#train_size <- 100
#test_size  <- 1000
#N <- train_size + test_size
#
#normalize <- function(x) x / sqrt(sum(x ^ 2))
#logit_inv <- function(l) exp(l) / (1 + exp(l))
#
#plane <-
#  normalize(c(rnorm(true_dim),
#              rep(0, full_dim - true_dim)))
#xs <-
#  matrix(rnorm(N * full_dim),
#         dimnames = list(NULL, paste0("x", 1:full_dim)),
#         ncol = full_dim)
#inner_products <- xs %*% plane
#label <- rbinom(N, 1, logit_inv(inner_products))
#
#dataset <- cbind(xs, label) %>% as_tibble
#train <- dataset[1:train_size, ]
#test  <- dataset[(train_size + 1):N, ]

#models <-
#  lapply(1:full_dim,
#         . %>% {
#           glm(reformulate(head(colnames(xs), .),
#                           response = label),
#               data = cbind(xs, label) %>% as_tibble,
#               family = binomial())
#         })
#
#accuracy <- function(model, test) {
#  predictions <- predict(model, newdata = test) >= 0
#  mean(predictions == test$class)
#}
#
#BICcs <-
#  sapply(models,
#         . %>% { AIC(., k = log(nobs(.) / (2 * pi))) })
#BICs <- sapply(models, BIC)
#accs <- sapply(models, . %>% { accuracy(., test) })
#list(BICc = inversions(accs[order(BICcs)], decreasing = TRUE),
#     BIC  = inversions(accs[order(BICs)], decreasing = TRUE))

###################################################
# Real data
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
      mBIC  <- stats::step(full, scope, k = log(nobs(full)), trace = 0)
      mBICc <- stats::step(full, scope, k = log(nobs(full) / (2 * pi)), trace = 0)

      different <- !identical(formula(mBIC), formula(mBICc))
      if (different) {
        predBIC <-
          if_else(predict(mBIC, newdata = test) >= 0, classes[[1]], classes[[2]])
        accBIC <- mean(predBIC == test$class)

        predBICc <-
          if_else(predict(mBICc, newdata = test) >= 0, classes[[1]], classes[[2]])
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
