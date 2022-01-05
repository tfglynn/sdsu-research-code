true_mse <- function(fit) {
  coeffs <- fit$coefficients
  beta0 <- coeffs[1]
  beta1 <- coeffs[2]
  beta2 <- coeffs[3]
  beta3 <- coeffs[4]
  if (is.na(beta3)) beta3 <- 0
  beta0^2 + beta1^2 - 2*beta1 + beta2^2 - 4*beta2 + beta3^2 + 6
}

get_train_size <- function(t) 10 + 10 * (t - 1)
get_val_size   <- function(t) 70 + 10 * (t - 1)

my_replication <- function(seed, rows = 27, splits = 100, iters = 1000) {
  # BEWARE!  This takes over 2 hours!
  # Seed used to generate data for paper: 2021
  if (missing(seed))
    stop("You should provide a seed!  (The seed for the paper was 2021.)")
  start <- proc.time()
  results <- tibble(trial = rep(seq_len(rows), each = iters),
                    iter  = rep(seq_len(iters), rows),
                    correct_choice = rep(F, rows * iters),
                    prefer_simpler = rep(F, rows * iters),
                    simpler_better = rep(F, rows * iters),
                    train_size = get_train_size(trial),
                    val_size = get_val_size(trial))
  if (rows * iters == 0) {
    print(proc.time() - start)
    return(results)
  }
  bar <- progress_bar$new(total = rows * iters, show_after = 0,
                          format = "[:bar] :percent (:eta)")
  bar$tick(0)
  set.seed(seed)
  for (r in seq_len(rows)) {
    train_size <- get_train_size(r)
    val_size   <- get_val_size(r)
    data_size  <- train_size + val_size

    for (i in seq_len(iters)) {
      dataset <-
        tibble(n = data_size,
               X1 = rnorm(n),
               X2 = rnorm(n),
               X3 = rnorm(n),
               noise = rnorm(n),
               Y = X1 + 2 * X2 + noise)

      mse1 <- rep(NA, splits)
      mse2 <- rep(NA, splits)

      for (s in seq_len(splits)) {
        train_indices <- sample(1:data_size, train_size)
        train_set <- dataset[ train_indices,]
        val_set   <- dataset[-train_indices,]

        fit1 <- lm(Y ~ X1 + X2     , data = train_set)
        fit2 <- lm(Y ~ X1 + X2 + X3, data = train_set)

        mse1[s] <- mean((predict(fit1, newdata = val_set) - val_set$Y)^2)
        mse2[s] <- mean((predict(fit2, newdata = val_set) - val_set$Y)^2)
      }

      final_fit1 <- lm(Y ~ X1 + X2, data = dataset)
      avg_mse1   <- mean(mse1)
      true_mse1  <- true_mse(final_fit1)

      final_fit2 <- lm(Y ~ X1 + X2 + X3, data = dataset)
      avg_mse2   <- mean(mse2)
      true_mse2  <- true_mse(final_fit2)

      if ((avg_mse1 >= avg_mse2 & true_mse1 >= true_mse2) |
          (avg_mse1 <= avg_mse2 & true_mse1 <= true_mse2))
        results[(r - 1) * iters + i, "correct_choice"] <- T
      if (avg_mse1 <= avg_mse2)
        results[(r - 1) * iters + i, "prefer_simpler"] <- T
      if (true_mse1 <= true_mse2)
        results[(r - 1) * iters + i, "simpler_better"] <- T

      bar$tick()
    }
  }
  print(proc.time() - start)
  results
}
