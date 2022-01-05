# kNN test proof of concept
# We generate normally distributed values of x,
# each labeled with 1 with probability Phi(x),
# and 0 otherwise.

set.seed(2021)
size <- 1e2 
train <- tibble(X = rnorm(size), Y = rbinom(size, 1, pnorm(X)))

# \int_{-\infty}^0 \phi(x)\Phi(x) dx +
# \int_0^\infty \phi(x)(1 - \Phi(x)) dx
err_Bayes <- 0.25

plurality <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

kNN <- function(test, k, train) {
  rows <- nrow(test)
  guesses <- rep(NA, rows)
  for (i in seq_len(rows)) {
    votes <-
      train %>%
      arrange(abs(X - test$X[[i]])) %>%
      head(k) %>%
      `$`(Y)
    guesses[[i]] <- plurality(votes)
  }
  guesses
}

test <- tibble(X = rnorm(50), Y = rbinom(50, 1, pnorm(X)))
