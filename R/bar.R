#' Make a new progress bar
#'
#' @param n number of ticks
#' @export
new_bar <- function(n) {
  fmt <- "[:bar] :percent (:eta)"
  progress_bar$new(total = n, format = fmt)
}

noop <- function(e) {}

#' Advance the progress bar
#'
#' @param bar the progress bar
#' @export
tick <- function(bar) {
  # Why would a progress bar throw exceptions???
  tryCatch(invisible(bar$tick()), error = noop)
}
