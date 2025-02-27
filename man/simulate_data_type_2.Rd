% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_datasets.R
\name{simulate_data_type_2}
\alias{simulate_data_type_2}
\title{Simulate a dataset (type 2)}
\usage{
simulate_data_type_2(n, d = 1, bayes_error = 0.1)
}
\description{
These datasets are just like type 1, but have a discrete variable ("bit")
that flips the classes.  For example, if the class would normally be TRUE,
and the bit is 1, then the class is FALSE, and vice versa.
}
