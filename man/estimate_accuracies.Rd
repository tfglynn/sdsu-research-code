% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kohavi.R
\name{estimate_accuracies}
\alias{estimate_accuracies}
\title{Estimate the accuracy of models on a dataset}
\usage{
estimate_accuracies(
  dataset_name,
  dataset,
  nsamples,
  times = 500,
  models = my_model_list,
  seed = 2021
)
}
\arguments{
\item{dataset_name}{a unique string identifying the dataset}

\item{dataset}{the data set to use}

\item{nsamples}{how large the training set should be}

\item{times}{how many times to train and test the models, default 500}

\item{models}{the models to train}

\item{seed}{random seed}
}
\value{
a matrix of accuracies, where each row corresponds to a model
}
\description{
\code{estimate_accuracies} repeatedly trains and evaluates models on a given
dataset.
}
