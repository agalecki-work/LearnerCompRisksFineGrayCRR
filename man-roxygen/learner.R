#' @param id `character(1)` \cr Identifier for the learner, e.g., \code{[mlr3]{mlr_learners}[[<id>]]}.
#' @param caller `character(1)` \cr Underlying function used for training the learner, e.g., \code{[cmprsk]{crr}}.
#' @param predict_method `character(1)` \cr Function used for making predictions, e.g., \code{[cmprsk]{predict.crr}}.
#'
#' @description
#' The learner uses the \code{[cmprsk]{crr}} function from the `cmprsk` package to fit a Fine-Gray competing risks regression model. This model estimates cumulative incidence functions (CIFs) for competing risks scenarios with multiple mutually exclusive events, supporting both fixed and time-varying covariates. The \code{[cmprsk]{predict.crr}} function is used to generate predictions, providing CIFs for specified event types across unique event times from the training data.
