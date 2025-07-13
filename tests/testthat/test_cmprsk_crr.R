library(testthat)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)  # Explicitly load the package
source("helper.R")


test_that("Task configuration is correct", {
  skip_if_not_installed("mlr3proba")
  setup <- setup_task()
  task <- setup$task
  expect_equal(task$target_names, c("time", "status"))
  expect_equal(task$col_roles$stratum, "status")
  expect_true(is.integer(task$data(cols = "status")$status))
})

test_that("Class and ID checks", {
  skip_if_not_installed("mlr3proba")
  learner <- lrn("cmprsk.crr")
  expect_s3_class(learner, "LearnerCompRisks")
  expect_true(exists("new", envir = mlr3proba::LearnerCompRisks))
  expect_equal(learner$id, "cmprsk.crr")
  expect_equal(learner$predict_types, c("cif"))
  expect_true(all(c("importance", "missings") %in% learner$properties))
})

test_that("Training and prediction with no cov2_info", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner <- lrn("cmprsk.crr")
  expect_silent(learner$train(task, part$train))
  expect_true(exists("PredictionCompRisks",
                     envir = asNamespace("mlr3proba"),
                     inherits = FALSE))
  pred <- learner$predict(task, part$test)
  expect_s3_class(pred, "PredictionCompRisks")
  expect_equal(names(pred$cif), as.character(task$cmp_events))
})

test_that("Numeric predictors with two-column tf matrix", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_numeric <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_silent(learner_numeric$train(task, part$train))
  pred_numeric <- learner_numeric$predict(task, part$test)
  expect_s3_class(pred_numeric, "PredictionCompRisks")
  expect_equal(names(pred_numeric$cif), as.character(task$cmp_events))
})

test_that("Mixed numeric and factor variables with two-column tf matrix", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_mixed <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "sex"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_silent(learner_mixed$train(task, part$train))
  pred_mixed <- learner_mixed$predict(task, part$test)
  expect_s3_class(pred_mixed, "PredictionCompRisks")
  expect_equal(names(pred_mixed$cif), as.character(task$cmp_events))
})

test_that("Repeats in cov2nms with two-column tf matrix", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_repeats <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "age"),
      tf = function(uft) cbind(log(uft), uft)
    )
  )
  expect_silent(learner_repeats$train(task, part$train))
  pred_repeats <- learner_repeats$predict(task, part$test)
  expect_s3_class(pred_repeats, "PredictionCompRisks")
  expect_equal(names(pred_repeats$cif), as.character(task$cmp_events))
})

test_that("cov2only with bili as time-varying only", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_cov2only <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) cbind(log(uft), uft),
      cov2only = c("bili")
    )
  )
  expect_silent(learner_cov2only$train(task, part$train))
  pred_cov2only <- learner_cov2only$predict(task, part$test)
  expect_s3_class(pred_cov2only, "PredictionCompRisks")
  expect_type(learner_cov2only$state$model, "list")
  expect_length(learner_cov2only$state$model, length(task$cmp_events))
  expect_s3_class(learner_cov2only$state$model[[1]], "crr")
  coef_names <- names(learner_cov2only$state$model[[1]]$coef)
  cov1_names <- coef_names[!grepl("\\*", coef_names)]
  cov2_names <- coef_names[grepl("\\*", coef_names)]
  expect_false("bili" %in% cov1_names, "bili not in cov1 with cov2only")
  expect_true(any(grepl("bili", cov2_names)), "bili should be in cov2 effects")
})

test_that("Convergence method", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner <- lrn("cmprsk.crr")
  expect_silent(learner$train(task, part$train))
  expect_type(learner$state$model, "list")
  expect_length(learner$state$model, length(task$cmp_events))
  expect_true(all(sapply(learner$convergence(), is.logical)))
})

test_that("Importance method", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner <- lrn("cmprsk.crr")
  expect_silent(learner$train(task, part$train))
  imp <- learner$importance()
  expect_s3_class(imp, "data.frame")
  expect_equal(ncol(imp), 3)
  expect_true(all(c("variable", "importance", "event") %in% colnames(imp)))
  expect_true(all(imp$importance >= 0 & imp$importance <= 1))
})

test_that("Single predictor", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  task_single <- task$clone()
  task_single$select(c("age"))
  learner_single <- lrn("cmprsk.crr")
  expect_silent(learner_single$train(task_single, part$train))
  pred_single <- learner_single$predict(task_single, part$test)
  expect_s3_class(pred_single, "PredictionCompRisks")
})

test_that("No features", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  task_nofeat <- task$clone()
  task_nofeat$select(character(0))
  learner_nofeat <- lrn("cmprsk.crr")
  expect_error(learner_nofeat$train(task_nofeat, part$train),
               "system is exactly singular")
})

test_that("Invalid cov2_info (non-existent feature)", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_invalid <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("invalid"),
      tf = function(uft) log(uft)
    )
  )
  expect_error(learner_invalid$train(task, part$train),
               "cov2nms element 'invalid' not in task features")
})

test_that("Mismatched tf output dimensions", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_mismatch <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) matrix(log(uft), ncol = 1)
    )
  )
  expect_error(learner_mismatch$train(task, part$train),
               "tf must return a matrix with")
})

test_that("Empty task", {
  skip_if_not_installed("mlr3proba")
  setup <- setup_task()
  task <- setup$task
  task_empty <- task$clone()
  expect_error(task_empty$filter(integer(0)), "competing event\\(s\\)")
})

test_that("Parameter validation", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner <- lrn("cmprsk.crr")
  expected_params <- c("maxiter", "gtol", "parallel", "cov2_info")
  expect_true(all(expected_params %in% learner$param_set$ids()))
  expect_silent(learner$param_set$assert(learner$param_set$values))
  learner$param_set$values$maxiter <- 50L
  learner$param_set$values$gtol <- 1e-7
  learner$param_set$values$parallel <- FALSE
  learner$param_set$values$cov2_info <- list(
    cov2nms = c("age", "bili"),
    tf = function(uft) cbind(log(uft), log(uft + 1)),
    cov2only = NULL
  )
  expect_silent(learner$param_set$assert(learner$param_set$values))
  expect_silent(learner$train(task, part$train))
  pred <- learner$predict(task, part$test)
  expect_s3_class(pred, "PredictionCompRisks")
})

test_that("Parallel execution", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("future.apply")
  setup <- setup_task()
  task <- setup$task
  part <- setup$part
  learner_parallel <- lrn("cmprsk.crr", parallel = TRUE)
  expect_silent(learner_parallel$train(task, part$train))
  pred_parallel <- learner_parallel$predict(task, part$test)
  expect_s3_class(pred_parallel, "PredictionCompRisks")
})
