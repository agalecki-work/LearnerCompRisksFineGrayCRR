# File: tests/testthat/test-LearnerCompRisksFineGrayCRR.R

library(testthat)
library(LearnerCompRisksFineGrayCRR)
library(mlr3proba)

test_that("LearnerCompRisksFineGrayCRR works correctly", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")

  task <- tsk("pbc")
  task$set_col_roles(cols = "status", add_to = "stratum")
  task$select(c("age", "sex", "bili"))
  set.seed(123)
  part <- partition(task, ratio = 0.7)

  # Test 1: No cov2_info
  learner <- LearnerCompRisksFineGrayCRR$new()
  expect_silent(learner$train(task, part$train))
  pred <- learner$predict(task, part$test)
  expect_true(inherits(pred, "PredictionCompRisks"))

  # Test 2: Numeric predictors with tf returning a single-column matrix
  learner_numeric <- LearnerCompRisksFineGrayCRR$new(
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft)cbind(log(uft), log(uft + 1))
    )
  )
  expect_silent(learner_numeric$train(task, part$train))
  pred_numeric <- learner_numeric$predict(task, part$test)
  expect_true(inherits(pred_numeric, "PredictionCompRisks"))

  # Test 3: Mixed numeric and factor variables with tf returning a two-column matrix
  learner_mixed <- LearnerCompRisksFineGrayCRR$new(
    cov2_info = list(
      cov2nms = c("age", "sex"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_silent(learner_mixed$train(task, part$train))
  pred_mixed <- learner_mixed$predict(task, part$test)
  expect_true(inherits(pred_mixed, "PredictionCompRisks"))

  # Test 4: Repeats in cov2nms with tf returning a two-column matrix
  learner_repeats <- LearnerCompRisksFineGrayCRR$new(
    cov2_info = list(
      cov2nms = c("age", "age"),
      tf = function(uft) cbind(log(uft), uft)
    )
  )
  expect_silent(learner_repeats$train(task, part$train))
  pred_repeats <- learner_repeats$predict(task, part$test)
  expect_true(inherits(pred_repeats, "PredictionCompRisks"))
})