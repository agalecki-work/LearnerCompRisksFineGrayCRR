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
  learner <- lrn("cmprsk.crr")
  expect_true({learner$train(task, part$train); TRUE})
  pred <- learner$predict(task, part$test)
  expect_true(inherits(pred, "PredictionCompRisks"))

  # Test 2: Numeric predictors with tf returning a two-column matrix
  learner_numeric <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_true({learner_numeric$train(task, part$train); TRUE})
  pred_numeric <- learner_numeric$predict(task, part$test)
  expect_true(inherits(pred_numeric, "PredictionCompRisks"))

  # Test 3: Mixed numeric and factor variables with tf returning a two-column matrix
  learner_mixed <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "sex"),
      tf = function(uft) cbind(log(uft), log(uft + 1))
    )
  )
  expect_true({learner_mixed$train(task, part$train); TRUE})
  pred_mixed <- learner_mixed$predict(task, part$test)
  expect_true(inherits(pred_mixed, "PredictionCompRisks"))

  # Test 4: Repeats in cov2nms with tf returning a two-column matrix
  learner_repeats <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "age"),
      tf = function(uft) cbind(log(uft), uft)
    )
  )
  expect_true({learner_repeats$train(task, part$train); TRUE})
  pred_repeats <- learner_repeats$predict(task, part$test)
  expect_true(inherits(pred_repeats, "PredictionCompRisks"))

  # Test 5: cov2only with bili as time-varying only
  learner_cov2only <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) cbind(log(uft), uft),
      cov2only = c("bili")
    )
  )
  expect_true({learner_cov2only$train(task, part$train); TRUE})
  pred_cov2only <- learner_cov2only$predict(task, part$test)
  expect_true(inherits(pred_cov2only, "PredictionCompRisks"))
  expect_true(is.list(learner_cov2only$state$model), "state$model should be a list")
  expect_true(inherits(learner_cov2only$state$model[[1]], "crr"), "state$model[[1]] should be a crr object")
  coef_names <- names(learner_cov2only$state$model[[1]]$coef)
  cov1_names <- coef_names[!grepl("\\*", coef_names)]
  cov2_names <- coef_names[grepl("\\*", coef_names)]
  expect_false("bili" %in% cov1_names, "bili should not be in cov1 with cov2only")
  expect_true(any(grepl("bili", cov2_names)), "bili should be in cov2 effects")
})