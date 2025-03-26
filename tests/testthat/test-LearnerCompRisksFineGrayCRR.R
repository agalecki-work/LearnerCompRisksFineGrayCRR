test_that("LearnerCompRisksFineGrayCRR works correctly", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")

  library(mlr3proba)
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

  # Test 2: Numeric predictors with common_tfun = TRUE
  learner_numeric <- LearnerCompRisksFineGrayCRR$new(
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tfun = function(uft, task) matrix(log(uft), ncol = 1, dimnames = list(NULL, "logtime")),
      common_tfun = TRUE
    )
  )
  expect_silent(learner_numeric$train(task, part$train))
  pred_numeric <- learner_numeric$predict(task, part$test)
  expect_true(inherits(pred_numeric, "PredictionCompRisks"))

  # Test 3: Factor variable (should fail)
  learner_factor <- LearnerCompRisksFineGrayCRR$new(
    cov2_info = list(
      cov2nms = c("age", "sex"),
      tfun = function(uft, task) matrix(log(uft), ncol = 1),
      common_tfun = TRUE
    )
  )
  expect_error(
    learner_factor$train(task, part$train),
    "must be numeric"
  )

  # Test 4: Repeats with common_tfun = FALSE
  learner_repeats <- LearnerCompRisksFineGrayCRR$new(
    cov2_info = list(
      cov2nms = c("age", "age"),
      tfun = function(uft, task) matrix(c(log(uft), uft), ncol = 2),
      common_tfun = FALSE
    )
  )
  expect_silent(learner_repeats$train(task, part$train))
  pred_repeats <- learner_repeats$predict(task, part$test)
  expect_true(inherits(pred_repeats, "PredictionCompRisks"))
})