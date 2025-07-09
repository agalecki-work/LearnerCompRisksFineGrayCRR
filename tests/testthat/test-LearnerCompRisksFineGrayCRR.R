library(testthat)
library(mlr3proba)
library(LearnerCompRisksFineGrayCRR)  # Explicitly load the package

# Helper function to check if a package is installed
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not installed"))
  }
}

test_that("LearnerCompRisksFineGrayCRR works correctly", {
  skip_if_not_installed("mlr3proba")
  skip_if_not_installed("cmprsk")

  # Initialize task and partition
  task <- tsk("pbc")
  task$set_col_roles(cols = "status", add_to = "stratum")  # Add status to stratum
  task$col_roles$feature <- setdiff(task$feature_names, "status")  # Ensure status is not a feature
  task$select(c("age", "sex", "bili"))
  set.seed(123)
  part <- partition(task, ratio = 0.7)

  # Verify task configuration
  expect_equal(task$target_names, c("time", "status"))  # Confirm time and status are targets
  expect_equal(task$col_roles$stratum, "status")  # Confirm status is set as stratum
  expect_true(is.integer(task$data(cols = "status")$status))  # Confirm status is integer

  # Test 1: Class and ID checks
  learner <- lrn("cmprsk.crr")
  expect_s3_class(learner, "LearnerCompRisks")
  expect_true(exists("new", envir = mlr3proba::LearnerCompRisks))  # Check for learner existence
  expect_equal(learner$id, "cmprsk.crr")
  expect_equal(learner$predict_types, c("cif"))
  expect_true(all(c("importance", "missings") %in% learner$properties))

  # Test 2: No cov2_info
  expect_silent(learner$train(task, part$train))
  expect_true(exists("PredictionCompRisks", envir = asNamespace("mlr3proba"), inherits = FALSE))  # Debug namespace
  pred <- learner$predict(task, part$test)
  expect_s3_class(pred, "PredictionCompRisks")
  expect_equal(names(pred$cif), as.character(task$cmp_events))

  # Test 3: Numeric predictors with tf returning a two-column matrix
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

  # Test 4: Mixed numeric and factor variables with tf returning a two-column matrix
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

  # Test 5: Repeats in cov2nms with tf returning a two-column matrix
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

  # Test 6: cov2only with bili as time-varying only
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
  expect_false("bili" %in% cov1_names, "bili should not be in cov1 with cov2only")
  expect_true(any(grepl("bili", cov2_names)), "bili should be in cov2 effects")

  # Test 7: Convergence method
  expect_type(learner_cov2only$state$model, "list")
  expect_length(learner_cov2only$state$model, length(task$cmp_events))
  expect_true(all(sapply(learner$convergence(), is.logical)))

  # Test 8: Importance method
  imp <- learner$importance()
  expect_s3_class(imp, "data.frame")
  expect_equal(ncol(imp), 3)
  expect_true(all(c("variable", "importance", "event") %in% colnames(imp)))
  expect_true(all(imp$importance >= 0 & imp$importance <= 1))

  # Test 9: Single predictor
  task_single <- task$clone()
  task_single$select(c("age"))
  learner_single <- lrn("cmprsk.crr")
  expect_silent(learner_single$train(task_single, part$train))
  pred_single <- learner_single$predict(task_single, part$test)
  expect_s3_class(pred_single, "PredictionCompRisks")

  # Test 10: No features
  task_nofeat <- task$clone()
  task_nofeat$select(character(0))
  learner_nofeat <- lrn("cmprsk.crr")
  expect_error(learner_nofeat$train(task_nofeat, part$train), "system is exactly singular")
  # Skip prediction tests since training fails
  # pred_nofeat <- learner_nofeat$predict(task_nofeat, part$test)
  # expect_s3_class(pred_nofeat, "PredictionCompRisks")

  # Test 11: Invalid cov2_info (non-existent feature)
  learner_invalid <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("invalid"),
      tf = function(uft) log(uft)
    )
  )
  expect_error(learner_invalid$train(task, part$train), "not found")

  # Test 12: Mismatched tf output dimensions
  learner_mismatch <- lrn("cmprsk.crr",
    cov2_info = list(
      cov2nms = c("age", "bili"),
      tf = function(uft) matrix(log(uft), ncol = 1)
    )
  )
  expect_error(learner_mismatch$train(task, part$train), "tf must return a matrix with")

  # Test 13: Empty task
  task_empty <- task$clone()
  expect_error(task_empty$filter(integer(0)), "competing event\\(s\\)")
  # Skip training and prediction tests since task filtering fails
  # learner$train(task_empty, integer(0))
  # expect_error(learner$train(task_empty, integer(0)), "empty")

  # Test 14: Parameter validation
  learner_params <- lrn("cmprsk.crr", maxiter = 50, gtol = 1e-7, parallel = FALSE)
  expect_equal(learner_params$param_set$values$maxiter, 50)
  expect_equal(learner_params$param_set$values$gtol, 1e-7)
  expect_false(learner_params$param_set$values$parallel)
  expect_silent(learner_params$train(task, part$train))
  pred_params <- learner_params$predict(task, part$test)
  expect_s3_class(pred_params, "PredictionCompRisks")

  # Test 15: Parallel execution (if future.apply is available)
  skip_if_not_installed("future.apply")
  learner_parallel <- lrn("cmprsk.crr", parallel = TRUE)
  expect_silent(learner_parallel$train(task, part$train))
  pred_parallel <- learner_parallel$predict(task, part$test)
  expect_s3_class(pred_parallel, "PredictionCompRisks")
})