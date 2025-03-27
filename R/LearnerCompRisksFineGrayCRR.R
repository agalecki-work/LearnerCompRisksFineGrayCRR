#' Fine-Gray Competing Risks Regression Learner
#'
#' @description
#' A learner for Fine-Gray competing risks regression using [cmprsk::crr()] within
#' the `mlr3proba` framework. Models cumulative incidence functions (CIFs) for
#' competing risks, supporting both fixed and time-varying covariates. The learner
#' automatically trains a model for each event type defined in the task, treating
#' the task's censoring code (typically 0) as the baseline.
#'
#' @param cov2_info `list()`\cr Optional list specifying time-varying covariates with:
#'   \describe{
#'     \item{cov2nms}{`character()` Names of numeric covariates in the task.}
#'     \item{tfun}{`function(uft, task)` Transformation function returning a numeric vector
#'       (if `common_tfun = TRUE`) or matrix with `ncol = length(cov2nms)` (if `common_tfun = FALSE`).}
#'     \item{common_tfun}{`logical(1)` Whether to apply a common transformation to all covariates (default: `TRUE`).}
#'   }
#' @section Parameters:
#' \describe{
#'   \item{maxiter}{`integer(1)`\cr Maximum iterations for convergence (default: 100, range: 1 to 1000).}
#' }
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' task <- tsk("pbc")
#' task$select(c("age", "bili", "sex"))
#' learner <- lrn("cmprisk.crr",
#'   cov2_info = list(
#'     cov2nms = c("age", "bili"),
#'     tfun = function(t, task) matrix(log(t), ncol = 1, dimnames = list(NULL, "logtime")),
#'     common_tfun = TRUE
#'   )
#' )
#' learner$train(task)
#' pred <- learner$predict(task)
#' print(pred)
LearnerCompRisksFineGrayCRR <- R6::R6Class("LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this learner.
    #' @param cov2_info See main description for details.
    initialize = function(cov2_info = NULL) {
      if (!is.null(cov2_info)) {
        if (!is.list(cov2_info)) stop("cov2_info must be a list")
        if (!all(c("cov2nms", "tfun") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tfun'")
        }
        if (!is.character(cov2_info$cov2nms) || length(cov2_info$cov2nms) == 0) {
          stop("cov2nms must be a non-empty character vector")
        }
        if (!is.function(cov2_info$tfun)) stop("tfun must be a function")
        cov2_info$common_tfun <- if (is.null(cov2_info$common_tfun)) TRUE else cov2_info$common_tfun
        if (!is.logical(cov2_info$common_tfun)) stop("common_tfun must be logical")
      }
      private$cov2_info <- cov2_info

      ps <- paradox::ps(
        maxiter = paradox::p_int(default = 100L, lower = 1L, upper = 1000L, tags = "train")
      )
      ps$values <- list(maxiter = 100L)

      super$initialize(
        id = "cmprisk.crr",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox"),
        label = "Fine-Gray CRR Model",
        man = "LearnerCompRisksFineGrayCRR::LearnerCompRisksFineGrayCRR"
      )
    }
  ),
  private = list(
    models = NULL,
    event_times = NULL,
    coefficients = NULL,
    cov2 = NULL,
    tf = NULL,
    feature_names = NULL,
    cov2_names = NULL,
    cov2_info = NULL,
    all_event_times = NULL,
    # ... (unchanged .train and .predict methods) ...
    .train = function(task, row_ids = task$row_ids) {
      pv <- self$param_set$get_values(tags = "train")
      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]

      formula <- as.formula(paste("~", paste(features, collapse = " + ")))
      cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
      if (any(is.na(cov1))) stop("NAs detected in cov1")

      if (!is.null(private$cov2_info)) {
        # ... (unchanged cov2 handling) ...
      } else {
        cov2 <- NULL
        tf_final <- NULL
      }
      if (!is.null(cov2) && any(is.na(cov2))) stop("NAs detected in cov2")

      ftime <- as.numeric(full_data[[time_col]])
      private$all_event_times <- sort(unique(ftime))

      models <- list()
      event_times <- list()
      coefficients <- list()
      event_levels <- as.character(task$cmp_events)

      for (target_event in event_levels) {
        fstatus <- as.numeric(full_data[[event_col]])
        model <- tryCatch({
          if (is.null(cov2)) {
            cmprsk::crr(ftime, fstatus, cov1, failcode = as.integer(target_event), cencode = 0L)
          } else {
            cmprsk::crr(ftime, fstatus, cov1, cov2, tf_final, failcode = as.integer(target_event), cencode = 0L)
          }
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s", target_event, e$message))
        })
        event_times[[target_event]] <- model$uftime
        coefficients[[target_event]] <- model$coef
        models[[target_event]] <- model
      }

      private$models <- models
      private$event_times <- event_times
      private$cov2 <- cov2
      private$tf <- tf_final
      private$feature_names <- features
      private$cov2_names <- if (!is.null(cov2)) colnames(cov2) else NULL
      invisible(self)
    },
    .predict = function(task, row_ids = task$row_ids) {
      # ... (unchanged predict method) ...
    }
  )
)