#' @templateVar title Fine-Gray Competing Risks
#' @templateVar fullname LearnerCompRisksFineGrayCRR
#' @templateVar caller [cmprsk::crr()]
#' @templateVar predict_method by [cmprsk::predict.crr()]
#' @templateVar id cmprsk.crr
#' @template cmprsk_learner
#'
#' @description
#' A learner for Fine-Gray competing risks regression, implemented via [cmprsk::crr()]
#' within the mlr3proba framework. This learner estimates cumulative incidence functions
#' (CIFs) for competing risks scenarios with multiple mutually exclusive events.
#' It supports fixed and time-varying covariates through a user-defined transformation
#' function. Models are fitted for each event type specified in the task's `cmp_events`,
#' with censoring coded as 0 in the task's `status` column. Predictions provide CIFs
#' for all event types across unique event times from training data.
#'
#' @param cov2_info `list()` \cr
#' Optional configuration for time-varying covariates. If `NULL` (default), all covariates
#' are treated as fixed. When provided, the list must contain:
#' \describe{
#'   \item{cov2nms}{`character()` \cr
#'     Names of covariates from the task's feature set to treat as time-varying.}
#'   \item{tf}{`function(uft)` \cr
#'     Function specifying time-varying effects, taking a numeric vector `uft` (unique
#'     failure times) and returning a matrix with `nrow = length(uft)` and `ncol`
#'     matching the columns of the `cov2` matrix derived from `cov2nms`. Example:
#'     `function(uft) log(uft)`.}
#'   \item{cov2only}{`character()` or `NULL` \cr
#'     Covariate names used only for `cov2` and excluded from fixed covariates (`cov1`).
#'     Must be a subset of `cov2nms`. If `NULL` (default), all task features contribute
#'     to `cov1`.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`convergence()`}{Returns a named list indicating convergence status for each
#'     event model (TRUE/FALSE).}
#'   \item{`importance()`}{Returns a data frame with coefficient-based variable importance
#'     for each event, scaled by absolute coefficient sums.}
#' }
#'
# @references
# `r format_bib("fine_1999")`
#' @references Fine JP and Gray RJ (1999). A proportional hazards model for the subdistribution of a competing risk.
#' Journal of the American Statistical Association, 94(446), 496-509.
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' task <- tsk("pbc")
#' task$select(c("age", "bili", "sex"))
#' learner <- lrn("cmprsk.crr",
#'   cov2_info = list(
#'     cov2nms = c("age", "sex"),
#'     tf = function(uft) cbind(log(uft), log(uft + 1)),
#'     cov2only = "sex"
#'   ),
#'   parallel = FALSE,
#'   maxiter = 100,
#'   gtol = 1e-6
#' )
#' learner$train(task)
#' pred <- learner$predict(task)
#' print(pred)
#' learner$convergence()
#' learner$importance()
LearnerCompRisksFineGrayCRR <- R6::R6Class("LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param cov2_info `list()` \cr
    #' Optional configuration for time-varying covariates. See class description for details.
    initialize = function(cov2_info = NULL) {
      if (!is.null(cov2_info)) {
        if (!is.list(cov2_info)) stop("cov2_info must be a list")
        if (!all(c("cov2nms", "tf") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tf'")
        }
        if (!is.character(cov2_info$cov2nms) || length(cov2_info$cov2nms) == 0) {
          stop("cov2nms must be a non-empty character vector")
        }
        if (!is.function(cov2_info$tf)) stop("tf must be a function")
        if (is.null(cov2_info$cov2only)) {
          cov2_info$cov2only <- NULL
        } else {
          if (!is.character(cov2_info$cov2only)) stop("cov2only must be a character vector or NULL")
          if (!all(cov2_info$cov2only %in% cov2_info$cov2nms)) {
            stop("cov2only must be a subset of cov2nms")
          }
        }
      }
      private$cov2_info <- cov2_info

      ps <- paradox::ps(
        maxiter = paradox::p_int(default = 100L, lower = 1L, upper = 1000L, tags = "train"),
        gtol = paradox::p_dbl(default = 1e-6, lower = 1e-9, upper = 1e-3, tags = "train"),
        parallel = paradox::p_lgl(default = FALSE, tags = "train")
      )
      ps$values <- list(maxiter = 100L, gtol = 1e-6, parallel = FALSE)

      super$initialize(
        id = "cmprsk.crr",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox", "future.apply"),
        label = "Fine-Gray Competing Risks Regression",
        man = "mlr3proba::mlr_learners_cmprsk.crr"
      )
    },

    #' @description
    #' Returns convergence status for each event model.
    #' @return Named list with TRUE/FALSE for each event.
    convergence = function() {
      if (is.null(self$state$convergence)) {
        stop("Model has not been trained yet")
      }
      return(self$state$convergence)
    },

    #' @description
    #' Returns coefficient-based variable importance for each event.
    #' @return Data frame with columns: variable, importance, event.
    importance = function() {
      if (is.null(self$state$model)) {
        stop("Model has not been trained yet")
      }
      importance <- lapply(names(self$state$model), function(event) {
        coefs <- self$state$model[[event]]$coef
        abs_coefs <- abs(coefs)
        sum_coefs <- sum(abs_coefs, na.rm = TRUE)
        if (sum_coefs == 0) sum_coefs <- 1 # Avoid division by zero
        data.frame(
          variable = names(coefs),
          importance = abs_coefs / sum_coefs,
          event = event
        )
      })
      do.call(rbind, importance)
    }
  ),

  private = list(
    cov2_info = NULL,

    .train = function(task, row_ids = task$row_ids) {
      pv <- self$param_set$get_values(tags = "train")
      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]

      # Cache covariate matrices
      if (is.null(private$cov2_info) || is.null(private$cov2_info$cov2only)) {
        cov1_features <- features
      } else {
        cov1_features <- setdiff(features, private$cov2_info$cov2only)
      }
      if (length(cov1_features) > 0) {
        formula <- as.formula(paste("~", paste(cov1_features, collapse = " + ")))
        cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
        if (any(is.na(cov1))) stop("NAs detected in cov1")
        self$state$cov1_formula <- formula
      } else {
        cov1 <- NULL
        self$state$cov1_formula <- NULL
      }

      if (!is.null(private$cov2_info)) {
        cov2nms <- private$cov2_info$cov2nms
        tf <- private$cov2_info$tf
        for (nm in cov2nms) {
          if (!nm %in% features) {
            stop(sprintf("cov2nms element '%s' not found in task features", nm))
          }
        }
        cov2_formulas <- lapply(cov2nms, function(nm) as.formula(paste("~", nm)))
        cov2_list <- lapply(cov2_formulas, function(f) {
          model.matrix(f, data = full_data)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)
        if (any(is.na(cov2))) stop("NAs detected in cov2")
        uft <- unique(as.numeric(full_data[[time_col]]))
        tf_out <- tf(uft)
        if (!is.matrix(tf_out)) stop("tf must return a matrix")
        if (nrow(tf_out) != length(uft)) stop("tf output must have rows equal to unique failure times")
        if (ncol(tf_out) != ncol(as.matrix(cov2))) {
          stop(sprintf("tf must return a matrix with %d columns", ncol(as.matrix(cov2))))
        }
        tf_final <- tf
        self$state$cov2_formulas <- cov2_formulas
      } else {
        cov2 <- NULL
        tf_final <- NULL
        self$state$cov2_formulas <- NULL
      }

      ftime <- as.numeric(full_data[[time_col]])
      all_event_times <- sort(unique(ftime))

      # Parallelized model fitting
      fit_model <- function(target_event, ftime, fstatus, cov1, cov2, tf_final, pv) {
        tryCatch({
          if (is.null(cov2)) {
            cmprsk::crr(ftime, fstatus, cov1, failcode = as.integer(target_event),
                        cencode = 0L, maxiter = pv$maxiter, gtol = pv$gtol)
          } else {
            cmprsk::crr(ftime, fstatus, cov1, cov2, tf_final, failcode = as.integer(target_event),
                        cencode = 0L, maxiter = pv$maxiter, gtol = pv$gtol)
          }
        }, warning = function(w) {
          warning(sprintf("Convergence warning for event %s: %s", target_event, w$message))
          return(NULL)
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s", target_event, e$message))
        })
      }

      event_levels <- as.character(task$cmp_events)
      fstatus <- as.numeric(full_data[[event_col]])
      if (pv$parallel) {
        requireNamespace("future.apply", quietly = TRUE)
        models <- future.apply::future_lapply(event_levels, fit_model,
                                              ftime = ftime,
                                              fstatus = fstatus,
                                              cov1 = cov1,
                                              cov2 = cov2,
                                              tf_final = tf_final,
                                              pv = pv,
                                              future.seed = TRUE)
      } else {
        models <- lapply(event_levels, fit_model,
                         ftime = ftime,
                         fstatus = fstatus,
                         cov1 = cov1,
                         cov2 = cov2,
                         tf_final = tf_final,
                         pv = pv)
      }
      names(models) <- event_levels

      # Store convergence status and event times
      convergence <- list()
      event_times <- list()
      for (event in event_levels) {
        convergence[[event]] <- if (is.null(models[[event]])) FALSE else TRUE
        event_times[[event]] <- models[[event]]$uftime
      }

      self$state$model <- models
      self$state$convergence <- convergence
      self$state$event_times <- event_times
      self$state$cov2 <- cov2
      self$state$tf <- tf_final
      self$state$feature_names <- features
      self$state$cov2_names <- if (!is.null(cov2)) colnames(as.matrix(cov2)) else NULL
      self$state$all_event_times <- all_event_times

      models
    },

    .predict = function(task, row_ids = task$row_ids) {
      newdata <- task$data(rows = row_ids)
      event_levels <- as.character(task$cmp_events)
      cif_list <- vector("list", length(event_levels))
      names(cif_list) <- event_levels

      # Reuse cached covariate formulas
      if (!is.null(self$state$cov1_formula)) {
        cov1 <- model.matrix(self$state$cov1_formula, data = newdata)[, -1, drop = FALSE]
        if (any(is.na(cov1))) stop("NAs detected in cov1")
      } else {
        cov1 <- NULL
      }

      if (!is.null(self$state$cov2_formulas)) {
        cov2_list <- lapply(self$state$cov2_formulas, function(f) {
          model.matrix(f, data = newdata)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)
        if (any(is.na(cov2))) stop("NAs detected in cov2")
        tf <- self$state$tf
      } else {
        cov2 <- NULL
        tf <- NULL
      }

      all_times <- self$state$all_event_times
      all_times_char <- as.character(all_times)

      for (event in event_levels) {
        model <- self$state$model[[event]]
        if (is.null(model)) stop(sprintf("No model available for event %s", event))
        pred <- tryCatch({
          pred_raw <- predict(model, cov1 = cov1, cov2 = cov2, tf = tf, time = all_times)
          pred_times <- pred_raw[, 1]
          pred_cif <- t(pred_raw[, -1, drop = FALSE])
          n_times <- length(all_times)
          if (ncol(pred_cif) != n_times) {
            full_cif <- matrix(0, nrow = nrow(pred_cif), ncol = n_times)
            colnames(full_cif) <- all_times_char
            time_idx <- match(pred_times, all_times)
            if (any(is.na(time_idx))) stop("Predicted times do not match training times")
            for (i in seq_len(nrow(pred_cif))) {
              for (j in seq_len(ncol(pred_cif))) {
                full_cif[i, time_idx[j]:n_times] <- pred_cif[i, j]
              }
            }
            pred_cif <- full_cif
          }
          colnames(pred_cif) <- all_times_char
          rownames(pred_cif) <- NULL
          pred_cif
        }, error = function(e) {
          stop(sprintf("Failed to predict for event %s: %s", event, e$message))
        })
        cif_list[[event]] <- pred
      }

      list(cif = cif_list)
    }
  )
)

#' @importFrom mlr3misc register_learner
# register_learner("cmprsk.crr", LearnerCompRisksFineGrayCRR)
