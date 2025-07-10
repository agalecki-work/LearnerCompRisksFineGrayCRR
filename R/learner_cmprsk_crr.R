#' @importFrom mlr3proba LearnerCompRisks PredictionCompRisks
#' @import paradox
#' @importFrom mlr3misc invoke
#'
#' @title Fine-Gray Competing Risks Regression
#' @name mlr_learners_cmprsk.crr
#' @templateVar id cmprsk.crr
#' @templateVar caller [cmprsk::crr()]
#' @templateVar predict_method [cmprsk::predict.crr()]
#' @template learner
#'
#' @description
#' Fits a Fine-Gray competing risks regression model using
#' \code{\link[cmprsk:crr]{cmprsk::crr}} from the \code{cmprsk} package.
#' It estimates cumulative incidence functions (CIFs) for competing risks
#' scenarios with multiple mutually exclusive events, supporting both fixed
#' and time-varying covariates. Predictions are generated using
#' \code{\link[cmprsk:predict.crr]{cmprsk::predict.crr}}, providing CIFs
#' for specified event types across unique event times from training data.
#'
#' @section Methods:
#' \describe{
#'   \item{`convergence()`}{Returns a named list with convergence status
#'     (\code{TRUE}/\code{FALSE}) for each event model.}
#'   \item{`importance()`}{Returns a data frame with coefficient-based
#'     variable importance for each event, scaled by absolute coefficient sums.}
#' }
#'
#' @references
#' Fine, J. P., & Gray, R. J. (1999). A Proportional Hazards Model for
#' the Subdistribution of a Competing Risk. \emph{Journal of the American
#' Statistical Association}, 94(446), 496--509.
#' \doi{10.1080/01621459.1999.10474144}
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
#'   maxiter = 100,
#'   gtol = 1e-6,
#'   parallel = FALSE
#' )
#' learner$train(task)
#' pred <- learner$predict(task)
#' print(pred)
#' learner$convergence()
#' learner$importance()
LearnerCompRisksFineGrayCRR <- R6::R6Class( # nolint: object_name_linter.
  "LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param cov2_info `list()` \cr
    #' Configures time-varying covariates for the Fine-Gray model. If
    #' \code{NULL} (default), all covariates are treated as fixed and
    #' included in the \code{cov1} matrix for
    #' \code{\link[cmprsk:crr]{cmprsk::crr}}. When specified,
    #' \code{cov2_info} defines covariates with time-varying effects
    #' (included in \code{cov2}) and their time dependency. The list
    #' must contain: \cr
    #' \describe{
    #'   \item{cov2nms}{`character()` \cr
    #'     Names of covariates from the task's feature set for
    #'     time-varying effects in the \code{cov2} matrix. Must be a
    #'     non-empty character vector and subset of task features.}
    #'   \item{tf}{`function(uft)` \cr
    #'     Function defining time-varying effects, taking a numeric
    #'     vector \code{uft} (unique failure times) and returning a
    #'     matrix with \code{nrow = length(uft)} and \code{ncol}
    #'     matching \code{cov2} columns (from \code{cov2nms}).
    #'     Example: \code{function(uft) cbind(log(uft), uft^2)}.}
    #'   \item{cov2only}{`character()` or \code{NULL} \cr
    #'     Subset of \code{cov2nms} for covariates used only in
    #'     \code{cov2} (time-varying), excluded from \code{cov1}
    #'     (fixed). If \code{NULL} (default), all task features
    #'     contribute to \code{cov1} unless in \code{cov2only}.}
    #' }
    #' Example: \preformatted{
    #' cov2_info = list(
    #'   cov2nms = c("age", "sex"),
    #'   tf = function(uft) cbind(log(uft), log(uft + 1)),
    #'   cov2only = "sex"
    #' )
    #' }
    #' Here, \code{age} is in both \code{cov1} and \code{cov2}, while
    #' \code{sex} is only in \code{cov2}.
    #' @param maxiter `integer(1)` \cr
    #' Maximum iterations for \code{\link[cmprsk:crr]{cmprsk::crr}}.
    #' Default: 100, range: 1 to 1000.
    #' @param gtol `numeric(1)` \cr
    #' Convergence tolerance for \code{\link[cmprsk:crr]{cmprsk::crr}}.
    #' Default: 1e-6, range: 1e-9 to 1e-3.
    #' @param parallel `logical(1)` \cr
    #' Use parallel processing via
    #' \code{\link[future.apply:future_lapply]{future.apply::future_lapply}}.
    #' Default: \code{FALSE}. Requires \code{future.apply}.
    initialize = function(cov2_info = NULL, maxiter = 100L,
                          gtol = 1e-6, parallel = FALSE) {
      if (!is.null(cov2_info)) {
        if (!is.list(cov2_info)) stop("cov2_info must be a list")
        if (!all(c("cov2nms", "tf") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tf'")
        }
        if (!is.character(cov2_info$cov2nms) ||
              length(cov2_info$cov2nms) == 0) {
          stop("cov2nms must be a non-empty character vector")
        }
        if (!is.function(cov2_info$tf)) stop("tf must be a function")
        if (!is.null(cov2_info$cov2only)) {
          if (!is.character(cov2_info$cov2only)) {
            stop("cov2only must be a character vector or NULL")
          }
          if (!all(cov2_info$cov2only %in% cov2_info$cov2nms)) {
            stop("cov2only must be a subset of cov2nms")
          }
        }
      }
      private$cov2_info <- cov2_info

      ps <- paradox::ps(
        maxiter = paradox::p_int(default = 100L, lower = 1L,
                                 upper = 1000L, tags = "train"),
        gtol = paradox::p_dbl(default = 1e-6, lower = 1e-9,
                              upper = 1e-3, tags = "train"),
        parallel = paradox::p_lgl(default = FALSE, tags = "train")
      )
      ps$values <- list(maxiter = maxiter, gtol = gtol, parallel = parallel)

      super$initialize(
        id = "cmprsk.crr",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox", "future.apply"),
        properties = c("importance", "missings"),
        label = "Fine-Gray Competing Risks Regression",
        man = "mlr3extralearners::mlr_learners_cmprsk.crr"
      )
    },

    #' @description
    #' Returns convergence status for each event model.
    #' @return Named list with \code{TRUE}/\code{FALSE} for each event.
    convergence = function() {
      if (is.null(self$state$convergence)) {
        stop("Model has not been trained yet")
      }
      self$state$convergence
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
        if (sum_coefs == 0) sum_coefs <- 1
        data.frame(
          variable = names(coefs),
          importance = abs_coefs / sum_coefs,
          event = event,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, importance)
    }
  ),

  private = list(
    cov2_info = NULL,

    .train = function(task, row_ids = task$row_ids) {
      if (!inherits(task, "Task")) stop("Task must be a valid mlr3 task")
      if (!all(row_ids %in% task$row_ids)) stop("Invalid row_ids")
      pv <- self$param_set$get_values(tags = "train")
      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]
      if (!is.numeric(full_data[[time_col]])) {
        stop("Time column must be numeric")
      }

      if (is.null(private$cov2_info) || is.null(private$cov2_info$cov2only)) {
        cov1_features <- features
      } else {
        cov1_features <- setdiff(features, private$cov2_info$cov2only)
      }
      if (length(cov1_features) > 0) {
        formula <- as.formula(
          paste("~", paste(cov1_features, collapse = " + "))
        )
        cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
        if (any(is.na(cov1))) stop("NAs detected in cov1")
        self$state$cov1_formula <- formula
      } else {
        cov1 <- matrix(0, nrow = nrow(full_data), ncol = 1)
        colnames(cov1) <- "dummy"
        self$state$cov1_formula <- as.formula("~ 1")
      }

      if (!is.null(private$cov2_info)) {
        cov2nms <- private$cov2_info$cov2nms
        tf <- private$cov2_info$tf
        for (nm in cov2nms) {
          if (!nm %in% features) {
            stop(sprintf("cov2nms element '%s' not in task features", nm))
          }
        }
        cov2_formulas <- lapply(cov2nms, function(nm) {
          as.formula(paste("~", nm))
        })
        cov2_list <- lapply(cov2_formulas, function(f) {
          model.matrix(f, data = full_data)[, -1, drop = FALSE]
        })
        cov2 <- do.call(cbind, cov2_list)
        if (ncol(cov2) == 1) cov2 <- as.vector(cov2)
        if (any(is.na(cov2))) stop("NAs detected in cov2")
        uft <- unique(as.numeric(full_data[[time_col]]))
        tf_out <- tf(uft)
        if (!is.matrix(tf_out)) stop("tf must return a matrix")
        if (nrow(tf_out) != length(uft)) {
          stop("tf output must have rows equal to unique failure times")
        }
        if (ncol(tf_out) != ncol(as.matrix(cov2))) {
          stop(sprintf(
            "tf must return a matrix with %d columns",
            ncol(as.matrix(cov2))
          ))
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
      event_levels <- as.character(task$cmp_events)
      fstatus <- as.numeric(full_data[[event_col]])

      fit_model <- function(target_event, ftime, fstatus, cov1, cov2,
                            tf_final, pv) {
        tryCatch({
          if (is.null(cov2)) {
            cmprsk::crr(
              ftime, fstatus, cov1,
              failcode = as.integer(target_event),
              cencode = 0L,
              maxiter = pv$maxiter,
              gtol = pv$gtol
            )
          } else {
            cmprsk::crr(
              ftime, fstatus, cov1, cov2, tf_final,
              failcode = as.integer(target_event),
              cencode = 0L,
              maxiter = pv$maxiter,
              gtol = pv$gtol
            )
          }
        }, warning = function(w) {
          warning(sprintf("Convergence warning for event %s: %s",
                          target_event, w$message))
          NULL
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s",
                       target_event, e$message))
        })
      }

      if (pv$parallel) {
        requireNamespace("future.apply", quietly = TRUE)
        models <- future.apply::future_lapply(
          event_levels, fit_model,
          ftime = ftime,
          fstatus = fstatus,
          cov1 = cov1,
          cov2 = cov2,
          tf_final = tf_final,
          pv = pv,
          future.seed = TRUE
        )
      } else {
        models <- lapply(
          event_levels, fit_model,
          ftime = ftime,
          fstatus = fstatus,
          cov1 = cov1,
          cov2 = cov2,
          tf_final = tf_final,
          pv = pv
        )
      }
      names(models) <- event_levels

      convergence <- list()
      event_times <- list()
      for (event in event_levels) {
        convergence[[event]] <- !is.null(models[[event]])
        event_times[[event]] <- if (!is.null(models[[event]])) {
          models[[event]]$uftime
        } else {
          NULL
        }
      }

      self$state$model <- models
      self$state$convergence <- convergence
      self$state$event_times <- event_times
      self$state$cov2 <- cov2
      self$state$tf <- tf_final
      self$state$feature_names <- features
      self$state$cov2_names <- if (!is.null(cov2)) {
        colnames(as.matrix(cov2))
      } else {
        NULL
      }
      self$state$all_event_times <- all_event_times

      models
    },

    .predict = function(task, row_ids = task$row_ids) {
      if (!inherits(task, "Task")) stop("Task must be a valid mlr3 task")
      if (!all(row_ids %in% task$row_ids)) stop("Invalid row_ids")
      newdata <- task$data(rows = row_ids)
      event_levels <- as.character(task$cmp_events)
      cif_list <- vector("list", length(event_levels))
      names(cif_list) <- event_levels

      if (!is.null(self$state$cov1_formula)) {
        cov1 <- model.matrix(
          self$state$cov1_formula,
          data = newdata
        )[, -1, drop = FALSE]
        if (ncol(cov1) == 0) {
          cov1 <- matrix(0, nrow = nrow(newdata), ncol = 1)
          colnames(cov1) <- "dummy"
        }
        if (any(is.na(cov1))) stop("NAs detected in cov1")
      } else {
        cov1 <- matrix(0, nrow = nrow(newdata), ncol = 1)
        colnames(cov1) <- "dummy"
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
        if (is.null(model)) {
          stop(sprintf("No model available for event %s", event))
        }
        pred <- tryCatch({
          pred_raw <- predict(
            model,
            cov1 = cov1,
            cov2 = cov2,
            tf = tf,
            time = all_times
          )
          pred_times <- pred_raw[, 1]
          pred_cif <- t(pred_raw[, -1, drop = FALSE])
          n_times <- length(all_times)
          if (ncol(pred_cif) != n_times) {
            full_cif <- matrix(0, nrow = nrow(pred_cif), ncol = n_times)
            colnames(full_cif) <- all_times_char
            time_idx <- match(pred_times, all_times)
            if (any(is.na(time_idx))) {
              stop("Predicted times do not match training times")
            }
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

      PredictionCompRisks$new(task = task, cif = cif_list)
    }
  )
)
