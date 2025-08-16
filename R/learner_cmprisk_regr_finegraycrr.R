#' @title Fine-Gray Competing Risks Regression Learner
#' @author agalecki
#' @name mlr_learners_cmprsk.crr
#'
#' @description
#' Fits a Fine-Gray competing risks regression model for estimating cumulative incidence functions (CIFs)
#' in scenarios with multiple mutually exclusive events. Calls [cmprsk::crr()] from the \CRANpkg{cmprsk}
#' package for training and [cmprsk::predict.crr()] for prediction. Supports both fixed and time-varying
#' covariates. Variable importance is cause-specific, based on the absolute value of regression coefficients
#' normalized by their sum per event.
#'
#' @template learner
#' @templateVar id cmprsk.crr
#'
#' @section Initial parameter values:
#' - `maxiter`: Maximum iterations for [cmprsk::crr()]. Default is 100, range: 1 to 1000.
#' - `gtol`: Convergence tolerance for [cmprsk::crr()]. Default is 1e-6, range: 1e-9 to 1e-3.
#' - `parallel`: Logical indicating whether to use parallel processing via [future.apply::future_lapply()].
#'   Required, initialized to `FALSE` during construction.
#' - `cov2_info`: Configuration for time-varying covariates. Required, initialized to `NULL`
#' during construction (fixed covariates only).
#' - `init_list`: Initial values for regression parameters per event. Required, initialized to
#" `NULL` during construction (uses zeros).
#' - `censor_group`: Column name specifying groups with different censoring distributions.
#'   Required, initialized to `NULL` during construction (single censoring distribution).
#'
#' @section Custom mlr3 parameters:
#' The following parameters extend the functionality of [cmprsk::crr()] within the mlr3 framework:
#' - `parallel`: Enables parallel processing for fitting cause-specific models, not available in the upstream function.
#' - `cov2_info`: Provides a structured interface for specifying time-varying covariates, simplifying their integration.
#' - `init_list`: Allows cause-specific initial regression parameters in a single named list, enhancing flexibility.
#' - `censor_group`: Specifies groups with different censoring distributions, integrated with mlr3 task data.
#'
#' @references
#' `r mlr3misc::format_bib("finegray1999crr")`
#'
#' @template seealso_learner
#' @importFrom R6 R6Class
#' @importFrom cmprsk crr predict.crr
#' @importFrom mlr3misc invoke
#' @importFrom mlr3proba LearnerCompRisks PredictionCompRisks
#' @importFrom paradox ps
#' @importFrom future.apply future_lapply
#' @examplesIf mlr3misc::require_namespaces(c("cmprsk", "future.apply"), quietly = TRUE)
#' # Define the learner
#' learner = mlr3::lrn("cmprsk.crr", maxiter = 100, gtol = 1e-6, parallel = FALSE,
#'                     cov2_info = NULL, init_list = NULL, censor_group = NULL)
#'
#' # Define a task
#' task = mlr3::tsk("pbc")
#' task$select(c("age", "bili", "sex"))
#' task$set_col_roles(cols = "status", add_to = "stratum")
#'
#' # Create train and test sets
#' ids = mlr3::partition(task)
#'
#' # Train the learner
#' learner$train(task, row_ids = ids$train)
#'
#' # Print model and importance
#' print(learner$model)
#' print(learner$importance(cause = "1")) # Importance for cause 1
#' print(learner$importance(cause = "2")) # Importance for cause 2
#'
#' # Make predictions
#' predictions = learner$predict(task, row_ids = ids$test)
#'
#' # Score predictions
#' predictions$score()
#' @export
LearnerCompRisksFineGrayCRR <- R6::R6Class(
  "LearnerCompRisksFineGrayCRR",
  inherit = mlr3proba::LearnerCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param cov2_info (`list()`)\cr
    #'   Configuration for time-varying covariates. A list with:
    #'   \itemize{
    #'     \item `cov2nms`: Character vector of covariate names (e.g., `c("age", "bili")`).
    #'     \item `tf`: Function transforming time values (returns a matrix).
    #'     \item `cov2only`: Optional character vector of covariates to exclude from fixed effects.
    #'   }
    #'   Required, initialized to `NULL` (fixed covariates only) during construction.
    #' @param maxiter (`integer(1)`)\cr
    #'   Maximum iterations for [cmprsk::crr()]. Default: 100, range: 1 to 1000.
    #' @param gtol (`numeric(1)`)\cr
    #'   Convergence tolerance for [cmprsk::crr()]. Default: 1e-6, range: 1e-9 to 1e-3.
    #' @param parallel (`logical(1)`)\cr
    #'   Use parallel processing via [future.apply::future_lapply()]. Required, initialized to `FALSE`
    #' during construction.
    #' @param init_list (`list()`)\cr
    #'   Named list of initial regression parameter values per event (names match `task$cmp_events`).
    #'   Each vector's length must match the number of covariates. Required, initialized to `NULL`
    #' (uses zeros) during construction.
    #' @param censor_group (`character(1)`)\cr
    #'   Column name specifying groups with different censoring distributions. Required, initialized
    #' to `NULL` during construction.
    initialize = function(cov2_info = NULL, maxiter = 100L, gtol = 1e-6,
                          parallel = FALSE, init_list = NULL, censor_group = NULL) {
      if (is.null(cov2_info)) {
        private$cov2_info <- NULL
      } else {
        if (!is.list(cov2_info)) stop("cov2_info must be a list or NULL")
        if (!all(c("cov2nms", "tf") %in% names(cov2_info))) {
          stop("cov2_info must contain 'cov2nms' and 'tf'")
        }
        if (!is.character(cov2_info$cov2nms) || length(cov2_info$cov2nms) == 0) {
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
        private$cov2_info <- cov2_info
      }
      if (is.null(init_list)) {
        private$init_list <- NULL
      } else {
        if (!is.list(init_list)) stop("init_list must be a list or NULL")
        if (!all(sapply(init_list, is.numeric))) {
          stop("init_list must contain numeric vectors")
        }
        if (length(names(init_list)) == 0 || any(names(init_list) == "")) {
          stop("init_list must have non-empty names for each event")
        }
        private$init_list <- init_list
      }
      if (is.null(censor_group)) {
        private$censor_group <- NULL
      } else {
        if (!is.character(censor_group) || length(censor_group) != 1) {
          stop("censor_group must be a single character string or NULL")
        }
        private$censor_group <- censor_group
      }
      if (!is.logical(parallel) || length(parallel) != 1) {
        stop("parallel must be a single logical value")
      }
      param_set <- ps(
        maxiter = p_int(default = 100L, lower = 1L, upper = 1000L, tags = "train"),
        gtol = p_dbl(default = 1e-6, lower = 1e-9, upper = 1e-3, tags = "train"),
        parallel = p_lgl(tags = c("train", "required")),
        cov2_info = p_uty(tags = c("train", "required")),
        init_list = p_uty(tags = c("train", "required")),
        censor_group = p_uty(tags = c("train", "required"))
      )
      param_set$values <- list(
        maxiter = maxiter,
        gtol = gtol,
        parallel = parallel,
        cov2_info = cov2_info,
        init_list = init_list,
        censor_group = censor_group
      )
      super$initialize(
        id = "cmprsk.crr",
        param_set = param_set,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = "cif",
        packages = c("mlr3proba", "cmprsk", "paradox", "future.apply"),
        properties = c("importance", "missings"),
        man = "mlr3extralearners::mlr_learners_cmprsk.crr",
        label = "Fine-Gray Competing Risks Regression"
      )
    },

    #' @description
    #' Extracts cause-specific variable importance based on the absolute value of regression
    #' coefficients normalized by their sum for the specified event. Returns `NULL` if the model
    #' for the specified cause is not trained or has not converged.
    #'
    #' @param cause (`integer(1)` or `character(1)`)\cr
    #'   The event of interest (must match an event in `task$cmp_events`). Default is 1.
    #' @return Named `numeric()` vector of importance scores, or `NULL` if the model is unavailable.
    importance = function(cause = 1) {
      if (is.null(self$state$model)) {
        stop("Model has not been trained yet")
      }
      causes <- names(self$state$model)
      cause <- as.character(cause)
      if (!cause %in% causes) {
        stop(sprintf("Invalid cause. Use one of: %s", paste(causes, collapse = ", ")))
      }
      if (is.null(self$state$model[[cause]]) || !self$state$convergence[[cause]]) {
        warning(sprintf("No converged model available for cause %s", cause))
        return(NULL)
      }
      coefs <- self$state$model[[cause]]$coef
      abs_coefs <- abs(coefs)
      sum_coefs <- sum(abs_coefs, na.rm = TRUE)
      if (sum_coefs == 0) sum_coefs <- 1
      importance <- abs_coefs / sum_coefs
      sort(importance, decreasing = TRUE)
    },

    #' @description
    #' Returns a named list indicating convergence status (`TRUE`/`FALSE`) for each event model.
    #' Raises an error if the model has not been trained.
    #'
    #' @return Named `list()` with convergence status for each event.
    convergence = function() {
      if (is.null(self$state$convergence)) {
        stop("Model has not been trained yet")
      }
      self$state$convergence
    }
  ),

  private = list(
    cov2_info = NULL,
    init_list = NULL,
    censor_group = NULL,

    crr_wrapper = function(ftime, fstatus, cov1, cov2 = NULL, tf = NULL,
                           cengroup = NULL, failcode = 1, cencode = 0,
                           subset = NULL, na.action = stats::na.omit, # nolint
                           gtol = 1e-06, maxiter = 10, init = NULL,
                           variance = TRUE) {
      if (!requireNamespace("cmprsk", quietly = TRUE)) {
        stop("Package 'cmprsk' is required. Please install it using install.packages('cmprsk').")
      }
      args <- list(
        ftime = ftime,
        fstatus = fstatus,
        cov1 = cov1,
        cov2 = cov2,
        tf = tf,
        cengroup = cengroup,
        failcode = failcode,
        cencode = cencode,
        subset = substitute(subset),
        na.action = na.action,
        gtol = gtol,
        maxiter = maxiter,
        init = init,
        variance = variance
      )
      args <- args[!sapply(args, is.null)] # nolint
      invoke(crr, .args = args)
    },

    .train = function(task, row_ids = task$row_ids) {
      if (!inherits(task, "TaskCompRisks")) stop("Task must be a valid mlr3 TaskCompRisks")
      if (!all(row_ids %in% task$row_ids)) stop("Invalid row_ids")
      pv <- self$param_set$get_values(tags = "train")

      cengroup <- if (!is.null(pv$censor_group)) {
        if (!pv$censor_group %in% task$backend$colnames) {
          stop(sprintf("censor_group column '%s' not found in task data", pv$censor_group))
        }
        task$backend$data(rows = row_ids, cols = pv$censor_group)[[pv$censor_group]]
      } else {
        NULL
      }
      if (!is.null(cengroup) && any(is.na(cengroup))) {
        stop("censor_group contains missing values")
      }

      full_data <- task$data(rows = row_ids)
      features <- task$feature_names
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]
      if (!is.numeric(full_data[[time_col]])) stop("Time column must be numeric")
      if (!is.numeric(full_data[[event_col]]) && !is.integer(full_data[[event_col]])) {
        stop("Event column must be numeric or integer")
      }

      if (is.null(pv$cov2_info) || is.null(pv$cov2_info$cov2only)) {
        cov1_features <- features
      } else {
        cov1_features <- setdiff(features, pv$cov2_info$cov2only)
      }
      if (length(cov1_features) > 0) {
        formula <- as.formula(paste("~", paste(cov1_features, collapse = " + ")))
        cov1 <- model.matrix(formula, data = full_data)[, -1, drop = FALSE]
        if (any(is.na(cov1))) stop("NAs detected in cov1")
        self$state$cov1_formula <- formula
      } else {
        cov1 <- matrix(0, nrow = nrow(full_data), ncol = 1)
        colnames(cov1) <- "dummy"
        self$state$cov1_formula <- as.formula("~ 1")
      }

      if (!is.null(pv$cov2_info)) {
        cov2nms <- pv$cov2_info$cov2nms
        tf <- pv$cov2_info$tf
        for (nm in cov2nms) {
          if (!nm %in% features) {
            stop(sprintf("cov2nms element '%s' not in task features", nm))
          }
        }
        cov2_formulas <- lapply(cov2nms, function(nm) as.formula(paste("~", nm)))
        cov2_list <- lapply(cov2_formulas, function(f) model.matrix(f, data = full_data)[, -1, drop = FALSE])
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
      event_levels <- as.character(task$cmp_events)
      fstatus <- as.numeric(full_data[[event_col]])

      if (!is.null(pv$init_list)) {
        n_cov <- ncol(cov1) + (if (is.null(cov2)) 0 else ncol(as.matrix(cov2)))
        for (event in names(pv$init_list)) {
          init_vec <- pv$init_list[[event]]
          if (length(init_vec) != n_cov) {
            stop(sprintf("init_list for event %s must have %d values", event, n_cov))
          }
          if (!is.numeric(init_vec) || any(is.na(init_vec))) {
            stop(sprintf("init_list for event %s must be a numeric vector with no NAs", event))
          }
        }
        if (!all(event_levels %in% names(pv$init_list))) {
          stop("init_list must have entries for all events in task$cmp_events")
        }
      }

      fit_model <- function(target_event, ftime, fstatus, cov1, cov2,
                            tf_final, pv, cengroup) {
        init <- if (!is.null(pv$init_list)) pv$init_list[[target_event]] else NULL
        tryCatch({
          private$crr_wrapper(
            ftime = ftime,
            fstatus = fstatus,
            cov1 = cov1,
            cov2 = cov2,
            tf = tf_final,
            cengroup = cengroup,
            failcode = as.integer(target_event),
            cencode = 0L,
            maxiter = pv$maxiter,
            gtol = pv$gtol,
            init = init
          )
        }, warning = function(w) {
          warning(sprintf("Convergence warning for event %s: %s", target_event, w$message))
          NULL
        }, error = function(e) {
          stop(sprintf("Failed to train model for event %s: %s", target_event, e$message))
        })
      }

      if (pv$parallel) {
        if (!requireNamespace("future.apply", quietly = TRUE)) {
          stop("Package 'future.apply' is required for parallel processing")
        }
        models <- future_lapply(
          event_levels, fit_model,
          ftime = ftime,
          fstatus = fstatus,
          cov1 = cov1,
          cov2 = cov2,
          tf_final = tf_final,
          pv = pv,
          cengroup = cengroup,
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
          pv = pv,
          cengroup = cengroup
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
      self$state$cov2_names <- if (!is.null(cov2)) colnames(as.matrix(cov2)) else NULL
      self$state$all_event_times <- all_event_times

      models
    },

    .predict = function(task, row_ids = task$row_ids) {
      if (!inherits(task, "TaskCompRisks")) stop("Task must be a valid mlr3 TaskCompRisks")
      if (!all(row_ids %in% task$row_ids)) stop("Invalid row_ids")
      newdata <- task$data(rows = row_ids)
      event_levels <- as.character(task$cmp_events)
      cif_list <- vector("list", length(event_levels))
      names(cif_list) <- event_levels

      if (!is.null(self$state$cov1_formula)) {
        cov1 <- model.matrix(self$state$cov1_formula, data = newdata)[, -1, drop = FALSE]
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
          if (!requireNamespace("cmprsk", quietly = TRUE)) {
            stop("Package 'cmprsk' is required")
          }
          pred_raw <- invoke(
            predict.crr,
            .args = list(
              object = model,
              cov1 = cov1,
              cov2 = cov2,
              tf = tf,
              time = all_times
            )
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

# .extralrns_dict$add("cmprsk.crr", LearnerCompRisksFineGrayCRR)
