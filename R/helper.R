# Helper function to check if a package is installed
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not installed"))
  }
}

# Task configuration function
configure_task <- function(stratum = NULL, features = "trt") {
  task <- tsk("pbc")
  data_cols <- names(task$data())
  if (!is.null(stratum) && !stratum %in% data_cols) {
    stop(sprintf("Stratum variable '%s' not found in task", stratum))
  }
  if (!is.null(features) && !all(features %in% data_cols)) {
    stop(sprintf("Features %s not found in task",
                 paste(setdiff(features, data_cols), collapse = ", ")))
  }
  if (!is.null(features)) {
    task$select(features)
  }
  if (!is.null(stratum)) {
    task$set_col_roles(cols = stratum, add_to = "stratum")
  }
  task
}

# Partition function
create_partition <- function(task, ratio = 0.7) {
  set.seed(123)
  partition(task, ratio = ratio)
}

# Shared setup for task and partition using helper functions
setup_task <- function() {
  task <- configure_task(stratum = "status", features = c("age", "sex", "bili"))
  part <- create_partition(task, ratio = 0.7)
  list(task = task, part = part)
}
