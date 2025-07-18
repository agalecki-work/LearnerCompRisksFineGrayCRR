# Helper function to check if a package is installed
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not installed"))
  }
}

# Shared setup for task and partition
setup_task <- function() {
  task <- mlr3::tsk("pbc")
  task$col_roles$feature <- setdiff(task$feature_names, "status") # Rm status
  task$select(c("age", "sex", "bili"))
  task$set_col_roles(cols = "status", add_to = "stratum")  # Status as stratum
  set.seed(123)
  part <- mlr3::partition(task, ratio = 0.7)
  list(task = task, part = part)
}
