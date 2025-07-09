#' <% shortname = id %>
#'
#' @name <%= paste("mlr_learners", shortname, sep = "_")%>
#'
#' @section Dictionary:
#' This [Learner][mlr3::Learner] can be instantiated via the [dictionary][mlr3misc::Dictionary]
#' [mlr_learners][mlr3::mlr_learners] or with the associated sugar function [lrn()][mlr3::lrn]:
#' ```
#' <%=fullname%>$new()
#' mlr_learners$get("<%=shortname%>")
#' lrn("<%=shortname%>")
#' ```
#'
#' @section Meta Information:
#' - Learner ID: <%= id %>
#' - Predict types: cif
#' - Feature types: logical, integer, numeric, factor
#' - Required packages: mlr3, mlr3proba, cmprsk
#' @md
#'
#' @section Parameters:
#' See the parameter set of <%= fullname %> for details.
#' @md
#'
#' @family competing risk learners
#' @template seealso_learner
