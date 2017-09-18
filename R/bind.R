#' The 'bind' operator
#'
#' \code{\%>>=\%} (pronounced "bind") is a generic operator intended to match
#' the semantics of `>>=` in other functional programming languages, such as F#
#' or Haskell. It is intended that the right-hand side will contain a function
#' or block, similar to the pipe operator \code{\link[magrittr]{\%>\%}}. Bind is
#' usually employed to apply functions to the contents of possibly-opaque
#' "container" objects. See [bind-result] or [bind-list] for examples.
#'
#' @param lhs The left-hand side of the operator.
#' @param rhs Although not required, it is intended that the right-hand side
#'   contain a function or block, evaluated with `.` bound to the value of
#'   `lhs`.
#'
#' @usage lhs \%>>=\% rhs
#'
#' @details
#'
#' By default, it will simply pipe the left-hand side into the function or block
#' on the right-hand side, as with \code{\link[magrittr]{\%>\%}}, although this
#' is not recommended -- any S3 classes with a "bind" implementation of their
#' own might yield very different results than the default behaviour.
#'
#' @rdname bind
#' @aliases %>>=% bind
#'
#' @export
`%>>=%` <- function(lhs, rhs) {
    UseMethod("%>>=%")
}

#' @rdname bind
#'
#' @export
`%>>=%.default` <- function(lhs, rhs) {
  rhs <- rlang::enquo(rhs)

  rhs <- normalize_pipe_rhs(rhs, lhs)

  expr <- rlang::new_quosure(rlang::expr(rlang::UQE(rhs)),
                             env = rlang::f_env(rhs))

  if (getOption("rrails.debug", FALSE)) {
    expr
  } else {
    rlang::eval_tidy(expr)
  }
}
