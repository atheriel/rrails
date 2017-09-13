#' The 'bind' operator
#'
#' \code{\%>>=\%} (pronounced "bind") is a generic operator intended to match
#' the semantics of `>>=` in other functional programming languages, such as F#
#' or Haskell. It is intended that the right-hand side will contain a function
#' or block, similar to the pipe operator \code{\link[magrittr]{\%>\%}}.
#'
#' @param lhs The left-hand side of the operator.
#' @param rhs Although not required, it is intended that the right-hand side
#'   contain a function or block, evaluated with `.` bound to the value of
#'   `lhs`.
#'
#' @usage lhs \%>>=\% rhs
#'
#' @rdname bind
#' @aliases %>>=% bind
#'
#' @export
`%>>=%` <- function(lhs, rhs) {
    UseMethod("%>>=%")
}
