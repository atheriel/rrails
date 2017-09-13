#' Safely pipe values through a chain of functions, avoiding errors
#'
#' The bind operator for an object of class `"result"` executes the function or
#' block on the right-hand side if the object contains a value, or passes on the
#' error it contains if not. If the right-hand side function would raise an
#' error, that error is captured in the returned value instead. To turn a
#' regular R object into a result (or vice-versa) see [result].
#'
#' @param lhs An object of class `"result"`.
#' @param rhs A function or block, evaluated with `.` bound to the value of
#'   `lhs`, or ignored if there is no value in `lhs`.
#'
#' @return An object of class `"result"` wrapping the output of the `rhs`
#'   function or block, or carrying forward its error if it has one.
#'
#' @usage lhs \%>>=\% rhs
#'
#' @details
#'
#' The right-hand side syntax is largely identitcal to that of the pipe
#' operator, \code{\link[magrittr]{\%>\%}}.
#'
#' @rdname bind-result
#'
#' @export
`%>>=%.result` <- function(lhs, rhs) {
  # If this is an error-valued result, skip executing the RHS and simply pass
  # the LHS on.
  if (is.null(lhs$result)) return(lhs)

  rhs <- rlang::enquo(rhs)

  rhs <- normalize_pipe_rhs(rhs, lhs$result)

  expr <- rlang::new_quosure(rlang::expr({
    res <- tryCatch(rlang::UQE(rhs), error = function(e) e)
    as_result(res)
  }),
  env = rlang::f_env(rhs))

  if (getOption("rrails.debug", FALSE)) {
    expr
  } else {
    rlang::eval_tidy(expr)
  }
}
