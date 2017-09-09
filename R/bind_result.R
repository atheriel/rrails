#' Bind result objects
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
  }), env = rlang::f_env(rhs))

  if (getOption("rrails.debug", FALSE)) {
    expr
  } else {
    rlang::eval_tidy(expr)
  }
}
