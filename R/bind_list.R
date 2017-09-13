#' Bind list objects
#'
#' @rdname bind-list
#'
#' @export
`%>>=%.list` <- function(lhs, rhs) {
  rhs <- rlang::enquo(rhs)

  rhs <- normalize_pipe_rhs(rhs, lhs)

  expr <- rlang::new_quosure(rlang::expr({
    # nolint start
    res <- purrr::map(., function(.) rlang::UQE(rhs))
    # nolint end
    res
  }),
  env = rlang::f_env(rhs))

  if (getOption("rrails.debug", FALSE)) {
    expr
  } else {
    rlang::eval_tidy(expr)
  }
}
