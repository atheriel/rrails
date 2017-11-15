#' Bind each element of a list
#'
#' The bind operator for an object of class `"list"` binds the function or block
#' on the right-hand side for each element in the list, returning the result.
#' That is, for each element `elt` in `lhs`, apply \code{elt \%>>=\% rhs} and
#' collect the outputs.
#'
#' @param lhs An object of class `"list"`.
#' @param rhs A function or block, bound to each element of `lhs` successively.
#'
#' @details
#'
#' The right-hand side syntax is largely identical to that of the pipe operator,
#' \code{\link[magrittr]{\%>\%}}.
#'
#' @rdname bind-list
#' @aliases %>>=%.list bind-list
#'
#' @export
`%>>=%.list` <- function(lhs, rhs) {
  rhs <- rlang::enquo(rhs)

  rhs <- normalize_pipe_rhs(rhs, lhs)

  expr <- rlang::new_quosure(rlang::expr({
    # nolint start
    res <- purrr::map(., function(x) {
      x %>>=% rlang::UQE(rhs)
    })
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
