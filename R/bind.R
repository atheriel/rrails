#' The 'bind' operator
#'
#' \code{\%>>=\%} (pronounced "bind") is a generic operator intended to match
#' the semantics of \code{>>=} in other functional programming languages, such
#' as F# or Haskell.
#'
#' @usage lhs \%>>=\% rhs
#'
#' @rdname bind
#'
#' @export
`%>>=%` <- function(lhs, rhs) {
    UseMethod("%>>=%")
}
