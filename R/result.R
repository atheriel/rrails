#' The Result object
#'
#' \code{is_result} and \code{as_result} test whether an object is a result and
#' coerce to a result object, respectively.
#'
#' @param x An object.
#' @param ... Further arguments passed on to other methods (unused by the
#'   default methods).
#'
#' @rdname result
#'
#' @export
as_result <- function(x, ...) {
    UseMethod("as_result")
}

#' @rdname result
#'
#' @export
as_result.default <- function(x, ...) {
    structure(list(result = x, error = NULL),
              class = "result")
}

#' @rdname result
#'
#' @export
as_result.error <- function(x, ...) {
    structure(list(result = NULL, error = x),
              class = "result")
}

#' @rdname result
#'
#' @export
as_result.result <- function(x, ...) x

#' @rdname result
#'
#' @export
is_result <- function(x) inherits(x, "result")

#' @rdname result
#'
#' @export
result_is_ok <- function(x) {
  stopifnot(is_result(x))
  is.null(x$error)
}

#' @rdname result
#'
#' @export
result_is_error <- function(x) {
  stopifnot(is_result(x))
  !is.null(x$error)
}

#' @rdname result
#'
#' @export
print.result <- function(x, ...) {
    if (typeof(x$error) == "NULL") {
        cat("Result is ok:\n")
        print(x$result, ...)
    } else {
        cat("Result has an error:\n")
        print(x$error, ...)
    }
}

#' @rdname result
#'
#' @export
unwrap_result <- function(x) {
  if (!is_result(x)) return(x)
  if (is.null(x$error)) {
    x$result
  } else {
    stop(x$error)
  }
}
