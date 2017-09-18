#' The Result object
#'
#' \code{is_result} and \code{as_result} test whether an object is a result and
#' coerce to a result object, respectively.
#'
#' @param x An object.
#' @param ... Further arguments passed on to other methods (unused by the
#'   default methods).
#'
#' @name result
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
result_is_ok <- function(x) {
  UseMethod("result_is_ok")
}

#' @rdname result
#'
#' @export
result_is_ok.result <- function(x) is.null(x$error)

#' @rdname result
#'
#' @export
result_is_ok.list <- function(x) {
  all(purrr::map_lgl(x, result_is_ok))
}

#' @rdname result
#'
#' @export
result_is_error <- function(x) {
  UseMethod("result_is_error")
}

#' @rdname result
#'
#' @export
result_is_error.result <- function(x) !is.null(x$error)

#' @rdname result
#'
#' @export
result_is_error.list <- function(x) {
  any(purrr::map_lgl(x, result_is_error))
}

#' @rdname result
#'
#' @export
unwrap <- function(x) {
  UseMethod("unwrap")
}

#' @rdname result
#'
#' @export
unwrap.result <- function(x) {
  if (is.null(x$error)) {
    x$result
  } else {
    stop(x$error)
  }
}

#' @rdname result
#'
#' @export
unwrap.list <- function(x) {
  purrr::map(x, unwrap)
}
