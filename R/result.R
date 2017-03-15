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
print.result <- function(x, ...) {
    if (typeof(x$error) == "NULL") {
        cat("Result:\n")
        print(x$result, ...)
    } else {
        cat("Error:\n")
        print(x$error, ...)
    }
}
