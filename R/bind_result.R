#' Bind result objects
#'
#' @rdname bind-result
#'
#' @export
`%>>=%.result` <- function(lhs, rhs) {
    if (typeof(lhs$error) == "NULL") {
        env <- new.env(parent = parent.frame())
        args <- match.call()
        rhs <- args$rhs

        }

        body <- if (magrittr:::is_function(rhs)) {
            # Turn `fn` into `fn(.)`.
            magrittr:::prepare_function(rhs)
        }
        else if (magrittr:::is_first(rhs)) {
            # Turn `fn(x, y, z)` into `fn(., x, y, z)`.
            magrittr:::prepare_first(rhs)
        }
        else {
            stop("Unsupported RHS: ", deparse(rhs), call. = FALSE)
        }

        env[["_fn"]] <- eval(call("function", as.pairlist(alist(. = )),
                                  body), env, env)
        env[["_lhs"]] <- lhs$result
        expr <- quote({
            res <- tryCatch(`_fn`(`_lhs`), error = function(e) e)
            as_result(res)
        })
        eval(expr, env, env)
    }
    # Otherwise, skip executing the rhs and simply pass the lhs on.
    else {
        lhs
    }
}
