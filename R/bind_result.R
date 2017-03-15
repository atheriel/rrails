#' Bind result objects
#'
#' @rdname bind-result
#'
#' @export
`%>>=%.result` <- function(lhs, rhs) {
    if (typeof(lhs$error) == "NULL") {
        env <- new.env(parent = parent.frame())
        args <- match.call()

        if (magrittr:::is_first(args$rhs)) {
            body <- magrittr:::prepare_first(args$rhs)
            env[["_fn"]] <- eval(call("function", as.pairlist(alist(. = )),
                                      body), env, env)
            env[["_lhs"]] <- lhs$result
            expr <- quote({
                res <- tryCatch(`_fn`(`_lhs`), error = function(e) e)
                as_result(res)
            })
            eval(expr, env, env)
        } else {
            stop("not yet implemented")
        }
    }
    # Otherwise, skip executing the rhs and simply pass the lhs on.
    else {
        lhs
    }
}
