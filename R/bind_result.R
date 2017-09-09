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

  # Temporarily give the `.` symbol the LHS value in the caller environment.
  # Source: https://gist.github.com/lionel-/10cd649b31f11512e4aea3b7a98fe381
  env <- rlang::caller_env()
  if (rlang::env_has(env, ".")) {
    dot <- env$dot
    on.exit(rlang::env_bind(.env = env, `.` = dot))
  } else {
    on.exit(rlang::env_unbind(env, "."))
  }
  env$. <- lhs$result

  # Turn bare symbols into functions.
  if (rlang::is_symbol(rlang::f_rhs(rhs))) {
    rlang::f_rhs(rhs) <- rlang::lang(rlang::f_rhs(rhs))
  }

  # Prevent some pathelogical inputs, such as `x %>>=% "cats"`.
  if (!rlang::is_lang(rlang::f_rhs(rhs))) {
    stop("RHS must be callable; '", rlang::f_rhs(rhs), "' is not",
         call. = FALSE)
  }

  # Handle parenthetical inputs and bare anonymous functions.
  rhs <- normalize_anon_fns(rhs)

  # For debugging:
  # return(rhs)

  # Blocks are simply executed with `.` bound.
  if (!is_block(rhs)) {
    rhs <- ensure_dot(rhs)
  }

  expr <- rlang::new_quosure(rlang::expr({
    res <- tryCatch(rlang::UQE(rhs), error = function(e) e)
    as_result(res)
  }), env = env)
  expr
  rlang::eval_tidy(expr)
}

normalize_anon_fns <- function(expr) {
  stopifnot(rlang::is_quosure(expr) && rlang::is_lang(rlang::f_rhs(expr)))
  rhs <- rlang::f_rhs(expr)
  # First, check for bare anonymous functions and wrap them in parentheses.
  if (identical(function_symbol, rlang::node_car(rhs))) {
    rlang::f_rhs(expr) <- rlang::lang(rhs)
    return(expr)
  }
  # Look for the first part of the pairlist that isn't another expression. This
  # supports forms like `(function(x, y) log(x, y))(., 20)`.
  while (rlang::is_lang(rlang::node_car(rhs))) {
    rhs <- rlang::node_car(rhs)
  }
  if (identical(paren_symbol, rlang::node_car(rhs))) { # Found a parenthesis.
    inside <- rlang::node_cadr(rhs)
    if (!rlang::is_lang(inside) ||
          !identical(function_symbol, rlang::node_car(inside))) {
      stop("parenthesized expressions must contain an anonymous function",
           call. = FALSE)
    }

    # If the right-hand-side doesn't have arguments, add them. This effectively
    # turns forms like `(function(x) x)` into `(function(x) x)()`, meaning they
    # can be handled natively by `ensure_dot()`.
    if (!rlang::is_lang(rlang::node_car(rlang::f_rhs(expr)))) {
      rlang::f_rhs(expr) <- rlang::lang(rlang::f_rhs(expr))
    }
  }
  expr
}

is_block <- function(expr) {
  stopifnot(rlang::is_quosure(expr) && rlang::is_lang(rlang::f_rhs(expr)))
  car <- rlang::node_car(rlang::f_rhs(expr))
  # Look for the first part of the pairlist that isn't another expression. This
  # supports forms like `{ y <- . * 2; function() { log(., y) } }()`, although
  # it seems unlikely they are useful.
  while(rlang::is_lang(car)) {
    car <- rlang::node_car(car)
  }
  identical(bracket_symbol, car)
}

ensure_dot <- function(expr) {
  stopifnot(rlang::is_quosure(expr) && rlang::is_lang(rlang::f_rhs(expr)))
  # The right-hand side is a CONS where the CAR is the function symbol and the
  # CDR is the pairlist of arguments (or NULL).
  args <- rlang::node_cdr(rlang::f_rhs(expr))
  # Loop over the CONS cells.
  arg <- args
  while (!rlang::is_null(arg)) {
    # The CAR of an argument is its value, as in `name = value` or just `value`
    # if it is not a named argument. We're looking for dots in these values. If
    # we find one, the quosure is fine as-is.
    if (identical(dot_symbol, rlang::node_car(arg))) {
      return(expr)
    }
    arg <- rlang::node_cdr(arg)
  }
  # No dots found. Prefix the argument CONS with one, in place.
  new_args <- rlang::node(dot_symbol, args)
  rlang::mut_node_cdr(rlang::f_rhs(expr), new_args)
  invisible(expr)
}

paren_symbol <- quote(`(`)
function_symbol <- quote(`function`)
dot_symbol <- quote(.)
bracket_symbol <- quote(`{`)
