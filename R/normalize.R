normalize_pipe_rhs <- function(rhs, binding) {
  if (!rlang::is_quosure(rhs)) {
    stop(paste("'rhs' parameter must be a quosure. Try calling",
               "'rhs <- rlang::enquo(rhs)' first"))
  }

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

  # Blocks are simply executed with `.` bound.
  if (!is_block(rhs)) {
    rhs <- ensure_dot(rhs)
  }

  # Modify the environment so that `.` is bound to `binding`.
  rlang::f_env(rhs) <- rlang::child_env(.parent = rlang::f_env(rhs), . = binding)

  rhs
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
