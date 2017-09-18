testthat::context("basic bind functionality")

testthat::test_that("bind-default acts like a pipe", {
  testthat::expect_equal(10 %>>=% log(), log(10))
  testthat::expect_error("a" %>>=% log())
})

testthat::test_that("bind-result returns a result", {
  x <- as_result(10) %>>=% log()
  y <- as_result("a") %>>=% log()

  testthat::expect_is(x, class = "result")
  testthat::expect_is(y, class = "result")

  # Continuation in the case of errors:
  testthat::expect_is(y %>>=% log(), class = "result")
})

testthat::test_that("bind-list applies bind to its elements", {
  testthat::expect_equal(list(a = 1, b = 10, c = 100) %>>=% log(base = 10),
                         list(a = 0, b = 1, c = 2))
})

testthat::context("basic right-hand-side syntax parsing")

testthat::test_that("bind-result handles bare functions", {
  testthat::expect_equal(as_result(10) %>>=% log,
                         as_result(log(10)))
})

testthat::test_that("bind-result handles explicit dots correctly", {
  # Recognizes explicit dots.
  testthat::expect_equal(as_result(10) %>>=% log(.), as_result(log(10)))
  testthat::expect_equal(as_result(10) %>>=% log(10, .), as_result(1))

  # Does not recognize dots if they are part of an operation.
  testthat::expect_equal(as_result(10) %>>=% log(. / 2),
                         as_result(log(10, 5)))
})

testthat::context("parentheses and anonymous function syntax")

testthat::test_that("bind handles parenthesized anonymous functions", {
  testthat::expect_equal(as_result(10) %>>=% (function(x) log(x / 2)),
                         as_result(log(5)))
  testthat::expect_equal(as_result(10) %>>=% (function(x) log(x / 2))(),
                         as_result(log(5)))
})

testthat::test_that("bind allows bare anonymous functions", {
  testthat::expect_equal(as_result(10) %>>=% function(x) log(x / 2),
                         as_result(log(5)))
})

testthat::test_that("bind forbids other parenthesized expressions", {
  testthat::expect_error(as_result(10) %>>=% (. + 10))
  testthat::expect_error(as_result(10) %>>=% (
    (function(x) x)
  ))
  testthat::expect_error(as_result(10) %>>=% (. + 10)(100))
})

testthat::context("block syntax")

testthat::test_that("bind handles basic blocks", {
  testthat::expect_equal(as_result(10) %>>=% {
    log(. / 2)
  },
  as_result(log(5)))
})

testthat::test_that("bind handles complex blocks", {
  testthat::expect_equal(as_result(10) %>>=% {
    base <- . * 2
    function() log(., base)
  }(),
  as_result(log(10, 20)))
})

testthat::context("testing results and unwrapping")

testthat::test_that("result inspection works as expected", {
  testthat::expect_true(result_is_ok(as_result(10)))
  testthat::expect_error(result_is_ok(10))
})

testthat::test_that("bind-results can be inspected and unwrapped", {
  x <- list(a = 1, b = 10, c = 100) %>>=% as_result() %>>=% log(base = 10)
  y <- list(a = "1", b = 10, c = 100) %>>=% as_result() %>>=% log(base = 10)

  testthat::expect_true(result_is_ok(x))
  testthat::expect_false(result_is_ok(y))

  testthat::expect_equal(unwrap(x), list(a = 0, b = 1, c = 2))
  testthat::expect_error(unwrap(y))
})

testthat::context("bind handles non-standard evaluation correctly")

testthat::test_that("bind-result env has the correct parent env", {
  # Override the log function in this environment.
  log <- sin
  x <- as_result(10) %>>=% log()
  y <- as_result(10) %>>=% (function(x) log(x))

  testthat::expect_equal(x, as_result(sin(10)))
  testthat::expect_equal(y, as_result(sin(10)))
})

testthat::test_that("bind works inside of dplyr verbs", {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    df <- dplyr::tibble(x = c(1, 10, 100))

    df <- dplyr::mutate(df, logs = log(x),
                        safe_logs = as.list(x) %>>=%
                          as_result() %>>=%
                          log()
                        )

    testthat::expect_true(result_is_ok(df$safe_logs))

    df <- dplyr::mutate(df, unwrapped_logs = unwrap(safe_logs),
                        log_list = as.list(logs))

    testthat::expect_equal(df$log_list, df$unwrapped_logs)
  }
})
