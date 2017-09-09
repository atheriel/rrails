testthat::context("basic bind functionality")

testthat::test_that("bind-result returns a result", {
    x <- as_result(10) %>>=% log()
    y <- as_result("a") %>>=% log()

    testthat::expect_is(x, class = "result")
    testthat::expect_is(y, class = "result")

    # Continuation in the case of errors:
    testthat::expect_is(y %>>=% log(), class = "result")
})

testthat::test_that("bind-result handles bare functions", {
    testthat::expect_silent(as_result(10) %>>=% log)
})

testthat::test_that("bind-result handles functions with explicit dots in first", {
    # Dots in the function itself.
    testthat::expect_equal(as_result(10) %>>=% log(.), as_result(log(10)))
})

testthat::test_that("bind-result handles functions with explicit dots", {
    testthat::expect_equal(as_result(10) %>>=% log(10, .), as_result(1))
})

testthat::test_that("bind-result handles functions with operations on dots", {
    testthat::expect_equal(as_result(10) %>>=% log(. / 2), as_result(log(5)))
})

testthat::context("parentheses and anonymous functions")

testthat::test_that("bind-result handles parenthesized anonymous functions", {
    testthat::expect_equal(as_result(10) %>>=% (function(x) log(x / 2)),
                           as_result(log(5)))
    testthat::expect_equal(as_result(10) %>>=% (function(x) log(x / 2))(),
                           as_result(log(5)))
})

testthat::test_that("bind-result allows bare anonymous functions", {
    testthat::expect_equal(as_result(10) %>>=% function(x) log(x / 2),
                           as_result(log(5)))
})

testthat::test_that("bind-result disallows non-function parenthesized expressions", {
    testthat::expect_error(as_result(10) %>>=% (. + 10))
    testthat::expect_error(as_result(10) %>>=% ((function(x) x)))
    testthat::expect_error(as_result(10) %>>=% (. + 10)(100))
})

testthat::context("blocks")

testthat::test_that("bind-result handles basic blocks", {
    testthat::expect_equal(as_result(10) %>>=% { log(. / 2) },
                           as_result(log(5)))
})

testthat::test_that("bind-result handles complex blocks", {
    testthat::expect_equal(as_result(10) %>>=% {
        base <- . * 2
        function() log(., base)
    }(), as_result(log(10, 20)))
})

testthat::context("non-standard evaluation in bind-result")

testthat::test_that("bind-result env has the correct parent env", {
    # Override the log function in this environment.
    log <- sin
    x <- as_result(10) %>>=% log()

    testthat::expect_equal(x, as_result(sin(10)))
})

testthat::test_that("bind-result anonymous functions have the correct parent env", {
    # Override the log function in this environment.
    log <- sin
    x <- as_result(10) %>>=% (function(x) log(x))

    testthat::expect_equal(x, as_result(sin(10)))
})
