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

testthat::context("non-standard evaluation in bind-result")

testthat::test_that("bind-result env has the correct parent env", {
    # Override the log function in this environment.
    log <- sin
    x <- as_result(10) %>>=% log()

    testthat::expect_equal(x, as_result(sin(10)))
})
