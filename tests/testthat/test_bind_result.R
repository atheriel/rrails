testthat::context("basic bind functionality")

testthat::test_that("bind-result returns a result", {
    x <- as_result(10) %>>=% log()
    y <- as_result("a") %>>=% log()

    testthat::expect_is(x, class = "result")
    testthat::expect_is(y, class = "result")

    # Continuation in the case of errors:
    testthat::expect_is(y %>>=% log(), class = "result")
})
