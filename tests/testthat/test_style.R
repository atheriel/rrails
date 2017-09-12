library(lintr)

context("package style")

test_that("package style matches the tidyverse", {
  lintr::expect_lint_free()
})
