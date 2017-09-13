# rrails

[![Build Status](https://travis-ci.org/atheriel/rrails.svg?branch=master)](https://travis-ci.org/atheriel/rrails)

This package provides a new pipe operator, `%>>=%` (pronounced "bind") for
writing failure-tolerant R code. Its approach is inspired by Scott Wlaschin's
articles and talks about ["Railroad Oriented Programming"](https://fsharpforfunandprofit.com/rop/) in
in the F# language.

## Overview

The bind operator works by wrapping the right-hand side in a `tryCatch` block
and returning an S3 object of class `"result"` wrapping either the result of the
call, or the error if one occured. This is useful when dealing with, for
example, untrusted user input:


```r
untrusted_input <- 100
(good <- as_result(untrusted_input) %>>=% log(base = 10) %>>=% sin())
#> Result is ok:
#> [1] 0.9092974
```

Which works just fine, versus


```r
untrusted_input <- "one hundred"
(bad <- as_result(untrusted_input) %>>=% log(base = 10) %>>=% sin())
#> Result has an error:
#> <simpleError in log(., base = 10): non-numeric argument to mathematical function>
```

which does not. Notice that the code does not actually raise an error.

You can inspect the return value and "unwrap" it (to borrow the term from the
Rust language) to get the underlying value, or raise its error if it has one:


```r
if (result_is_ok(good)) unwrap_result(good)
#> [1] 0.9092974
if (!result_is_error(bad)) unwrap_result(bad)
```

As a slightly more realistic example, the bind operator allows writing code that
looks like the following:


```r
library(magrittr)

validate_username <- function(user) {
  if (nchar(user$name) > 10) {
    stop("username too long; must be 10 chars or fewer")
  }
  user
}

validate_email <- function(user) {
  if (!grepl("[a-zA-Z]+@[a-zA-Z\\.]+", user$email)) {
    stop("invalid email address")
  }
  user
}

list(name = "Dr. Oak", email = "oak@@pkmn.jk") %>%
  as_result() %>>=%
  validate_username() %>>=%
  validate_email()
#> Result has an error:
#> <simpleError in validate_email(.): invalid email address>

list(name = "Albus Dumbledor", email = "albus@hogwarts.co.uk") %>%
  as_result() %>>=%
  validate_username() %>>=%
  validate_email()
#> Result has an error:
#> <simpleError in validate_username(.): username too long; must be 10 chars or fewer>
```

## Installation

**rrails** is not yet available on CRAN, so for now you'll have to install it
from GitHub:


```r
# install.packages("devtools")
devtools::install_github("atheriel/rrails")
```
