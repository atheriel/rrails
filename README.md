<!-- README.md is generated from README.Rmd. Please edit that file. -->

# rrails

[![Build Status](https://travis-ci.org/atheriel/rrails.svg?branch=master)](https://travis-ci.org/atheriel/rrails)

This package provides a new pipe operator, `%>>=%` (pronounced "bind") for
writing failure-tolerant R code. Its approach is inspired by Scott Wlaschin's
articles and talks about ["Railroad Oriented Programming"](https://fsharpforfunandprofit.com/rop/) in
in the F# language.

## A Motivating Problem

Let's say you are faced with the following scenario, quite familiar to some:

> You have a list of URLs that point to some API. You need to download each of
> the JSON objects from these URLs, parse a few fields out of them, and collect
> the results into a data frame.

The code you write to do this looks something like the following, adopting some
"Tidyverse" idioms:


```r
entries <- urls %>%
  purrr::map(httr::GET) %>%
  purrr::map(my_parsing_fn) %>%
  dplyr::bind_rows()
```

The problem with this code is that it can -- and will -- break in numerous ways.
Perhaps some of your API requests time out, or the URLs point to nonexistant
endpoints. Perhaps some of the JSON objects don't have the field you are after,
or have it under a different name than you expected. Perhaps the internet
connection at the cafe cuts out halfway through the list. If some of these
computations are expensive, this is a seriously headache.

The **purrr** library has a `safely` function that wraps an input function to
prevent it from failing, e.g. `safe_log <- purrr::safely(log)`. But it doesn't
work nicely with the pipe operator `%>%`, and won't compose with another
function that isn't expecting a `list(result = ..., error = NULL)`. You can't
write what would seem like a natural, "safe" pipeline:


```r
entries <- urls %>%
  safely(httr::GET) %>%
  safely(my_parsing_fn)
```

Writing these "safe" pipelines is the purpose of the **rails** library. Using
the bind operator, `%>>=%`, you can rewrite the pipeline as follows:


```r
raw_entries <- urls %>>=%
  as_result() %>>=%
  httr::GET() %>>=%
  my_parsing_fn()
```

This won't actually return your entries, by design. Instead, it will return a
list of `"result"` objects, which you should inspect for errors before
"unwrapping" the result:


```r
# Check if everything worked:
result_is_ok(raw_entries)
#> TRUE

# "Unwrap" the result, if so:
entries <- unwrap(raw_entries) %>%
  dplyr::bind_rows()
```

The `"result"` objects will carefully collect and catalogue the errors in your
processing pipeline. If there are none, great! Otherwise, you can inspect
exactly which operations failed, and why, by examinging the elements where
`result_is_error`:


```r
purrr::keep(raw_entries, result_is_error)
#> [[37]]
#> Result has an error:
#> ...
```

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
if (result_is_ok(good)) unwrap(good)
#> [1] 0.9092974
if (!result_is_error(bad)) unwrap(bad)
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
