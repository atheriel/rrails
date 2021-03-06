
<!-- README.md is generated from README.Rmd. Please edit that file. -->
rrails
======

[![Build Status](https://travis-ci.org/atheriel/rrails.svg?branch=master)](https://travis-ci.org/atheriel/rrails)

This package provides a new pipe operator, `%>>=%` (pronounced "bind") for writing failure-tolerant R code. Its approach is inspired by Scott Wlaschin's articles and talks about ["Railroad Oriented Programming"](https://fsharpforfunandprofit.com/rop/) in in the F\# language, as well as the error-handling paradigms of the Rust and Haskell languages.

A Motivating Problem
--------------------

Let's say you are faced with the following scenario, quite familiar to some:

> You have a list of URLs that point to some API. You need to download each of the JSON objects from these URLs, parse a few fields out of them, and collect the results into a data frame.

The code you write to do this looks something like the following, adopting some "Tidyverse" idioms:

``` r
entries <- urls %>%
  purrr::map(httr::GET) %>%
  purrr::map(my_parsing_fn) %>%
  dplyr::bind_rows()
```

The problem with this code is that it can -- and will -- break in numerous ways. Perhaps some of your API requests time out, or the URLs point to nonexistent endpoints. Perhaps some of the JSON objects don't have the field you are after, or have it under a different name than you expected. Perhaps the internet connection at the cafe cuts out halfway through the list. If some of these computations are expensive, this is a seriously headache.

The **purrr** library has a `safely` function that wraps an input function to prevent it from failing, e.g. `safe_log <- purrr::safely(log)`. But it doesn't work nicely with the pipe operator `%>%`, and won't compose with another function that isn't expecting a `list(result = ..., error = NULL)`. You can't write what would seem like a natural, "safe" pipeline:

``` r
entries <- urls %>%
  safely(httr::GET) %>%
  safely(my_parsing_fn)
```

Writing these "safe" pipelines is the purpose of the **rrails** library. Using the bind operator, `%>>=%`, you can rewrite the pipeline as follows:

``` r
raw_entries <- urls %>>=%
  as_result() %>>=%
  httr::GET() %>>=%
  my_parsing_fn()
```

This won't actually return your entries, by design. Instead, it will return a list of `"result"` objects, which you should inspect for errors before "unwrapping" the result:

``` r
# Check if everything worked:
result_is_ok(raw_entries)
#> TRUE

# "Unwrap" the result, if so:
entries <- unwrap(raw_entries) %>%
  dplyr::bind_rows()
```

The `"result"` objects will carefully collect and catalogue the errors in your processing pipeline. If there are none, great! Otherwise, you can inspect exactly which operations failed, and why, by examinging the elements where `result_is_error`:

``` r
purrr::keep(raw_entries, result_is_error)
#> [[37]]
#> Result has an error:
#> ...
```

How Does it Work?
-----------------

The bind operator differs from other pipe operators in R in that it is generic, although it has been tuned for the use case of handling errors. Bind does have a default implementation -- it will simply act like the pipe operator from the **magrittr** package -- but it's not a good idea to use it in this way, since any object that has a different implementation may do something completely unexpected. For example, an R `data.frame` inherits from the `"list"` class, which has its own bind method -- one that is unlikely to do what you want.

**rrails** comes with two generic S3 methods for bind which are intended to be complementary: one for `"result"` objects, and one for `"list"` objects.

For `"result"` objects, the bind operator wraps the right-hand side in a `tryCatch` block to catch any errors. If an error occurs, it is captured inside of the returned value, also a `"result"`. If there is no error, the returned value is *also* captured inside a `"result"`.

For `"list"` objects, the bind operator does something much simpler: it applies the bind operator to the right-hand side and each element of the list itself -- returning a list of the outputs. This enables a kind of concurrency.

So, to revisit the example shown above:

``` r
raw_entries <- urls %>>=%
  as_result() %>>=%
  httr::GET() %>>=%
  my_parsing_fn()
```

If `urls` is a character vector of URLs, this pipeline will wrap the vector in a result and evaluate the `httr::GET()` and `my_parsing_fn()` calls in a `tryCatch` block. On the other hand, if `urls` is a list of character vectors, the pipeline will wrap each element in a `"result"` object, evaluate the `httr::GET()` and `my_parsing_fn()` calls in a `tryCatch` block for each element, and return a list of `"result"` objects containing the results.

Because the bind operator is generic, you can extend it however you'd like for your own classes. However, the idioms used in this library basically expect bind to be used in places where a "container" object could have the right-hand side applied to its contents in different ways depending on the context. An example would be to write an implementation for the `response` object from the **httr** package that only applies the right-hand side to requests that return status code 200 (e.g. when the HTML request worked without a hitch).

Installation
------------

**rrails** is not yet available on CRAN, so for now you'll have to install it from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("atheriel/rrails")
```
