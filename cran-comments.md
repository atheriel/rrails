## Release summary

This is a new submission.

## Test environments
* local manjaro linux, R 3.4.2
* local macOS, R 3.3.2
* ubuntu 14.04 (on travis-ci), R 3.4.1
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aaron Jacobs <atheriel@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Wlaschin's (9:35)

  This is indeed a new submission. The potentially mis-spelled word is a name,
  and I have verified that it is correct.

* checking R code for possible problems ... NOTE
%>>=%.list: no visible binding for global variable ‘.’
Undefined global functions or variables:
  .

  This is due to the fact that `.` is bound manually in the environment (see
  R/normalize.R#26-28), and the block in question uses non-standard evaluation.

## Downstream dependencies

There are currently no downstream dependencies for this package.
