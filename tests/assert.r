# Make testing fun.
# Inspired by many ruby libraries: shoulda, rspec, autotest, bacon, 
# expectations, matchy.
# http://robots.thoughtbot.com/post/159806175
#
# Stateful interface, based around how tests are typically written in R 
# already - interface shouldn't look object oriented because most R code isn't # oo. 
# 
# Should be able to give a list of directories.  Checks every x seconds for
# changes, then re-sources files and reruns all tests.
#
# When run stand-alone (without global suite object), each just immediately
# returns results (as everything should work well interactively).  When run
# as part of a test suite, each assertion contributes to the global object, 
# which produces a final report.
# 
# assert assert_equal   assert_in_delta   assert_instance_of  
# assert_match   assert_null   assert_no_match  
# assert_not_equal   assert_not_null assert_nothing_thrown assert_throws 
# 
# should_be_error
# should_be_identical
# should_be_true
# 
# 
# should_be_same_as_last_time
#   default lookup default should hash of deparse + current test context
#
# Ancestor iteration:
#   Simple hierarchical case: ...F..
#   * a not identical to b
# 
#   Circular case: ...
#

all_ancestors <- function(obj) {
  i <- ancestor_iterator(obj)
  i$get_next() # Skip self
  
  ancestors <- list()
  while(i$has_next()) {
    ancestors <- c(ancestors, list(i$get_next()))
  }
  ancestors
}

assert_has_ancestors <- function(obj, ancestors) {
  stopifnot(identical(all_ancestors(obj), ancestors))
  cat(".")
}

assert_identical <- function(a, b) {
  ok <- identical(a, b)
  if (!ok) {
    msg <- paste(deparse(substitute(a)), " != ", deparse(substitute(b)), sep ="")
    stop(msg, call. = FALSE)
  }
  cat(".")  
}
expect_error <- function(x, message = NULL) {
  res <- try(force(x), TRUE)
  
  stopifnot(inherits(res, "try-error"))
  if (!is.null(message)) {
    assert_matches(res, message)
  }
  cat(".")
}

assert <- function(x) {
  stopifnot(identical(x, TRUE))
  cat(".")
}

assert_matches <- function(test, regexp) {
  assert(all(grepl(regexp, test)))
  cat(".")
}

context <- function(desc) {
  cat(desc, "\n")
}

test_that <- function(desc, code) {
  cat("  ", desc, ": ", sep = "")
  
  env <- new.env(parent = globalenv())
  eval(substitute(code), env)
  cat("\n")
}
