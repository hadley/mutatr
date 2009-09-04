context("Test parent function")

test_that("object doesn't have a parent", {
  expect_that(Object$parent(), throws_error("no parent"))
})

test_that("simple linear inheritance works", {
  a <- Object$clone()
  a$a <- 5

  b <- a$clone()
  b$a <- 10

  expect_that(b$parent()$a, equals(5))

  a$f <- function() 1
  b$f <- function() 1 + self$parent()$f()

  expect_that(b$f(), equals(2))

  c <- b$clone()
  c$f <- function() 1 + self$parent()$f()

  expect_that(c$f(), equals(3))
})

test_that("circular inheritance works", {
  d <- Object$clone()
  e <- Object$clone()
  e$prepend_proto(d)
  d$prepend_proto(e)

  d$a <- 1
  e$a <- 2

  expect_that(d$a, equals(1))
  expect_that(d$parent()$a, equals(2))
  expect_that(d$parent()$parent()$a, equals(1))
  expect_that(e$a, equals(2))
  expect_that(e$parent()$a, equals(1))
  expect_that(e$parent()$parent()$a, equals(2))
})

test_that("assignment should happen in original object", {
  a <- Object$clone()
  b <- a$clone()

  # Check contexts
  expect_that(a$context, equals(a))
  expect_that(a$parent()$context, equals(a))
  expect_that(b$context, equals(b))
  expect_that(b$parent()$context, equals(b))

  a$a <- 10
  b$a <- 15
  a$f <- function() {
    self$a <- 5
  }
  b$f <- function() self$parent()$f()

  b$f()
  expect_that(a$a, equals(10))
  expect_that(b$a, equals(5))
})