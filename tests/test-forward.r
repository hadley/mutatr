context("Test method forwarding")

test_that("accessing an undefined property returns an error", {
  a <- Object$clone()
  expect_that(a$a, throws_error())
})

test_that("basic forwarding works", {
  a <- Object$clone()
  a$forward <- function(name) name
  expect_that(a$a, equals("a"))
  expect_that(a$b, equals("b"))
})

test_that("actual value returned, if present", {
  a <- Object$clone()
  a$forward <- function(name) name

  a$a <- 15
  expect_that(a$a, equals(15))
})

test_that("forwarding is inherited by children", {
  a <- Object$clone()
  a$forward <- function(name) name
  a$a <- 15

  b <- a$clone()
  b$d <- 5
  expect_that(b$a, equals(15))
  expect_that(b$b, equals("b"))
  expect_that(b$c, equals("c"))
  expect_that(b$d, equals(5))
})