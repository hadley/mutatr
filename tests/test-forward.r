context("Test method forwarding")

test_that("accessing an undefined property returns an error", {
  a <- Object$clone()
  expect_error(a$a)
})

test_that("basic forwarding works", {
  a <- Object$clone()
  a$forward <- function(name) name
  assert_identical(a$a, "a")
  assert_identical(a$b, "b")
})

test_that("actual value returned, if present", {
  a <- Object$clone()
  a$forward <- function(name) name

  a$a <- 15
  assert_identical(a$a, 15)  
})

test_that("forwarding is inherited by children", {
  a <- Object$clone()
  a$forward <- function(name) name
  a$a <- 15

  b <- a$clone()
  b$d <- 5
  assert_identical(b$a, 15)
  assert_identical(b$b, "b")
  assert_identical(b$c, "c")
  assert_identical(b$d, 5)
})