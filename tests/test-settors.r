context("Gettors and settors")
 
test_that("simple settor works", {
  a <- Object$clone()

  a$set_double <- function(value) {
    core(self)$set_slot("double", value * 2)
  }

  a$double <- 5
  expect_that(a$double, equals(10))
})

test_that("settors are inherited", {
  a <- Object$clone()
  a$set_double <- function(value) {
    core(self)$set_slot("double", value * 2)
  }

  b <- a$clone()

  b$double <- 5
  expect_that(b$double, equals(10))
})

test_that("value overrides gettor", {
  a <- Object$clone()

  a$get_a <- function() 10
  expect_that(a$a, equals(10))

  a$a <- 5
  expect_that(a$a, equals(5))
})