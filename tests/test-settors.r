context("Settors")
 
test_that("simple case works", {
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