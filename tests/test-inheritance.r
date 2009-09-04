context("Inheritance")

test_that("values in parent inherited by children", {
  a <- Object$clone()
  a$a <- 5

  b <- a$clone()
  c <- b$clone()

  expect_that(a$a, equals(5))
  expect_that(b$a, equals(5))
  expect_that(c$a, equals(5))
})

test_that("values in children override values in parent", {
  a <- Object$clone()
  b <- a$clone()
  c <- b$clone()

  b$b <- 5
  c$b <- 10

  expect_that(a$b, throws_error("not found"))
  expect_that(b$b, equals(5))
  expect_that(c$b, equals(10))
})

test_that("inheritance is dynamic", {
  a <- Object$clone()
  a$a <- 1

  b <- a$clone()
  a$b <- 2

  expect_that(b$b, equals(2))
})