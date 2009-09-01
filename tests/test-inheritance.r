context("Inheritance")

test_that("values in parent inherited by children", {
  a <- Object$clone()
  a$a <- 5

  b <- a$clone()
  c <- b$clone()

  assert_identical(a$a, 5)
  assert_identical(b$a, 5)
  assert_identical(c$a, 5)
})


test_that("values in children override values in parent", {
  a <- Object$clone()
  b <- a$clone()
  c <- b$clone()

  b$b <- 5
  c$b <- 10

  expect_error(a$b)
  assert_identical(b$b, 5)
  assert_identical(c$b, 10)
})

test_that("inheritance is dynamic", {
  a <- Object$clone()
  a$a <- 1

  b <- a$clone()
  a$b <- 2

  assert_identical(b$b, 2)
})