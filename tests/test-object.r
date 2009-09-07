context("Basic object properties")

test_that("clone creates fields", {
  a <- Object$clone(a = 1, b = 2)
  
  expect_that(a$a, equals(1))
  expect_that(a$b, equals(2))
  
})