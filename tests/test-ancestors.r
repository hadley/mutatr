context("Ancestor iterator")

test_that("simple hierarchical case works", {
  a <- Object$clone()
  b <- a$clone()
  c <- b$clone()

  expect_that(a, has_ancestors(Object))
  expect_that(b, has_ancestors(a, Object))
  expect_that(c, has_ancestors(b, a, Object))

  expect_that(c$has_ancestor(b), is_true())
  expect_that(c$has_ancestor(a), is_true())
  expect_that(c$has_ancestor(Object), is_true())
  expect_that(c$has_ancestor(c), is_false())
})

test_that("circular case works", {
  d <- Object$clone()
  e <- Object$clone()
  e$append_proto(d)
  d$append_proto(e)

  expect_that(e, has_ancestors(Object, d))
  expect_that(d, has_ancestors(Object, e))
})