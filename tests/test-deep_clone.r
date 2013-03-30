library(testthat)
library(mutatr)

context("Deep copying")

test_that("Diamond inheritance processed correctly", {
  # Make diamond inheritance structure

  #   A
  #  / \
  # B   C
  #  \ /
  #   D

  A <- Object$clone()$do({
    self$name <- "A"
  })

  B <- A$clone()$do({
    self$name <- "B"
  })

  C <- A$clone()$do({
    self$name <- "C"
  })

  D <- B$clone()$do({
    self$name <- "D"
  })

  D$append_proto(C)

  D.copy <- D$deep_copy()


  D.copy$protos[[1]]$name
  D.copy$protos[[2]]$name

  A1 <- D.copy$protos[[1]]$protos[[1]]
  A2 <- D.copy$protos[[2]]$protos[[1]]

  expect_identical(A1, A2)
  expect_false(identical(A1, A))
})
