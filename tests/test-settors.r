context("Settors")
 
test("Test base case", {
  a <- Object$clone()

  a$set_double <- function(value) {
    core(self)$set_slot("double", value * 2)
  }

  a$double <- 5
  assert_identical(a$double, 10)
})

test("Test inheritance", {
  b <- a$clone()

  b$double <- 5
  assert_identical(b$double, 10)
})